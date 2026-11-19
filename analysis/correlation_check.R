# --- Are players' scoring errors independent? --------------------------------------------
# Both fantasy apps (this one and the dynasty playoff-odds site) draw every player's score
# independently around its projection and add spreads in quadrature. Real scores are linked:
# teammates share an offense's game script, opposing players share the same game. This script
# MEASURES that from the 2023-2025 backtest instead of asserting it:
#
#   1. Standardized projection errors z = (actual - projected) / (CV_pos x projected), per player-week
#      (same starter-restricted pool the app's shape/CV work uses).
#   2. Correlation of z between (a) teammates, (b) opponents in the same game, (c) players in
#      different games (control: should be ~0), by position pair, with week-cluster bootstrap CIs.
#   3. What that implies for real rosters: how much larger a team's score SD is than the
#      independent-sum figure, on this league's and the dynasty league's current lineups.
#   4. So-what: how much the Guillotine elimination probabilities and the dynasty head-to-head
#      win probabilities would move.
#
# Run from the guillotine_app directory:  Rscript analysis/correlation_check.R
# Caches each season/week's team/game info in cache/backtest/gameinfo_*.rds (small).

suppressMessages({library(dplyr); library(purrr); library(tidyr); library(tibble); library(httr2)})
setwd_here <- function() { if (file.exists("R/config.R")) return(invisible()); if (file.exists("../R/config.R")) { setwd(".."); return(invisible()) }; stop("Run from the guillotine_app directory.") }
setwd_here()
for (f in c("config", "sleeper_api", "sleeper_projections", "stat_scoring", "bestball", "simulation")) source(file.path("R", paste0(f, ".R")))

BT <- "cache/backtest"
DYNASTY_LEAGUE_ID <- "1314018983648727040"

# ---- team / opponent / game_id per player-week (raw projections feed carries them) -------------
get_gameinfo <- function(season, week) {
  f <- file.path(BT, sprintf("gameinfo_%s_wk%02d.rds", season, week))
  if (file.exists(f)) return(readRDS(f))
  url <- sprintf("https://api.sleeper.app/projections/nfl/%s/%s?season_type=regular", season, week)
  raw <- request(url) |> req_user_agent("guillotine-tracker-backtest/0.1") |> req_retry(max_tries = 3) |> req_perform() |> resp_body_json(simplifyVector = FALSE)
  out <- map_dfr(raw, function(e) {
    if (is.null(e$game_id) || is.null(e$team)) return(NULL)
    tibble(player_id = e$player_id, team = e$team, opponent = e$opponent %||% NA_character_, game_id = as.character(e$game_id))
  }) |> distinct(player_id, .keep_all = TRUE)
  saveRDS(out, f)
  out
}

# ---- 1. standardized errors ----------------------------------------------------------------------
resid <- readRDS(file.path(BT, "calibration_summary_topn.rds"))$resid_topn
gi <- map_dfr(unique(resid[, c("season", "week")]) |> split(seq_len(nrow(unique(resid[, c("season", "week")])))),
              function(r) get_gameinfo(r$season, r$week) |> mutate(season = r$season, week = r$week))
z <- resid |>
  mutate(season = as.integer(season), week = as.integer(week), player_id = as.character(player_id)) |>
  inner_join(gi |> mutate(season = as.integer(season), week = as.integer(week)), by = c("season", "week", "player_id")) |>
  mutate(cv = unname(UNDERDOG_CV[position]), sdm = cv * projected_pts) |>
  filter(sdm > 0.5) |>
  group_by(position) |>
  mutate(z = (residual - mean(residual)) / sdm, z = z / sd(z)) |>       # unit variance per position so E[z_i z_j] IS a correlation
  ungroup() |>
  mutate(sw = paste(season, week))
cat(sprintf("Player-weeks with an error, team and game: %d (of %d in the pool)\n", nrow(z), nrow(resid)))

# ---- 2. pairwise correlations --------------------------------------------------------------------
pair_type <- function(a, b) ifelse(a < b, paste(a, b, sep = "-"), paste(b, a, sep = "-"))
zz <- z |> select(sw, player_id, position, team, game_id, z)
teammates <- inner_join(zz, zz, by = c("sw", "team"), suffix = c(".x", ".y"), relationship = "many-to-many") |>
  filter(player_id.x < player_id.y) |> transmute(sw, type = pair_type(position.x, position.y), prod = z.x * z.y, rel = "teammates")
opponents <- inner_join(zz, zz, by = c("sw", "game_id"), suffix = c(".x", ".y"), relationship = "many-to-many") |>
  filter(team.x != team.y, player_id.x < player_id.y) |> transmute(sw, type = pair_type(position.x, position.y), prod = z.x * z.y, rel = "opponents")
set.seed(1)
control <- inner_join(zz, zz, by = "sw", suffix = c(".x", ".y"), relationship = "many-to-many") |>
  filter(player_id.x < player_id.y, game_id.x != game_id.y) |> slice_sample(n = 200000) |>
  transmute(sw, type = pair_type(position.x, position.y), prod = z.x * z.y, rel = "different games")
pairs <- bind_rows(teammates, opponents, control)

# week-cluster bootstrap (resample season-weeks) for each relation and position pair
by_week <- pairs |> group_by(rel, type, sw) |> summarise(s = sum(prod), n = n(), .groups = "drop")
boot_rho <- function(d, B = 400) {
  weeks <- unique(z$sw); tot <- d |> group_by(sw) |> summarise(s = sum(s), n = sum(n), .groups = "drop")
  est <- sum(tot$s) / sum(tot$n)
  bs <- replicate(B, { w <- sample(weeks, replace = TRUE); t <- tot[match(w, tot$sw), ]; sum(t$s, na.rm = TRUE) / sum(t$n, na.rm = TRUE) })
  c(rho = est, lo = unname(quantile(bs, .025, na.rm = TRUE)), hi = unname(quantile(bs, .975, na.rm = TRUE)), n = sum(tot$n))
}
overall <- by_week |> group_by(rel) |> group_map(~ c(rel = .y$rel, round(boot_rho(.x), 3)), .keep = TRUE) |> map_dfr(~ as_tibble(as.list(.x)))
cat("\n== Correlation of standardized scoring errors ==\n"); print(overall |> mutate(across(-rel, as.numeric)), n = 10)
by_type <- by_week |> group_by(rel, type) |> filter(sum(n) >= 60) |> group_map(~ c(rel = .y$rel, type = .y$type, round(boot_rho(.x, 300), 3)), .keep = TRUE) |>
  map_dfr(~ as_tibble(as.list(.x))) |> mutate(across(c(rho, lo, hi, n), as.numeric)) |> arrange(rel, desc(rho))
cat("\n== By position pair (>= 60 pairs) ==\n"); print(by_type, n = 60)
saveRDS(list(overall = overall, by_type = by_type), file.path(BT, "correlation_check_results.rds"))

# ---- 3. what it means for real rosters -------------------------------------------------------------
# Correlation model: QB-WR and QB-TE teammates use their own estimates; every other teammate pair
# uses the pooled "other teammates" estimate; opponents in the same game use the pooled opponent
# estimate (per-pair opponent estimates are too noisy to trust); players in different games: 0.
pool <- function(d) sum(d$prod) / nrow(d)
r_qbwr <- pool(filter(teammates, type == "QB-WR")); r_qbte <- pool(filter(teammates, type == "QB-TE"))
r_other <- pool(filter(teammates, !type %in% c("QB-WR", "QB-TE"))); r_opp <- pool(opponents)
cat(sprintf("\nCorrelations used: teammate QB-WR %.3f, QB-TE %.3f, other teammates %.3f | opponents (any pair) %.3f\n", r_qbwr, r_qbte, r_other, r_opp))

rho_pair <- function(pos_i, pos_j, team_i, team_j, game_i, game_j, r = list(qbwr = r_qbwr, qbte = r_qbte, other = r_other, opp = r_opp)) {
  same_game <- !is.na(game_i) & !is.na(game_j) & game_i == game_j
  same_team <- same_game & team_i == team_j
  pt <- pair_type(pos_i, pos_j)
  ifelse(!same_game, 0, ifelse(same_team, ifelse(pt == "QB-WR", r$qbwr, ifelse(pt == "QB-TE", r$qbte, r$other)), r$opp))
}
cross_cov <- function(A, B, r) {                       # sum over i in A, j in B of rho_ij sigma_i sigma_j
  if (nrow(A) == 0 || nrow(B) == 0) return(0)
  g <- expand.grid(i = seq_len(nrow(A)), j = seq_len(nrow(B)))
  sum(rho_pair(A$position[g$i], B$position[g$j], A$team[g$i], B$team[g$j], A$game_id[g$i], B$game_id[g$j], r) * A$sigma[g$i] * B$sigma[g$j])
}
within_cov <- function(A, r) {                         # sum over pairs i<j, counted once
  n <- nrow(A); if (n < 2) return(0)
  g <- t(combn(n, 2))
  sum(rho_pair(A$position[g[, 1]], A$position[g[, 2]], A$team[g[, 1]], A$team[g[, 2]], A$game_id[g[, 1]], A$game_id[g[, 2]], r) * A$sigma[g[, 1]] * A$sigma[g[, 2]])
}
scen <- list(
  point = list(qbwr = r_qbwr, qbte = r_qbte, other = r_other, opp = r_opp),
  low = list(qbwr = 0.243, qbte = 0.117, other = max(r_other - 0.05, 0), opp = 0.016),     # bootstrap CI lower bounds
  high = list(qbwr = 0.472, qbte = 0.367, other = r_other + 0.05, opp = 0.084)             # bootstrap CI upper bounds
)

week <- 3
gi26 <- get_gameinfo(2026, week)
players_db <- get_players_db()
lineup_table <- function(roster_df, projs, scoring, slots) {
  roster_df |>
    mutate(proj = vapply(player_id, function(p) pmax(score_stats(projs[[p]], scoring), 0), numeric(1))) |>
    left_join(gi26, by = "player_id") |>
    group_by(roster_id) |>
    group_modify(function(df, k) { s <- optimal_lineup_assignment(df$position, df$proj, slots = slots); df[s != "BN", ] }) |>
    ungroup() |>
    mutate(sigma = unname(UNDERDOG_CV[position]) * proj)
}
projs26 <- get_sleeper_projections(2026, week)

# Guillotine league (best ball: lineup = best players by projection)
lg_g <- get_league(LEAGUE_ID)
ros_g <- get_rosters(LEAGUE_ID)
alive <- map_dfr(ros_g, function(r) tibble(roster_id = r$roster_id, elim = (r$settings$eliminated %||% 0) > 0)) |> filter(!elim) |> pull(roster_id)
rt_g <- build_roster_table(LEAGUE_ID, players_db) |> filter(roster_id %in% alive) |> select(roster_id, team_name, player_id, position)
lu_g <- lineup_table(rt_g, projs26, lg_g$scoring_settings, ROSTER_SLOTS)

# Dynasty league (head-to-head; taxi + IR can't start)
lg_d <- get_league(DYNASTY_LEAGUE_ID); ros_d <- get_rosters(DYNASTY_LEAGUE_ID); users_d <- build_user_lookup(get_users(DYNASTY_LEAGUE_ID))
rt_d <- map_dfr(ros_d, function(r) {
  ids <- setdiff(unlist(r$players %||% list()), c(unlist(r$taxi %||% list()), unlist(r$reserve %||% list())))
  tibble(roster_id = r$roster_id, owner_id = r$owner_id, player_id = ids)
}) |> left_join(users_d, by = "owner_id") |>
  mutate(position = map_chr(player_id, ~ players_db[[.x]]$position %||% NA_character_)) |> filter(position %in% c("QB", "RB", "WR", "TE")) |>
  select(roster_id, team_name, player_id, position)
slots_d <- unlist(lg_d$roster_positions); slots_d <- slots_d[slots_d != "BN"]
lu_d <- lineup_table(rt_d, projs26, lg_d$scoring_settings, slots_d)

team_inflation <- function(lu, r) lu |> group_by(roster_id, team_name) |> group_modify(function(df, k) {
  v0 <- sum(df$sigma^2); cv <- within_cov(df, r)
  tibble(mu = sum(df$proj), sd_indep = sqrt(v0), sd_corr = sqrt(v0 + 2 * cv), infl = sqrt(1 + 2 * cv / v0),
         qb_wr_pairs = sum(outer(df$position == "QB", df$position == "WR") & outer(df$team, df$team, "==") & outer(df$game_id, df$game_id, "==") & row(diag(nrow(df))) != col(diag(nrow(df)))) )
}) |> ungroup()
cat(sprintf("\n== Week %d lineups: how much bigger is a team's score SD than the independent-sum figure? ==\n", week))
summ <- function(x) sprintf("mean +%.1f%%  (min +%.1f%%, max +%.1f%%)", 100 * (mean(x) - 1), 100 * (min(x) - 1), 100 * (max(x) - 1))
res_infl <- list()
for (nm in names(scen)) {
  g <- team_inflation(lu_g, scen[[nm]]); d <- team_inflation(lu_d, scen[[nm]]); res_infl[[nm]] <- list(g = g, d = d)
  cat(sprintf("  [%s] Guillotine (%d teams): %s | Dynasty (%d teams): %s\n", nm, nrow(g), summ(g$infl), nrow(d), summ(d$infl)))
}
cat(sprintf("  QB-WR/TE same-team stacks in a lineup: Guillotine avg %.2f per team; Dynasty avg %.2f per team\n",
            mean(res_infl$point$g$qb_wr_pairs) / 2, mean(res_infl$point$d$qb_wr_pairs) / 2))

# ---- 4a. Guillotine: elimination probabilities, independent vs correlated team spread (pre-game illustration)
g0 <- res_infl$point$g
p_ind <- elimination_prob_instant(g0$mu, g0$sd_indep); p_cor <- elimination_prob_instant(g0$mu, g0$sd_corr)
cmp <- tibble(team = g0$team_name, mean = round(g0$mu, 1), elim_independent = p_ind, elim_correlated = p_cor, change = p_cor - p_ind) |> arrange(desc(elim_independent))
cat("\n== Guillotine, week-3 pre-game lineups: elimination probability, independent vs correlated within-team spread ==\n")
print(cmp |> mutate(across(starts_with("elim"), ~ round(100 * .x, 1)), change = round(100 * change, 2)) |> head(8), n = 8)
cat(sprintf("   largest absolute change over all %d teams: %.2f percentage points; teams with |change| > 0.5 pt: %d\n", nrow(cmp), 100 * max(abs(cmp$change)), sum(abs(cmp$change) > 0.005)))

# ---- 4b. Dynasty: week-3 head-to-head win probabilities, independent vs correlated (within-team + cross-team links)
mm <- get_matchups(DYNASTY_LEAGUE_ID, week)
pr <- map_dfr(mm, ~ tibble(roster_id = .x$roster_id, matchup_id = .x$matchup_id)) |> group_by(matchup_id) |> summarise(a = roster_id[1], b = roster_id[2], .groups = "drop")
wp <- pmap_dfr(pr, function(matchup_id, a, b) {
  A <- filter(lu_d, roster_id == a); B <- filter(lu_d, roster_id == b)
  muA <- sum(A$proj); muB <- sum(B$proj)
  out <- tibble(a = A$team_name[1], b = B$team_name[1], muA = muA, muB = muB)
  for (nm in c("point", "low", "high")) {
    r <- scen[[nm]]
    var_ind <- sum(A$sigma^2) + sum(B$sigma^2)
    var_cor <- sum(A$sigma^2) + 2 * within_cov(A, r) + sum(B$sigma^2) + 2 * within_cov(B, r) - 2 * cross_cov(A, B, r)
    out[[paste0("p_", nm)]] <- pnorm((muA - muB) / sqrt(var_cor)); out[[paste0("sd_ratio_", nm)]] <- sqrt(var_cor / var_ind)
  }
  out$p_indep <- pnorm((muA - muB) / sqrt(sum(A$sigma^2) + sum(B$sigma^2)))
  out
})
cat("\n== Dynasty, week-3 matchups: P(first team wins), independent vs correlated ==\n")
print(wp |> transmute(matchup = paste(a, "vs", b), proj = sprintf("%.0f-%.0f", muA, muB),
                      change = round(100 * (p_point - p_indep), 2),          # computed BEFORE p_indep is rescaled below
                      p_indep = round(100 * p_indep, 1), p_corr = round(100 * p_point, 1),
                      sd_of_difference_ratio = round(sd_ratio_point, 3)) |> select(matchup, proj, p_indep, p_corr, change, sd_of_difference_ratio), n = 10)
cat(sprintf("   mean |change| in win probability: %.2f points (max %.2f); with the low/high CI scenarios the mean |change| is %.2f / %.2f\n",
            100 * mean(abs(wp$p_point - wp$p_indep)), 100 * max(abs(wp$p_point - wp$p_indep)),
            100 * mean(abs(wp$p_low - wp$p_indep)), 100 * mean(abs(wp$p_high - wp$p_indep))))
saveRDS(list(overall = overall, by_type = by_type, infl = res_infl, guillotine = cmp, dynasty = wp), file.path(BT, "correlation_check_results.rds"))

# ---- 5. Larger-sample check of teammate pairs (all players projected >= 2 pts, 13,200 player-weeks) ----------
# The starter-only pool above has few QB-RB / RB-TE pairs (275 / 343), enough to give misleading estimates
# (QB-RB -0.001 +/- 0.10, RB-TE -0.09). The broader residuals_all pool has ~10x the pairs:
# QB-RB +0.074 (0.036 to 0.105), QB-WR +0.273, QB-TE +0.208, RB-WR / RB-TE / RB-RB ~ 0.
allr <- readRDS(file.path(BT, "residuals_all.rds")) |> mutate(season = as.integer(season), week = as.integer(week), player_id = as.character(player_id))
gi_all <- map_dfr(list.files(BT, "^gameinfo_20(23|24|25)_", full.names = TRUE), function(f) { m <- regmatches(f, regexec("gameinfo_(\\d+)_wk(\\d+)", f))[[1]]; readRDS(f) |> mutate(season = as.integer(m[2]), week = as.integer(m[3])) })
za <- allr |> inner_join(gi_all, by = c("season", "week", "player_id")) |>
  mutate(sdm = unname(UNDERDOG_CV[position]) * projected_pts) |> filter(sdm > 0.5) |>
  group_by(position) |> mutate(z = (residual - mean(residual)) / sdm, z = z / sd(z)) |> ungroup() |> mutate(sw = paste(season, week))
zza <- za |> select(sw, player_id, position, team, z)
boot_pair <- function(pa, pb, B = 500) {
  d <- inner_join(filter(zza, position == pa), filter(zza, position == pb), by = c("sw", "team"), suffix = c(".a", ".b"), relationship = "many-to-many") |>
    filter(player_id.a != player_id.b) |> transmute(sw, prod = z.a * z.b)
  tot <- d |> group_by(sw) |> summarise(s = sum(prod), n = n(), .groups = "drop"); weeks <- unique(za$sw)
  bs <- replicate(B, { w <- sample(weeks, replace = TRUE); t <- tot[match(w, tot$sw), ]; sum(t$s, na.rm = TRUE) / sum(t$n, na.rm = TRUE) })
  sprintf("%s-%s: rho %.3f (95%% %.3f to %.3f), %d pairs", pa, pb, sum(tot$s) / sum(tot$n), quantile(bs, .025), quantile(bs, .975), sum(tot$n))
}
set.seed(2)
cat("\n== Teammate correlations, broader pool ==\n")
for (pp in list(c("QB", "RB"), c("QB", "WR"), c("QB", "TE"), c("RB", "WR"), c("RB", "TE"), c("RB", "RB"))) cat(" ", boot_pair(pp[1], pp[2]), "\n")
