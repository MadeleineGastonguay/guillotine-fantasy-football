# --- Week preview -------------------------------------------------------------------------------------------------
# Built on Wednesday morning, after the waivers have run and before any game of the week: for each team, the projection and the chance of being chopped
# BEFORE this morning's waivers against AFTER, which players the waivers added, which players are on a bye, and the claims that were lost.
# Both versions use today's projections, so a change is the waiver moves alone. The chance of being chopped is the app's Approximate model (run_elimination_instant()).
# "Before" rosters are rebuilt by undoing this run's waiver claims from the current rosters (Sleeper has no snapshot of the rosters just before the waivers; the upcoming
# week's matchup rosters are just the current ones). Checked against last week's final matchup rosters: they agree except for free-agent moves made after that week locked.
# Needs R/ (as sourced by collector/weekly_recap.R), collector/recap_lib.R and collector/waivers_lib.R.

# Every player on a current roster, as roster_id + player_id (the app's build_roster_table() keeps only QB/RB/WR/TE later)
preview_membership <- function(league_id = LEAGUE_ID) {
  dplyr::bind_rows(lapply(get_rosters(league_id), function(r) {
    p <- unlist(r$players %||% list()); if (!length(p)) return(NULL)
    tibble::tibble(roster_id = r$roster_id, player_id = as.character(p))
  }))
}

# Undo completed waiver claims, newest first: the added player leaves the roster and the player dropped to make room comes back
undo_waivers <- function(mem, won) {
  won <- won[order(-won$ts), ]
  for (i in seq_len(nrow(won))) {
    w <- won[i, ]
    mem <- mem[!(mem$roster_id == w$rid & mem$player_id == w$add), ]
    if (!is.na(w$drop)) mem <- dplyr::distinct(rbind(mem, tibble::tibble(roster_id = w$rid, player_id = w$drop)))
  }
  mem
}

# What each player is usually projected for: the mean pregame projection over the weeks already played this season (replay files), for players
# who were projected for something. Used to estimate what a bye costs a team.
preview_usual_projections <- function(season, weeks) {
  d <- dplyr::bind_rows(lapply(weeks, function(w) {
    f <- file.path(REPLAY_DIR, sprintf("%s_w%02d_players.csv.gz", season, w)); if (!file.exists(f)) return(NULL)
    readr::read_csv(f, show_col_types = FALSE, col_select = c("player_id", "pre_proj", "is_final_row")) |> dplyr::filter(is_final_row) |>
      dplyr::transmute(player_id = as.character(player_id), pre_proj) |> dplyr::distinct(player_id, .keep_all = TRUE)
  }))
  if (is.null(d) || !nrow(d)) return(tibble::tibble(player_id = character(), usual = numeric()))
  d |> dplyr::filter(!is.na(pre_proj), pre_proj > 0) |> dplyr::group_by(player_id) |> dplyr::summarise(usual = mean(pre_proj), .groups = "drop")
}

# What the league would look like if one lost claim had gone through instead of the winner's: the losing team gets the player (and loses the player it would have dropped),
# the winner does not get him and keeps the player it dropped. Every team's chance of being chopped is recomputed with the app's model. Returns the losing team's chance.
chance_if_claim_won <- function(after, before, team_rid, winner_rid, add_id, drop_team = NA_character_, drop_winner = NA_character_) {
  rt <- after$roster_table
  row_p <- rt[rt$roster_id == winner_rid & rt$player_id == add_id, ]; if (!nrow(row_p)) return(NA_real_)
  row_p <- row_p[1, ]
  idcols <- c("roster_id", "owner_id", "owner_name", "team_name", "avatar_url")
  idcols <- intersect(idcols, names(rt))
  row_p[, idcols] <- rt[rt$roster_id == team_rid, idcols][1, ]
  rt2 <- rt[!(rt$roster_id == winner_rid & rt$player_id == add_id), ]
  if (!is.na(drop_team)) rt2 <- rt2[!(rt2$roster_id == team_rid & rt2$player_id == drop_team), ]
  back <- if (!is.na(drop_winner)) before$roster_table[before$roster_table$roster_id == winner_rid & before$roster_table$player_id == drop_winner, ] else NULL
  rt2 <- dplyr::bind_rows(rt2, row_p, back)
  meta <- after$team_meta
  for (r in c(team_rid, winner_rid)) {
    x <- rt2[rt2$roster_id == r, ]
    meta$naive_projected_total[meta$roster_id == r] <- optimal_lineup_points(x$position, x$locked_points + x$remaining_mean)
  }
  res <- run_elimination_instant(rt2, meta, sd_col = "remaining_sd_underdog")
  res$elimination_prob[match(team_rid, res$roster_id)]
}

# Did the player play in the last game his NFL team played? (TRUE/FALSE, NA if his team has no earlier game in the replay files this season.) Used to tell a NEW injury from a long one.
preview_played_last <- function(player_id, nfl_team, season, weeks_prev) {
  fr <- lapply(weeks_prev, function(w) {
    f <- file.path(REPLAY_DIR, sprintf("%s_w%02d_players.csv.gz", season, w)); if (!file.exists(f)) return(NULL)
    readr::read_csv(f, show_col_types = FALSE, col_select = c("player_id", "nfl_team", "is_final_row", "off_snp")) |> dplyr::filter(is_final_row) |>
      dplyr::transmute(player_id = as.character(player_id), nfl_team, off_snp)
  }); names(fr) <- weeks_prev
  mapply(function(pid, tm) {
    for (w in rev(weeks_prev)) { d <- fr[[as.character(w)]]; if (is.null(d) || !(tm %in% d$nfl_team)) next
      return(any(d$player_id == pid & !is.na(d$off_snp) & d$off_snp > 0)) }
    NA
  }, player_id, nfl_team, USE.NAMES = FALSE)
}

# Injured players (Out, IR, Doubtful, Questionable) who would be in their fantasy team's lineup if healthy, are not on a bye, and played their last game (so this is the first week of
# the problem). Sleeper gives many injured players a 0.0 projection, so "healthy" means his projection if it is above 0, else his usual projection. For each: the team's chance of being
# chopped as the model has it now, if he plays, and if he sits (the rest of the league unchanged). `assumed` is what the model counts him as now.
preview_injuries <- function(after, teams, usual, season, week) {
  rt <- dplyr::left_join(after$roster_table, usual, by = "player_id")
  rt$bye <- rt$matchup_label == "BYE"
  rt$healthy <- ifelse(rt$bye, 0, ifelse(rt$remaining_mean > 0, rt$remaining_mean, dplyr::coalesce(rt$usual, 0)))
  rt$slot_h <- NA_character_
  for (r in unique(rt$roster_id)) { i <- which(rt$roster_id == r); rt$slot_h[i] <- optimal_lineup_assignment(rt$position[i], rt$healthy[i]) }
  cand <- rt[!is.na(rt$injury_status) & rt$injury_status %in% c("Out", "IR", "PUP", "Doubtful", "Questionable") & !rt$bye & rt$slot_h != "BN", ]
  empty <- tibble::tibble(roster_id = integer(), team_name = character(), owner_name = character(), player_id = character(), player = character(), position = character(), nfl_team = character(),
                          status = character(), new = logical(), healthy_proj = numeric(), sleeper_proj = numeric(), assumed = character(), chance_now = numeric(), chance_if_plays = numeric(), chance_if_sits = numeric(), swing = numeric(), asof = character())
  if (!nrow(cand)) return(empty)
  cand$new <- preview_played_last(cand$player_id, cand$nfl_team, season, seq_len(week - 1))
  rt0 <- after$roster_table; meta0 <- after$team_meta
  scen <- function(rid, pid, mean, sd) {
    t2 <- rt0; k <- which(t2$roster_id == rid & t2$player_id == pid)
    t2$remaining_mean[k] <- mean; t2$remaining_sd_underdog[k] <- sd
    m2 <- meta0; x <- t2[t2$roster_id == rid, ]; m2$naive_projected_total[m2$roster_id == rid] <- optimal_lineup_points(x$position, x$locked_points + x$remaining_mean)
    res <- run_elimination_instant(t2, m2, sd_col = "remaining_sd_underdog"); res$elimination_prob[match(rid, res$roster_id)]
  }
  dplyr::bind_rows(lapply(seq_len(nrow(cand)), function(i) {
    r <- cand[i, ]; now <- teams$chance_after[match(r$roster_id, teams$roster_id)]; playing <- r$remaining_mean > 0
    ir <- identical(r$injury_status, "IR")   # injured reserve: there is no chance he plays, so no "if he plays" figure (NA); he is counted as sitting
    plays <- if (ir) NA_real_ else if (playing) now else scen(r$roster_id, r$player_id, r$healthy, full_game_sd_underdog(r$position, r$healthy))
    sits <- if (!playing) now else scen(r$roster_id, r$player_id, 0, 0)
    tibble::tibble(roster_id = r$roster_id, team_name = r$team_name, owner_name = r$owner_name, player_id = r$player_id, player = r$full_name, position = r$position, nfl_team = r$nfl_team,
                   status = r$injury_status, new = isTRUE(r$new), healthy_proj = r$healthy, sleeper_proj = r$remaining_mean, assumed = if (playing) "playing" else "sitting", chance_now = now, chance_if_plays = plays, chance_if_sits = sits, swing = if (is.na(plays)) 0 else abs(plays - sits))
  }))
}

# The preview covers the waiver run that happens early on the Wednesday after a chop (about 3am Eastern), not later runs: the cutoff is 9:00am Eastern on that Wednesday.
preview_cutoff <- function(chop_ts) {
  d <- as.POSIXct(chop_ts, origin = "1970-01-01", tz = "America/New_York")
  wd <- as.integer(format(d, "%u"))   # Monday = 1 ... Sunday = 7
  ahead <- (3 - wd) %% 7; if (ahead == 0 && as.integer(format(d, "%H")) >= 9) ahead <- 7
  as.numeric(as.POSIXct(paste(as.Date(d, tz = "America/New_York") + ahead, "09:00:00"), tz = "America/New_York"))
}

# Every completed roster move (waiver, free agent, trade, commissioner) after the cutoff, as one row per added or dropped player; undone to get the rosters as they stood at the cutoff
preview_moves_after <- function(week, cutoff, league_id = LEAGUE_ID) {
  tx <- c(get_transactions(league_id, week - 1), get_transactions(league_id, week))
  ids <- vapply(tx, function(z) z$transaction_id %||% NA_character_, character(1)); tx <- tx[!duplicated(ids)]
  dplyr::bind_rows(lapply(tx, function(z) {
    if (!identical(z$status, "complete") || identical(z$type, "chopped")) return(NULL)
    ts <- as.numeric(z$status_updated %||% z$created %||% NA) / 1000; if (is.na(ts) || ts <= cutoff) return(NULL)
    a <- z$adds %||% list(); d <- z$drops %||% list()
    if (!length(a) && !length(d)) return(NULL)
    tibble::tibble(ts = ts, kind = c(rep("add", length(a)), rep("drop", length(d))), player_id = c(names(a), names(d)), roster_id = as.integer(c(unlist(a), unlist(d))))
  }))
}
undo_moves <- function(mem, moves) {
  if (is.null(moves) || !nrow(moves)) return(mem)
  moves <- moves[order(-moves$ts), ]
  for (i in seq_len(nrow(moves))) { m <- moves[i, ]
    mem <- if (m$kind == "add") mem[!(mem$roster_id == m$roster_id & mem$player_id == m$player_id), ] else dplyr::distinct(rbind(mem, tibble::tibble(roster_id = m$roster_id, player_id = m$player_id))) }
  mem
}

# "Wednesday morning" for a time (morning, afternoon or evening of that day), used in the page's disclaimer
day_part <- function(x = Sys.time()) {
  h <- as.integer(format(x, "%H", tz = "America/New_York"))
  paste(format(x, "%A", tz = "America/New_York"), if (h < 12) "morning" else if (h < 17) "afternoon" else "evening")
}

# Adds the injury news to a preview's saved data without touching anything else in it (the numbers published on Wednesday stay as they were): the rosters as they stood at the Wednesday 9am cutoff,
# with today's projections and Sleeper's statuses as of now (Sleeper keeps no history of statuses), and the chance of being chopped recomputed from that snapshot for the "now / plays / sits" figures.
preview_add_injuries <- function(pv, league_id = LEAGUE_ID, log = function(...) message(sprintf(...))) {
  chops <- chop_txns(pv$week - 1, league_id); cutoff <- preview_cutoff(max(chops$ts))
  mem_after <- undo_moves(preview_membership(league_id), preview_moves_after(pv$week, cutoff, league_id))
  after <- build_live_snapshot(league_id, membership = mem_after)
  ra <- run_elimination_instant(after$roster_table, after$team_meta, sd_col = "remaining_sd_underdog")
  inj <- preview_injuries(after, tibble::tibble(roster_id = ra$roster_id, chance_after = ra$elimination_prob), preview_usual_projections(pv$season, seq_len(pv$week - 1)), pv$season, pv$week)
  inj$asof <- rep(day_part(), nrow(inj))
  pv$injuries <- inj; pv$cutoff <- cutoff; pv$injuries_asof <- day_part()
  pv
}

# The whole data build for one preview. Returns the team table, the waiver pickups, the bye players and the lost claims.
build_preview <- function(season, week, league_id = LEAGUE_ID, log = function(...) message(sprintf(...))) {
  chops <- chop_txns(week - 1, league_id)
  chop_ts <- if (nrow(chops)) max(chops$ts) else 0
  cutoff <- preview_cutoff(chop_ts)
  claims <- waiver_claims_raw(week - 1, league_id); claims <- claims[claims$ts > chop_ts & claims$ts <= cutoff, ]   # Wednesday morning's waiver run only
  won <- claims[claims$status == "complete", ]
  lost <- claims[claims$status != "complete" & grepl("claimed by another", claims$note, fixed = TRUE), ]
  if (!nrow(won)) log("preview: no completed waiver claims found since the chop; has the waiver run happened yet? (before and after will be the same)")
  # rosters as they stood at the cutoff (Wednesday 9am): the current rosters with every later move undone; and before the waivers: those with the Wednesday run's claims undone as well
  later <- preview_moves_after(week, cutoff, league_id)
  mem_now <- preview_membership(league_id); mem_after <- undo_moves(mem_now, later); mem_before <- undo_waivers(mem_after, won)
  log("preview: rosters at the Wednesday cutoff (%d later moves undone); before-waivers rosters (%d claims undone)", if (is.null(later)) 0L else nrow(later), nrow(won))
  after <- build_live_snapshot(league_id, membership = mem_after); before <- build_live_snapshot(league_id, membership = mem_before)
  ra <- run_elimination_instant(after$roster_table, after$team_meta, sd_col = "remaining_sd_underdog")
  rb <- run_elimination_instant(before$roster_table, before$team_meta, sd_col = "remaining_sd_underdog")
  teams <- ra |> dplyr::select(roster_id, team_name, owner_name, proj_after = naive_projected_total, chance_after = elimination_prob) |>
    dplyr::left_join(dplyr::select(rb, roster_id, proj_before = naive_projected_total, chance_before = elimination_prob), by = "roster_id")

  rt_a <- after$roster_table
  players_db <- get_players_db()
  pool <- dplyr::distinct(rbind(rt_a[, c("player_id", "full_name", "position", "nfl_team", "remaining_mean")], before$roster_table[, c("player_id", "full_name", "position", "nfl_team", "remaining_mean")]), player_id, .keep_all = TRUE)
  team_proj <- function(rows) if (!nrow(rows)) 0 else optimal_lineup_points(rows$position, rows$remaining_mean)
  rows_of <- function(rid) rt_a[rt_a$roster_id == rid, c("player_id", "position", "remaining_mean")]
  now_proj <- vapply(teams$roster_id, function(r) team_proj(rows_of(r)), numeric(1))
  lab <- function(rid) teams$team_name[match(rid, teams$roster_id)]
  own <- function(rid) teams$owner_name[match(rid, teams$roster_id)]

  # the waiver pickups: what each claim adds to the team's projected score (its lineup with the player minus the lineup with the claim undone)
  pick <- dplyr::bind_rows(lapply(seq_len(nrow(won)), function(i) {
    w <- won[i, ]; if (!w$rid %in% teams$roster_id) return(NULL)   # claimed by a team that has since been chopped
    cur <- rows_of(w$rid); cf <- cur[cur$player_id != w$add, ]
    # the runner-up: the highest losing bid on this player in the same waiver run
    comp <- claims[claims$add == w$add & claims$run == w$run & claims$id != w$id & grepl("claimed by another", claims$note, fixed = TRUE), ]; sb <- if (nrow(comp)) comp[which.max(comp$bid), ] else NULL
    if (!is.na(w$drop) && w$drop %in% pool$player_id) cf <- rbind(cf, pool[match(w$drop, pool$player_id), c("player_id", "position", "remaining_mean")])
    p <- pool[match(w$add, pool$player_id), ]; d <- pool[match(w$drop, pool$player_id), ]
    if (is.na(p$player_id)) { q <- players_db[[w$add]] %||% list(); p <- tibble::tibble(player_id = w$add, full_name = q$full_name %||% paste(q$first_name %||% "", q$last_name %||% "") |> trimws(), position = q$position %||% "?", nfl_team = q$team %||% "FA", remaining_mean = NA_real_) }
    if (is.na(d$player_id) && !is.na(w$drop)) { q <- players_db[[w$drop]] %||% list(); d <- tibble::tibble(player_id = w$drop, full_name = q$full_name %||% paste(q$first_name %||% "", q$last_name %||% "") |> trimws()) }
    ra_row <- rt_a[match(w$add, rt_a$player_id), ]
    tibble::tibble(roster_id = w$rid, team_name = lab(w$rid), owner_name = own(w$rid), player_id = w$add, player = p$full_name %||% w$add, position = p$position %||% "?", nfl_team = p$nfl_team %||% "FA",
                   price = w$bid, dropped = if (is.na(w$drop)) NA_character_ else (d$full_name %||% w$drop), proj = if (is.na(p$remaining_mean)) NA_real_ else p$remaining_mean,
                   bye = isTRUE(ra_row$matchup_label == "BYE"), adds = round(team_proj(cur) - team_proj(cf), 1) + 0,
                   n_bids = nrow(comp) + 1L, second_bid = if (is.null(sb)) NA_real_ else sb$bid, second_team = if (is.null(sb)) NA_character_ else lab(sb$rid))
  }))
  if (!nrow(pick)) pick <- tibble::tibble(roster_id = integer(), team_name = character(), owner_name = character(), player_id = character(), player = character(), position = character(), nfl_team = character(), price = numeric(), dropped = character(), proj = numeric(), bye = logical(), adds = numeric(), n_bids = integer(), second_bid = numeric(), second_team = character())

  # players on a bye, and what the byes cost each team: the projected lineup if they were playing at their usual projection, minus the projection now
  usual <- preview_usual_projections(season, seq_len(week - 1))
  rb_t <- rt_a |> dplyr::left_join(usual, by = "player_id")
  byes <- rb_t[rb_t$matchup_label == "BYE", ]
  bye_cost <- vapply(teams$roster_id, function(r) {
    x <- rb_t[rb_t$roster_id == r, ]
    team_proj(transform(x, remaining_mean = ifelse(x$matchup_label == "BYE", ifelse(is.na(x$usual), 0, x$usual), x$remaining_mean))) - team_proj(x)
  }, numeric(1))
  # would each bye player be in the lineup if he were playing at his usual level? (only those who would are part of the bye cost)
  start_key <- unlist(lapply(unique(rb_t$roster_id), function(r) {
    x <- rb_t[rb_t$roster_id == r, ]
    pts <- ifelse(x$matchup_label == "BYE", ifelse(is.na(x$usual), 0, x$usual), x$remaining_mean)
    stats::setNames(optimal_lineup_assignment(x$position, pts) != "BN", paste(r, x$player_id))
  }))
  byes$would_start <- unname(start_key[paste(byes$roster_id, byes$player_id)])
  byes <- dplyr::transmute(byes, roster_id, team_name, owner_name, player_id, player = full_name, position, nfl_team, usual = usual, would_start)

  # claims the team LOST this morning to another owner, with what winning would have added to its projected score
  lost_tbl <- dplyr::bind_rows(lapply(seq_len(nrow(lost)), function(i) {
    l <- lost[i, ]; if (!l$rid %in% teams$roster_id) return(NULL)
    wn <- won[won$add == l$add & won$run == l$run, ]; if (!nrow(wn)) return(NULL)
    p <- pool[match(l$add, pool$player_id), ]; if (is.na(p$player_id)) return(NULL)
    cur <- rows_of(l$rid); cf <- rbind(cur[!(cur$player_id %in% l$drop), ], p[, c("player_id", "position", "remaining_mean")])
    wrow <- wn[which.max(wn$bid), ]
    tibble::tibble(roster_id = l$rid, team_name = lab(l$rid), owner_name = own(l$rid), player_id = l$add, player = p$full_name, position = p$position, nfl_team = p$nfl_team, their_bid = l$bid, win_bid = max(wn$bid),
                   winner = lab(wrow$rid), winner_rid = wrow$rid, drop_team = l$drop, drop_winner = wrow$drop, proj = p$remaining_mean, gain = team_proj(cf) - team_proj(cur))
  }))
  if (!nrow(lost_tbl)) lost_tbl <- tibble::tibble(roster_id = integer(), team_name = character(), owner_name = character(), player_id = character(), player = character(), position = character(), nfl_team = character(), their_bid = numeric(), win_bid = numeric(), winner = character(), winner_rid = integer(), drop_team = character(), drop_winner = character(), proj = numeric(), gain = numeric(), chance_now = numeric(), chance_if_won = numeric())
  lost_tbl <- lost_tbl[order(-lost_tbl$gain), ]
  # if the claim had gone through: the losing team's chance of being chopped (only for the claims that would have mattered, to keep the build quick)
  lost_tbl$chance_now <- teams$chance_after[match(lost_tbl$roster_id, teams$roster_id)]
  lost_tbl$chance_if_won <- vapply(seq_len(nrow(lost_tbl)), function(i) {
    r <- lost_tbl[i, ]; if (r$gain < 3) return(NA_real_)
    tryCatch(chance_if_claim_won(after, before, r$roster_id, r$winner_rid, r$player_id, r$drop_team, r$drop_winner), error = function(e) NA_real_)
  }, numeric(1))

  teams$n_pickups <- vapply(teams$roster_id, function(r) sum(pick$roster_id == r), numeric(1))
  teams$n_bye <- vapply(teams$roster_id, function(r) sum(byes$roster_id == r), numeric(1))
  teams$bye_cost <- bye_cost
  teams$n_lost <- vapply(teams$roster_id, function(r) sum(lost_tbl$roster_id == r & lost_tbl$gain >= 1), numeric(1))
  teams$lost_gain <- vapply(teams$roster_id, function(r) { g <- lost_tbl$gain[lost_tbl$roster_id == r]; if (length(g)) max(g) else 0 }, numeric(1))
  teams$check_proj <- now_proj   # should equal proj_after (a self-test)
  # waiver spending: what each team paid this morning, and what share of the budget it had left going in (the $1,000 minus everything won in earlier runs)
  won_all <- { a <- waiver_claims_raw(seq_len(week - 1), league_id); a[a$status == "complete", ] }
  teams$spent_run <- vapply(teams$roster_id, function(r) sum(won$bid[won$rid == r], na.rm = TRUE), numeric(1))
  teams$budget_before <- FAAB_BUDGET - vapply(teams$roster_id, function(r) sum(won_all$bid[won_all$rid == r & won_all$ts <= chop_ts], na.rm = TRUE), numeric(1))
  teams$spent_share <- ifelse(teams$budget_before > 0, teams$spent_run / teams$budget_before, NA_real_)
  # self-test against Sleeper's own running total of dollars used
  fb <- tryCatch(faab_table(league_id), error = function(e) NULL)
  later_spend <- vapply(teams$roster_id, function(r) sum(won_all$bid[won_all$rid == r & won_all$ts > cutoff], na.rm = TRUE), numeric(1))   # Sleeper's total includes runs after the cutoff
  if (!is.null(fb)) { k <- match(teams$team_name, fb$team_name); bad <- !is.na(k) & abs((FAAB_BUDGET - teams$budget_before + teams$spent_run + later_spend) - fb$used[k]) > 0.5
    if (any(bad)) log("preview: WARNING the waiver spending does not match Sleeper's total for: %s", paste(teams$team_name[bad], collapse = ", ")) }
  teams <- teams[order(-teams$chance_after), ]
  # NFL teams on a bye this week: every team in the player table that has no game on this week's schedule
  st <- tryCatch(get_nfl_state(), error = function(e) NULL)
  gs <- tryCatch(espn_get_game_status(season, week, st$season_type %||% "regular"), error = function(e) NULL)
  bye_teams <- if (is.null(gs)) character(0) else sort(setdiff(unique(slim_players_table()$nfl_team), gs$nfl_team))
  injuries <- preview_injuries(after, teams, usual, season, week); injuries$asof <- rep(day_part(), nrow(injuries))
  list(built_at = Sys.time(), injuries_asof = day_part(), season = season, week = week, cutoff = cutoff, teams = teams, pickups = pick, byes = byes, lost = lost_tbl, injuries = injuries, claims_won = nrow(won), n_teams = nrow(teams), bye_teams = bye_teams)
}

# --- Fact sheet, prompt, checks ---------------------------------------------------------------------------------------
pctx <- function(x) ifelse(x < 0.095, sprintf("%.1f%%", 100 * x), sprintf("%.0f%%", 100 * x))   # one decimal for small chances

preview_fact_sheet <- function(pv, nfl_bye_teams = character(0)) {
  tm <- pv$teams; n <- pv$n_teams; even <- 1 / n
  who <- function(r) sprintf("%s (managed by %s)", r$team_name, manager_label(r$owner_name))
  # every team named in this sheet, with its manager
  mg_teams <- unique(c(tm$team_name, pv$lost$winner))
  mg <- tm[match(mg_teams[mg_teams %in% tm$team_name], tm$team_name), c("team_name", "owner_name")]
  line <- function(i) {
    r <- tm[i, ]; pk <- pv$pickups[pv$pickups$roster_id == r$roster_id, ]; by <- pv$byes[pv$byes$roster_id == r$roster_id, ]; lo <- pv$lost[pv$lost$roster_id == r$roster_id & pv$lost$gain >= 5 & !is.na(pv$lost$chance_if_won), ]; lo <- head(lo, 2)
    parts <- c(sprintf("- %s: chance of being chopped %s before the waivers, %s after. Projected score %.1f before the waivers, %.1f after (%+.1f).", who(r), pctx(r$chance_before), pctx(r$chance_after), r$proj_before, r$proj_after, round(r$proj_after - r$proj_before, 1) + 0),
               if (r$spent_run > 0) sprintf("Spent $%.0f on waivers in Wednesday morning's run, %.0f%% of the $%.0f budget left before it.", r$spent_run, 100 * r$spent_share, r$budget_before),
               if (nrow(pk)) sprintf("Waiver pickups: %s.", paste(sprintf("%s, %s ($%.0f, net change to the projected score %+.1f%s)", pk$player, pk$position, pk$price, pk$adds, ifelse(pk$bye, "; on a bye this week, so he adds nothing yet", ifelse(pk$adds < 0, "; negative because the player dropped to make room fit a lineup spot this week that the new player does not, for example at a position where the team is thin", ""))), collapse = "; ")) else "No waiver pickups.",
               if (nrow(by)) sprintf("On a bye: %s. The byes lower this team's projected score by an estimated %.1f points, the net drop once the other players fill the lineup spots (only the players marked 'would be in the lineup' change the lineup, so this is not the sum of their usual projections).", paste(sprintf("%s, %s (usually projected for %s; %s)", by$player, by$position, ifelse(is.na(by$usual), "no projection", sprintf("%.1f", by$usual)), ifelse(by$would_start, "would be in the lineup", "would be on the bench anyway")), collapse = "; "), r$bye_cost),
               if (nrow(lo)) sprintf("Lost waiver bids that would have mattered (with the claim won instead of the winner's, this team's chance of being chopped would have been the figure shown): %s.", paste(sprintf("%s, %s ($%.0f vs the winning $%.0f from %s; the chance of being chopped would have been %s instead of %s)", lo$player, lo$position, lo$their_bid, lo$win_bid, lo$winner, pctx(lo$chance_if_won), pctx(lo$chance_now)), collapse = "; ")))
    paste(parts, collapse = " ")
  }
  contested <- pv$lost |> dplyr::group_by(player, position) |> dplyr::summarise(bidders = dplyr::n() + 1L, winner = dplyr::first(winner), win_bid = dplyr::first(win_bid), top_loser = max(their_bid), .groups = "drop") |> dplyr::arrange(dplyr::desc(bidders))
  contested <- head(contested[contested$bidders >= 4, ], 3)
  movers_dn <- tm[order(tm$chance_after - tm$chance_before), ][1:min(2, n), ]; movers_dn <- movers_dn[movers_dn$chance_after < movers_dn$chance_before - 0.01, ]
  movers_up <- tm[order(-(tm$chance_after - tm$chance_before)), ][1:min(2, n), ]; movers_up <- movers_up[movers_up$chance_after > movers_up$chance_before + 0.01, ]
  heavy_bye <- tm[tm$bye_cost >= 5, ]; heavy_bye <- heavy_bye[order(-heavy_bye$bye_cost), ]
  # injuries: only players who would start for their fantasy team if healthy and played their last game (so this is the first week of the problem); the biggest swings in the chance of being chopped first
  inj <- if (is.null(pv$injuries)) NULL else pv$injuries[pv$injuries$new & pv$injuries$swing >= 0.01, ]
  inj <- if (is.null(inj) || !nrow(inj)) NULL else head(inj[order(-inj$swing), ], 8)
  asof <- if (!is.null(inj) && "asof" %in% names(inj) && nrow(inj)) inj$asof[1] else "this morning"
  inj_line <- function(i) { r <- inj[i, ]
    st <- switch(r$status, Out = "OUT (will not play)", IR = "OUT (will not play)", PUP = "OUT (will not play)", Doubtful = "DOUBTFUL (unlikely to play)", "QUESTIONABLE (uncertain)")
    sleeper <- if (r$assumed == "sitting") sprintf("Sleeper's own projection for him is 0.0 points, which means Sleeper expects him to sit, and the chance of being chopped already assumes he sits. When healthy he is usually projected for %.1f.", r$healthy_proj)
               else sprintf("Sleeper projects him for %.1f points, so Sleeper expects him to play, and the chance of being chopped already assumes he plays.", r$sleeper_proj)
    if (is.na(r$chance_if_plays)) sprintf("- %s (managed by %s): %s, %s, %s, on injured reserve. He would be in the starting lineup if healthy, but a player on injured reserve cannot play, so there is no 'if he plays' figure. The team's chance of being chopped is %s, with him out.",
            r$team_name, manager_label(r$owner_name), r$player, r$position, st, pctx(r$chance_now))
    else sprintf("- %s (managed by %s): %s, %s, %s. He would be in the starting lineup if healthy. %s The team's chance of being chopped is %s now, %s if he plays and %s if he sits.",
            r$team_name, manager_label(r$owner_name), r$player, r$position, st, sleeper, pctx(r$chance_now), pctx(r$chance_if_plays), pctx(r$chance_if_sits)) }
  paste(c(sprintf("SEASON %s, WEEK %d PREVIEW (written Wednesday morning after the waivers ran, before any game this week)", pv$season, pv$week),
    "Managers (use the manager's name where you would otherwise say they/their):",
    sprintf("- %s: managed by %s", mg$team_name, manager_label(mg$owner_name)),
    "",
    sprintf("Teams still alive: %d. With nothing known, each team's even-split chance of being chopped is %s (1 in %d).", n, sprintf("%.1f%%", 100 * even + 1e-9), n),
    sprintf("%d waiver claims were won in Wednesday morning's waiver run. Both versions below use the same projections, so every change from before to after comes from the waiver moves alone. The chance of being chopped is the app's own estimate.", pv$claims_won),
    if (length(nfl_bye_teams)) sprintf("NFL teams on a bye this week: %s.", paste(nfl_bye_teams, collapse = ", ")),
    "",
    "EVERY TEAM, from most to least likely to be chopped after the waivers:",
    vapply(seq_len(nrow(tm)), line, character(1)),
    "",
    if (nrow(movers_dn)) sprintf("BIGGEST DROPS IN CHANCE OF BEING CHOPPED (the waivers helped most): %s.", paste(sprintf("%s, from %s to %s", movers_dn$team_name, pctx(movers_dn$chance_before), pctx(movers_dn$chance_after)), collapse = "; ")),
    if (nrow(movers_up)) sprintf("BIGGEST RISES IN CHANCE OF BEING CHOPPED (others improved and this team did not keep up): %s.", paste(sprintf("%s, from %s to %s", movers_up$team_name, pctx(movers_up$chance_before), pctx(movers_up$chance_after)), collapse = "; ")),
    if (!is.null(inj)) c(paste0(sprintf("INJURY NEWS (players who would be in their fantasy team's starting lineup if healthy and who are listed for the first time this week; the statuses are Sleeper's as of %s and can change before kickoff", asof),
                                if (!grepl("Wednesday", asof)) sprintf(" (these injury figures use projections refreshed on %s, so they can differ a little from the figures elsewhere in this sheet)", asof) else "",
                                "; Out means will not play, Doubtful means unlikely to play, Questionable means uncertain; the chance of being chopped follows Sleeper's projections: when Sleeper projects a player for 0.0 points it expects him to sit, and the chance assumes he sits. Write the 'Injuries' section from this block only, give the status and what it does to the team's chance of being chopped, and never say what the injury is):"),
                         vapply(seq_len(nrow(inj)), inj_line, character(1)), ""),
    if (nrow(heavy_bye)) sprintf("TEAMS HIT HARDEST BY BYES (estimated projected points lost): %s.", paste(sprintf("%s, %.1f", heavy_bye$team_name, heavy_bye$bye_cost), collapse = "; ")),
    if (nrow(contested)) sprintf("MOST CONTESTED WAIVER PLAYERS: %s.", paste(sprintf("%s, %s: %d teams bid, %s paid the winning $%.0f", contested$player, contested$position, contested$bidders, contested$winner, contested$win_bid), collapse = "; "))), collapse = "\n")
}

PREVIEW_SYSTEM_PROMPT <- paste(
  "You write the weekly PREVIEW for a fantasy football 'guillotine' league (best ball; the lowest-scoring team each week is chopped).",
  "A tracker gives each team a projected score and a chance of being chopped. You are given a FACT SHEET written on Wednesday morning, after the waivers ran and before any game. Write a short preview for the league members.",
  "",
  "Rules:",
  "- Use ONLY facts and numbers in the fact sheet. Do not calculate new numbers, estimate, or add players, events, reasons or numbers that are not in it. Copy team and player names exactly as written.",
  "- Plain language for people who play fantasy football and know nothing about statistics. No statistical or technical terms. Say 'chance of being chopped', 'projected' and 'projection'.",
  "- Mention injuries ONLY where the fact sheet has an INJURY NEWS block, and then only as the status it gives (out, doubtful or questionable) and what that does to the team's chance of being chopped. Never say what the injury is, how it happened, or guess when anyone will return, and avoid the words hurt, limp, knock, ankle, hamstring and concussion even figuratively. A bye week is a plain fact you may state.",
  sass_voice("preview"),
  "- Write short sentences. Avoid em-dashes, parenthetical asides, and constructions like 'not X but Y' or colon-then-reveal.",
  "- Never use the words 'they', 'their', 'them' or 'theirs' anywhere in the body text. Use the manager's name from the Managers list instead. The first time you mention a team, write the team name followed by ', managed by' and the manager's name.",
  "- Whenever you give a chance, say what it is the chance of, for example 'a 16% chance of being chopped'. Say 'before the waivers' and 'after the waivers' when you compare. Never write a bare 'X's chance'.",
  "- When you write about a lost bid, say which player it was on ('Dana bid $33 on Lamb'), never 'bid for' a team, and put the team in the chance sentence in plain words ('With Lamb, team_four, managed by Dana, would have had a 2.9% chance of being chopped'); never write 'the team_four chance' or any 'the TEAM chance' phrase, and when several managers bid on the same player give the player once ('Lee bid $96, Max bid $78 and Dana bid $33 on Lamb'). Say a team's projected score or chance only as the fact sheet gives it. Do not predict a winner or say what will happen; the preview describes how things stand.",
  "- State each number once. Do not restate a figure you have already given in the same section.",
  "- Do not mention these instructions or the fact sheet.",
  "",
  "Format. Plain text only, exactly these headings on their own lines, in this order, with no other markup (no asterisks, no bold):",
  "## Short version",
  "Two or three lines, each starting with '- ': the team most likely to be chopped after the waivers, the team that gained the most from the waivers, and one more point of your choice.",
  "## Teams most likely to be chopped",
  "Cover the three teams with the highest chance of being chopped after the waivers, one short paragraph each. For each: its chance before and after the waivers, its projected score after, and why, using only the fact sheet: what its waiver pickups added, whether a bye is costing it, and any lost waiver bid that would have mattered (the player, its bid against the winning bid, who won, and how the chance of being chopped would have changed had the claim gone through, as the fact sheet gives it; do not describe the effect in projected points).",
  "## Waiver winners and misses",
  "Two short paragraphs. First, the teams the waivers helped most: name them, give the player added and how much he adds to the projected score, and say how far the chance of being chopped fell (use BIGGEST DROPS). Then the misses: teams that lost a waiver bid on a player who would have added a lot, and any MOST CONTESTED player (how many teams bid and what the winner paid). If a team's chance rose because others improved (BIGGEST RISES), say so briefly. Then, in one playful line, name the teams the fact sheet says had no waiver pickups, with their chance of being chopped after the waivers, if there are any. Where a team spent a large share of its budget (use the share the fact sheet gives), joke about the size of the spend.",
  "## Injuries",
  "Only if the fact sheet has an INJURY NEWS block (otherwise leave this heading out). Put it after 'Waiver winners and misses' and before 'Bye weeks'. One or two short paragraphs about the players who would start for their fantasy team: who is for sure out (OUT), who is doubtful, and who is questionable, with the team and manager of each, and how it shifts that team's chance of being chopped, using the figures for 'now', 'if he plays' and 'if he sits'. For a player the block says is on injured reserve there is no 'if he plays' figure, because a player on injured reserve cannot play: say he is on injured reserve and out, give only the chance of being chopped with him out, and never write an 'if he plays' figure for him. For everyone else say what Sleeper's projection expects: Sleeper projecting him for 0.0 points means Sleeper expects him to sit, and a projection above that means Sleeper expects him to play. Never write 'the model counts him'; name Sleeper's projection as the reason. Lead with the biggest swings. Say that statuses are as of this morning and can change before kickoff. Do not list a player the block does not list.",
  "## Bye weeks",
  "Only if the fact sheet has TEAMS HIT HARDEST BY BYES (otherwise leave this heading out). Name the NFL teams on a bye and the fantasy teams that lose the most (the players on a bye, their usual projection, and the estimated points lost). The estimated points lost is the net drop in the team's projected score after other players fill the lineup, so never present it as the sum of the players' projections; say which bye players would be in the lineup and which would be on the bench anyway. Mention a hit team's chance of being chopped when it is notable.",
  sep = "\n")

check_preview <- function(text, fact_sheet) {
  reasons <- character(0)
  sections <- parse_narrative(text)
  need <- c("Short version", "Teams most likely to be chopped", "Waiver winners and misses", if (grepl("INJURY NEWS", fact_sheet, fixed = TRUE)) "Injuries", if (grepl("TEAMS HIT HARDEST BY BYES", fact_sheet, fixed = TRUE)) "Bye weeks")
  if (is.null(sections) || !all(need %in% names(sections))) reasons <- c(reasons, sprintf("missing section(s): %s", paste(setdiff(need, names(sections)), collapse = ", ")))
  if (grepl("hurt|limp|knock|ankle|hamstring|concussion", text, ignore.case = TRUE)) reasons <- c(reasons, "uses a banned injury word")
  if (grepl("[*_`]{2}|^#{3,}", text)) reasons <- c(reasons, "contains markdown formatting")
  if (grepl("fact sheet", text, ignore.case = TRUE)) reasons <- c(reasons, "mentions the fact sheet")
  if (grepl("achievement|\\bthe system\\b|crawler", text, ignore.case = TRUE)) reasons <- c(reasons, "uses the books' own vocabulary (achievement / the System / crawler)")
  team_names <- sub(": managed by .*$", "", sub("^- ", "", grep("^- .+: managed by ", strsplit(fact_sheet, "\n")[[1]], value = TRUE)))
  masked <- gsub("(?m)^##[^\n]*$", " ", text, perl = TRUE)
  for (tn in team_names[order(-nchar(team_names))]) masked <- gsub(tn, " ", masked, fixed = TRUE)
  if (grepl("\\b(they|their|theirs|them)\\b", masked, ignore.case = TRUE, perl = TRUE)) reasons <- c(reasons, "uses they/their/them instead of the manager's name")
  strip_names <- function(x) { for (u in all_usernames()) x <- gsub(u, " ", x, fixed = TRUE); x }
  allowed <- extract_numbers(strip_names(fact_sheet)); used <- extract_numbers(strip_names(text))
  bad <- used[!vapply(used, function(n) any(abs(allowed - n) <= if (n == round(n)) 0.5001 else 0.0501), logical(1))]
  if (length(bad)) reasons <- c(reasons, sprintf("numbers not in the fact sheet: %s", paste(unique(bad), collapse = ", ")))
  list(ok = length(reasons) == 0, reasons = reasons, sections = sections)
}

# The automatic text, used when no draft exists
template_preview <- function(pv, nfl_bye_teams = character(0)) {
  tm <- pv$teams; top <- head(tm, 3); n <- pv$n_teams
  mgr <- function(r) manager_label(r$owner_name)
  drop <- tm[order(tm$chance_after - tm$chance_before), ][1, ]
  short <- c(sprintf("- %s, managed by %s, is the team most likely to be chopped after the waivers, with a %s chance of being chopped.", top$team_name[1], mgr(top[1, ]), pctx(top$chance_after[1])),
             if (drop$chance_after < drop$chance_before - 0.01) sprintf("- %s, managed by %s, gained the most from the waivers: the chance of being chopped fell from %s to %s.", drop$team_name, mgr(drop), pctx(drop$chance_before), pctx(drop$chance_after)),
             sprintf("- %d waiver claims were won in Wednesday morning's waiver run.", pv$claims_won))
  risk <- vapply(seq_len(nrow(top)), function(i) { r <- top[i, ]
    sprintf("%s, managed by %s, has a %s chance of being chopped after the waivers, against %s before the waivers. The projected score is %.1f.%s%s", r$team_name, mgr(r), pctx(r$chance_after), pctx(r$chance_before), r$proj_after,
            if (r$n_bye > 0) sprintf(" %d %s on a bye, which lowers the projected score by an estimated %.1f.", r$n_bye, plural(r$n_bye, "player is", "players are"), r$bye_cost) else "",
            if (nrow(lo <- pv$lost[pv$lost$roster_id == r$roster_id & !is.na(pv$lost$chance_if_won), ]) > 0) sprintf(" Had the lost waiver bid on %s gone through, the chance of being chopped would have been %s.", lo$player[1], pctx(lo$chance_if_won[1])) else "") }, character(1))
  sec <- list(`Short version` = short, `Teams most likely to be chopped` = risk,
              `Waiver winners and misses` = sprintf("%d claims were won in Wednesday morning's waiver run. The biggest gain in the chance of being chopped was %s.", pv$claims_won, if (drop$chance_after < drop$chance_before - 0.01) sprintf("%s's, from %s to %s", drop$team_name, pctx(drop$chance_before), pctx(drop$chance_after)) else "small"))
  ij <- if (is.null(pv$injuries)) NULL else pv$injuries[pv$injuries$new & pv$injuries$swing >= 0.01, ]
  if (!is.null(ij) && nrow(ij)) { ij <- head(ij[order(-ij$swing), ], 3)
    sec[["Injuries"]] <- vapply(seq_len(nrow(ij)), function(i) { r <- ij[i, ]
      if (is.na(r$chance_if_plays)) return(sprintf("%s, %s, is on injured reserve for %s, managed by %s, so he cannot play. The chance of being chopped is %s with him out. Statuses can change before kickoff.", r$player, r$position, r$team_name, manager_label(r$owner_name), pctx(r$chance_now)))
      sprintf("%s, %s, is %s for %s, managed by %s. Sleeper %s. The chance of being chopped is %s now, %s if he plays and %s if he sits. Statuses can change before kickoff.", r$player, r$position,
              switch(r$status, Out = "out", IR = "out", PUP = "out", Doubtful = "doubtful", "questionable"), r$team_name, manager_label(r$owner_name), if (r$assumed == "sitting") "projects him for 0.0 points, so it expects him to sit" else sprintf("projects him for %.1f points, so it expects him to play", r$sleeper_proj), pctx(r$chance_now), pctx(r$chance_if_plays), pctx(r$chance_if_sits)) }, character(1)) }
  hb <- tm[tm$bye_cost >= 5, ]; hb <- hb[order(-hb$bye_cost), ]
  if (nrow(hb)) sec[["Bye weeks"]] <- sprintf("%s %s.", if (length(nfl_bye_teams)) paste("With", join_and(nfl_bye_teams), "on a bye, the fantasy teams that lose the most to byes:") else "The fantasy teams that lose the most to byes:", paste(sprintf("%s, an estimated %.1f projected points", hb$team_name, hb$bye_cost), collapse = "; "))
  sec
}

# The draft workflow, as for the recaps: write the fact sheet and the prompt, use narrative_wNN-style draft if it passes the checks, else the automatic text
make_preview_text <- function(fact_sheet, template, out_prefix, narrative_file = NULL, log = function(...) message(sprintf(...))) {
  load_recap_env()
  notes_f <- paste0(out_prefix, ".notes.txt")
  notes <- if (file.exists(notes_f)) trimws(paste(readLines(notes_f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")) else ""
  notes_block <- if (nzchar(notes)) paste0("\n\nEDITOR'S NOTES FOR THIS WEEK (follow these; they apply to this week only):\n", notes) else ""
  lf <- file.path("collector", "league_facts.txt")
  facts_txt <- if (file.exists(lf)) trimws(paste(readLines(lf, warn = FALSE, encoding = "UTF-8"), collapse = "\n")) else ""
  league_block <- if (nzchar(facts_txt)) paste0("\n\nLEAGUE FACTS (true; you may use them in jokes, and they add no numbers):\n", facts_txt) else ""
  writeLines(enc2utf8(fact_sheet), paste0(out_prefix, ".facts.txt"), useBytes = TRUE)
  writeLines(enc2utf8(paste0(PREVIEW_SYSTEM_PROMPT, "\n\nFACT SHEET\n\n", fact_sheet, league_block, notes_block, "\n\nWrite the preview now.")), paste0(out_prefix, ".prompt.txt"), useBytes = TRUE)
  draft <- paste0(out_prefix, ".draft.txt")
  use <- if (!is.null(narrative_file)) narrative_file else if (file.exists(draft)) draft else NULL
  if (!is.null(use)) {
    text <- paste(readLines(use, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    chk <- check_preview(text, fact_sheet)
    if (chk$ok) {
      unlink(paste0(out_prefix, ".rejected.txt"))
      writeLines(enc2utf8(text), paste0(out_prefix, ".txt"), useBytes = TRUE); writeLines(c("draft", "a chat with Claude"), paste0(out_prefix, ".kind"), useBytes = TRUE)
      return(list(text = text, sections = chk$sections, kind = "draft"))
    }
    writeLines(enc2utf8(text), paste0(out_prefix, ".rejected.txt"), useBytes = TRUE)
    log("preview text REJECTED (%s); using the automatic text. Draft saved to %s.rejected.txt", paste(chk$reasons, collapse = " | "), out_prefix)
  }
  ttext <- sections_to_text(template)
  tchk <- check_preview(ttext, fact_sheet)
  if (!tchk$ok) log("preview text: WARNING the automatic text failed its own checks (%s)", paste(tchk$reasons, collapse = " | "))
  writeLines(enc2utf8(ttext), paste0(out_prefix, ".txt"), useBytes = TRUE); writeLines(c("template", "a script"), paste0(out_prefix, ".kind"), useBytes = TRUE)
  list(text = ttext, sections = template, kind = "template")
}

# --- Figure and page section ----------------------------------------------------------------------------------------
# One row per team: the chance of being chopped before the waivers (hollow dot) to after (filled dot); teal = it fell, red = it rose.
plot_preview_chances <- function(teams, week, save_path = NULL, width = 9, height = NULL, dpi = 150) {
  d <- teams[order(teams$chance_after), ]; d$team <- factor(d$team_name, levels = d$team_name)
  d$dir <- ifelse(d$chance_after < d$chance_before - 0.002, "Fell", ifelse(d$chance_after > d$chance_before + 0.002, "Rose", "No change"))
  cols <- c(Fell = FIG$survive, Rose = FIG$chop, `No change` = FIG$muted)
  p <- ggplot2::ggplot(d, ggplot2::aes(y = team)) +
    ggplot2::geom_segment(ggplot2::aes(x = chance_before, xend = chance_after, yend = team, color = dir), linewidth = 1.6, alpha = 0.85, arrow = grid::arrow(length = grid::unit(0.11, "inches"), type = "closed")) +
    ggplot2::geom_point(ggplot2::aes(x = chance_before), shape = 21, size = 3.2, fill = FIG$bg, color = FIG$dim, stroke = 1) +
    ggplot2::geom_text(ggplot2::aes(x = chance_after, label = pctx(chance_after), hjust = ifelse(chance_after >= chance_before, -0.45, 1.45)), color = FIG$text, size = 4) +   # label at the arrow head, on the side the arrow points to
    ggplot2::scale_color_manual(values = cols, name = NULL) +
    ggplot2::scale_x_continuous(labels = function(x) sprintf("%.0f%%", 100 * x), expand = ggplot2::expansion(mult = c(0.06, 0.14))) +
    ggplot2::labs(x = "Chance of being chopped", y = NULL, title = sprintf("Week %d preview: chance of being chopped, before and after the waivers", week),
                  subtitle = "Hollow dot: before the waivers. Arrow head and number: after. Same projections both times.") +
    theme_sleeper(legend = "top") + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), plot.title.position = "plot")
  if (is.null(height)) height <- 0.38 * nrow(d) + 2.3
  if (!is.null(save_path)) ggplot2::ggsave(save_path, p, width = width, height = height, dpi = dpi, bg = FIG$bg)
  p
}

# The page section for one preview (everything it needs is in preview_*_wNN.csv, preview_wNN.txt and img/preview_wN.png)
# ids on the preview's text headings, for its jump bar
PV_SECTION_IDS <- c(`Teams most likely to be chopped` = "pv-sec-teams", `Waiver winners and misses` = "pv-sec-waivers", Injuries = "pv-sec-injuries", `Bye weeks` = "pv-sec-byes")

preview_section_html <- function(out_dir, w) {
  rd <- function(f) { p <- file.path(out_dir, sprintf(f, w)); if (file.exists(p)) readr::read_csv(p, show_col_types = FALSE) else NULL }
  tm <- rd("preview_w%02d.csv"); if (is.null(tm)) return("")
  pk <- rd("preview_pickups_w%02d.csv"); by <- rd("preview_byes_w%02d.csv")
  team_names <- tm$team_name
  tf <- file.path(out_dir, sprintf("preview_w%02d.txt", w))
  sec <- if (file.exists(tf)) parse_narrative(apply_manager_key(paste(readLines(tf, warn = FALSE, encoding = "UTF-8"), collapse = "
"), team_names)) else NULL
  kf <- file.path(out_dir, sprintf("preview_w%02d.kind", w)); kind <- if (file.exists(kf)) readLines(kf, warn = FALSE, encoding = "UTF-8") else character(0)
  money <- function(x) ifelse(is.na(x), "-", sprintf("$%.0f", x)); pp <- function(x) sprintf("%.1f", x)
  mgr <- function(x) vapply(x, manager_label, character(1))
  t1 <- data.frame(Team = tm$team_name, Manager = mgr(tm$owner_name), `Chance before` = pctx(tm$chance_before), `Chance after` = pctx(tm$chance_after),
                   Projected_before = pp(tm$proj_before), Projected_after = pp(tm$proj_after), Change = sprintf("%+.1f", round(tm$proj_after - tm$proj_before, 1) + 0),
                   `Waiver pickups` = tm$n_pickups, `Waiver spend` = ifelse(tm$spent_run > 0, money(tm$spent_run), "-"), `Share of budget` = ifelse(tm$spent_run > 0, sprintf("%.0f%%", 100 * tm$spent_share), "-"), `On a bye` = tm$n_bye, `Bye cost` = ifelse(tm$bye_cost > 0, pp(tm$bye_cost), "-"), check.names = FALSE, stringsAsFactors = FALSE)
  names(t1)[names(t1) == "Projected_before"] <- "Projected before"; names(t1)[names(t1) == "Projected_after"] <- "Projected after"
  chg <- tm$chance_after - tm$chance_before
  t1_html <- html_table(t1, num_cols = c("Chance before", "Chance after", "Projected before", "Projected after", "Change", "Waiver pickups", "Waiver spend", "Share of budget", "On a bye", "Bye cost"), sortable = TRUE,
                        cell_cls = list(`Chance after` = ifelse(tm$chance_after >= 2 / nrow(tm), "heat-red", ifelse(tm$chance_after >= 1.4 / nrow(tm), "heat-orange", ""))),
                        sort_vals = list(`Chance before` = round(tm$chance_before, 4), `Chance after` = round(tm$chance_after, 4), `Projected before` = tm$proj_before, `Projected after` = tm$proj_after,
                                         Change = round(tm$proj_after - tm$proj_before, 2), `Waiver spend` = tm$spent_run, `Share of budget` = round(tm$spent_share, 4), `Bye cost` = round(tm$bye_cost, 2)))
  pk_html <- if (!is.null(pk) && nrow(pk)) {
    pk <- pk[order(-pk$adds, -pk$price), ]
    d <- data.frame(Player = sprintf("%s, %s (%s)", pk$player, pk$position, pk$nfl_team), `Claimed by` = sprintf('%s<span class="t2">(%s)</span>', vapply(pk$team_name, h, character(1)), vapply(mgr(pk$owner_name), h, character(1))), Price = money(pk$price), `Second-highest bid` = ifelse(is.na(pk$second_bid), "-", sprintf('$%.0f<span class="t2">(%s)</span>', pk$second_bid, vapply(ifelse(is.na(match(pk$second_team, tm$team_name)), pk$second_team, manager_label(tm$owner_name[match(pk$second_team, tm$team_name)])), h, character(1)))),
                    Overpay = ifelse(is.na(pk$second_bid), "-", sprintf("$%.0f", pk$price - pk$second_bid)), `Dropped` = ifelse(is.na(pk$dropped), "-", pk$dropped),
                    `Projected this week` = ifelse(pk$bye, "bye", pp(pk$proj)), `Adds to the team` = sprintf("%+.1f", round(pk$adds, 1) + 0), check.names = FALSE, stringsAsFactors = FALSE)
    paste0('<h4 id="pv-pickups" class="sublabel">Waiver pickups</h4>', '<p class="fine">Every claim won in Wednesday morning\'s waiver run. Second-highest bid is the best losing bid on the player in the same run, with the manager who made it underneath. Overpay is the price minus the second-highest bid (a dash when nobody else bid). Adds to the team is the team\'s projected best-ball score with the player minus its score with the claim undone (the player dropped to make room comes back). A player on a bye adds nothing this week. A negative number means the player dropped to make room would have helped this week\'s lineup more, for example by filling a position where the team is thin.</p>',
           html_table(d, num_cols = c("Price", "Second-highest bid", "Overpay", "Projected this week", "Adds to the team"), raw_cols = c("Claimed by", "Second-highest bid"), sortable = TRUE, sort_vals = list(Price = pk$price, `Second-highest bid` = pk$second_bid, Overpay = pk$price - pk$second_bid, `Projected this week` = ifelse(pk$bye, 0, pk$proj), `Adds to the team` = round(pk$adds, 2)))) } else ""
  by_html <- if (!is.null(by) && nrow(by)) {
    by <- by[order(by$team_name, -ifelse(is.na(by$usual), 0, by$usual)), ]
    d <- data.frame(Team = by$team_name, Manager = mgr(by$owner_name), Player = sprintf("%s, %s (%s)", by$player, by$position, by$nfl_team), `Usually projected` = ifelse(is.na(by$usual), "-", pp(by$usual)), `Would be in the lineup` = ifelse(by$would_start, "Yes", "No"), check.names = FALSE, stringsAsFactors = FALSE)
    paste0('<h4 id="pv-byes" class="sublabel">Players on a bye</h4>', '<p class="fine">Usually projected is the player\'s average pregame projection in the weeks he has played this season. Would be in the lineup says whether he would start if he were playing at that level. Bye cost, in the table above, is the net drop in the team\'s projected score once the other players fill the lineup, so it is not the sum of these projections: a player who would sit on the bench anyway adds nothing to it.</p>',
           html_table(d, num_cols = "Usually projected", sortable = TRUE, sort_vals = list(`Usually projected` = ifelse(is.na(by$usual), NA, round(by$usual, 2))))) } else ""
  # the short version first, then the figure, then the rest of the text
  story_of <- function(x) if (is.null(x) || !length(x)) "" else paste0('<div class="story">', narrative_html(x), "</div>")
  ij <- rd("preview_injuries_w%02d.csv")
  ij_html <- if (!is.null(ij) && nrow(ij[ij$new, ])) { ij <- ij[ij$new, ]; ij <- ij[order(-ij$swing), ]
    sp <- if ("sleeper_proj" %in% names(ij)) ij$sleeper_proj else ifelse(ij$assumed == "sitting", 0, ij$healthy_proj)
    d <- data.frame(Player = sprintf("%s, %s (%s)", ij$player, ij$position, ij$nfl_team), Team = sprintf('%s<span class="t2">(%s)</span>', vapply(ij$team_name, h, character(1)), vapply(mgr(ij$owner_name), h, character(1))),
                    `Sleeper's status` = ij$status, `Sleeper's projection` = sprintf("%.1f", sp), `Sleeper expects him to` = ifelse(ij$assumed == "sitting", "sit", "play"),
                    `Chance of being chopped now` = pctx(ij$chance_now), `If he plays` = ifelse(is.na(ij$chance_if_plays), "-", pctx(ij$chance_if_plays)), `If he sits` = pctx(ij$chance_if_sits), check.names = FALSE, stringsAsFactors = FALSE)
    asof <- if ("asof" %in% names(ij)) ij$asof[1] else "the morning the preview was built"
    paste0('<h4 id="pv-injuries" class="sublabel">Injury news</h4>', sprintf('<p class="fine">Players who would be in their fantasy team\'s starting lineup if healthy and who are listed for the first time this week (a player who has been out for weeks is not listed). Statuses are Sleeper\'s as of %s and can change before kickoff.%s Sleeper\'s status is the designation Sleeper lists for him. Sleeper\'s projection is the points Sleeper projects for him this week: 0.0 means Sleeper expects him to sit, and anything above that means it expects him to play. The tracker follows that, so the first chance is the team\'s chance of being chopped with him playing or sitting as Sleeper expects. The next two columns show the same chance if he plays and if he sits, with the rest of the league unchanged.</p>', asof,
                                                if (!grepl("Wednesday", asof)) sprintf(" The projections were refreshed on %s, so these chances can differ a little from the table above.", asof) else ""),
           html_table(d, num_cols = c("Sleeper's projection", "Chance of being chopped now", "If he plays", "If he sits"), raw_cols = "Team", sortable = TRUE, sort_vals = list(`Sleeper's projection` = round(sp, 2), `Chance of being chopped now` = round(ij$chance_now, 4), `If he plays` = round(ij$chance_if_plays, 4), `If he sits` = round(ij$chance_if_sits, 4)))) } else ""
  # every section of the text after the short version ends with a link to its table further down
  see_all <- c(`Teams most likely to be chopped` = "pv-teams|See every team, before and after the waivers", `Waiver winners and misses` = if (nzchar(pk_html)) "pv-pickups|See all waiver pickups",
               Injuries = if (nzchar(ij_html)) "pv-injuries|See all injury news", `Bye weeks` = if (nzchar(by_html)) "pv-byes|See all players on a bye")
  one_section <- function(n) {
    lk <- see_all[n]; link <- if (!is.na(lk)) { parts <- strsplit(unname(lk), "|", fixed = TRUE)[[1]]; sprintf('<p class="fine seeall"><a href="#%s">%s &darr;</a></p>', parts[1], parts[2]) } else ""
    paste0('<div class="story">', narrative_html(sec[n], ids = PV_SECTION_IDS), link, "</div>")
  }
  meta <- rd("preview_meta_w%02d.csv")
  proj_when <- if (!is.null(meta) && !is.na(meta$projections[1])) meta$projections[1] else "Wednesday morning"
  inj_when <- if (!is.null(meta) && !is.na(meta$injuries[1])) meta$injuries[1] else proj_when
  disclaimer <- sprintf('<div class="ai disclaimer">This preview was generated using Sleeper\'s player projections %s%s, and may not reflect any updates Sleeper has made since then, up to kickoff.</div>',
                        if (identical(proj_when, inj_when)) sprintf("and injury statuses from %s", h(proj_when)) else sprintf("from %s", h(proj_when)),
                        if (identical(proj_when, inj_when)) "" else sprintf(" and injury statuses from %s", h(inj_when)))
  story_top <- if (is.null(sec)) "" else story_of(sec[names(sec) == "Short version"])
  story_rest <- if (is.null(sec)) "" else paste(vapply(setdiff(names(sec), "Short version"), one_section, character(1)), collapse = "")
  img <- file.path(out_dir, "img", sprintf("preview_w%d.png", w))
  fig <- if (file.exists(img)) sprintf('<figure><div class="plate" style="max-width:820px"><img src="img/preview_w%d.png" loading="lazy" alt="Each team\'s chance of being chopped before and after the waivers."></div></figure>', w) else ""
  # the jump bar: one chip per text section that exists, in the colors of the headings, then the tables
  jb <- list(list("j-chop", "At risk", "pv-sec-teams", "Teams most likely to be chopped"), list("j-money", "Waivers", "pv-sec-waivers", "Waiver winners and misses"),
             list("j-close", "Injuries", "pv-sec-injuries", "Injuries"), list("j-reading", "Byes", "pv-sec-byes", "Bye weeks"), list("j-budget", "The numbers", "pv-numbers", NA))
  jb <- Filter(function(x) is.na(x[[4]]) || (!is.null(sec) && x[[4]] %in% names(sec)), jb)
  jump <- paste0('<nav class="jump" aria-label="Jump to a part of the preview">', paste(vapply(jb, function(x) sprintf('<a class="%s" href="#%s">%s</a>', x[[1]], x[[3]], h(x[[2]])), character(1)), collapse = ""), "</nav>")
  paste0(sprintf('<section class="preview" id="preview-%d" data-week="%d"><h2>Week %d preview</h2>', w, w, w), jump,
         disclaimer, '<p class="note">Written Wednesday morning, after the waivers ran and before any game. It shows each team\'s projection and chance of being chopped before the waivers and after them, using the same projections both times, so any change comes from the waiver moves alone.</p>',
         story_top, fig, story_rest, '<h3 id="pv-numbers" class="wvh h-numbers">The numbers</h3><h4 id="pv-teams" class="sublabel">Every team, before and after the waivers</h4>',
         '<p class="fine">Chance is the chance of being chopped this week. Red: twice the even split or more after the waivers. Orange: at least 1.4 times the even split. Waiver spend is what the team paid in Wednesday morning\'s waiver run, and Share of budget is that as a share of the waiver budget it had left going in. Bye cost is the net drop in the projected score caused by players on a bye, after other players fill the lineup. Click a heading to sort.</p>',
         t1_html, pk_html, ij_html, by_html, "</section>")
}
