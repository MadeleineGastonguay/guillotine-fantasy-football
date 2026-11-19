# --- Retrospective replay: what would the app have said at each game minute? --
# Rebuilds every player's points-so-far at each minute of a past (or finished)
# game from nflverse play-by-play, pairs it with Sleeper's archived pre-game
# projection, and runs the result through the app's OWN live-projection formula
# (add_live_projection_columns() in R/pipeline.R). The output is one row per
# player per game-minute holding the pre-game distribution, the current
# distribution, the game clock, the matchup, whether the player started or
# came in as a backup, and the actual final points -- everything needed to
# check how accurate the distributions (and so the elimination
# probabilities) are, for any past week, without waiting for live games.
#
# Entry point: replay_week(season, week). See collector/README.md.

REPLAY_DIR <- file.path("accuracy_data", "replay")
REPLAY_SKILL <- c("QB", "RB", "WR", "TE")
BACKTEST_CACHE_DIR <- file.path("cache", "backtest")

# Raw stat fields tracked per play, named exactly like Sleeper's scoring
# settings so score_stats()/the league's weights apply directly.
REPLAY_STAT_FIELDS <- c("pass_att", "pass_cmp", "pass_yd", "pass_td", "pass_int", "pass_2pt",
                        "rush_att", "rush_yd", "rush_td", "rush_2pt",
                        "tgt", "rec", "rec_yd", "rec_td", "rec_2pt", "fum_lost")
REPLAY_CUM_KEEP <- c("pass_att", "pass_cmp", "pass_yd", "pass_td", "pass_int",
                     "rush_att", "rush_yd", "rush_td", "tgt", "rec", "rec_yd", "rec_td", "fum_lost")

# --- Sleeper archives ---------------------------------------------------------

# kind: "stats" (actual weekly stat lines, incl. gs/off_snp/tm_off_snp) or
# "projections". Reads the backtest's cache when present. A week that is not
# finished yet is fetched fresh and NOT cached (partial data must not stick).
load_sleeper_archive <- function(kind, season, week, cache_final_only = TRUE) {
  f <- file.path(BACKTEST_CACHE_DIR, sprintf("%s_%s_wk%02d.rds", kind, season, week))
  if (file.exists(f)) return(readRDS(f))

  url <- sprintf("https://api.sleeper.app/%s/nfl/%s/%s?season_type=regular", kind, season, week)
  raw <- with_retry(function() {
    httr2::request(url) |> httr2::req_user_agent("guillotine-tracker-replay/0.1") |>
      httr2::req_perform() |> httr2::resp_body_json(simplifyVector = FALSE)
  })
  out <- list()
  for (e in raw) {
    pid <- e$player_id
    if (is.null(pid) || is.null(e$stats) || length(e$stats) == 0) next
    out[[pid]] <- list(position = e$player$position %||% NA_character_,
                       stats = setNames(as.numeric(unlist(e$stats)), names(e$stats)))
  }
  state <- tryCatch(get_nfl_state(), error = function(e) NULL)
  finished <- !is.null(state) && (as.integer(season) < as.integer(state$season) ||
                                    (as.integer(season) == as.integer(state$season) && as.integer(week) < as.integer(state$week)))
  if (finished || !cache_final_only) {
    dir.create(BACKTEST_CACHE_DIR, showWarnings = FALSE, recursive = TRUE)
    saveRDS(out, f)
  }
  out
}

# --- Play-by-play -> per-player stat events -------------------------------------

# One row per (player, play) with that play's stat increments and the play's
# elapsed game time (seconds at the start of the play; OT plays are placed after
# 3600). Only plays that credit a stat to a player produce rows.
pbp_stat_events <- function(pbp, week) {
  p <- pbp[pbp$week == week & pbp$season_type == "REG", , drop = FALSE]
  num <- function(x) dplyr::coalesce(as.numeric(x), 0)
  base <- data.frame(
    game_id = p$game_id,
    t = ifelse(p$qtr <= 4, 3600 - p$game_seconds_remaining, 3600 + (600 - p$quarter_seconds_remaining)),
    passer = p$passer_player_id, rusher = p$rusher_player_id,
    receiver = p$receiver_player_id, fumbler = p$fumbled_1_player_id,
    pass_attempt = num(p$pass_attempt), complete_pass = num(p$complete_pass),
    passing_yards = num(p$passing_yards), receiving_yards = num(p$receiving_yards),
    rushing_yards = num(p$rushing_yards), rush_attempt = num(p$rush_attempt),
    pass_touchdown = num(p$pass_touchdown), rush_touchdown = num(p$rush_touchdown),
    interception = num(p$interception), fumble_lost = num(p$fumble_lost),
    two_pt = as.numeric(num(p$two_point_attempt) == 1 & p$two_point_conv_result %in% "success"),
    stringsAsFactors = FALSE
  )
  zero <- function(n) as.data.frame(matrix(0, n, length(REPLAY_STAT_FIELDS), dimnames = list(NULL, REPLAY_STAT_FIELDS)))
  role_rows <- function(id_col, fill) {
    keep <- !is.na(base[[id_col]])
    b <- base[keep, , drop = FALSE]
    out <- cbind(data.frame(game_id = b$game_id, gsis_id = b[[id_col]], t = b$t, stringsAsFactors = FALSE), zero(nrow(b)))
    for (nm in names(fill)) out[[nm]] <- fill[[nm]](b)
    out
  }
  ev <- rbind(
    role_rows("passer", list(pass_att = function(b) b$pass_attempt, pass_cmp = function(b) b$complete_pass,
                             pass_yd = function(b) b$passing_yards, pass_td = function(b) b$pass_touchdown,
                             pass_int = function(b) b$interception, pass_2pt = function(b) b$two_pt)),
    role_rows("rusher", list(rush_att = function(b) b$rush_attempt, rush_yd = function(b) b$rushing_yards,
                             rush_td = function(b) b$rush_touchdown, rush_2pt = function(b) b$two_pt)),
    role_rows("receiver", list(tgt = function(b) b$pass_attempt, rec = function(b) b$complete_pass,
                               rec_yd = function(b) b$receiving_yards, rec_td = function(b) b$pass_touchdown,
                               rec_2pt = function(b) b$two_pt)),
    role_rows("fumbler", list(fum_lost = function(b) b$fumble_lost))
  )
  ev <- ev[!is.na(ev$t), , drop = FALSE] # plays with no game clock (e.g. some end-of-period markers)
  ev[order(ev$game_id, ev$gsis_id, ev$t), , drop = FALSE]
}

# Points each event is worth under the league's scoring weights.
score_event_matrix <- function(ev, scoring_settings) {
  w <- vapply(REPLAY_STAT_FIELDS, function(f) as.numeric(scoring_settings[[f]] %||% 0), numeric(1))
  as.numeric(as.matrix(ev[, REPLAY_STAT_FIELDS, drop = FALSE]) %*% w)
}

# Position-specific standardized-shape quantiles, optionally leaving a season
# out of the pool (out-of-sample): list(position -> named quantile vector).
replay_shape_quantiles <- function(exclude_season = NULL, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)) {
  pool <- get_position_shape_pool(exclude_season)
  pool[["ALL"]] <- unlist(pool, use.names = FALSE)
  lapply(pool, function(z) stats::quantile(z, probs, names = FALSE))
}

replay_game_clock <- function(t_seconds, final = FALSE) {
  if (final) return("Final")
  if (t_seconds <= 0) return("Q1 15:00")
  q <- min(4L, as.integer(t_seconds %/% 900) + 1L)
  rem <- max(0, q * 900 - t_seconds)
  sprintf("Q%d %d:%02d", q, as.integer(rem %/% 60), as.integer(round(rem %% 60)) %% 60)
}

# --- The replay ------------------------------------------------------------------

# season/week: a finished (or partly played) NFL regular-season week.
# step_minutes: snapshot spacing in game minutes (1 = every minute).
# exclude_season_from_shape: leave `season` out of the residual-shape pool used for
#   the quantile columns (TRUE = out-of-sample evaluation of that season).
# write: save data/replay file; the returned tibble is the same either way.
replay_week <- function(season, week, step_minutes = 1, exclude_season_from_shape = TRUE,
                        write = TRUE, league_id = LEAGUE_ID) {
  message(sprintf("replay %s week %s: loading archives...", season, week))
  scoring <- with_retry(function() get_league(league_id))$scoring_settings

  proj <- load_sleeper_archive("projections", season, week)
  actual <- load_sleeper_archive("stats", season, week)
  xw <- load_id_crosswalk()
  pbp_all <- load_nflverse_pbp(season)
  ev <- pbp_stat_events(pbp_all, week)
  ev$points <- score_event_matrix(ev, scoring)

  pbp_wk <- pbp_all[pbp_all$week == week & pbp_all$season_type == "REG", , drop = FALSE]
  games <- unique(pbp_wk[, c("game_id", "home_team", "away_team")])
  # team a player played for = the team on offense in plays crediting them
  inv <- rbind(
    data.frame(gsis_id = pbp_wk$passer_player_id, team = pbp_wk$posteam, stringsAsFactors = FALSE),
    data.frame(gsis_id = pbp_wk$rusher_player_id, team = pbp_wk$posteam, stringsAsFactors = FALSE),
    data.frame(gsis_id = pbp_wk$receiver_player_id, team = pbp_wk$posteam, stringsAsFactors = FALSE)
  )
  inv <- unique(inv[!is.na(inv$gsis_id) & !is.na(inv$team), ])
  snaps <- tryCatch(load_nflverse_snap_counts(season), error = function(e) NULL)
  if (!is.null(snaps)) {
    sn <- snaps[snaps$week == week & snaps$game_type == "REG", c("pfr_player_id", "team")]
    sn <- merge(sn, xw[!is.na(xw$pfr_id) & !is.na(xw$gsis_id), c("pfr_id", "gsis_id")], by.x = "pfr_player_id", by.y = "pfr_id")
    inv <- unique(rbind(inv, data.frame(gsis_id = sn$gsis_id, team = sn$team, stringsAsFactors = FALSE)))
  }
  inv <- inv[!duplicated(inv$gsis_id), ] # one team per player-week (mid-week trades are rare)

  # Universe: Sleeper-projected skill players (with a crosswalk id and a team
  # that actually played this week), plus anyone credited with a stat.
  sk <- names(proj)[vapply(proj, function(e) (e$position %||% "") %in% REPLAY_SKILL, logical(1))]
  uni <- tibble::tibble(
    player_id = sk,
    position = vapply(proj[sk], function(e) e$position, character(1)),
    full_week_projection = vapply(proj[sk], function(e) pmax(score_stats(e$stats, scoring), 0), numeric(1))
  )
  # Players credited with a stat but absent from Sleeper's projections (surprise
  # backups): kept, with a pre-game projection of 0 -- these are exactly the
  # "came in unexpectedly" cases the accuracy check needs to see.
  extra_ids <- setdiff(unique(ev$gsis_id), xw$gsis_id[match(uni$player_id, xw$sleeper_id)])
  extra <- xw[!is.na(xw$gsis_id) & xw$gsis_id %in% extra_ids, c("sleeper_id", "gsis_id")]
  extra$position <- vapply(extra$sleeper_id, function(pid) actual[[pid]]$position %||% proj[[pid]]$position %||% NA_character_, character(1))
  extra <- extra[extra$position %in% REPLAY_SKILL, , drop = FALSE]
  if (nrow(extra) > 0) {
    uni <- dplyr::bind_rows(uni, tibble::tibble(player_id = extra$sleeper_id, position = extra$position, full_week_projection = 0))
  }
  uni <- uni |>
    dplyr::inner_join(dplyr::select(xw, player_id = sleeper_id, gsis_id), by = "player_id") |>
    dplyr::inner_join(inv, by = "gsis_id") |>
    dplyr::filter(full_week_projection >= 0.5 | gsis_id %in% ev$gsis_id) |>
    dplyr::mutate(nfl_team = team) |>
    dplyr::select(-team)
  # Attach the game (one game per team per week) and matchup/context.
  gm <- rbind(
    data.frame(nfl_team = games$home_team, opponent = games$away_team, home_away = "home", game_id = games$game_id, stringsAsFactors = FALSE),
    data.frame(nfl_team = games$away_team, opponent = games$home_team, home_away = "away", game_id = games$game_id, stringsAsFactors = FALSE)
  )
  uni <- dplyr::inner_join(uni, gm, by = "nfl_team")
  uni$matchup <- ifelse(uni$home_away == "home", paste(uni$opponent, "@", uni$nfl_team), paste(uni$nfl_team, "@", uni$opponent))
  uni$matchup_label <- ifelse(uni$home_away == "home", paste("vs", uni$opponent), paste("@", uni$opponent))

  sched <- tryCatch({
    s <- load_nflverse_schedules()
    s[s$game_id %in% games$game_id, c("game_id", "home_team", "spread_line", "total_line", "roof", "temp", "wind")]
  }, error = function(e) NULL)
  if (!is.null(sched)) {
    uni <- dplyr::left_join(uni, sched[, c("game_id", "spread_line", "total_line", "roof", "temp", "wind")], by = "game_id")
    uni$team_spread <- ifelse(uni$home_away == "home", -uni$spread_line, uni$spread_line)
    uni$implied_team_total <- uni$total_line / 2 - uni$team_spread / 2
  }

  # Role fields from Sleeper's archived weekly stats (official started flag +
  # snaps), and first involvement time from the play-by-play.
  stat_of <- function(pid, key) {
    v <- actual[[pid]]$stats[key]
    if (is.null(v) || length(v) == 0 || is.na(v)) NA_real_ else unname(v)
  }
  uni$gs <- vapply(uni$player_id, stat_of, numeric(1), key = "gs")
  # Sleeper omits `gs` for anyone who did not start; a player with a stats
  # record and no `gs` therefore started 0 games (NA = no record at all).
  has_record <- uni$player_id %in% names(actual)
  uni$gs <- ifelse(is.na(uni$gs) & has_record, 0, uni$gs)
  uni$off_snp <- vapply(uni$player_id, stat_of, numeric(1), key = "off_snp")
  uni$tm_off_snp <- vapply(uni$player_id, stat_of, numeric(1), key = "tm_off_snp")
  uni$snap_share <- ifelse(!is.na(uni$off_snp) & !is.na(uni$tm_off_snp) & uni$tm_off_snp > 0, uni$off_snp / uni$tm_off_snp, NA_real_)
  uni$final_actual_points <- vapply(uni$player_id, function(pid) {
    a <- actual[[pid]]$stats
    if (is.null(a)) NA_real_ else score_stats(a, scoring)
  }, numeric(1))
  first_t <- tapply(ev$t, ev$gsis_id, min)
  uni$first_involved_seconds <- as.numeric(first_t[uni$gsis_id])
  uni$role_final <- dplyr::case_when(
    !is.na(uni$gs) & uni$gs == 1 ~ "starter",
    (!is.na(uni$off_snp) & uni$off_snp > 0) | !is.na(uni$first_involved_seconds) ~ "backup_entered",
    TRUE ~ "inactive_or_dnp"
  )
  # Depth-chart order as of the start of the week (latest snapshot on or before
  # the week's first game day): nflverse depth charts are timestamped snapshots
  # with pos_rank 1, 2, 3 ... within each position.
  uni$depth_chart_order <- NA_integer_
  depth <- tryCatch(load_nflverse_depth_charts(season), error = function(e) NULL)
  if (!is.null(depth) && all(c("dt", "gsis_id", "pos_abb", "pos_rank") %in% names(depth)) && !is.null(sched)) {
    week_start <- min(as.Date(load_nflverse_schedules()$gameday[load_nflverse_schedules()$game_id %in% games$game_id]))
    d <- depth[!is.na(depth$gsis_id) & depth$pos_abb %in% REPLAY_SKILL & as.Date(substr(depth$dt, 1, 10)) <= week_start, ]
    d <- d[order(d$dt, decreasing = TRUE), ]
    d <- d[!duplicated(d$gsis_id), ]
    uni$depth_chart_order <- as.integer(d$pos_rank[match(uni$gsis_id, d$gsis_id)])
  }

  # Cumulative stats per player-game, sampled on the minute grid (+ a final row).
  ev$key <- paste(ev$game_id, ev$gsis_id)
  cum_by_key <- lapply(split(ev, ev$key), function(g) {
    cm <- apply(as.matrix(g[, c(REPLAY_CUM_KEEP, "points")]), 2, cumsum)
    list(t = g$t, cum = matrix(cm, nrow = nrow(g), dimnames = list(NULL, c(REPLAY_CUM_KEEP, "points"))))
  })
  grid <- seq(0, 3600, by = step_minutes * 60)
  # game-level score timeline for team_score/opp_score
  score_tl <- lapply(split(pbp_wk, pbp_wk$game_id), function(g) {
    tt <- ifelse(g$qtr <= 4, 3600 - g$game_seconds_remaining, 3600 + (600 - g$quarter_seconds_remaining))
    ok <- !is.na(tt) & !is.na(g$total_home_score) & !is.na(g$total_away_score)
    o <- order(tt[ok])
    list(t = tt[ok][o], home = g$total_home_score[ok][o], away = g$total_away_score[ok][o])
  })
  last_t <- max(ev$t, na.rm = TRUE)

  rows <- lapply(seq_len(nrow(uni)), function(i) {
    u <- uni[i, ]
    ce <- cum_by_key[[paste(u$game_id, u$gsis_id)]]
    tg <- c(grid, Inf) # Inf = final row (everything that happened, including OT)
    idx <- if (is.null(ce)) rep(0L, length(tg)) else findInterval(tg, ce$t)
    cum <- matrix(0, length(tg), length(REPLAY_CUM_KEEP) + 1, dimnames = list(NULL, c(REPLAY_CUM_KEEP, "points")))
    if (!is.null(ce)) { ok <- idx > 0; cum[ok, ] <- ce$cum[idx[ok], , drop = FALSE] }
    sc <- score_tl[[u$game_id]]
    si <- findInterval(tg, sc$t)
    hs <- ifelse(si > 0, sc$home[pmax(si, 1)], 0); as_ <- ifelse(si > 0, sc$away[pmax(si, 1)], 0)
    hs[is.infinite(tg)] <- tail(sc$home, 1); as_[is.infinite(tg)] <- tail(sc$away, 1)
    tibble::tibble(
      grid_seconds = ifelse(is.infinite(tg), NA_real_, tg),
      is_final_row = is.infinite(tg),
      locked_points = cum[, "points"],
      team_score = if (u$home_away == "home") hs else as_,
      opp_score = if (u$home_away == "home") as_ else hs
    ) |> dplyr::bind_cols(tibble::as_tibble(cum[, REPLAY_CUM_KEEP, drop = FALSE]) |> stats::setNames(paste0("cum_", REPLAY_CUM_KEEP))) |>
      dplyr::mutate(uni_row = i)
  })
  snaps_df <- dplyr::bind_rows(rows)
  out <- dplyr::bind_cols(uni[snaps_df$uni_row, ], dplyr::select(snaps_df, -uni_row))

  out$elapsed_fraction <- ifelse(out$is_final_row, 1, pmin(1, out$grid_seconds / 3600))
  out$game_clock <- mapply(replay_game_clock, ifelse(out$is_final_row, 3600, out$grid_seconds), out$is_final_row)
  out$period <- ifelse(out$is_final_row, NA_integer_, pmin(4L, as.integer(out$grid_seconds %/% 900) + 1L))
  out$entered_yet <- !is.na(out$first_involved_seconds) & (out$is_final_row | out$first_involved_seconds <= out$grid_seconds)

  # The app's own live-projection math (R/pipeline.R), plus the comparison
  # model (R/bayesian.R) -- same replay, both models, so they can be compared
  # on identical player-minutes instead of two separate replay runs.
  out <- add_live_projection_columns(out) |> add_bayesian_projection_columns()

  q_shape <- replay_shape_quantiles(if (exclude_season_from_shape) season else NULL)
  qz <- function(pos) q_shape[[pos]] %||% q_shape[["ALL"]]
  qnames <- c("q05", "q25", "q50", "q75", "q95")
  cur_mean <- out$locked_points + out$remaining_mean
  bayes_cur_mean <- out$locked_points + out$bayes_remaining_mean
  pre_sd <- out$pregame_sd_underdog
  for (k in seq_along(qnames)) {
    zk <- vapply(out$position, function(p) qz(p)[k], numeric(1))
    out[[paste0("cur_", qnames[k])]] <- pmax(cur_mean + out$remaining_sd_underdog * zk, PLAYER_FLOOR)
    out[[paste0("pre_", qnames[k])]] <- pmax(out$full_week_projection + pre_sd * zk, PLAYER_FLOOR)
    # Same empirical shape, fed the BAYESIAN mean/SD instead -- isolates the
    # comparison to "does the mean/SD evolve differently", not a difference in
    # what shape is assumed (bayes_pre_q* isn't needed: at t=0 both models'
    # mean/SD are identical, so pre_q* already covers the Bayesian pregame case).
    out[[paste0("bayes_cur_", qnames[k])]] <- pmax(bayes_cur_mean + out$remaining_sd_bayesian * zk, PLAYER_FLOOR)
  }

  shape_label <- if (exclude_season_from_shape) paste0("excluding_", season) else "all_seasons"
  out <- out |>
    dplyr::mutate(
      season = !!season, week = !!week,
      pre_proj = full_week_projection, pre_sd = pregame_sd_underdog,
      pre_cv = dplyr::if_else(full_week_projection > 0, pregame_sd_underdog / full_week_projection, NA_real_),
      current_points = locked_points,
      projected_final = locked_points + remaining_mean,
      projected_sd = remaining_sd_underdog,
      projected_cv = dplyr::if_else(locked_points + remaining_mean > 0, remaining_sd_underdog / (locked_points + remaining_mean), NA_real_),
      bayes_projected_final = locked_points + bayes_remaining_mean,
      bayes_projected_sd = remaining_sd_bayesian,
      shape_pool = shape_label
    ) |>
    dplyr::select(
      season, week, game_id, matchup, matchup_label, opponent, home_away,
      game_clock, period, grid_seconds, elapsed_fraction, is_final_row, team_score, opp_score,
      player_id, gsis_id, position, nfl_team,
      pre_proj, pre_sd, pre_cv, current_points, projected_final, projected_sd, projected_cv,
      remaining_mean, blended_rate, bayes_projected_final, bayes_projected_sd, bayes_remaining_mean,
      dplyr::starts_with("pre_q"), dplyr::starts_with("cur_q"), dplyr::starts_with("bayes_cur_q"),
      role_final, entered_yet, first_involved_seconds, gs, off_snp, tm_off_snp, snap_share, depth_chart_order,
      dplyr::starts_with("cum_"),
      dplyr::any_of(c("team_spread", "total_line", "implied_team_total", "roof", "temp", "wind")),
      final_actual_points, shape_pool
    )

  if (write) {
    dir.create(REPLAY_DIR, showWarnings = FALSE, recursive = TRUE)
    path <- file.path(REPLAY_DIR, sprintf("%s_w%02d_players.csv.gz", season, week))
    tmp <- paste0(path, ".tmp")
    readr::write_csv(out, tmp)
    file.rename(tmp, path)
    message(sprintf("replay %s week %s: wrote %s (%d rows, %d players)", season, week, path, nrow(out), dplyr::n_distinct(out$player_id)))
  }
  invisible(out)
}

# --- League (team-level) replay --------------------------------------------------

# Replays THIS league's week: for every replay grid time, each roster's current /
# projected score and its elimination probability (the app's Approximate model,
# exact arithmetic, so no simulation is needed), next to what actually happened
# (each team's final score and whether it was the one chopped). Only meaningful
# for the league's own weeks (rosters come from Sleeper's matchups endpoint).
#
# Timeline: every game is placed at the SAME elapsed game minute (a synthetic
# "everything kicks off together" clock), not real wall-clock time -- staggered
# kickoffs are not modelled. Compare by elapsed fraction, not by time of day.
replay_league_week <- function(season, week, players_replay, league_id = LEAGUE_ID, write = TRUE) {
  matchups <- with_retry(function() get_matchups(league_id, week))
  ros <- purrr::map_dfr(matchups, function(m) {
    ids <- unlist(m$players %||% list())
    if (length(ids) == 0) return(NULL)
    tibble::tibble(roster_id = m$roster_id, player_id = as.character(ids), final_team_points = m$points %||% NA_real_)
  })
  if (nrow(ros) == 0) stop("no roster data for league week ", week)

  lookup <- build_user_lookup(with_retry(function() get_users(league_id)))
  owners <- purrr::map_dfr(with_retry(function() get_rosters(league_id)), function(r) tibble::tibble(roster_id = r$roster_id, owner_id = r$owner_id))
  names_tbl <- dplyr::left_join(owners, lookup, by = "owner_id")[, c("roster_id", "team_name")]

  slim <- slim_players_table()[, c("player_id", "position")]
  grid_rows <- players_replay |>
    dplyr::distinct(is_final_row, grid_seconds) |>
    dplyr::arrange(is_final_row, grid_seconds)

  per_time <- lapply(seq_len(nrow(grid_rows)), function(gi) {
    g <- grid_rows[gi, ]
    at <- players_replay[players_replay$is_final_row == g$is_final_row &
                           (is.na(g$grid_seconds) | players_replay$grid_seconds == g$grid_seconds), ]
    rt <- ros |>
      dplyr::left_join(dplyr::select(slim, player_id, pos_slim = position), by = "player_id") |>
      dplyr::left_join(dplyr::select(at, player_id, position, current_points, remaining_mean,
                                     remaining_sd_underdog = projected_sd, pre_proj, elapsed_fraction), by = "player_id") |>
      dplyr::mutate(
        position = dplyr::coalesce(position, pos_slim),
        locked_points = dplyr::coalesce(current_points, 0),
        remaining_mean = dplyr::coalesce(remaining_mean, 0),
        remaining_sd_underdog = dplyr::coalesce(remaining_sd_underdog, 0)
      ) |>
      dplyr::filter(position %in% REPLAY_SKILL)
    meta <- rt |>
      dplyr::group_by(roster_id) |>
      dplyr::summarise(
        naive_projected_total = optimal_lineup_points(position, locked_points + remaining_mean),
        pregame_projected_total = optimal_lineup_points(position, dplyr::coalesce(pre_proj, 0)),
        current_score = optimal_lineup_points(position, locked_points), # best lineup from points already scored (see R/pipeline.R)
        mean_elapsed_fraction = mean(dplyr::coalesce(elapsed_fraction, 1)),
        .groups = "drop"
      )
    res <- run_elimination_instant(rt, meta, sd_col = "remaining_sd_underdog")
    tibble::tibble(
      season = season, week = week, grid_seconds = g$grid_seconds, is_final_row = g$is_final_row,
      roster_id = res$roster_id, current_score = res$current_score,
      projected_final = res$naive_projected_total, pregame_projected = res$pregame_projected_total,
      projected_sd = res$projected_sd, elimination_prob = res$elimination_prob,
      mean_elapsed_fraction = res$mean_elapsed_fraction
    )
  })
  out <- dplyr::bind_rows(per_time) |>
    dplyr::left_join(names_tbl, by = "roster_id") |>
    dplyr::left_join(dplyr::distinct(ros, roster_id, final_team_points), by = "roster_id") |>
    dplyr::group_by(grid_seconds, is_final_row) |>
    dplyr::ungroup()
  lowest <- ros |> dplyr::distinct(roster_id, final_team_points)
  chopped_id <- lowest$roster_id[which.min(lowest$final_team_points)]
  out$chopped <- out$roster_id == chopped_id # lowest final score of the week (the chop rule)
  out <- dplyr::select(out, season, week, grid_seconds, is_final_row, mean_elapsed_fraction, roster_id, team_name,
                       current_score, projected_final, pregame_projected, projected_sd, elimination_prob,
                       final_team_points, chopped)

  if (write) {
    dir.create(REPLAY_DIR, showWarnings = FALSE, recursive = TRUE)
    path <- file.path(REPLAY_DIR, sprintf("%s_w%02d_league_teams.csv.gz", season, week))
    tmp <- paste0(path, ".tmp")
    readr::write_csv(out, tmp)
    file.rename(tmp, path)
    message(sprintf("league replay %s week %s: wrote %s (%d rows)", season, week, path, nrow(out)))
  }
  invisible(out)
}

# Sleeper's matchup endpoint gives each roster as it stood at the END of the week: a player added during the week is on it, a player
# dropped during the week is not. A replay that used that list for every minute would show the post-move roster before the move
# happened. This turns the week's completed transactions (add / drop / trade; `t$adds` and `t$drops` map player_id -> roster_id) into
# intervals: `from` = when the player joined the roster (-Inf = on it from the start), `to` = when he left it (Inf = still on it at the
# end). Working backward from the final roster, newest move first, each move inside [window_start, window_end] (UTC seconds) is undone
# for the time before it. Moves outside the window (the main Wednesday waiver run, before the games) change nothing here; the league's early-morning Friday/Saturday/Sunday waiver runs are inside it and are applied.
# Limitation: a player who was dropped and later re-added gets one interval (the later stint), not two.
roster_intervals <- function(ros, transactions, window_start = -Inf, window_end = Inf) {
  ros$from <- -Inf; ros$to <- Inf
  tx <- Filter(function(t) identical(t$status, "complete") && t$type %in% c("free_agent", "waiver", "trade", "commissioner"), transactions %||% list())
  if (!length(tx)) return(ros)
  ts <- vapply(tx, function(t) as.numeric(t$status_updated %||% t$created %||% NA) / 1000, numeric(1))
  keep <- !is.na(ts) & ts >= window_start & ts <= window_end
  tx <- tx[keep]; ts <- ts[keep]
  for (k in order(ts, decreasing = TRUE)) {
    t <- tx[[k]]
    for (pid in names(t$adds %||% list())) {            # on the roster only from this moment
      i <- which(ros$roster_id == t$adds[[pid]] & ros$player_id == pid)
      if (length(i)) ros$from[i[1]] <- ts[k]
    }
    for (pid in names(t$drops %||% list())) {           # was on the roster until this moment (unless he is already there from a later add)
      rid <- t$drops[[pid]]
      if (!any(ros$roster_id == rid & ros$player_id == pid)) {
        ros <- dplyr::bind_rows(ros, tibble::tibble(roster_id = rid, player_id = pid,
                                                    final_team_points = ros$final_team_points[match(rid, ros$roster_id)], from = -Inf, to = ts[k]))
      }
    }
  }
  ros
}

# Same team-level replay as replay_league_week(), but on the REAL kickoff clock instead of a
# synthetic "everything starts together" one -- what the live app actually showed, with games
# starting Thursday / Sunday 1pm / Sunday 4:25 / Sunday night / Monday and each progressing on
# its own clock. Each game's real wall-clock timeline comes from nflverse play-by-play's
# per-play `time_of_day` stamps: for every game, wall time -> game seconds elapsed (so halftime,
# commercial breaks and stoppages are real, not assumed). A game's players are then read from
# the 1-minute player replay (`players_replay`, from replay_week()) at whatever game second that
# game has reached at each wall-clock step, and its final row once the game is over.
#
# Only time during which some game is on is evaluated (steps every `step_minutes`, plus
# `pad_minutes` before each block's first kickoff so the pre-kickoff odds are included);
# overnight gaps carry no information. Blocks of games separated by more than `gap_hours` of dead
# time (e.g. Thursday night vs. Sunday) get their own `block`, and `x_min` is a compressed
# "active minutes" axis with a fixed visual gap between blocks, for plotting.
#
# Elimination probabilities are the app's Approximate model (run_elimination_instant()), same as
# the live app's. `current_score` is the best-ball lineup from points scored so far
# (R/pipeline.R). Also writes a small games table (kickoff/end on the same axis) for plotting.
replay_league_week_realtime <- function(season, week, players_replay, step_minutes = 5, pad_minutes = 10,
                                        gap_hours = 4, visual_gap_minutes = 60,
                                        league_id = LEAGUE_ID, write = TRUE, keep_rt_steps = integer(0), time_aware_rosters = TRUE) {
  matchups <- with_retry(function() get_matchups(league_id, week))
  ros <- purrr::map_dfr(matchups, function(m) {
    ids <- unlist(m$players %||% list())
    if (length(ids) == 0) return(NULL)
    tibble::tibble(roster_id = m$roster_id, player_id = as.character(ids), final_team_points = m$points %||% NA_real_)
  })
  if (nrow(ros) == 0) stop("no roster data for league week ", week)
  lookup <- build_user_lookup(with_retry(function() get_users(league_id)))
  owners <- purrr::map_dfr(with_retry(function() get_rosters(league_id)), function(r) tibble::tibble(roster_id = r$roster_id, owner_id = r$owner_id))
  names_tbl <- dplyr::left_join(owners, lookup, by = "owner_id")[, c("roster_id", "team_name", "owner_name")]
  slim <- slim_players_table()[, c("player_id", "position")]
  transactions <- if (time_aware_rosters) tryCatch(with_retry(function() get_transactions(league_id, week)), error = function(e) NULL) else NULL # mid-week add/drops, for roster_intervals()

  # --- each game's wall clock (UTC seconds) <-> game seconds elapsed ---
  pbp <- load_nflverse_pbp(season)
  p <- pbp[pbp$week == week & pbp$season_type == "REG" & pbp$game_id %in% unique(players_replay$game_id), , drop = FALSE]
  wall <- as.numeric(as.POSIXct(sub("Z$", "", p$time_of_day), format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  t_el <- ifelse(p$qtr <= 4, 3600 - p$game_seconds_remaining, 3600 + (600 - p$quarter_seconds_remaining))
  ok <- !is.na(wall) & !is.na(t_el)
  clocks <- lapply(split(data.frame(wall = wall[ok], t = t_el[ok]), p$game_id[ok]), function(g) {
    g <- g[order(g$wall, g$t), ]
    g$t <- cummax(g$t) # stamps are occasionally a few seconds out of order; a game clock never runs backwards
    list(start = min(g$wall), end = max(g$wall) + 30, wall = g$wall, t = g$t) # +30s: the last stamped play precedes the final whistle
  })
  game_ids <- names(clocks)
  starts <- vapply(clocks, `[[`, numeric(1), "start")
  ends <- vapply(clocks, `[[`, numeric(1), "end")

  # --- blocks of play and the step grid ---
  o <- order(starts)
  blk <- integer(length(o)); b <- 1L; cur_end <- ends[o[1]]
  for (i in seq_along(o)) {
    if (i > 1) {
      if (starts[o[i]] - cur_end > gap_hours * 3600) { b <- b + 1L; cur_end <- ends[o[i]] } else cur_end <- max(cur_end, ends[o[i]])
    }
    blk[o[i]] <- b
  }
  block_tbl <- tibble::tibble(block = seq_len(max(blk))) |>
    dplyr::mutate(
      b_start = vapply(block, function(k) min(starts[blk == k]) - pad_minutes * 60, numeric(1)),
      b_end = vapply(block, function(k) max(ends[blk == k]) + 10 * 60, numeric(1)),
      offset = cumsum(dplyr::lag((b_end - b_start) / 60 + visual_gap_minutes, default = 0)),
      label = format(as.POSIXct(b_start + pad_minutes * 60, origin = "1970-01-01", tz = "UTC"), "%a %m/%d", tz = "America/New_York")
    )
  steps <- dplyr::bind_rows(lapply(seq_len(nrow(block_tbl)), function(k) {
    w <- seq(block_tbl$b_start[k], block_tbl$b_end[k], by = step_minutes * 60)
    tibble::tibble(wall = w, block = k, x_min = block_tbl$offset[k] + (w - block_tbl$b_start[k]) / 60)
  }))

  # each roster as it actually stood at every step (see roster_intervals()); `ros` stays the end-of-week roster for the final scores
  ros_iv <- roster_intervals(ros, transactions, window_start = min(steps$wall), window_end = max(steps$wall))
  n_moves <- sum(is.finite(ros_iv$from)) + sum(is.finite(ros_iv$to))
  if (n_moves) message(sprintf("realtime league replay %s week %s: %d mid-week roster move(s) applied", season, week, n_moves))

  pr <- players_replay |>
    dplyr::mutate(player_id = as.character(player_id),
                  key = paste(game_id, ifelse(is_final_row, "F", as.integer(grid_seconds))))
  by_key <- split(seq_len(nrow(pr)), pr$key)
  pr_cols <- c("player_id", "position", "current_points", "remaining_mean", "projected_sd", "pre_proj", "elapsed_fraction")

  message(sprintf("realtime league replay %s week %s: %d steps over %d block(s)", season, week, nrow(steps), nrow(block_tbl)))
  rt_keep <- list() # per-player tables at the steps named in keep_rt_steps, for cross-checking other models (analysis/simulation_spot_check.R)
  per_step <- lapply(seq_len(nrow(steps)), function(si) {
    W <- steps$wall[si]
    keys <- vapply(game_ids, function(g) {
      cl <- clocks[[g]]
      if (W >= cl$end) return(paste(g, "F"))
      s <- if (W <= cl$start) 0 else stats::approx(cl$wall, cl$t, xout = W, ties = max, rule = 2)$y
      paste(g, as.integer(min(3600, floor(s / 60) * 60)))
    }, character(1))
    at <- pr[unlist(by_key[keys], use.names = FALSE), pr_cols]
    rt <- ros_iv[ros_iv$from <= W & W < ros_iv$to, c("roster_id", "player_id", "final_team_points")] |>
      dplyr::left_join(dplyr::select(slim, player_id, pos_slim = position), by = "player_id") |>
      dplyr::left_join(at, by = "player_id") |>
      dplyr::mutate(
        position = dplyr::coalesce(position, pos_slim),
        locked_points = dplyr::coalesce(current_points, 0),
        remaining_mean = dplyr::coalesce(remaining_mean, 0),
        remaining_sd_underdog = dplyr::coalesce(projected_sd, 0),
        elapsed_fraction = dplyr::coalesce(elapsed_fraction, 1), # no game found in the replay (bye / not projected): nothing more to come
        remaining_fraction = 1 - elapsed_fraction
      ) |>
      dplyr::filter(position %in% REPLAY_SKILL)
    meta <- rt |>
      dplyr::group_by(roster_id) |>
      dplyr::summarise(
        naive_projected_total = optimal_lineup_points(position, locked_points + remaining_mean),
        pregame_projected_total = optimal_lineup_points(position, dplyr::coalesce(pre_proj, 0)),
        current_score = optimal_lineup_points(position, locked_points), # best lineup from points already scored (see R/pipeline.R)
        mean_elapsed_fraction = mean(elapsed_fraction),
        .groups = "drop"
      )
    if (si %in% keep_rt_steps) rt_keep[[as.character(si)]] <<- list(rt = rt, meta = meta)
    res <- run_elimination_instant(rt, meta, sd_col = "remaining_sd_underdog")
    tibble::tibble(
      step = si, roster_id = res$roster_id, current_score = res$current_score,
      projected_final = res$naive_projected_total, pregame_projected = res$pregame_projected_total,
      projected_sd = res$projected_sd, elimination_prob = res$elimination_prob,
      mean_elapsed_fraction = res$mean_elapsed_fraction, complete = res$complete
    )
  })

  finals <- ros |> dplyr::distinct(roster_id, final_team_points)
  chopped_id <- finals$roster_id[which.min(finals$final_team_points)] # the chop rule: lowest final score of the week
  out <- dplyr::bind_rows(per_step) |>
    dplyr::left_join(dplyr::mutate(steps, step = dplyr::row_number()), by = "step") |>
    dplyr::left_join(names_tbl, by = "roster_id") |>
    dplyr::left_join(finals, by = "roster_id") |>
    dplyr::left_join(dplyr::select(block_tbl, block, block_label = label), by = "block") |>
    dplyr::mutate(season = season, week = week, chopped = roster_id == chopped_id,
                  wall_utc = format(as.POSIXct(wall, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S")) |>
    dplyr::select(season, week, wall_utc, block, block_label, x_min, roster_id, team_name, owner_name, current_score,
                  projected_final, pregame_projected, projected_sd, elimination_prob, mean_elapsed_fraction,
                  complete, final_team_points, chopped)

  # where each game sits on the compressed axis, for the plots' kickoff marks
  games_tbl <- tibble::tibble(game_id = game_ids, block = blk, start = starts, end = ends) |>
    dplyr::left_join(dplyr::select(block_tbl, block, b_start, offset), by = "block") |>
    dplyr::transmute(season = season, week = week, game_id, block,
                     x_start = offset + (start - b_start) / 60, x_end = offset + (end - b_start) / 60,
                     kickoff_et = format(as.POSIXct(start, origin = "1970-01-01", tz = "UTC"), "%a %H:%M", tz = "America/New_York"))

  if (write) {
    dir.create(REPLAY_DIR, showWarnings = FALSE, recursive = TRUE)
    for (nm in c("teams", "games")) {
      path <- file.path(REPLAY_DIR, sprintf("%s_w%02d_league_%s_realtime.csv.gz", season, week, nm))
      tmp <- paste0(path, ".tmp")
      readr::write_csv(if (nm == "teams") out else games_tbl, tmp)
      file.rename(tmp, path)
      message(sprintf("realtime league replay %s week %s: wrote %s", season, week, path))
    }
  }
  invisible(list(teams = out, games = games_tbl, steps = steps, rt = rt_keep, ros = ros, ros_iv = ros_iv, transactions = transactions))
}
