# --- Elimination-probability history ----------------------------------------
# Every time a refresh brings NEW data (see fetch_pipeline() in app.R), one row
# per team is appended to data/elim_history/{season}_w{WW}.csv with that team's
# elimination probability from the Approximate model, for later analysis or
# charting (see analysis/backfill_elim_history.R, which rebuilds this same
# file retroactively from logs/player_projection_log.csv).
#
# An earlier version of this file also had a plotly-based "Odds over time" tab
# plotting this data live in the app -- removed because plotly::plotlyOutput()
# is evaluated while the UI is built, at app startup, so a deploy environment
# without the plotly package installed (e.g. shinyapps.io, if plotly isn't
# picked up as a dependency) crashed the WHOLE app immediately, not just that
# tab. The data collection itself doesn't need plotly and was kept.
#
# Rows are only written when the model inputs actually changed (the same input
# hash the app already uses to skip redundant simulations), so:
#   * two browser sessions refreshing the same data don't write duplicates, and
#   * the x-axis (in any future chart) would be "snapshot number", not clock
#     time -- idle stretches between Thursday / Sunday / Monday games take no
#     space on the chart.
#
# Note: on shinyapps.io the file system is temporary (cleared on redeploy or
# when the instance is recycled), so history there only lasts that long.

ELIM_HISTORY_DIR <- file.path("data", "elim_history")

elim_history_file <- function(season, week) {
  file.path(ELIM_HISTORY_DIR, sprintf("%s_w%02d.csv", season, as.integer(week)))
}

read_elim_history <- function(season, week) {
  f <- elim_history_file(season, week)
  if (!file.exists(f)) return(NULL)
  out <- readr::read_csv(
    f, show_col_types = FALSE,
    col_types = readr::cols(
      ts_utc = readr::col_character(), input_hash = readr::col_character(),
      team_name = readr::col_character(), .default = readr::col_guess()
    )
  )
  if (nrow(out) == 0) return(NULL)
  out$ts <- as.POSIXct(out$ts_utc, tz = "UTC")
  out
}

# Returns TRUE if rows were written, FALSE if skipped (same inputs as the last
# stored snapshot). approx_res: result of run_elimination_instant() -- the
# Approximate model, which is exact arithmetic (no Monte Carlo noise, so the
# lines are smooth) and is ready the moment new data arrives, without waiting
# for the ~10s simulation.
append_elim_history <- function(snap, approx_res, input_hash) {
  f <- elim_history_file(snap$season, snap$week)
  dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
  input_hash <- as.character(input_hash %||% NA_character_)

  if (file.exists(f)) {
    prev <- read_elim_history(snap$season, snap$week)
    if (!is.null(prev) && !is.na(input_hash) && identical(tail(prev$input_hash, 1), input_hash)) return(FALSE)
  }

  rows <- data.frame(
    ts_utc = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    season = snap$season,
    week = snap$week,
    input_hash = input_hash,
    roster_id = approx_res$roster_id,
    team_name = approx_res$team_name,
    approx_prob = approx_res$elimination_prob,
    projected = approx_res$naive_projected_total,
    projected_sd = approx_res$projected_sd,
    current_score = approx_res$current_score,
    players_started = approx_res$players_started,
    players_total = approx_res$players_total,
    stringsAsFactors = FALSE
  )
  # readr (not base write.csv): real UTF-8 for team names with curly apostrophes/emoji.
  readr::write_csv(rows, f, append = file.exists(f))
  TRUE
}

# --- Odds formatting ---------------------------------------------------------
# American odds for "this team gets eliminated": favorites (>= 50%) are
# negative, longshots positive, e.g. 20% -> +400, 80% -> -400. Probabilities
# are clamped to [1%, 99%] (i.e. never beyond +9900 / -9900) so a 0% or 100%
# doesn't print as infinity or an absurdly long number.
prob_to_american <- function(p) {
  p <- pmin(pmax(p, 0.01), 0.99)
  ifelse(p >= 0.5, -round(100 * p / (1 - p)), round(100 * (1 - p) / p))
}
fmt_american <- function(p) {
  a <- prob_to_american(p)
  ifelse(a > 0, paste0("+", a), as.character(a))
}

# --- Backfill from the app's own projection log --------------------------------
# The app has been appending every refresh's per-player numbers (locked points,
# projected remaining, remaining SD, game status) to
# logs/player_projection_log.csv since the first week. That is everything the
# Approximate model needs, so the elimination-probability history can be
# rebuilt for weeks that were logged before the "Odds over time" tab existed.
#
# Each snapshot is re-run through the SAME functions the live app uses
# (optimal_lineup_assignment / optimal_lineup_points, elimination_prob_instant),
# so backfilled points are computed exactly like live ones. Caveats: each
# snapshot uses the projection numbers as the app computed them at that time
# (the model's blending has been tuned since), and snapshots only exist for
# moments when someone had the app open. Consecutive snapshots with identical
# inputs are collapsed. Existing history files are left alone unless
# overwrite = TRUE.
backfill_elim_history_from_log <- function(log_file = file.path("logs", "player_projection_log.csv"),
                                           sd_col = "remaining_sd_underdog", overwrite = FALSE) {
  stopifnot(file.exists(log_file))
  lg <- readr::read_csv(
    log_file, show_col_types = FALSE,
    col_types = readr::cols(fetched_at = readr::col_character(), team_name = readr::col_character(),
                            full_name = readr::col_character(), player_id = readr::col_character(),
                            .default = readr::col_guess())
  )
  started_status <- c("STATUS_FINAL", "STATUS_IN_PROGRESS", "STATUS_HALFTIME", "STATUS_END_PERIOD")

  wk <- unique(lg[, c("season", "week")])
  for (i in seq_len(nrow(wk))) {
    season <- wk$season[i]; week <- wk$week[i]
    f <- elim_history_file(season, week)
    if (file.exists(f) && !overwrite) {
      message(sprintf("Week %s/%s: history already exists, skipping (overwrite = TRUE to rebuild)", season, week))
      next
    }
    d_wk <- lg[lg$season == season & lg$week == week, ]
    d_wk$remaining_sd <- dplyr::coalesce(d_wk[[sd_col]], 0)
    snaps <- split(d_wk, d_wk$fetched_at)
    snaps <- snaps[order(names(snaps))]

    hashes <- vapply(snaps, function(d) {
      digest::digest(d[order(d$roster_id, d$player_id), c("roster_id", "player_id", "locked_points", "remaining_mean", "remaining_sd")])
    }, character(1))
    keep <- c(TRUE, hashes[-1] != hashes[-length(hashes)]) # drop consecutive duplicates
    message(sprintf("Week %s/%s: %d logged snapshots, %d distinct -> computing", season, week, length(snaps), sum(keep)))

    out <- lapply(which(keep), function(k) {
      d <- snaps[[k]]
      by_team <- split(d, d$roster_id)
      stats <- do.call(rbind, lapply(by_team, function(t) {
        total <- t$locked_points + t$remaining_mean
        slot <- optimal_lineup_assignment(t$position, total)
        st <- slot != "BN"
        data.frame(
          roster_id = t$roster_id[1], team_name = t$team_name[1],
          projected = sum(total[st]), projected_sd = sqrt(sum(pmax(t$remaining_sd[st], 0)^2)),
          current_score = optimal_lineup_points(t$position, t$locked_points), # best lineup from points already scored (see R/pipeline.R)
          players_started = sum(t$status %in% started_status), players_total = sum(!is.na(t$status)),   # bye players (no game, status NA) are not counted
          # every rostered player's game over (bye weeks log elapsed_fraction 1): the team's
          # final score is a known constant -- see elimination_prob_instant()
          complete = all(t$elapsed_fraction >= 1 - 1e-9),
          # best lineup from FINISHED players alone (pending counted as 0): the lowest
          # score the team can still end with -- see elimination_prob_instant()
          floor_score = optimal_lineup_points(t$position, ifelse(t$elapsed_fraction >= 1 - 1e-9, t$locked_points, 0))
        )
      }))
      stats$approx_prob <- elimination_prob_instant(stats$projected, stats$projected_sd, complete = stats$complete, floors = stats$floor_score)
      cbind(ts_utc = names(snaps)[k], season = season, week = week, input_hash = hashes[[k]], stats)
    })
    rows <- do.call(rbind, out)
    rows <- rows[, c("ts_utc", "season", "week", "input_hash", "roster_id", "team_name", "approx_prob",
                     "projected", "projected_sd", "current_score", "players_started", "players_total")]
    dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
    readr::write_csv(rows, f)
    message(sprintf("  wrote %s (%d rows, %d snapshots)", f, nrow(rows), length(out)))
  }
  invisible(TRUE)
}
