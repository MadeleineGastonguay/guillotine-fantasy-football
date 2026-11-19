# --- Player-level projection logging --------------------------------------
# Appends one row per rostered player every time fetch_pipeline() actually
# runs a real refresh (see app.R), so projection accuracy and variance-model
# behavior can be analyzed retrospectively against final actual outcomes --
# e.g. "how did remaining_mean/SD for this player evolve over the course of
# the game, and how did it compare to what they actually scored."
#
# Written with readr::write_csv() rather than base write.table()/write.csv():
# under a non-UTF-8 session locale (this environment runs locale "C"), base
# R's CSV writers silently mangle non-ASCII characters into a literal
# "<U+2019>"-style escape instead of the real UTF-8 bytes -- which shows up
# constantly here since several team names have curly apostrophes.
# readr::write_csv() always writes real UTF-8 regardless of locale.

PROJECTION_LOG_FILE <- file.path("logs", "player_projection_log.csv")

log_player_snapshot <- function(snap, log_file = PROJECTION_LOG_FILE) {
  dir.create(dirname(log_file), showWarnings = FALSE, recursive = TRUE)

  tbl <- snap$roster_table
  safe_cv <- function(sd, mean) ifelse(!is.na(mean) & mean > 0, sd / mean, NA_real_)

  log_rows <- data.frame(
    fetched_at = format(snap$fetched_at, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    week = snap$week,
    season = snap$season,
    roster_id = tbl$roster_id,
    team_name = tbl$team_name,
    player_id = tbl$player_id,
    full_name = tbl$full_name,
    position = tbl$position,
    nfl_team = tbl$nfl_team,
    status = tbl$status,
    elapsed_fraction = round(tbl$elapsed_fraction, 4),
    locked_points = tbl$locked_points,
    full_week_projection = round(tbl$full_week_projection, 3),
    blended_rate = round(tbl$blended_rate, 3),
    remaining_mean = round(tbl$remaining_mean, 3),
    projected_final = round(tbl$locked_points + tbl$remaining_mean, 3),
    remaining_sd_backtest = round(tbl$remaining_sd_backtest, 3),
    remaining_cv_backtest = round(safe_cv(tbl$remaining_sd_backtest, tbl$remaining_mean), 4),
    remaining_sd_underdog = round(tbl$remaining_sd_underdog, 3),
    remaining_cv_underdog = round(safe_cv(tbl$remaining_sd_underdog, tbl$remaining_mean), 4),
    remaining_sd_playerprofiler = round(tbl$remaining_sd_playerprofiler, 3),
    remaining_cv_playerprofiler = round(safe_cv(tbl$remaining_sd_playerprofiler, tbl$remaining_mean), 4),
    remaining_sd_player = round(tbl$remaining_sd_player, 3),
    remaining_cv_player = round(safe_cv(tbl$remaining_sd_player, tbl$remaining_mean), 4),
    stringsAsFactors = FALSE
  )

  readr::write_csv(log_rows, log_file, append = file.exists(log_file))

  invisible(log_rows)
}
