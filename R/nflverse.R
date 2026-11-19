# --- nflverse / player-id downloads (cached) -------------------------------
# Free public archives used by the retrospective replay (R/replay.R). Read
# straight from GitHub releases with base R -- no nflreadr dependency. Past
# seasons never change, so they are cached forever; the CURRENT season's files
# are refreshed when older than a few hours (nflverse updates them nightly).

NFLVERSE_CACHE_DIR <- file.path("accuracy_data", "nflverse_cache") # under accuracy_data/ (excluded from the shinyapps.io bundle, see .rscignore)

nflverse_fetch <- function(tag, file, max_age_hours = Inf) {
  dir.create(NFLVERSE_CACHE_DIR, showWarnings = FALSE, recursive = TRUE)
  dest <- file.path(NFLVERSE_CACHE_DIR, file)
  fresh <- file.exists(dest) &&
    as.numeric(difftime(Sys.time(), file.mtime(dest), units = "hours")) < max_age_hours
  if (!fresh) {
    url <- sprintf("https://github.com/nflverse/nflverse-data/releases/download/%s/%s", tag, file)
    old <- options(timeout = max(600, getOption("timeout")))
    on.exit(options(old), add = TRUE)
    tmp <- paste0(dest, ".tmp")
    utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
    file.rename(tmp, dest)
  }
  dest
}

# Past seasons: keep forever. Current (or later) season: re-download every 6h.
nflverse_max_age <- function(season) {
  if (as.integer(season) >= as.integer(format(Sys.Date(), "%Y")) - 0L && as.integer(format(Sys.Date(), "%m")) >= 8) 6 else Inf
}

load_nflverse_pbp <- function(season) {
  readRDS(nflverse_fetch("pbp", sprintf("play_by_play_%s.rds", season), nflverse_max_age(season)))
}
load_nflverse_snap_counts <- function(season) {
  readRDS(nflverse_fetch("snap_counts", sprintf("snap_counts_%s.rds", season), nflverse_max_age(season)))
}
load_nflverse_depth_charts <- function(season) {
  tryCatch(readRDS(nflverse_fetch("depth_charts", sprintf("depth_charts_%s.rds", season), nflverse_max_age(season))),
           error = function(e) NULL)
}
# The NFL's official weekly injury reports (report_status Out / Doubtful / Questionable, one row per listed player per week); used to tell
# whether a waiver claim was ruled out in a week it was on a roster. NULL when unavailable.
load_nflverse_injuries <- function(season) {
  tryCatch(readRDS(nflverse_fetch("injuries", sprintf("injuries_%s.rds", season), nflverse_max_age(season))), error = function(e) NULL)
}
# On-field participation is only published after a season ends (none for 2026
# yet); NULL when unavailable.
load_nflverse_participation <- function(season) {
  tryCatch(readRDS(nflverse_fetch("pbp_participation", sprintf("pbp_participation_%s.rds", season), nflverse_max_age(season))),
           error = function(e) NULL)
}

# Sleeper player_id <-> gsis_id (nflverse) <-> pfr_id (snap counts) <-> espn_id.
# Sleeper's own players DB carries gsis_id for only ~20% of current skill
# players (155 of 817 checked), so the community crosswalk maintained by
# dynastyprocess is used instead (covers every skill player with a real
# projection). Refreshed weekly.
load_id_crosswalk <- function(max_age_hours = 24 * 7) {
  dir.create(NFLVERSE_CACHE_DIR, showWarnings = FALSE, recursive = TRUE)
  dest <- file.path(NFLVERSE_CACHE_DIR, "ff_playerids.csv")
  stale <- !file.exists(dest) || as.numeric(difftime(Sys.time(), file.mtime(dest), units = "hours")) > max_age_hours
  if (stale) {
    tmp <- paste0(dest, ".tmp")
    utils::download.file("https://raw.githubusercontent.com/dynastyprocess/data/master/files/db_playerids.csv",
                         tmp, mode = "wb", quiet = TRUE)
    file.rename(tmp, dest)
  }
  x <- readr::read_csv(dest, show_col_types = FALSE, col_types = readr::cols(.default = readr::col_character()))
  x[!is.na(x$sleeper_id), c("sleeper_id", "gsis_id", "pfr_id", "espn_id")] |>
    dplyr::distinct(sleeper_id, .keep_all = TRUE)
}

# All seasons' schedules/results in one file: kickoff, spread, total, roof,
# temperature, wind.
load_nflverse_schedules <- function() {
  readRDS(nflverse_fetch("schedules", "games.rds", 6))
}
