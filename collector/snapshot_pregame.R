#!/usr/bin/env Rscript
# Freezes this week's pre-game picture (Sleeper projections, depth chart,
# injuries, ESPN odds/weather) into an append-only timestamped file. Meant to
# run a few times a week just before kickoff windows (see collector/README.md);
# safe to run any time. Usage:
#   Rscript collector/snapshot_pregame.R           # skips when no game is still upcoming
#   Rscript collector/snapshot_pregame.R --force   # write a snapshot regardless
args <- commandArgs(trailingOnly = TRUE)
force <- "--force" %in% args

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- if (length(script_arg)) normalizePath(file.path(dirname(sub("^--file=", "", script_arg[1])), "..")) else getwd()
setwd(root)

suppressMessages({
  library(dplyr); library(purrr); library(tidyr); library(tibble); library(httr2); library(readr)
})
for (f in c("config", "sleeper_api", "sleeper_projections", "espn_api", "stat_scoring", "pipeline", "nflverse", "collector")) {
  source(file.path("R", paste0(f, ".R")))
}

log_line <- function(...) {
  dir.create(PREGAME_DIR, showWarnings = FALSE, recursive = TRUE)
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "|", ..., "\n",
      file = file.path(PREGAME_DIR, "snapshot.log"), append = TRUE)
}

status <- tryCatch({
  res <- build_pregame_snapshot()
  upcoming <- any(res$games$state == "pre")
  if (!upcoming && !force) {
    log_line(sprintf("skipped (season %s week %s): no game still upcoming", res$season, res$week))
    "skipped"
  } else {
    path <- write_pregame_snapshot(res)
    log_line(sprintf("wrote %s (%d players, %d games, %d after kickoff)", basename(path), nrow(res$snapshot),
                      dplyr::n_distinct(res$snapshot$game_id), sum(res$snapshot$taken_after_kickoff)))
    "written"
  }
}, error = function(e) {
  # Network down or an endpoint misbehaved: write nothing, log why, exit
  # non-zero; the next scheduled run simply tries again.
  log_line("FAILED:", conditionMessage(e))
  "failed"
})
quit(status = if (identical(status, "failed")) 1 else 0)
