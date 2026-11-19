#!/usr/bin/env Rscript
# Replay one or more past weeks through the app's projection model.
#   Rscript collector/run_replay.R 2025 1            # season 2025 week 1, every game minute
#   Rscript collector/run_replay.R 2025 1:18 5       # weeks 1-18, every 5 game minutes
#   Rscript collector/run_replay.R 2025 1 1 --in-sample   # let the season's own residuals into the shape pool
#   Rscript collector/run_replay.R 2026 1 5 --league      # also the league's team-level replay (2026 weeks only)
# Output: accuracy_data/replay/{season}_w{WW}_players.csv.gz
args <- commandArgs(trailingOnly = TRUE)
in_sample <- "--in-sample" %in% args
with_league <- "--league" %in% args   # also write the league's team-level replay (this league's own weeks only)
args <- setdiff(args, c("--in-sample", "--league"))
season <- as.integer(args[1])
weeks <- eval(parse(text = args[2]))
step <- if (length(args) >= 3) as.numeric(args[3]) else 1

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) setwd(normalizePath(file.path(dirname(sub("^--file=", "", script_arg[1])), "..")))

suppressMessages({ library(dplyr); library(purrr); library(tidyr); library(tibble); library(httr2); library(readr); library(scales) })
for (f in c("config", "sleeper_api", "sleeper_projections", "espn_api", "stat_scoring", "bestball", "simulation",
            "pipeline", "bayesian", "nflverse", "collector", "replay")) source(file.path("R", paste0(f, ".R")))

for (w in weeks) {
  tryCatch({
    pr <- replay_week(season, w, step_minutes = step, exclude_season_from_shape = !in_sample)
    if (with_league) replay_league_week(season, w, pr)
  }, error = function(e) message(sprintf("replay %s week %s FAILED: %s", season, w, conditionMessage(e))))
}
