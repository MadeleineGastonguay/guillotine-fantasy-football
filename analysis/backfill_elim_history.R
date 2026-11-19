# Rebuild the "Odds over time" history from logs/player_projection_log.csv.
# Run from the app directory:  Rscript analysis/backfill_elim_history.R [--overwrite]
suppressMessages({library(dplyr); library(purrr); library(tidyr); library(tibble)})
for (f in c("config", "bestball", "simulation", "history")) source(file.path("R", paste0(f, ".R")))
args <- commandArgs(trailingOnly = TRUE)
backfill_elim_history_from_log(overwrite = "--overwrite" %in% args)
