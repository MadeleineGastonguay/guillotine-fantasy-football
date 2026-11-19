# --- First-look calibration diagnostics on replayed data ---------------------
# Reads accuracy_data/replay/*_players.csv.gz (written by collector/run_replay.R)
# and reports how often a player's FINAL points landed inside the model's
# predicted 50% and 90% intervals, split by how far into the game the
# prediction was made, by position, and by starter vs backup.
#
# For a well-calibrated distribution: inside-50% ~ 50%, inside-90% ~ 90%, and
# the two tails (below the 5th / above the 95th percentile) ~ 5% each.
#   Rscript analysis/replay_calibration.R [min_pre_proj]   (default 2, like the backtest)
#
# Notes:
#  * Files replayed with the default shape pool exclude their own season
#    (shape_pool == "excluding_<season>"), so coverage is out-of-sample for the
#    residual SHAPE. The Underdog CV constants are published numbers, not fitted.
#  * A snapshot belongs to a player-game; players in the same game are not
#    independent, so treat percentages as descriptive, not with tight error bars.

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) setwd(normalizePath(file.path(dirname(sub("^--file=", "", script_arg[1])), "..")))
suppressMessages({ library(dplyr); library(readr); library(tidyr) })

args <- commandArgs(trailingOnly = TRUE)
min_pre_proj <- if (length(args) >= 1) as.numeric(args[1]) else 2

files <- list.files("accuracy_data/replay", "_players\\.csv\\.gz$", full.names = TRUE)
stopifnot(length(files) > 0)
d <- bind_rows(lapply(files, read_csv, show_col_types = FALSE,
                      col_types = cols(player_id = col_character(), gsis_id = col_character(),
                                       roof = col_character(), .default = col_guess())))
cat(sprintf("%d files, %s rows, %d player-games, seasons: %s\n", length(files), format(nrow(d), big.mark = ","),
            n_distinct(paste(d$season, d$week, d$player_id)), paste(sort(unique(d$season)), collapse = ", ")))

# --- data health ---------------------------------------------------------------
fin <- d |> filter(is_final_row)
cat(sprintf("Final-row parity, PBP-derived points vs Sleeper's final points: %.1f%% within 0.05 pt, %.1f%% within 1 pt\n",
            100 * mean(abs(fin$current_points - fin$final_actual_points) <= 0.05, na.rm = TRUE),
            100 * mean(abs(fin$current_points - fin$final_actual_points) <= 1, na.rm = TRUE)))
cat("Role mix (player-games):\n"); print(table(fin$role_final))

cover <- function(df, lo50, hi50, lo90, hi90) {
  df |> summarise(
    n = n(),
    inside50 = mean(final_actual_points >= {{ lo50 }} & final_actual_points <= {{ hi50 }}, na.rm = TRUE),
    inside90 = mean(final_actual_points >= {{ lo90 }} & final_actual_points <= {{ hi90 }}, na.rm = TRUE),
    below5 = mean(final_actual_points < {{ lo90 }}, na.rm = TRUE),
    above95 = mean(final_actual_points > {{ hi90 }}, na.rm = TRUE),
    .groups = "drop"
  )
}
pct <- function(x) sprintf("%.1f%%", 100 * x)
show <- function(x) { x$inside50 <- pct(x$inside50); x$inside90 <- pct(x$inside90); x$below5 <- pct(x$below5); x$above95 <- pct(x$above95); print(as.data.frame(x), row.names = FALSE) }

base <- d |> filter(!is_final_row, pre_proj >= min_pre_proj)

cat("\n== PRE-GAME distribution (one row per player-game, at kickoff) ==\n")
pre <- base |> filter(grid_seconds == 0)
cat("-- by position\n"); show(pre |> group_by(position) |> cover(pre_q25, pre_q75, pre_q05, pre_q95))
cat("-- by role (final)\n"); show(pre |> group_by(role_final) |> cover(pre_q25, pre_q75, pre_q05, pre_q95))
cat("-- all\n"); show(pre |> cover(pre_q25, pre_q75, pre_q05, pre_q95))

cat("\n== CURRENT distribution, by how far into the game the prediction was made ==\n")
cur <- base |> mutate(elapsed_bucket = cut(elapsed_fraction, c(-0.01, 0.1, 0.25, 0.5, 0.75, 0.95),
                                           labels = c("0-10%", "10-25%", "25-50%", "50-75%", "75-95%")))
show(cur |> group_by(elapsed_bucket) |> cover(cur_q25, cur_q75, cur_q05, cur_q95))
cat("-- 25-75% into the game, by position\n")
show(cur |> filter(elapsed_fraction > 0.25, elapsed_fraction <= 0.75) |> group_by(position) |> cover(cur_q25, cur_q75, cur_q05, cur_q95))
cat("-- 25-75% into the game, starters vs backups who had entered vs backups not yet entered\n")
show(cur |> filter(elapsed_fraction > 0.25, elapsed_fraction <= 0.75) |>
       mutate(status_now = case_when(role_final == "starter" ~ "starter",
                                     entered_yet ~ "backup, already in",
                                     TRUE ~ "backup, not yet in")) |>
       group_by(status_now) |> cover(cur_q25, cur_q75, cur_q05, cur_q95))
