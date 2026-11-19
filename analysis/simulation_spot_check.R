# --- Does the Simulations tab agree with the Approximate tab? -----------------------------------
# The team timelines (analysis/build_team_elim_timeline.R) use the Approximate tab's closed-form
# elimination probabilities, because they can be computed at every 5 minutes of a week. The Simulations
# tab instead draws many full weeks (each player resampled from real, skewed outcome shapes, and each
# team's best-ball lineup re-optimized in every draw), which is far too slow to run at every step. This
# script spot-checks the two against each other at a handful of moments per week: the first step
# (pre-kickoff), `n_points - 1` evenly spaced steps, and the kickoff of the week's last block of games.
#
# Results are written per week to accuracy_data/replay/{season}_w{week}_sim_check.csv.gz (cached: a week
# already on disk is not recomputed unless redo = TRUE), since the simulations take a few minutes a week.
#
# Usage (from the guillotine_app directory):
#   source("analysis/simulation_spot_check.R")
#   simulation_spot_check(2026, 1:3)                   # 7 moments a week, 5,000 simulated weeks each
#   read_sim_check(2026, 1:3)                           # the cached results, all weeks stacked

suppressMessages({ library(dplyr); library(readr) })
if (!file.exists("R/config.R") && file.exists("../R/config.R")) setwd("..")
for (f in list.files("R", full.names = TRUE)) suppressMessages(source(f))

sim_check_path <- function(season, week) file.path(REPLAY_DIR, sprintf("%s_w%02d_sim_check.csv.gz", season, week))

simulation_spot_check <- function(season, weeks, n_points = 7, n_sims = 5000, redo = FALSE, seed = 1) {
  for (wk in weeks) {
    path <- sim_check_path(season, wk)
    if (file.exists(path) && !redo) { message("week ", wk, ": cached (", path, ")"); next }
    tl <- read_csv(file.path(REPLAY_DIR, sprintf("%s_w%02d_league_teams_realtime.csv.gz", season, wk)), show_col_types = FALSE)
    xs <- sort(unique(tl$x_min))
    last_block_start <- match(min(tl$x_min[tl$block == max(tl$block)]), xs)
    idx <- sort(unique(c(round(seq(1, length(xs), length.out = n_points - 1)), last_block_start)))
    pr <- read_csv(file.path(REPLAY_DIR, sprintf("%s_w%02d_players.csv.gz", season, wk)), show_col_types = FALSE)
    r <- replay_league_week_realtime(season, wk, pr, write = FALSE, keep_rt_steps = idx)
    set.seed(seed + wk)
    rows <- lapply(idx, function(si) {
      k <- r$rt[[as.character(si)]]
      t0 <- Sys.time()
      roster_data <- split(k$rt[, c("position", "locked_points", "remaining_mean", "remaining_sd_underdog")], k$rt$roster_id)
      sim <- run_elimination_simulation(roster_data, k$meta, n_sims = n_sims, sd_col = "remaining_sd_underdog")
      message(sprintf("  week %d step %d/%d: %d simulated weeks in %.0fs", wk, si, length(xs), n_sims, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
      approx <- run_elimination_instant(k$rt, k$meta, sd_col = "remaining_sd_underdog")
      tibble::tibble(season = season, week = wk, step = si, x_min = r$steps$x_min[si], wall_utc = format(as.POSIXct(r$steps$wall[si], origin = "1970-01-01", tz = "UTC")),
                     roster_id = approx$roster_id, approx_prob = approx$elimination_prob, n_sims = n_sims) |>
        left_join(select(sim, roster_id, sim_prob = elimination_prob), by = "roster_id")
    })
    out <- bind_rows(rows) |>
      left_join(distinct(r$teams, roster_id, team_name, final_team_points, chopped), by = "roster_id")
    write_csv(out, path)
    message("week ", wk, ": wrote ", path)
  }
  invisible(read_sim_check(season, weeks))
}

read_sim_check <- function(season, weeks) {
  bind_rows(lapply(weeks, function(w) read_csv(sim_check_path(season, w), show_col_types = FALSE)))
}

# Run when called as a script: Rscript analysis/simulation_spot_check.R 2026
if (!interactive() && sys.nframe() == 0) {
  a <- commandArgs(trailingOnly = TRUE)
  simulation_spot_check(as.integer(if (length(a)) a[1] else 2026), 1:3)
}
