# --- Build the real-kickoff-time team elimination timelines (weeks of a season) ----------------
# Runs R/replay.R::replay_league_week_realtime() for each week from the existing 1-minute player
# replay files (accuracy_data/replay/{season}_w{week}_players.csv.gz, from replay_week()), and
# writes {season}_w{week}_league_teams_realtime.csv.gz / ..._league_games_realtime.csv.gz next to
# them. ~15s per week. Prints a sanity check per week: the modelled final team scores against
# Sleeper's own, that each timestep's elimination probabilities sum to 1, and that the team with
# the lowest final score matches the one Sleeper actually chopped that week.
#
# Usage (from the guillotine_app directory): source("analysis/build_team_elim_timeline.R")
#   build_team_elim_timeline(2026, 1:3)

suppressMessages({ library(dplyr); library(readr) })
if (!file.exists("R/config.R") && file.exists("../R/config.R")) setwd("..")
for (f in list.files("R", full.names = TRUE)) suppressMessages(source(f))

build_team_elim_timeline <- function(season, weeks, ...) {
  chopped <- get_eliminated_teams(LEAGUE_ID, current_week = max(weeks) + 1)
  for (wk in weeks) {
    pr <- read_csv(file.path(REPLAY_DIR, sprintf("%s_w%02d_players.csv.gz", season, wk)), show_col_types = FALSE)
    r <- replay_league_week_realtime(season, wk, pr, ...)
    t <- r$teams
    last <- t |> filter(x_min == max(x_min))
    sums <- tapply(t$elimination_prob, t$x_min, sum)
    actual <- chopped$team_name[chopped$chopped_week == wk]
    message(sprintf(
      "week %d sanity: %d teams; max |modelled - Sleeper final| = %.2f pts; per-step probability sums in [%.4f, %.4f]; lowest final = '%s', Sleeper chopped '%s'",
      wk, n_distinct(t$roster_id), max(abs(last$current_score - last$final_team_points)), min(sums), max(sums),
      t$team_name[t$chopped][1], paste(actual, collapse = "/")))
  }
}
if (identical(environment(), globalenv()) && !interactive() && length(commandArgs(trailingOnly = TRUE)) == 0) NULL
