# --- Fit the compound-Poisson observation-noise constant for R/bayesian.R -----------------
# R/bayesian.R needs Var(observed_rate(t) | true full-game rate R) -- how noisy is
# locked_points/elapsed_fraction as an estimator of a player's true rate, at a given point in
# the game. The original version assumed a hand-picked (1-phi)*CV^2*R^2/t (Normal, variance
# proportional to R^2). Treating points as a compound Poisson process instead -- events (plays
# crediting the player with some stat) arriving at rate lambda, each worth a random number of
# points X -- gives a closed form instead of a guess:
#   Var(points scored by time t | true full-game rate R) = R * t * k,   k := E[X^2] / E[X]
# (standard compound-Poisson variance identity Var(S(t)) = lambda*t*E[X^2], combined with
# R = lambda*E[X] to eliminate lambda). Notably this is LINEAR in R, not quadratic like the
# CV^2*R^2 form -- a real, testable difference in functional form, not just a different
# constant.
#
# k is a single per-position number (order of a typical event's points-squared-per-point), and
# can be estimated straight from data already on hand (no new downloads): for every real
# player-game-minute in the replay files, the actual final score F is used as a stand-in for
# that player's true realized rate (conditioning on what actually happened, not the pregame
# guess), so e(t) := current_points - elapsed_fraction*F is (approximately) a mean-zero
# compound-Poisson residual with Var(e(t)) = F*t*k. This also absorbs real within-game
# rate-inhomogeneity (game script, garbage time) into k, same as every other "measure the real
# residuals rather than assume a clean process" choice already made elsewhere in this project
# (see R/simulation.R::get_position_shape_pool()) -- deliberate, not an oversight.
#
# Usage (from the guillotine_app directory): source("analysis/fit_compound_poisson_k.R")
# Prints the fit; update COMPOUND_POISSON_K in R/config.R by hand if it changes materially.

suppressMessages({ library(dplyr); library(readr) })

setwd_here <- function() {
  if (file.exists("R/config.R")) return(invisible())
  if (file.exists("../R/config.R")) { setwd(".."); return(invisible()) }
  stop("Run from the guillotine_app directory (or analysis/ within it).")
}
setwd_here()

fit_compound_poisson_k <- function(season = 2026, weeks = NULL, replay_dir = file.path("accuracy_data", "replay")) {
  if (is.null(weeks)) {
    f <- list.files(replay_dir, pattern = sprintf("^%s_w(\\d+)_players\\.csv\\.gz$", season))
    weeks <- sort(as.integer(sub(sprintf("^%s_w(\\d+)_players\\.csv\\.gz$", season), "\\1", f)))
  }
  files <- file.path(replay_dir, sprintf("%s_w%02d_players.csv.gz", season, as.integer(weeks)))
  raw <- bind_rows(lapply(files[file.exists(files)], read_csv, show_col_types = FALSE))

  d <- raw |>
    filter(position %in% c("QB", "RB", "WR", "TE"), !is_final_row,
          role_final %in% c("starter", "backup_entered"),
          elapsed_fraction > 0.02, !is.na(final_actual_points)) |>
    mutate(t = elapsed_fraction, resid = current_points - t * final_actual_points, xw = t * final_actual_points)

  d |>
    group_by(position) |>
    summarise(k = sum(resid^2) / sum(xw), n_rows = n(), .groups = "drop")
}

if (identical(environment(), globalenv())) print(fit_compound_poisson_k())
