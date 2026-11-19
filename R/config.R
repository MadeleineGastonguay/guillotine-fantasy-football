# --- League / app configuration -------------------------------------------

# Per-league values come from league_config.R in the app folder (loaded here, so whoever sources this file gets them). Anything not set there falls back to the defaults below,
# so the scripts under collector/ and analysis/, which run from the app folder, get the same values.
if (file.exists("league_config.R")) source("league_config.R", local = TRUE)
if (!exists("LEAGUE_ID")) LEAGUE_ID <- "YOUR_LEAGUE_ID"
if (identical(LEAGUE_ID, "YOUR_LEAGUE_ID")) message("LEAGUE_ID is not set: copy league_config.example.R to league_config.R and put your Sleeper league ID in it.")
if (!exists("APP_TITLE")) APP_TITLE <- "Guillotine"
if (!exists("APP_ICON")) APP_ICON <- "\U0001F52A"
if (!exists("SHOW_RECAP_LINK")) SHOW_RECAP_LINK <- FALSE
if (!exists("RECAP_URL")) RECAP_URL <- "https://YOUR_USER.github.io/YOUR_RECAP_REPO/"
# Teams chopped each week. 1 = classic guillotine (the lowest score goes); 2 = the two lowest go, and so on. Set in league_config.R.
if (!exists("N_CHOPS_PER_WEEK")) N_CHOPS_PER_WEEK <- 1L
if (!exists("N_TEAMS_FALLBACK")) N_TEAMS_FALLBACK <- 17L   # plot-height guess before the first snapshot arrives; init_lineup() sets it from the league

# Timezone for timestamps shown in the UI (e.g. "Updated 14:50:26 ET").
# "America/New_York" rather than a fixed "EDT" offset so it correctly
# tracks the EDT/EST switch across daylight saving time instead of being
# right for half the year -- the hosting server's own local time zone
# (e.g. UTC on shinyapps.io) isn't necessarily what a US-based league wants
# to see, so this is set explicitly rather than left to Sys.time()'s default.
APP_TZ <- "America/New_York"

# How often the live dashboard checks for updates (seconds). This drives
# both the instant (closed-form, ~0.04s) and simulated (Monte Carlo) tables,
# but the two aren't equally expensive: with only Underdog active (see
# VARIANCE_MODEL_NAMES below), one full Monte Carlo pass is ~1-2s, not the
# ~6s it was with 4 models running -- 60s is now comfortable headroom rather
# than tight. This is just the check interval, not a forced recompute: the
# app skips the simulation step entirely on any tick where nothing about the
# players' inputs actually changed since last time (see app.R), so ticking
# more often than the data actually updates isn't wasted compute -- it just
# means the instant table (which recomputes from whatever's cached, with no
# simulation cost at all) reflects new data sooner.
REFRESH_SECONDS <- 60

# Monte Carlo draws used to estimate elimination probability each refresh,
# per model (all four models run sequentially -- see USE_PARALLEL_SIMULATIONS
# above -- so total wall-clock time is ~4x one model's time). Precision vs.
# speed tradeoff, benchmarked locally: N=500 -> ~15s for all 4 models,
# MC std error ~1.3% at p=0.1; N=200 -> ~6s, std error ~2.1%. 200 is a
# reasonable default for this app (nobody needs sub-1% precision on an
# elimination probability), but bump it back up if you want tighter numbers
# and don't mind the wait.
# N_SIMULATIONS <- 200
if (!exists("N_SIMULATIONS")) N_SIMULATIONS <- 500L

# Roster slots this league starts (from Sleeper league settings). Best ball:
# Sleeper auto-selects the highest-scoring valid lineup from the full roster
# each week, so all rostered players (not just the nominally-listed
# "starters") are candidates for these slots.
ROSTER_SLOTS <- c("QB", "RB", "RB", "WR", "WR", "TE", "FLEX", "FLEX", "SUPER_FLEX")

# Which starting slots each roster position is eligible for.
SLOT_ELIGIBILITY <- list(
  QB = c("QB", "SUPER_FLEX"),
  RB = c("RB", "FLEX", "SUPER_FLEX"),
  WR = c("WR", "FLEX", "SUPER_FLEX"),
  TE = c("TE", "FLEX", "SUPER_FLEX")
)

# The two constants above are this league's lineup and the fallback. At app start init_lineup() (below) replaces them with the lineup of the
# league being tracked, read from Sleeper's `roster_positions`, so another league with a different lineup needs no code change. Every lineup
# function (R/bestball.R) takes them as default arguments, evaluated at call time, so nothing else has to be passed around.
# Which positions each Sleeper flex slot accepts:
SLOT_FLEX_POSITIONS <- list(FLEX = c("RB", "WR", "TE"), SUPER_FLEX = c("QB", "RB", "WR", "TE"), REC_FLEX = c("WR", "TE"), WRRB_FLEX = c("WR", "RB"))

# Pure: Sleeper's `roster_positions` (e.g. QB RB RB WR WR TE FLEX FLEX SUPER_FLEX BN BN ...) -> list(slots, eligibility) in the same shape as
# ROSTER_SLOTS / SLOT_ELIGIBILITY. Bench, IR and taxi spots are not lineup slots ("BN" is also the label R/bestball.R gives to players left out of the
# lineup, so it must never be in `slots`). A slot the app cannot score (K, DEF, IDP, ...) stops with a clear message.
derive_lineup <- function(roster_positions) {
  pos <- as.character(unlist(roster_positions))
  slots <- pos[!pos %in% c("BN", "IR", "TAXI")]
  if (!length(slots)) stop("the league has no starting lineup slots in roster_positions")
  supported <- c("QB", "RB", "WR", "TE", names(SLOT_FLEX_POSITIONS))
  bad <- setdiff(unique(slots), supported)
  if (length(bad)) stop(sprintf("this league starts slot(s) the app does not support: %s (supported: %s)", paste(bad, collapse = ", "), paste(supported, collapse = ", ")))
  eligibility <- lapply(stats::setNames(c("QB", "RB", "WR", "TE"), c("QB", "RB", "WR", "TE")), function(p) {
    flex <- names(SLOT_FLEX_POSITIONS)[vapply(SLOT_FLEX_POSITIONS, function(e) p %in% e, logical(1))]
    intersect(c(p, flex), unique(slots))   # only slots this league actually has
  })
  list(slots = slots, eligibility = eligibility)
}

# Called once at app start, after R/sleeper_api.R is sourced: reads the league and sets ROSTER_SLOTS, SLOT_ELIGIBILITY and N_TEAMS_FALLBACK (the league's
# starting size, only a plot-height guess before the first snapshot) in every environment that holds them (see assign_in_scope). If Sleeper cannot be
# reached the built-in lineup above stays in force.
# Shiny also auto-sources every file in R/ into its own environment before app.R runs (and setting shiny.autoload.r = FALSE in global.R does not stop that in
# current Shiny), so the settings can exist in two places: that environment and the global one, and app code sees the first before the second. A plain
# assign() into the global environment alone would be shadowed, so the new values are written to EVERY environment, from the caller up to the global one,
# that already holds the name.
assign_in_scope <- function(name, value, from = parent.frame(2)) {
  e <- from; done <- FALSE
  while (!identical(e, emptyenv())) {
    if (exists(name, envir = e, inherits = FALSE)) { assign(name, value, envir = e); done <- TRUE }
    if (identical(e, globalenv())) break
    e <- parent.env(e)
  }
  if (!done) assign(name, value, envir = globalenv())
  invisible(done)
}

init_lineup <- function(league_id = LEAGUE_ID) {
  lg <- tryCatch(get_league(league_id), error = function(e) NULL)
  if (is.null(lg)) { message("init_lineup: could not read the league from Sleeper; keeping the built-in lineup"); return(invisible(FALSE)) }
  if (!is.null(lg$total_rosters)) assign_in_scope("N_TEAMS_FALLBACK", as.integer(lg$total_rosters))
  if (length(lg$roster_positions)) {
    ld <- derive_lineup(lg$roster_positions)
    assign_in_scope("ROSTER_SLOTS", ld$slots); assign_in_scope("SLOT_ELIGIBILITY", ld$eligibility)
    message(sprintf("init_lineup: %s starts %s", league_id, paste(ld$slots, collapse = " ")))
  }
  invisible(TRUE)
}

# Full-game weekly projection-error model, by position: SD(actual - pregame
# projection) = intercept + slope * pregame_projection. Fitted in
# analysis/backtest_topn.R against 3 real NFL seasons (2023-2025) of actual
# vs. projected fantasy points, re-scored under this league's own rules,
# restricted to each season's top-N players by position (top-12 QB, top-24
# RB, top-36 WR, top-12 TE -- i.e. clear starters, matching the methodology
# published fantasy-analytics sources use). Cross-checked against two
# independent published estimates (Underdog Network, PlayerProfiler) and
# landed close to both -- see README for the full writeup, including why
# this is NOT scaled to match Sleeper's own in-app win-probability numbers
# (those imply meaningfully tighter variance than every source checked here,
# historical or published).
#
# An intercept (rather than pure proportional SD = mean * CV) matters: real
# variance doesn't shrink to 0 as the projection shrinks to 0 -- a player
# projected for 2 points can still easily land anywhere from 0 to 12.
PROJECTION_ERROR_MODEL <- list(
  QB = c(a = 4.12, b = 0.194),
  RB = c(a = 5.38, b = 0.176),
  WR = c(a = 5.24, b = 0.210),
  TE = c(a = 5.85, b = 0.064)
)
DEFAULT_ERROR_MODEL <- c(a = 5, b = 0.16) # fallback; shouldn't be hit (only QB/RB/WR/TE rostered)

# Alternative, simpler variance model: published Underdog Network weekly
# coefficients of variation. This is SD / mean of a player's own actual
# weekly scoring across a season -- a related but distinct notion of
# uncertainty from the backtest above (which measures SD of actual minus
# that specific week's projection). Flat proportional CV, no intercept, so
# unlike the backtest model it forces SD -> 0 as the projection -> 0.
# Source: https://underdognetwork.com/football/best-ball-research/weekly-variance-by-position-a-key-to-best-ball
UNDERDOG_CV <- c(QB = 0.36, RB = 0.54, WR = 0.58, TE = 0.63)
DEFAULT_UNDERDOG_CV <- 0.55

# How noisy is a player's in-game pace (locked_points / elapsed_fraction) as an estimate of
# their TRUE full-game rate, at a given point in the game -- see R/bayesian.R for the model
# this drives. Treating points as a compound Poisson process (events arriving at some rate,
# each worth a random number of points) gives Var(points by time t | true rate R) = R*t*k,
# where k = E[event points^2] / E[event points]; fitted per position by
# analysis/fit_compound_poisson_k.R from real replay data (2026 weeks 1-3; no separate
# calibration season needed since this is an in-game-noise constant, not a shape fit against
# a specific week's outcomes). Replaces an earlier hand-picked constant (BAYESIAN_PHI = 0.5,
# an unfitted 50/50 split) that didn't have this closed form behind it and, being quadratic
# rather than linear in rate, systematically overreacted to noisy early-game pace (see the
# coverage/bias comparison in analysis/plot_accuracy_summary.R::plot_model_accuracy_comparison()
# and the discussion that led to this fit). Re-run that script if more replay weeks accumulate
# and the fit should be refreshed.
COMPOUND_POISSON_K <- c(QB = 1.25, RB = 1.01, TE = 1.24, WR = 1.61)
DEFAULT_COMPOUND_POISSON_K <- 1.3 # ~= mean of the above, for any position missing from the table

# A third variance model: PlayerProfiler's published weekly CVs. Same
# structure as the Underdog model (flat proportional CV, no intercept), but
# these numbers are less precise -- PlayerProfiler states them as the visual
# "peak" of a density chart per position ("QBs peak at about 0.4", "RBs peak
# at about 0.5-0.6", etc.), not a single computed average like Underdog's.
# Values below are the midpoint of each stated range.
# Source: https://www.playerprofiler.com/article/the-player-variance-manifesto/
PLAYERPROFILER_CV <- c(QB = 0.40, RB = 0.55, WR = 0.65, TE = 0.75)
DEFAULT_PLAYERPROFILER_CV <- 0.60

# A fourth variance model: player-specific, shrunk toward the backtest
# position-level model. Precomputed by analysis/backtest_player_variance.R
# into cache/player_variance_lookup.rds -- a per-player multiplier applied
# on top of the backtest model's SD, not recomputed live each refresh.
# Players missing from the lookup (rookies, or anyone with too little
# history) implicitly get a multiplier of 1.0, i.e. fall back cleanly to
# the plain position-level backtest model. See that script for the
# shrinkage methodology and why QBs get much heavier shrinkage than
# RB/WR/TE (their own year-to-year variance barely predicts itself).
PLAYER_VARIANCE_LOOKUP_FILE <- file.path("cache", "player_variance_lookup.rds")

# Real per-position weekly residuals (2023-2025, starter-restricted -- see
# analysis/backtest_topn.R), reused by the Monte Carlo simulation as an
# empirical noise SHAPE instead of assuming a symmetric normal distribution.
# Verified directly against this backtest: RB/WR/TE weekly residuals are
# meaningfully right-skewed (skew ~1.2-1.4) and fat-tailed on both sides
# (excess kurtosis ~2.5-3.2) -- a bust is more bounded than a normal
# distribution assumes, but a real bust, when it happens, can still be more
# severe than normal's tail predicts, and booms are both more frequent and
# larger. QB is close enough to normal (skew ~0.24) that this barely matters
# there, but there's no real cost to treating all four positions the same
# way. See R/simulation.R's get_position_shape_pool() for how this file is
# used -- residuals are standardized (mean 0, sd 1) and resampled per
# position, then rescaled by whichever variance model's own per-player SD is
# active, so only the *shape* of the noise comes from real games; the scale
# still comes from the selected model (Underdog/PlayerProfiler/backtest).
BACKTEST_TOPN_FILE <- file.path("cache", "backtest", "calibration_summary_topn.rds")

# Names of the variance models the app runs side by side (see
# full_game_sd_*() in R/pipeline.R for how each is computed). Every model
# actually simulated (Monte Carlo pass in app.R) and every SD/Elim% column
# shown in Standings/Team Detail/Live Games is driven off this list, so
# removing an entry here is enough to skip it everywhere -- no other code
# changes needed. "player" (the per-player backtested model) is commented
# out for now to cut simulation time by ~25% (3 models instead of 4); the
# underlying remaining_sd_player column is still computed in R/pipeline.R
# (cheap, not the bottleneck), just not simulated or displayed. Uncomment
# to bring it back.
#
# Display names deliberately avoid stats jargon ("backtest", "CV") since
# these show up in front of league members who know fantasy football, not
# statistics -- see MODEL_DESCRIPTIONS below for the plain-language
# one-liners shown next to each in the Standings tab.
# Order here is also the display order for every SD/Elim% column pair in
# Standings/Team Detail/Live Games (and the simulation run order) -- the two
# outside, published sources first, our own home-grown one last.
VARIANCE_MODEL_NAMES <- c(
  underdog = "Underdog"
  # playerprofiler = "PlayerProfiler",
  # backtest = "Historical Data",
  # player = "Player-specific"
)

# A 4th, Standings-only column: NOT a per-player variance model (it has no
# roster-composition detail at all, so it can't plug into Team Detail/Live
# Games' per-player ± SD columns the way the three models above do). This is
# our own reverse-engineering of Sleeper's in-app "Safe %" (Chop tab):
# fitting Safe%_i = Phi((Proj_i - Proj_worst) / SD) by probit regression
# against 17 real teams' actual Safe%/Proj values from a screenshot recovered
# a single constant SD (~30.7 per team) that reproduces every team's Safe%
# to within about 1 percentage point (R^2 = 0.986) -- so Sleeper appears to
# assume the SAME flat variance for every team regardless of roster
# construction, unlike our three models which vary team to team.
# IMPORTANT: this is inferred purely by curve-fitting Sleeper's own displayed
# outputs. Sleeper does not document how Safe % is calculated anywhere we
# could find (their own support article and blog give zero methodology
# detail), so SLEEPER_IMPLIED_TEAM_SD is our best empirical match to their
# behavior, not a confirmed figure -- we do not actually know where Sleeper's
# own number comes from, and it could change without notice.
SLEEPER_IMPLIED_TEAM_SD <- 30.7
# STANDINGS_EXTRA_MODELS <- c(sleeper_reverse = "Sleeper (est.)")
# ALL_STANDINGS_MODEL_NAMES <- c(VARIANCE_MODEL_NAMES, STANDINGS_EXTRA_MODELS)
ALL_STANDINGS_MODEL_NAMES <- VARIANCE_MODEL_NAMES # == VARIANCE_MODEL_NAMES while STANDINGS_EXTRA_MODELS above stays commented out

# One-sentence, jargon-free description of each model above (shown in the
# Standings tab), plus a source link for the two built from someone else's
# published research. Written for a fantasy football audience, not a
# statistics one -- no "variance", "coefficient of variation", "SD", or
# regression-speak. Keyed by the same ids as VARIANCE_MODEL_NAMES.
MODEL_DESCRIPTIONS <- list(
  backtest = list(
    text = paste(
      "Our own estimate -- not a published source. Calculated empirically",
      "by comparing 3 real NFL seasons of players' actual scores to their pregame",
      "projections, position by position, under this league's own scoring rules, to see",
      "how boom-or-bust each position has really been."
    ),
    url = NULL
  ),
  underdog = list(
    text = "Underdog Network's published research on how boom-or-bust each position tends to run week to week.",
    url = "https://underdognetwork.com/football/best-ball-research/weekly-variance-by-position-a-key-to-best-ball"
  ),
  playerprofiler = list(
    text = "PlayerProfiler's published research on the same question, as an independent second opinion.",
    url = "https://www.playerprofiler.com/article/the-player-variance-manifesto/"
  ),
  player = list(
    text = "The Historical Data model, adjusted for each individual player based on how consistent (or streaky) they personally have been.",
    url = NULL
  ),
  sleeper_reverse = list(
    text = paste(
      "Our own reverse-engineering of Sleeper's in-app 'Safe %' (Chop tab) --",
      "NOT something Sleeper documents or confirms. We fit the single flat",
      "variance number that best reproduces Sleeper's own displayed Safe %",
      "across every team, then used that same number here. We don't actually",
      "know how Sleeper really calculates it; this is our best guess from the",
      "outside, matched to their outputs, not their real methodology."
    ),
    url = NULL
  )
)

# Whether to run the four variance models' Monte Carlo passes in parallel
# (parallel::mclapply, forked processes) vs. sequentially. Forking multiplies
# whatever's resident in the R process at fork time -- R's garbage collector
# touching pages during a fork tends to defeat true copy-on-write sharing in
# practice, so 4 forked children can mean close to 4x real memory use, not a
# free lunch. That's fine on a beefy local machine but caused OOM crashes on
# shinyapps.io (commonly capped around 1GB per app instance). Default FALSE
# (sequential, ~15s for 4 models at N_SIMULATIONS=500) for safety on
# constrained hosts; set TRUE for local dev if you want the ~4s parallel
# version and have RAM to spare.
USE_PARALLEL_SIMULATIONS <- FALSE

# Floor applied to any single player's simulated weekly total, just to keep
# a rare extreme draw (e.g. multiple turnovers) from blowing up the tail.
PLAYER_FLOOR <- -10

CACHE_DIR <- "cache"
PLAYERS_CACHE_FILE <- file.path(CACHE_DIR, "sleeper_players.rds")
PLAYERS_CACHE_MAX_AGE_HOURS <- 24
