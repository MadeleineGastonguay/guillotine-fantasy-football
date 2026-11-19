# --- Monte Carlo elimination simulation -----------------------------------
# For each team: locked_points (already scored, from Sleeper, per this
# league's own scoring rules) is fixed; remaining_mean / remaining_sd_<model>
# (from Sleeper's own projections, re-scored under this league's rules and
# blended with live pace as each player's game progresses -- see
# R/pipeline.R) is simulated using an empirical noise shape resampled from
# real historical residuals (see get_position_shape_pool() below), not an
# assumed normal distribution. For each draw we solve the exact best-ball
# optimal lineup, then across all the teams in that draw the lowest total is
# "eliminated". Elimination probability per team = fraction of draws where
# they were lowest.
#
# `sd_col` selects which of the variance models computed in R/pipeline.R to
# simulate with (see VARIANCE_MODEL_NAMES in R/config.R) -- the app runs
# this once per model so the two can be compared side by side.

# Real per-position weekly residuals (BACKTEST_TOPN_FILE, see R/config.R),
# standardized to mean 0 / sd 1 so they can be resampled as pure "shape" and
# rescaled by whichever per-player SD is active for a given variance model.
# Verified empirically that this matters even after summing a whole
# lineup: it shifts a real roster's simulated left tail down and its right
# tail up by several points relative to rnorm() noise, despite the central
# limit theorem washing out most of the raw player-level skew (~1.2-1.4 for
# RB/WR/TE) by the time you're looking at a team total.
get_position_shape_pool <- function(exclude_season = NULL) {
  resid_topn <- readRDS(BACKTEST_TOPN_FILE)$resid_topn
  # exclude_season: leave that season out of the pool. The retrospective replay
  # (R/replay.R) uses this so a season's own outcomes never shape the curves
  # it is being tested against (out-of-sample evaluation). The app never sets it.
  if (!is.null(exclude_season)) resid_topn <- resid_topn[resid_topn$season != exclude_season, , drop = FALSE]
  standardized <- resid_topn |>
    dplyr::group_by(position) |>
    dplyr::mutate(z = (residual - mean(residual)) / sd(residual)) |>
    dplyr::ungroup()
  split(standardized$z, standardized$position)
}

simulate_team_totals <- function(team_df, n_sims, sd_col = "remaining_sd_backtest", shape_pool) {
  n_players <- nrow(team_df)
  sd_vec <- pmax(team_df[[sd_col]], 0)

  # z_mat: n_players x n_sims, each row resampled (with replacement) from
  # that player's own position's real standardized residuals. vapply() over
  # positions is meant to return an n_sims x n_players matrix (one column
  # per player) to transpose into the n_players x n_sims layout the rest of
  # this function expects -- but exactly like the outer sim_matrix in
  # run_elimination_simulation() below, vapply() silently collapses to a
  # plain vector (dropping matrix-ness) whenever each call's result has
  # length 1, i.e. whenever n_sims == 1. Setting dim() explicitly first
  # guarantees the real shape regardless, before transposing.
  pool_for <- function(pos) {
    pool <- shape_pool[[pos]]
    if (is.null(pool)) pool <- unlist(shape_pool, use.names = FALSE) # unexpected position: fall back to the full pool rather than crash
    pool
  }
  z_raw <- vapply(team_df$position, function(pos) {
    sample(pool_for(pos), n_sims, replace = TRUE)
  }, numeric(n_sims))
  dim(z_raw) <- c(n_sims, n_players)
  z_mat <- t(z_raw)

  remaining <- team_df$remaining_mean + sd_vec * z_mat
  totals <- team_df$locked_points + remaining
  totals <- pmax(totals, PLAYER_FLOOR)

  vapply(seq_len(n_sims), function(j) {
    optimal_lineup_points(team_df$position, totals[, j])
  }, numeric(1))
}

# roster_data: list of data.frames (one per roster_id), each with columns
#   position, locked_points, remaining_mean, remaining_sd_backtest,
#   remaining_sd_underdog
# team_meta: data.frame with roster_id, team_name (for output labeling)
run_elimination_simulation <- function(roster_data, team_meta, n_sims = N_SIMULATIONS,
                                        sd_col = "remaining_sd_backtest") {
  roster_ids <- names(roster_data)
  shape_pool <- get_position_shape_pool() # same real-residual shape regardless of which variance model's SD scale is active

  # Both sapply() and vapply() silently collapse to a plain vector --
  # dropping matrix dimensions -- whenever each team's result has length 1,
  # which happens exactly when n_sims == 1 (a one-element FUN.VALUE template
  # doesn't save vapply from this either, confirmed directly: vapply(1:5,
  # function(i) i, numeric(1)) returns a plain vector, not a 1x5 matrix).
  # That breaks apply()/colMeans() below with an opaque "dim(X) must have a
  # positive length" error. Setting dim() explicitly afterward is the only
  # way to guarantee a real n_sims x n_teams matrix regardless of n_sims.
  sim_matrix <- vapply(roster_ids, function(rid) {
    simulate_team_totals(roster_data[[rid]], n_sims, sd_col = sd_col, shape_pool = shape_pool)
  }, numeric(n_sims))
  dim(sim_matrix) <- c(n_sims, length(roster_ids))
  # sim_matrix: n_sims x n_teams

  finalize_elimination_simulation(sim_matrix, roster_ids, team_meta)
}

# How many of the teams still alive are chopped this week. With more than one chop a week the configured number (N_CHOPS_PER_WEEK) applies until the
# field is down to that many plus one: one team always survives, so with 2 teams left and 2 chops configured the answer is 1 (the higher score wins,
# which is week 16 of a two-chop league: 2 chops a week for weeks 1-15, then the top of the last two wins), and with one team left it is 0.
# A one-chop league (N_CHOPS_PER_WEEK = 1) is left exactly as it was.
effective_chops <- function(n_alive, n_chops = N_CHOPS_PER_WEEK) {
  if (n_chops <= 1) return(1L)
  as.integer(max(0L, min(as.integer(n_chops), as.integer(n_alive) - 1L)))
}

# Share of draws in which each team is among the `k` lowest totals (k chops a week). Columns of `sim_matrix` are teams, rows are draws.
chopped_share <- function(sim_matrix, k) {
  n_sims <- nrow(sim_matrix)
  ranks <- matrix(unlist(lapply(seq_len(n_sims), function(i) rank(sim_matrix[i, ], ties.method = "first"))), nrow = n_sims, byrow = TRUE)
  colMeans(ranks <= k)
}

# Turns the n_sims x n_teams matrix of simulated team totals into the result
# table. Split out of run_elimination_simulation() so the app can build that
# matrix in small pieces -- yielding to the event loop between pieces so
# clicks and tab changes stay responsive -- and still finish the same way.
finalize_elimination_simulation <- function(sim_matrix, roster_ids, team_meta) {
  n_sims <- nrow(sim_matrix)
  k <- effective_chops(ncol(sim_matrix))
  if (N_CHOPS_PER_WEEK <= 1) {
    lowest_idx <- apply(sim_matrix, 1, which.min)
    elim_counts <- table(factor(lowest_idx, levels = seq_along(roster_ids)))
    elim_prob <- as.numeric(elim_counts) / n_sims
  } else {
    # more than one team is chopped each week: a team is "eliminated" in a draw when it is among the k lowest totals
    elim_prob <- if (k >= 1) chopped_share(sim_matrix, k) else rep(0, ncol(sim_matrix))
  }

  result <- tibble::tibble(
    roster_id = as.integer(roster_ids),
    projected_mean = colMeans(sim_matrix),
    projected_sd = apply(sim_matrix, 2, sd),
    elimination_prob = elim_prob
  ) |>
    dplyr::left_join(team_meta, by = "roster_id") |>
    dplyr::arrange(dplyr::desc(elimination_prob))

  # Keep the raw draws (n_sims x n_teams, columns named by roster_id) for the
  # distribution charts. Attached LAST: dplyr verbs above would drop it.
  colnames(sim_matrix) <- roster_ids
  attr(result, "draws") <- sim_matrix
  result
}

# Standings-only "Sleeper (est.)" column -- see SLEEPER_IMPLIED_TEAM_SD in
# R/config.R for what this is and (importantly) what it isn't: a confirmed
# account of Sleeper's real methodology, which is undocumented. Unlike
# run_elimination_simulation() above, this has no per-player detail and no
# bench-swap-per-draw optionality -- it simulates each team's ALREADY-FIXED
# naive lineup total (team_meta$naive_projected_total, the same number shown
# as "Projected Final Score") plus one flat, roster-composition-blind SD
# shared by every team, because that flat/fixed-lineup combination is
# what actually reproduced Sleeper's real Safe % numbers when checked
# against a live screenshot.
run_elimination_simulation_flat_sd <- function(team_meta, team_sd = SLEEPER_IMPLIED_TEAM_SD,
                                                n_sims = N_SIMULATIONS) {
  n_teams <- nrow(team_meta)
  sim_matrix <- vapply(team_meta$naive_projected_total, function(m) {
    rnorm(n_sims, mean = m, sd = team_sd)
  }, numeric(n_sims))
  dim(sim_matrix) <- c(n_sims, n_teams)

  lowest_idx <- apply(sim_matrix, 1, which.min)
  elim_counts <- table(factor(lowest_idx, levels = seq_len(n_teams)))
  if (N_CHOPS_PER_WEEK > 1) elim_counts <- (if (effective_chops(n_teams) >= 1) chopped_share(sim_matrix, effective_chops(n_teams)) else rep(0, n_teams)) * n_sims

  tibble::tibble(
    roster_id = team_meta$roster_id,
    team_name = team_meta$team_name,
    owner_name = team_meta$owner_name,
    avatar_url = team_meta$avatar_url,
    naive_projected_total = team_meta$naive_projected_total,
    projected_mean = colMeans(sim_matrix),
    projected_sd = apply(sim_matrix, 2, sd),
    elimination_prob = as.numeric(elim_counts) / n_sims
  ) |>
    dplyr::arrange(dplyr::desc(elimination_prob))
}

# Instant, closed-form elimination probability -- no simulation anywhere.
# Trades away the real model's two expensive/hard-to-avoid pieces:
#   1. Per-draw bench-swap re-optimization (each team's FIXED lineup, chosen
#      once from mean projections -- the same starters "Projected Final
#      Score" already uses -- is used for every team, not re-picked per draw).
#   2. The empirical noise shape (each team's total is approximated as
#      Normal(mean, sd) -- justified by the same CLT finding from the
#      empirical-vs-normal check: team-level skew after summing 9 players
#      was ~0.08-0.18, close enough to 0 for this approximation).
# Once a lineup is FIXED, its SD needs no simulation either: for independent
# players, Var(sum) = sum(Var), so team SD is just the quadrature sum
# (sqrt of sum of squares) of its starters' own SDs -- plain arithmetic.
# "Team i is the minimum of N independent teams" then has a real, if not
# elementary, closed form: a single 1-D integral over each team's own
# density times the product of every other team's survival probability at
# that point. stats::integrate() evaluates it directly -- exact up to
# ordinary numerical-integration tolerance, with no Monte Carlo noise and no
# lpSolve calls (the actual bottleneck of the real simulation) at all.

# Mean is deliberately NOT recomputed here -- team_meta$naive_projected_total
# (from R/pipeline.R) already is exactly this (same optimal_lineup_points()
# call on the same mean-projection totals), and duplicating it under a
# different name here just creates a naive_projected_total.x/.y collision
# once joined back to team_meta downstream. Only the SD is new.
#
# `mean_col` selects which per-player mean column picks the lineup (default
# "remaining_mean", the app's usual model) -- a comparison model (e.g.
# "bayes_remaining_mean", R/bayesian.R) may pick a slightly different optimal
# lineup if its mean estimate disagrees near the margin, which is correct:
# each model's SD should be the quadrature sum of ITS OWN chosen starters.
compute_instant_team_sds <- function(roster_table, sd_col, mean_col = "remaining_mean") {
  roster_table |>
    dplyr::group_by(roster_id) |>
    dplyr::group_modify(function(df, key) {
      total <- df$locked_points + df[[mean_col]]
      slot <- optimal_lineup_assignment(df$position, total)
      starters <- slot != "BN"
      tibble::tibble(sd = sqrt(sum(pmax(df[[sd_col]][starters], 0)^2)))
    }) |>
    dplyr::ungroup()
}

# Two kinds of certainty are used, both exact:
#
# complete: TRUE for a team whose every rostered player's game is over -- its
# final score is a KNOWN CONSTANT, not a normal distribution. (Treating it as
# N(mean, 0.1), the old approach, leaked small spurious probabilities: a
# finished team 0.3 points above another finished team got ~2% just because
# two 0.1-pt bells overlap slightly.)
#
# floors: the lowest score an UNFINISHED team can still end with. Best ball
# only ever improves: players who have finished are locked in, and a pending
# player can only add options to the lineup, never subtract. So a team's final
# score is >= the best lineup built from its finished players alone (pending
# players counted as 0). A team whose floor already exceeds the lowest
# finished score cannot possibly be eliminated, even though it isn't finished.
#
# The exact logic:
#   * Let L be the lowest score among complete teams. Any team -- complete or
#     not -- whose final score is guaranteed above L can NEVER be eliminated:
#     exactly 0. (A complete team above L; or an unfinished team with
#     floor > L.) Such a team also finishes above L with probability exactly 1,
#     so it drops out of every other team's calculation.
#   * The lowest complete team(s) are eliminated only if EVERY remaining
#     unfinished team finishes above L: prod_j P(X_j > L), split evenly if
#     several tie at L.
#   * An unfinished team i is eliminated only by finishing below L AND below
#     every other unfinished team: the usual min-of-normals integral, but cut
#     off at L (its upper limit) instead of running to mean + 10 sd.
# Floors are used ONLY for those exact zeros -- the normal curves of the other
# unfinished teams are deliberately NOT truncated at their floors: that would
# shift their implied average score above their projection and move published
# numbers by up to ~5 points, a modeling change beyond this logic fix.
# With no complete teams this reduces exactly to the original formula.
elimination_prob_instant <- function(means, sds, complete = rep(FALSE, length(means)),
                                     floors = rep(-Inf, length(means)), n_chops = N_CHOPS_PER_WEEK) {
  if (n_chops <= 1) return(elimination_prob_instant_one(means, sds, complete, floors))
  k <- effective_chops(length(means), n_chops)   # never more chops than teams alive minus one
  if (k < 1) return(rep(0, length(means)))
  if (k == 1) return(elimination_prob_instant_one(means, sds, complete, floors))
  elimination_prob_instant_k(means, sds, complete, floors, k)
}

# The single-chop version described above (the original logic, unchanged).
elimination_prob_instant_one <- function(means, sds, complete = rep(FALSE, length(means)),
                                         floors = rep(-Inf, length(means))) {
  n <- length(means)
  probs <- numeric(n)
  inc <- which(!complete)
  comp <- which(complete)

  # Incomplete teams still carry a tiny floor on their SD: a team whose 9
  # starters are all final but whose bench isn't has sd 0 here (the fixed
  # lineup only counts starters), which would make dnorm()/pnorm() degenerate
  # (crashed the table once: "non-finite function value"). 0.1 pt is
  # negligible next to real ~10-20 pt SDs.
  sds <- pmax(sds, 0.1)

  upper <- Inf
  active <- inc # unfinished teams that can still finish below L
  if (length(comp) > 0) {
    L <- min(means[comp])
    lowest <- comp[means[comp] <= L + 1e-9]
    active <- inc[!(floors[inc] > L + 1e-9)] # drop unfinished teams already guaranteed above L
    all_active_above <- prod(1 - stats::pnorm(L, means[active], sds[active])) # empty product = 1
    probs[lowest] <- all_active_above / length(lowest)
    upper <- L
  }

  for (i in active) {
    others <- setdiff(active, i)
    lo <- means[i] - 10 * sds[i]
    hi <- min(means[i] + 10 * sds[i], upper)
    if (hi <= lo) next # entirely above the cutoff: no chance
    integrand <- function(x) {
      survival <- Reduce(`*`, lapply(others, function(j) 1 - stats::pnorm(x, means[j], sds[j])), 1)
      stats::dnorm(x, means[i], sds[i]) * survival
    }
    probs[i] <- stats::integrate(integrand, lower = lo, upper = hi, subdivisions = 200L, stop.on.error = FALSE)$value
  }
  probs # complete teams above L and unfinished teams with floor > L stay at exactly 0
}

# Several teams chopped each week (n_chops = k): a team is eliminated when it is among the k lowest final scores. Same inputs and same logic as above,
# generalised. Writing "below x" for "finishes below the score x":
#   * An unfinished team i is eliminated when, at its own score x, at most k-1 other teams are below x:
#       P(i) = integral of dnorm(x; mean_i, sd_i) * P(at most k-1 other teams are below x) dx.
#     Each finished team is below x or not (a fixed number m(x) of them, a step in x), each other unfinished team j is below x with probability
#     pnorm(x; mean_j, sd_j), so the count of unfinished teams below x follows a Poisson-binomial distribution, whose CDF is built by a small DP.
#     Between two finished scores m(x) is constant, so the integral is split at the finished scores and uses the allowed count k-1-m.
#   * A finished team c (score s_c) is eliminated when at most k-1 teams end below s_c: the m_c finished teams below it plus the unfinished ones,
#     Poisson-binomial with p_j = pnorm(s_c; mean_j, sd_j).
#   * If k or more teams are finished, any team (finished or not) guaranteed above the k-th lowest finished score is out of every calculation
#     (probability exactly 0), exactly as in the one-chop version. Ties between two finished teams on the very same score are not split.
# With no finished teams the probabilities add up to k.
pb_cdf_at <- function(x, means, sds, t) {
  # P(at most t of the teams are below x) for a vector of x; teams j are below x with prob pnorm(x; means[j], sds[j]); t >= 0
  m <- length(x)
  if (t < 0) return(rep(0, m))
  probs <- replicate(t + 1, rep(0, m), simplify = FALSE)   # probs[[c + 1]] = P(exactly c below so far)
  probs[[1]] <- rep(1, m)
  for (j in seq_along(means)) {
    p <- stats::pnorm(x, means[j], sds[j])
    for (cc in rev(seq_len(t + 1))) {
      stay <- probs[[cc]] * (1 - p)
      move <- if (cc > 1) probs[[cc - 1]] * p else 0
      probs[[cc]] <- stay + move
    }
  }
  Reduce(`+`, probs)
}

elimination_prob_instant_k <- function(means, sds, complete, floors, k) {
  n <- length(means); probs <- numeric(n)
  if (n <= k) return(rep(1, n))
  sds <- pmax(sds, 0.1)
  inc <- which(!complete); comp <- which(complete)
  cs <- sort(means[comp]); nc <- length(comp)
  Lk <- if (nc >= k) cs[k] else Inf
  eps <- 1e-9
  active <- if (is.finite(Lk)) inc[!(floors[inc] > Lk + eps)] else inc   # unfinished teams not already guaranteed above the k-th lowest finished score
  # finished teams
  for (c in comp) {
    if (means[c] > Lk + eps) next
    below <- sum(means[comp] < means[c] - eps)
    probs[c] <- if (below > k - 1) 0 else pb_cdf_at(means[c], means[active], sds[active], k - 1 - below)
  }
  # unfinished teams: split the integral at the finished scores
  cuts <- c(-Inf, cs[cs <= Lk + eps | !is.finite(Lk)], Inf)   # piece m runs from cuts[m + 1] to cuts[m + 2] with m finished teams below
  for (i in active) {
    others <- setdiff(active, i)
    lo <- means[i] - 10 * sds[i]; hi <- means[i] + 10 * sds[i]
    total <- 0
    for (m in 0:min(nc, k - 1)) {
      a <- max(lo, cuts[m + 1]); b <- min(hi, cuts[m + 2])
      if (!is.finite(b) || b <= a) next
      integrand <- function(x) stats::dnorm(x, means[i], sds[i]) * pb_cdf_at(x, means[others], sds[others], k - 1 - m)
      total <- total + stats::integrate(integrand, lower = a, upper = b, subdivisions = 200L, stop.on.error = FALSE)$value
    }
    probs[i] <- total
  }
  probs
}

# roster_table: the full per-player table (snap$roster_table) -- needs every
# rostered player, not just the starters, since the fixed lineup itself is
# derived here from mean projections.
#
# `mean_col`/`projected_col` let a comparison model (e.g. the Bayesian one,
# R/bayesian.R) reuse this exact function instead of a parallel copy: pass
# mean_col = "bayes_remaining_mean" (per-player) and projected_col =
# "bayesian_projected_total" (the matching team-level column in team_meta,
# built the same way as naive_projected_total -- see R/pipeline.R). Floors/
# completeness are model-independent (a finished player's locked points don't
# depend on which mean/SD model is asking), so that part is unchanged either way.
run_elimination_instant <- function(roster_table, team_meta, sd_col = "remaining_sd_underdog",
                                    mean_col = "remaining_mean", projected_col = "naive_projected_total") {
  team_sds <- compute_instant_team_sds(roster_table, sd_col, mean_col)
  team_stats <- team_meta |>
    dplyr::select(roster_id, projected_total = dplyr::all_of(projected_col)) |>
    dplyr::left_join(team_sds, by = "roster_id") |>
    dplyr::rename(projected_sd = sd)
  # A team is "complete" only when EVERY rostered player's game is over (bench
  # included -- a pending bench player can still lift the team's best-ball
  # total), not just its starters.
  # Its floor: the best lineup buildable from players who have FINISHED alone
  # (pending players counted as 0) -- the lowest score the team can still end
  # with, since best ball only ever improves as more players score.
  team_certainty <- roster_table |>
    dplyr::group_by(roster_id) |>
    dplyr::summarise(
      complete = all(remaining_fraction <= 1e-9),
      floor_score = optimal_lineup_points(position, ifelse(remaining_fraction <= 1e-9, locked_points, 0)),
      .groups = "drop"
    )
  idx <- match(team_stats$roster_id, team_certainty$roster_id)
  team_stats$complete <- team_certainty$complete[idx]
  team_stats$floor_score <- team_certainty$floor_score[idx]
  team_stats$elimination_prob <- elimination_prob_instant(
    team_stats$projected_total, team_stats$projected_sd,
    complete = team_stats$complete, floors = team_stats$floor_score
  )
  team_stats |>
    dplyr::rename(!!projected_col := projected_total) |>
    dplyr::left_join(team_meta, by = c("roster_id", projected_col)) |>
    dplyr::arrange(dplyr::desc(elimination_prob))
}
