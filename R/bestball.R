# --- Best-ball lineup optimizer ------------------------------------------
# Exact solution (binary assignment ILP via lpSolve) to "which valid lineup
# maximizes total points", matching what Sleeper's best-ball engine does
# after the fact. Small problem (~15 players x 9 slots) -- benchmarks at
# well under 1ms per call, so it's fine to call once per simulated week per
# team.

library(lpSolve)

solve_lineup_lp <- function(positions, points, slots = ROSTER_SLOTS,
                             eligibility = SLOT_ELIGIBILITY) {
  n_players <- length(positions)
  n_slots <- length(slots)

  pairs <- expand.grid(p = seq_len(n_players), s = seq_len(n_slots))
  pairs$eligible <- mapply(function(pi, si) {
    slots[si] %in% eligibility[[positions[pi]]]
  }, pairs$p, pairs$s)
  pairs <- pairs[pairs$eligible, , drop = FALSE]

  if (nrow(pairs) == 0) return(NULL)

  obj <- points[pairs$p]

  slot_con <- matrix(0, n_slots, nrow(pairs))
  for (s in seq_len(n_slots)) slot_con[s, pairs$s == s] <- 1

  play_con <- matrix(0, n_players, nrow(pairs))
  for (p in seq_len(n_players)) play_con[p, pairs$p == p] <- 1

  res <- lp(
    "max", obj,
    rbind(slot_con, play_con),
    c(rep("=", n_slots), rep("<=", n_players)),
    c(rep(1, n_slots), rep(1, n_players)),
    all.bin = TRUE
  )

  list(res = res, pairs = pairs)
}

optimal_lineup_points <- function(positions, points, slots = ROSTER_SLOTS,
                                   eligibility = SLOT_ELIGIBILITY) {
  solved <- solve_lineup_lp(positions, points, slots, eligibility)
  if (is.null(solved) || solved$res$status != 0) return(if (is.null(solved)) 0 else NA_real_)
  solved$res$objval
}

# Returns a character vector (length(positions)) giving each player's
# assigned starting slot ("QB", "FLEX", "SUPER_FLEX", ...), or "BN" for
# anyone not in the optimal lineup. Used for the Team Detail display, not
# the Monte Carlo loop (optimal_lineup_points() above stays on the fast
# path for that).
optimal_lineup_assignment <- function(positions, points, slots = ROSTER_SLOTS,
                                       eligibility = SLOT_ELIGIBILITY) {
  n_players <- length(positions)
  assignment <- rep("BN", n_players)

  solved <- solve_lineup_lp(positions, points, slots, eligibility)
  if (is.null(solved) || solved$res$status != 0) return(assignment)

  chosen <- solved$pairs[round(solved$res$solution) == 1, , drop = FALSE]
  assignment[chosen$p] <- slots[chosen$s]
  assignment
}
