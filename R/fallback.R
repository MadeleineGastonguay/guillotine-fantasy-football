# --- Fallback projections for players Sleeper has no projection for ---------
# Sleeper's projection feed (a RotoWire feed) sometimes has no real projection
# for a player who is nonetheless about to play -- typically a backup promoted
# to starter after an injury (e.g. a QB1 listed Out: the replacement can have
# only an average-draft-position placeholder, and the whole team no passing
# projection at all). Scored as-is that is a pre-game projection of exactly 0
# AND a spread of 0 (the spread is a multiple of the projection), i.e. a
# starter treated as a certain zero.
#
# For such a player we substitute an ESTIMATE and flag it (projection_source =
# "estimate", shown as "(est.)" in the tables):
#   * the player's own average league-scored points over the games he actually
#     played in the last few weeks of this season, shrunk toward
#   * a typical projection for a starter at his position (median projection
#     among this league's rostered starters at that position).
# With n recent games the estimate is (n * recent_avg + K * typical) / (n + K),
# so one big game doesn't dominate; with no recent games it's the typical value.
# It only applies to players who look like expected starters (Sleeper depth
# chart), have a game this week (a bye-week player stays at 0) and carry no
# injury designation: an Out / IR player correctly stays at 0,
# and a Questionable one with no projection is more likely to sit than start.
#
# The spread then comes from the normal CV x projection formula; an estimate
# deserves more doubt than a published projection, which the "(est.)" flag
# signals but the spread does not (yet) reflect.

FALLBACK_SHRINK_K <- 2
FALLBACK_RECENT_WEEKS <- 3
# depth_chart_order at or below this counts as an expected starter
FALLBACK_STARTER_DEPTH <- c(QB = 1, RB = 1, WR = 2, TE = 1)
# stat fields that only exist in a real projection (a placeholder entry has none)
REAL_PROJECTION_FIELDS <- c("pass_att", "pass_yd", "rush_att", "rush_yd", "rec", "rec_yd")

has_real_projection <- function(stats) {
  !is.null(stats) && any(REAL_PROJECTION_FIELDS %in% names(stats))
}

# Sleeper's season stat lines for one finished week (player_id -> named stat
# vector), cached to disk since a finished week never changes.
fetch_week_stats <- function(season, week) {
  f <- file.path("cache", sprintf("week_stats_%s_wk%02d.rds", season, week))
  if (file.exists(f)) return(readRDS(f))
  raw <- sleeper_get(sprintf("/stats/nfl/regular/%s/%s", season, week))
  out <- lapply(raw, function(s) setNames(as.numeric(unlist(s)), names(s)))
  dir.create("cache", showWarnings = FALSE, recursive = TRUE)
  saveRDS(out, f)
  out
}

# Per player: n (games played) and mean league points over the last
# `weeks` COMPLETED weeks before `week`.
recent_game_points <- function(player_ids, season, week, scoring_settings) {
  prior <- rev(seq_len(max(week - 1, 0)))[seq_len(min(FALLBACK_RECENT_WEEKS, max(week - 1, 0)))]
  pts <- lapply(prior, function(w) {
    st <- tryCatch(fetch_week_stats(season, w), error = function(e) NULL)
    if (is.null(st)) return(NULL)
    vapply(player_ids, function(pid) {
      s <- st[[pid]]
      if (is.null(s) || is.na(s["gp"]) || s["gp"] < 1) NA_real_ else score_stats(s, scoring_settings)
    }, numeric(1))
  })
  m <- do.call(cbind, pts)
  if (is.null(m)) return(data.frame(n = rep(0L, length(player_ids)), avg = NA_real_))
  data.frame(n = rowSums(!is.na(m)), avg = ifelse(rowSums(!is.na(m)) > 0, rowMeans(m, na.rm = TRUE), NA_real_))
}

# roster_table needs: player_id, position, depth_chart_order, injury_status,
# full_week_projection, has_sleeper_projection. Returns it with
# full_week_projection filled in for qualifying players, plus projection_source
# ("sleeper" / "estimate").
apply_fallback_projections <- function(roster_table, season, week, scoring_settings) {
  roster_table$projection_source <- "sleeper"

  # A player whose team has no game this week (bye) has no projection BECAUSE
  # he isn't playing -- exactly the same "no real projection" signature as a
  # promoted backup, except the right answer is 0, not an estimate. Never
  # estimate these, and pin their pre-game projection to 0 so nothing
  # downstream (team pre-game totals, trend colors) can pick up a stray value.
  on_bye <- roster_table$matchup_label %in% "BYE"
  roster_table$full_week_projection[on_bye] <- 0

  thr <- unname(FALLBACK_STARTER_DEPTH[roster_table$position])
  expected_starter <- !on_bye & !is.na(roster_table$depth_chart_order) & !is.na(thr) &
    roster_table$depth_chart_order <= thr
  # Only players with NO injury designation: a Questionable/Doubtful/Out player
  # with no projection is more likely to sit than to be an unprojected starter.
  healthy <- is.na(roster_table$injury_status) | !nzchar(roster_table$injury_status)
  need <- which(!roster_table$has_sleeper_projection & expected_starter & healthy)
  if (length(need) == 0) return(roster_table)

  # typical starter projection per position from players who DO have one
  ref <- !is.na(thr) & expected_starter & roster_table$has_sleeper_projection & roster_table$full_week_projection > 0
  typical <- vapply(c("QB", "RB", "WR", "TE"), function(p) {
    v <- roster_table$full_week_projection[ref & roster_table$position == p]
    if (length(v) >= 3) stats::median(v) else {
      v2 <- roster_table$full_week_projection[roster_table$has_sleeper_projection & roster_table$position == p & roster_table$full_week_projection > 0]
      if (length(v2) > 0) stats::median(v2) else 8
    }
  }, numeric(1))

  rec <- recent_game_points(roster_table$player_id[need], season, week, scoring_settings)
  base <- unname(typical[roster_table$position[need]])
  est <- ifelse(rec$n > 0, (rec$n * rec$avg + FALLBACK_SHRINK_K * base) / (rec$n + FALLBACK_SHRINK_K), base)
  roster_table$full_week_projection[need] <- pmax(est, 0)
  roster_table$projection_source[need] <- "estimate"
  roster_table
}
