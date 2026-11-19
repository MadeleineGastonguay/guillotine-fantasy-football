# --- Score a raw stat line under this league's scoring_settings -----------
# Works for any stat vector that uses Sleeper's native field names (e.g. the
# projections feed in R/sleeper_projections.R) -- just sums stat * weight
# over whatever fields both the stat line and scoring_settings define, so it
# automatically covers every scored category (receptions, yardage, TDs,
# fumbles lost, 2-point conversions, etc.) with no manual id mapping.
score_stats <- function(stat_vec, scoring_settings) {
  if (is.null(stat_vec) || length(stat_vec) == 0) return(0)
  fields <- intersect(names(stat_vec), names(scoring_settings))
  if (length(fields) == 0) return(0)

  weights <- as.numeric(unlist(scoring_settings[fields]))
  values <- as.numeric(stat_vec[fields])
  sum(values * weights, na.rm = TRUE)
}
