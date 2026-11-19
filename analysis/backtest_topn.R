# --- Restrict the backtest to top-N players by position, per season -------
# Matches the methodology published sources (Underdog Network, PlayerProfiler)
# use for their week-to-week variance figures: rank players by season-average
# actual scoring, keep only the clear starters, and compute variance within
# that set. This should read closer to "conditional on being a stable,
# clearly-rostered starter" than the full backtest (which also includes
# committee backs, streamers, and other higher-variance marginal players).
#
# Run standalone (needs residuals_all.rds from backtest_projection_error.R):
# Rscript analysis/backtest_topn.R

if (file.exists("R/config.R")) {
  # already at repo root
} else if (file.exists("../R/config.R")) {
  setwd("..")
} else {
  stop("Run from the guillotine_app directory (or analysis/ within it).")
}

library(dplyr)

resid_df <- readRDS("cache/backtest/residuals_all.rds")

TOP_N <- c(QB = 12, RB = 24, WR = 36, TE = 12)

season_avg <- resid_df |>
  group_by(season, position, player_id) |>
  summarise(season_avg_actual = mean(actual_pts), n_weeks = n(), .groups = "drop")

top_players <- season_avg |>
  group_by(season, position) |>
  arrange(desc(season_avg_actual), .by_group = TRUE) |>
  mutate(rank = row_number()) |>
  filter(rank <= TOP_N[position]) |>
  ungroup() |>
  select(season, position, player_id)

resid_topn <- resid_df |>
  inner_join(top_players, by = c("season", "position", "player_id"))

cat("Full backtest player-weeks:", nrow(resid_df), "\n")
cat("Top-N-restricted player-weeks:", nrow(resid_topn), "\n\n")

summary_topn <- resid_topn |>
  group_by(position) |>
  summarise(
    n = n(),
    n_players = n_distinct(paste(season, player_id)),
    mean_projected = mean(projected_pts),
    mean_actual = mean(actual_pts),
    mean_residual = mean(residual),
    sd_residual = sd(residual),
    implied_cv_at_mean = sd_residual / mean_projected,
    .groups = "drop"
  )
cat("=== top-N summary by position ===\n")
print(as.data.frame(summary_topn))

fits_topn <- resid_topn |>
  group_by(position) |>
  group_map(function(df, key) {
    df$bucket <- round(df$projected_pts)
    agg <- df |> group_by(bucket) |> summarise(sd_r = sd(residual), n = n(), .groups = "drop") |>
      filter(n >= 8)
    fit <- lm(sd_r ~ bucket, data = agg, weights = agg$n)
    tibble::tibble(position = key$position, intercept = coef(fit)[1], slope = coef(fit)[2],
                    r_squared = summary(fit)$r.squared, n_buckets = nrow(agg))
  }) |>
  bind_rows()
cat("\n=== top-N fitted sd(residual) = a + b * projected_pts ===\n")
print(as.data.frame(fits_topn))

saveRDS(list(summary_topn = summary_topn, fits_topn = fits_topn, resid_topn = resid_topn),
        "cache/backtest/calibration_summary_topn.rds")
cat("\nSaved calibration_summary_topn.rds\n")
