# --- Player-specific variance, shrunk toward the position-level model -----
# Reuses the residuals_all.rds cache from backtest_projection_error.R (no
# new API calls) to give each player their own variance "ratio" relative to
# what the position-level model (PROJECTION_ERROR_MODEL) would predict for
# a player projected at their own historical average.
#
# Why shrinkage, and why the strength differs by position: a single
# player's SD from ~15-45 games of history is noisy. Before trusting it, we
# checked whether a player's own variance is even a stable trait year to
# year: correlate each player's season-SD against their SD the following
# season (>=6 games each season, 2023-2025). Result: RB 0.31, WR 0.36,
# TE 0.42, QB 0.04 (a shuffled/null baseline centers on 0 +/- 0.05, so QB's
# 0.04 is indistinguishable from no signal at all -- a QB's variance one
# year barely predicts their variance the next). So RB/WR/TE get moderate
# shrinkage and QB gets heavy shrinkage (effectively always falling back
# close to the position-level model, which is the honest conclusion given
# the data, not a special-cased rule).
#
# Run standalone (needs residuals_all.rds from backtest_projection_error.R):
# Rscript analysis/backtest_player_variance.R

if (file.exists("R/config.R")) {
  # already at repo root
} else if (file.exists("../R/config.R")) {
  setwd("..")
} else {
  stop("Run from the guillotine_app directory (or analysis/ within it).")
}

library(dplyr)
source("R/config.R")

resid_df <- readRDS("cache/backtest/residuals_all.rds")

# --- derive shrinkage strength (k) per position from year-over-year signal
season_sd <- resid_df |>
  group_by(player_id, position, season) |>
  summarise(n_games = n(), sd_resid = sd(residual), .groups = "drop") |>
  filter(n_games >= 6)

yoy <- season_sd |>
  select(player_id, position, season, sd_resid) |>
  inner_join(
    season_sd |> select(player_id, position, season, sd_resid),
    by = c("player_id", "position"), suffix = c("_a", "_b"),
    relationship = "many-to-many"
  ) |>
  filter(season_b == season_a + 1)

cor_yoy <- yoy |>
  group_by(position) |>
  summarise(cor_yoy = cor(sd_resid_a, sd_resid_b), .groups = "drop")

# k = "games of history needed before the player's own data counts as much
# as the position prior" -- inversely proportional to the observed
# persistence signal, anchored so ~1 season (15 games) of a highly-
# persistent trait (cor ~1) would already earn substantial weight.
cor_yoy$k <- pmax(15 / pmax(cor_yoy$cor_yoy, 0.05), 15) # floor cor at 0.05 to avoid absurd k for near-zero signal
cat("=== shrinkage strength (k) by position ===\n")
print(as.data.frame(cor_yoy))
K_BY_POSITION <- setNames(cor_yoy$k, cor_yoy$position)

# --- per-player pooled stats across all cached seasons ---------------------
player_stats <- resid_df |>
  group_by(player_id, position) |>
  summarise(n_games = n(), sd_i = sd(residual), mean_proj_i = mean(projected_pts), .groups = "drop") |>
  filter(n_games >= 2, !is.na(sd_i)) # need >=2 games for a defined sd()

baseline_sd <- function(position, mean_proj) {
  a <- vapply(position, function(p) (PROJECTION_ERROR_MODEL[[p]] %||% DEFAULT_ERROR_MODEL)[["a"]], numeric(1))
  b <- vapply(position, function(p) (PROJECTION_ERROR_MODEL[[p]] %||% DEFAULT_ERROR_MODEL)[["b"]], numeric(1))
  pmax(a + b * mean_proj, 1) # floor at 1 to avoid divide-by-near-zero below
}
`%||%` <- function(a, b) if (is.null(a)) b else a

player_stats <- player_stats |>
  mutate(
    baseline = baseline_sd(position, mean_proj_i),
    raw_ratio = sd_i / baseline,
    k = K_BY_POSITION[position],
    w = n_games / (n_games + k),
    shrunk_ratio = w * raw_ratio + (1 - w) * 1.0,
    shrunk_ratio = pmin(pmax(shrunk_ratio, 0.4), 2.5) # safety clamp against fluky small samples
  )

cat("\n=== player_stats summary ===\n")
cat("n players:", nrow(player_stats), "\n")
print(as.data.frame(player_stats |> group_by(position) |>
  summarise(n = n(), mean_ratio = mean(shrunk_ratio), .groups = "drop")))

player_variance_lookup <- setNames(player_stats$shrunk_ratio, player_stats$player_id)
saveRDS(player_variance_lookup, "cache/player_variance_lookup.rds")
cat("\nSaved cache/player_variance_lookup.rds (", length(player_variance_lookup), "players )\n")
