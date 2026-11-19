# --- Backtest: how wrong are pregame projections, really? -----------------
# Pulls real historical actual-vs-projected stat lines from Sleeper's public
# API (confirmed to be genuine past-season data, not synthetic), re-scores
# both under this league's own scoring_settings, and fits an empirical
# residual (actual - projected) distribution by position. This replaces the
# guessed POSITION_CV constants in R/config.R with numbers grounded in real
# projection error.
#
# Run standalone: Rscript analysis/backtest_projection_error.R
# Caches each season/week pull to cache/backtest/ so re-running is cheap.

setwd_here <- function() {
  # allow running from repo root or from analysis/
  if (file.exists("R/config.R")) return(invisible())
  if (file.exists("../R/config.R")) { setwd(".."); return(invisible()) }
  stop("Run from the guillotine_app directory (or analysis/ within it).")
}
setwd_here()

source("R/config.R")
source("R/sleeper_api.R")
source("R/stat_scoring.R")

library(httr2)
library(dplyr)
library(purrr)

BACKTEST_CACHE_DIR <- "cache/backtest"
dir.create(BACKTEST_CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

fetch_cached <- function(kind, season, week) {
  # kind: "stats" or "projections"
  f <- file.path(BACKTEST_CACHE_DIR, sprintf("%s_%s_wk%02d.rds", kind, season, week))
  if (file.exists(f)) return(readRDS(f))

  url <- sprintf("https://api.sleeper.app/%s/nfl/%s/%s?season_type=regular", kind, season, week)
  resp <- request(url) |> req_user_agent("guillotine-tracker-backtest/0.1") |> req_perform()
  raw <- resp_body_json(resp, simplifyVector = FALSE)

  out <- list()
  for (e in raw) {
    pid <- e$player_id
    if (is.null(pid) || is.null(e$stats) || length(e$stats) == 0) next
    pos <- e$player$position %||% NA_character_
    out[[pid]] <- list(position = pos, stats = setNames(as.numeric(unlist(e$stats)), names(e$stats)))
  }
  saveRDS(out, f)
  out
}

`%||%` <- function(a, b) if (is.null(a)) b else a

build_residuals <- function(seasons, weeks, scoring_settings, min_projected = 2) {
  purrr::map_dfr(seasons, function(season) {
    purrr::map_dfr(weeks, function(week) {
      message(sprintf("season %s week %s...", season, week))
      actual <- tryCatch(fetch_cached("stats", season, week), error = function(e) NULL)
      proj <- tryCatch(fetch_cached("projections", season, week), error = function(e) NULL)
      if (is.null(actual) || is.null(proj) || length(actual) < 500 || length(proj) < 500) {
        message("  skipped (missing or too-small payload)")
        return(NULL)
      }

      common_ids <- intersect(names(actual), names(proj))
      purrr::map_dfr(common_ids, function(pid) {
        pos <- proj[[pid]]$position
        if (is.na(pos) || !(pos %in% c("QB", "RB", "WR", "TE"))) return(NULL)

        projected_pts <- score_stats(proj[[pid]]$stats, scoring_settings)
        if (projected_pts < min_projected) return(NULL)
        actual_pts <- score_stats(actual[[pid]]$stats, scoring_settings)

        tibble::tibble(
          season = season, week = week, player_id = pid, position = pos,
          projected_pts = projected_pts, actual_pts = actual_pts,
          residual = actual_pts - projected_pts
        )
      })
    })
  })
}

# --- run ---------------------------------------------------------------
league <- get_league(LEAGUE_ID)
scoring_settings <- league$scoring_settings

seasons <- c(2023, 2024, 2025)
weeks <- 1:18

resid_df <- build_residuals(seasons, weeks, scoring_settings)
saveRDS(resid_df, file.path(BACKTEST_CACHE_DIR, "residuals_all.rds"))
cat("\nTotal player-weeks:", nrow(resid_df), "\n")

# --- summarize by position -----------------------------------------------
summary_by_pos <- resid_df |>
  group_by(position) |>
  summarise(
    n = n(),
    mean_projected = mean(projected_pts),
    mean_residual = mean(residual),                 # bias: + means projections run low
    sd_residual = sd(residual),
    implied_cv_at_mean = sd_residual / mean_projected,
    .groups = "drop"
  )
print(summary_by_pos)

# Does residual SD scale with the projection itself, or is there a floor
# (intercept) that a pure proportional (SD = mean * CV) model misses?
# Bin by projected_pts decile within position and look at SD per bin.
binned <- resid_df |>
  group_by(position) |>
  mutate(bin = ntile(projected_pts, 5)) |>
  group_by(position, bin) |>
  summarise(
    n = n(),
    mean_projected = mean(projected_pts),
    sd_residual = sd(residual),
    .groups = "drop"
  ) |>
  arrange(position, bin)
cat("\n--- residual SD by projected-points quintile, per position ---\n")
print(binned, n = 40)

# Fit SD as a linear function of projected points per position:
# sd_residual ~ a + b * projected_pts (fit on |residual| as a proxy, since
# residual itself is ~mean-zero within position after removing bias).
cat("\n--- fitted sd(residual) = a + b * projected_pts, by position ---\n")
fits <- resid_df |>
  group_by(position) |>
  group_map(function(df, key) {
    # bin into ~15 pt-projection buckets and fit on bucket SDs to avoid
    # heteroskedastic-OLS-on-raw-abs-residual issues
    df$bucket <- round(df$projected_pts)
    agg <- df |> group_by(bucket) |> summarise(sd_r = sd(residual), n = n(), .groups = "drop") |>
      filter(n >= 10)
    fit <- lm(sd_r ~ bucket, data = agg, weights = agg$n)
    tibble::tibble(position = key$position, intercept = coef(fit)[1], slope = coef(fit)[2],
                    r_squared = summary(fit)$r.squared)
  }) |>
  bind_rows()
print(fits)

saveRDS(list(summary_by_pos = summary_by_pos, binned = binned, fits = fits),
        file.path(BACKTEST_CACHE_DIR, "calibration_summary.rds"))
cat("\nSaved calibration_summary.rds\n")
