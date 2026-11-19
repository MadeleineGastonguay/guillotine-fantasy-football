# --- Summary diagnostic: projection accuracy over the game, by position, all players ------
# Pools every player-game in the replay data (R/replay.R; not just this league's rosters) and,
# for each position and minute of raw game clock, plots:
#   (top)    coverage: the share of players whose ACTUAL score fell inside the model's
#            predicted 50%/90% interval, for both the empirical shape and the normal
#            approximation -- dashed lines mark the nominal 50%/90% targets. Below its target
#            line = intervals too narrow (overconfident) at that point in the game; above =
#            too wide.
#   (bottom) point accuracy: mean signed error (bias) and mean absolute error of the point
#            projection itself. Bias only needs one line -- the empirical and normal models
#            share the same mean, only their spread differs.
# These are two different questions ("is the number close" vs. "is the stated uncertainty
# trustworthy"), so they're two panels, not one axis.
#
# Each WEEK is a replicate: its own value at every bin is plotted as a faint point, and the
# solid trend line is the plain average of the three weeks' values (not a re-pooled average
# over all players, which would let the biggest week dominate) -- so a line that holds up
# across weeks looks like a line with points hugging it, and a fluke week shows up as a
# stray point.
#
# Two separate calls -- one for STARTERS, one for BACKUPS who ended up playing -- rather than
# one plot with both, since they're different questions (is the model right about the players
# it was always going to trust vs. the ones nobody expected to matter) and mixing them would
# average over that difference. True inactives (role_final == "inactive_or_dnp": on the roster
# but never played) are excluded from both -- they have no real prediction to check.
#
# Raw minutes, not % of game elapsed: games that go to overtime run past 60 -- those extra
# minutes land in their own bins past 60 rather than being folded back into the 0-100% range.
# Bins default to native 1-minute resolution (the replay data's own grid).
#
# Usage (from the guillotine_app directory):
#   source("analysis/plot_accuracy_summary.R")
#   plot_accuracy_summary(2026, role = "starter")
#   plot_accuracy_summary(2026, role = "backup", weeks = 1:2, save_path = "backups.png")

suppressMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
})

setwd_here <- function() {
  if (file.exists("R/config.R")) return(invisible())
  if (file.exists("../R/config.R")) { setwd(".."); return(invisible()) }
  stop("Run from the guillotine_app directory (or analysis/ within it).")
}
setwd_here()
source(file.path("analysis", "my_theme.R"))

REPLAY_DIR <- file.path("accuracy_data", "replay")

load_replay_weeks <- function(season, weeks, replay_dir = REPLAY_DIR) {
  files <- file.path(replay_dir, sprintf("%s_w%02d_players.csv.gz", season, as.integer(weeks)))
  found <- file.exists(files)
  if (!any(found)) stop("No replay files found for season ", season, " weeks ", paste(weeks, collapse = ","))
  if (!all(found)) message("No replay file for week(s) ", paste(weeks[!found], collapse = ", "), " -- skipping.")
  bind_rows(lapply(files[found], read_csv, show_col_types = FALSE))
}

# All weeks with a replay file for a season, in order (so `plot_accuracy_summary(2026)` with
# no `weeks` argument just uses whatever's been generated -- see R/replay.R::replay_week()).
available_weeks <- function(season, replay_dir = REPLAY_DIR) {
  f <- list.files(replay_dir, pattern = sprintf("^%s_w(\\d+)_players\\.csv\\.gz$", season))
  sort(as.integer(sub(sprintf("^%s_w(\\d+)_players\\.csv\\.gz$", season), "\\1", f)))
}

plot_accuracy_summary <- function(season, weeks = available_weeks(season), role = c("starter", "backup"),
                                  min_n = 10, bin_width = 1, save_path = NULL,
                                  width = 11, height = 7, dpi = 150) {
  role <- match.arg(role)
  role_value <- if (role == "starter") "starter" else "backup_entered"

  raw <- load_replay_weeks(season, weeks) |>
    filter(position %in% c("QB", "RB", "WR", "TE"), !is.na(final_actual_points), !is_final_row,
          role_final == role_value)
  if (nrow(raw) == 0) stop("No rows with role_final == '", role_value, "' in the requested weeks.")

  d <- raw |>
    mutate(
      minutes = grid_seconds / 60,
      bin_mid = (floor(minutes / bin_width) + 0.5) * bin_width,
      # Normal-approximation quantiles from the SAME running mean/sd the empirical ones use
      # (projected_final, projected_sd), so the two are a fair side-by-side comparison.
      norm_q25 = qnorm(.25, projected_final, pmax(projected_sd, 0.01)),
      norm_q75 = qnorm(.75, projected_final, pmax(projected_sd, 0.01)),
      norm_q05 = qnorm(.05, projected_final, pmax(projected_sd, 0.01)),
      norm_q95 = qnorm(.95, projected_final, pmax(projected_sd, 0.01)),
      in50_emp = final_actual_points >= cur_q25 & final_actual_points <= cur_q75,
      in90_emp = final_actual_points >= cur_q05 & final_actual_points <= cur_q95,
      in50_norm = final_actual_points >= norm_q25 & final_actual_points <= norm_q75,
      in90_norm = final_actual_points >= norm_q05 & final_actual_points <= norm_q95,
      error = final_actual_points - projected_final
    )

  # Per-WEEK values (the points) ------------------------------------------------------------
  cov_wk <- d |>
    group_by(week, position, bin_mid) |>
    summarise(n = n(), `50% - Empirical` = mean(in50_emp), `90% - Empirical` = mean(in90_emp),
             `50% - Normal` = mean(in50_norm), `90% - Normal` = mean(in90_norm), .groups = "drop") |>
    filter(n >= min_n) |>
    pivot_longer(c(`50% - Empirical`, `90% - Empirical`, `50% - Normal`, `90% - Normal`),
                 names_to = "which", values_to = "coverage") |>
    separate(which, into = c("interval", "series"), sep = " - ")

  acc_wk <- d |>
    group_by(week, position, bin_mid) |>
    summarise(n = n(), Bias = mean(error), `Mean absolute error` = mean(abs(error)), .groups = "drop") |>
    filter(n >= min_n) |>
    pivot_longer(c(Bias, `Mean absolute error`), names_to = "metric", values_to = "value")

  # Trend = the plain average of the per-week values (each week weighted equally, not by how
  # many players it happened to contribute) -----------------------------------------------
  cov_avg <- cov_wk |> group_by(position, bin_mid, interval, series) |>
    summarise(coverage = mean(coverage), n_weeks = n(), .groups = "drop")
  acc_avg <- acc_wk |> group_by(position, bin_mid, metric) |>
    summarise(value = mean(value), n_weeks = n(), .groups = "drop")

  n_weeks <- n_distinct(raw$week)
  n_players <- n_distinct(paste(raw$week, raw$player_id))
  x_max <- max(cov_wk$bin_mid, acc_wk$bin_mid)
  role_label <- if (role == "starter") "Starters" else "Backups who played"
  cap <- sprintf("%s -- %s, week%s %s (%d player-games); faint points = each week's own value, line = average of the weeks; %d-minute bins, bins under %d players dropped.",
                role_label, season, if (length(weeks) == 1) "" else "s", paste(weeks, collapse = ", "), n_players, bin_width, min_n)

  p_cov <- ggplot(cov_avg, aes(bin_mid, coverage, color = series, linetype = interval)) +
    geom_hline(yintercept = c(0.5, 0.9), color = "grey70", linewidth = 0.4) +
    geom_point(data = cov_wk, alpha = 0.35, size = 1, stroke = 0) +
    geom_line(linewidth = 0.9) +
    scale_color_brewer(palette = "Dark2") +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    scale_x_continuous(breaks = seq(0, 60, 15), limits = c(0, x_max)) +
    facet_wrap(~position, nrow = 1) +
    labs(x = NULL, y = "Actual score fell\ninside the interval", color = "Distribution", linetype = "Interval",
        title = sprintf("%s: is the uncertainty band honest? (coverage of the predicted interval)", role_label),
        subtitle = "Dashed grey = nominal target (50% / 90%). Below its target = interval too narrow there; above = too wide.") +
    theme_house(legend = "right")

  p_acc <- ggplot(acc_avg, aes(bin_mid, value, color = metric)) +
    geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
    geom_point(data = acc_wk, alpha = 0.35, size = 1, stroke = 0) +
    geom_line(linewidth = 0.9) +
    scale_color_brewer(palette = "Dark2") +
    scale_x_continuous(breaks = seq(0, 60, 15), limits = c(0, x_max)) +
    facet_wrap(~position, nrow = 1) +
    labs(x = "Minutes into the game", y = "Points", color = NULL,
        title = sprintf("%s: is the point projection close? (bias and mean absolute error)", role_label)) +
    theme_house(legend = "right")

  p <- (p_cov / p_acc) + plot_layout(heights = c(1, 1)) +
    plot_annotation(caption = cap, theme = theme(plot.caption = element_text(size = rel(0.75), color = "grey50")))

  if (!is.null(save_path)) ggsave(save_path, p, width = width, height = height, dpi = dpi)
  p
}

# Convenience: build and (optionally) save both role versions in one call.
plot_accuracy_summary_both <- function(season, weeks = available_weeks(season),
                                       save_starters = NULL, save_backups = NULL, ...) {
  list(
    starter = plot_accuracy_summary(season, weeks, role = "starter", save_path = save_starters, ...),
    backup = plot_accuracy_summary(season, weeks, role = "backup", save_path = save_backups, ...)
  )
}

# --- Same two-panel diagnostic, but Current model vs Bayesian model (R/bayesian.R) ---------
# plot_accuracy_summary() above asks "is the Empirical shape more honest than a Normal one,
# for the model this app actually runs" -- a shape question, holding the mean/SD source fixed.
# This asks a different question: "does the BAYESIAN mean/SD (R/bayesian.R) produce a better-
# calibrated, more accurate interval than the CURRENT model's (R/pipeline.R)?" -- so each
# model is scored against its OWN empirical-shape quantiles (cur_q* vs bayes_cur_q*), exactly
# the intervals each one's own app tab would actually show, rather than mixing in the
# Normal-vs-Empirical question from the other plot.
#
# Usage (from the guillotine_app directory):
#   source("analysis/plot_accuracy_summary.R")
#   plot_model_accuracy_comparison(2026, role = "starter")
plot_model_accuracy_comparison <- function(season, weeks = available_weeks(season), role = c("starter", "backup"),
                                           min_n = 10, bin_width = 1, save_path = NULL,
                                           width = 11, height = 7, dpi = 150) {
  role <- match.arg(role)
  role_value <- if (role == "starter") "starter" else "backup_entered"

  raw <- load_replay_weeks(season, weeks) |>
    filter(position %in% c("QB", "RB", "WR", "TE"), !is.na(final_actual_points), !is_final_row,
          role_final == role_value)
  if (nrow(raw) == 0) stop("No rows with role_final == '", role_value, "' in the requested weeks.")

  d <- raw |>
    mutate(
      minutes = grid_seconds / 60,
      bin_mid = (floor(minutes / bin_width) + 0.5) * bin_width,
      in50_cur = final_actual_points >= cur_q25 & final_actual_points <= cur_q75,
      in90_cur = final_actual_points >= cur_q05 & final_actual_points <= cur_q95,
      in50_bayes = final_actual_points >= bayes_cur_q25 & final_actual_points <= bayes_cur_q75,
      in90_bayes = final_actual_points >= bayes_cur_q05 & final_actual_points <= bayes_cur_q95,
      error_cur = final_actual_points - projected_final,
      error_bayes = final_actual_points - bayes_projected_final
    )

  # Per-WEEK values (the points) ------------------------------------------------------------
  cov_wk <- d |>
    group_by(week, position, bin_mid) |>
    summarise(n = n(), `50% - Current` = mean(in50_cur), `90% - Current` = mean(in90_cur),
             `50% - Bayesian` = mean(in50_bayes), `90% - Bayesian` = mean(in90_bayes), .groups = "drop") |>
    filter(n >= min_n) |>
    pivot_longer(c(`50% - Current`, `90% - Current`, `50% - Bayesian`, `90% - Bayesian`),
                 names_to = "which", values_to = "coverage") |>
    separate(which, into = c("interval", "series"), sep = " - ")

  acc_wk <- d |>
    group_by(week, position, bin_mid) |>
    summarise(n = n(), `Bias - Current` = mean(error_cur), `Bias - Bayesian` = mean(error_bayes),
             `MAE - Current` = mean(abs(error_cur)), `MAE - Bayesian` = mean(abs(error_bayes)), .groups = "drop") |>
    filter(n >= min_n) |>
    pivot_longer(c(`Bias - Current`, `Bias - Bayesian`, `MAE - Current`, `MAE - Bayesian`),
                 names_to = "which", values_to = "value") |>
    separate(which, into = c("metric", "series"), sep = " - ")

  # Trend = the plain average of the per-week values (each week weighted equally) -----------
  cov_avg <- cov_wk |> group_by(position, bin_mid, interval, series) |>
    summarise(coverage = mean(coverage), n_weeks = n(), .groups = "drop")
  acc_avg <- acc_wk |> group_by(position, bin_mid, metric, series) |>
    summarise(value = mean(value), n_weeks = n(), .groups = "drop")

  n_weeks <- n_distinct(raw$week)
  n_players <- n_distinct(paste(raw$week, raw$player_id))
  x_max <- max(cov_wk$bin_mid, acc_wk$bin_mid)
  role_label <- if (role == "starter") "Starters" else "Backups who played"
  cap <- sprintf("%s -- %s, week%s %s (%d player-games); faint points = each week's own value, line = average of the weeks; %d-minute bins, bins under %d players dropped. Both models use their own empirical-shape interval (what each app tab actually shows).",
                role_label, season, if (length(weeks) == 1) "" else "s", paste(weeks, collapse = ", "), n_players, bin_width, min_n)

  p_cov <- ggplot(cov_avg, aes(bin_mid, coverage, color = series, linetype = interval)) +
    geom_hline(yintercept = c(0.5, 0.9), color = "grey70", linewidth = 0.4) +
    geom_point(data = cov_wk, alpha = 0.35, size = 1, stroke = 0) +
    geom_line(linewidth = 0.9) +
    scale_color_brewer(palette = "Dark2") +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    scale_x_continuous(breaks = seq(0, 60, 15), limits = c(0, x_max)) +
    facet_wrap(~position, nrow = 1) +
    labs(x = NULL, y = "Actual score fell\ninside the interval", color = "Model", linetype = "Interval",
        title = sprintf("%s: Current vs Bayesian model -- is the uncertainty band honest?", role_label),
        subtitle = "Dashed grey = nominal target (50% / 90%). Below its target = interval too narrow there; above = too wide.") +
    theme_house(legend = "right")

  p_acc <- ggplot(acc_avg, aes(bin_mid, value, color = series, linetype = metric)) +
    geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
    geom_point(data = acc_wk, alpha = 0.35, size = 1, stroke = 0) +
    geom_line(linewidth = 0.9) +
    scale_color_brewer(palette = "Dark2") +
    scale_x_continuous(breaks = seq(0, 60, 15), limits = c(0, x_max)) +
    facet_wrap(~position, nrow = 1) +
    labs(x = "Minutes into the game", y = "Points", color = "Model", linetype = NULL,
        title = sprintf("%s: Current vs Bayesian model -- is the point projection close?", role_label)) +
    theme_house(legend = "right")

  p <- (p_cov / p_acc) + plot_layout(heights = c(1, 1)) +
    plot_annotation(caption = cap, theme = theme(plot.caption = element_text(size = rel(0.75), color = "grey50")))

  if (!is.null(save_path)) ggsave(save_path, p, width = width, height = height, dpi = dpi)
  p
}

plot_model_accuracy_comparison_both <- function(season, weeks = available_weeks(season),
                                                save_starters = NULL, save_backups = NULL, ...) {
  list(
    starter = plot_model_accuracy_comparison(season, weeks, role = "starter", save_path = save_starters, ...),
    backup = plot_model_accuracy_comparison(season, weeks, role = "backup", save_path = save_backups, ...)
  )
}
