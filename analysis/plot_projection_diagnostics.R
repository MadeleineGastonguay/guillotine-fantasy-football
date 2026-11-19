# --- Diagnostic plot: one player's projected-score distribution over a game ---------------
# For a single player-game, shows how the model's belief about their FINAL weekly score
# evolves as the game plays out (every `step_minutes` of game time, from the replay data in
# R/replay.R): a shaded band for the normal approximation next to one for the empirical
# (position-residual-shape) distribution the app actually uses, the points scored so far,
# and two reference lines -- the pre-game projection and what they actually finished with.
#
# Needs a replay file for that season/week (R/replay.R::replay_week(); already written to
# accuracy_data/replay/{season}_w{week}_players.csv.gz for 2026 weeks 1-3 as of this analysis).
#
# Usage (from the guillotine_app directory):
#   source("analysis/plot_projection_diagnostics.R")
#   plot_player_diagnostic(2026, 1, "Derrick Henry")   # partial, case-insensitive name match
#   plot_player_diagnostic(2026, 1, "3198")            # or an exact Sleeper player_id
# Add save_path = "somefile.png" to also write the figure to disk.

suppressMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
})

setwd_here <- function() {
  if (file.exists("R/config.R")) return(invisible())
  if (file.exists("../R/config.R")) { setwd(".."); return(invisible()) }
  stop("Run from the guillotine_app directory (or analysis/ within it).")
}
setwd_here()
for (f in c("config", "sleeper_api", "collector")) source(file.path("R", paste0(f, ".R")))
source(file.path("analysis", "my_theme.R")) # house ggplot2 theme (r-figure-theme skill); copy kept in this project, not the skill's own (session-scoped) path

REPLAY_DIR <- file.path("accuracy_data", "replay")

find_player <- function(replay, query) {
  names_tbl <- slim_players_table()[, c("player_id", "full_name")] |> mutate(player_id = as.character(player_id))
  replay <- replay |> mutate(player_id = as.character(player_id)) |> left_join(names_tbl, by = "player_id")
  if (query %in% replay$player_id) return(replay |> filter(player_id == query))
  hit <- replay |> filter(grepl(query, full_name, ignore.case = TRUE))
  ids <- unique(hit$player_id)
  if (length(ids) == 0) stop("No player matched '", query, "' in this week's replay data.")
  if (length(ids) > 1) {
    stop("'", query, "' matches more than one player: ",
        paste(unique(hit$full_name), collapse = ", "), " -- be more specific or use their player_id.")
  }
  hit
}

plot_player_diagnostic <- function(season, week, player, replay_dir = REPLAY_DIR, save_path = NULL,
                                   width = 9, height = 5.5, dpi = 150) {
  f <- file.path(replay_dir, sprintf("%s_w%02d_players.csv.gz", season, as.integer(week)))
  if (!file.exists(f)) {
    stop("No replay file at ", f, " -- generate it first with:\n",
        sprintf('  replay_week(%s, %d, step_minutes = 5)  # see R/replay.R', season, week))
  }
  raw <- read_csv(f, show_col_types = FALSE)
  d <- find_player(raw, as.character(player))

  final_row <- d |> filter(is_final_row)
  gd <- d |> filter(!is_final_row) |> arrange(grid_seconds) |> mutate(minutes = grid_seconds / 60)
  if (nrow(gd) == 0) stop(final_row$full_name[1], " has no in-game replay rows for week ", week, " (bye or inactive).")

  name <- final_row$full_name[1]; pos <- final_row$position[1]; matchup <- final_row$matchup_label[1]
  pre_proj <- final_row$pre_proj[1]; final_actual <- final_row$final_actual_points[1]
  step_min <- min(diff(sort(unique(gd$minutes)))) # detected from the data, not assumed
  qp <- c(.05, .25, .5, .75, .95)

  # Empirical quantiles come straight from the replay (built from the position's real
  # residual shape, exactly as R/distributions.R does for the app's own charts). Normal
  # quantiles use the SAME running mean/sd (projected_final, projected_sd) so the two
  # bands are a fair side-by-side comparison, not just different centers.
  normal_q <- t(mapply(function(m, s) qnorm(qp, m, pmax(s, 0.01)), gd$projected_final, gd$projected_sd))
  colnames(normal_q) <- c("lo90", "lo50", "median", "hi50", "hi90")
  empirical_q <- gd |> select(cur_q05, cur_q25, cur_q50, cur_q75, cur_q95) |>
    rename(lo90 = cur_q05, lo50 = cur_q25, median = cur_q50, hi50 = cur_q75, hi90 = cur_q95)

  fan <- bind_rows(
    bind_cols(minutes = gd$minutes, as.data.frame(normal_q), series = "Normal approximation"),
    bind_cols(minutes = gd$minutes, empirical_q, series = "Empirical distribution (used by the app)")
  )

  ymax <- max(fan$hi90, final_actual, pre_proj, na.rm = TRUE)
  ymin <- min(fan$lo90, 0, na.rm = TRUE)
  label_x <- max(gd$minutes) * 1.03
  labels_df <- tibble(
    x = label_x, y = c(pre_proj, final_actual),
    text = c(sprintf("Pre-game\nprojection\n%.1f", pre_proj), sprintf("Actual\nfinal\n%.1f", final_actual)),
    bold = c("plain", "bold")
  )

  p <- ggplot(fan, aes(x = minutes)) +
    geom_ribbon(aes(ymin = lo90, ymax = hi90, fill = series), alpha = 0.16) +
    geom_ribbon(aes(ymin = lo50, ymax = hi50, fill = series), alpha = 0.30) +
    geom_line(aes(y = median, color = series), linewidth = 0.9) +
    geom_step(data = gd, aes(y = current_points), color = "grey25", linewidth = 0.5, linetype = "12") +
    geom_hline(yintercept = pre_proj, linetype = "dashed", color = "grey35", linewidth = 0.6) +
    geom_hline(yintercept = final_actual, color = "black", linewidth = 0.7) +
    geom_label(data = labels_df, aes(x = x, y = y, label = text, fontface = bold),
               hjust = 0, size = 2.9, linewidth = 0, fill = "white", color = "grey15", inherit.aes = FALSE) +
    scale_fill_brewer(palette = "Dark2") +
    scale_color_brewer(palette = "Dark2") +
    scale_x_continuous(breaks = seq(0, 60, 15), expand = expansion(mult = c(0.01, 0.16))) +
    coord_cartesian(ylim = c(ymin, ymax * 1.05), clip = "off") +
    labs(
      x = "Minutes into the game", y = "Projected final points for the week",
      title = sprintf("%s (%s, %s) -- Week %s, %s", name, pos, matchup, week, season),
      subtitle = paste0("Bands = middle 50% / 90% of the model's belief about this player's FINAL weekly score,\n",
                        sprintf("updated every %g game-minute%s. Dotted grey = points scored so far.", step_min, if (step_min == 1) "" else "s")),
      fill = NULL, color = NULL
    ) +
    theme_house(legend = "bottom") +
    theme(plot.margin = margin(10, 78, 10, 10))

  if (!is.null(save_path)) ggsave(save_path, p, width = width, height = height, dpi = dpi)
  p
}

# --- Diagnostic plot: Current model vs Bayesian model, same player-game ---------------------
# Same fan-chart layout as plot_player_diagnostic(), but instead of comparing Normal vs
# Empirical shape for ONE model, it compares the two MODELS' predictive intervals against
# each other -- both using the app's usual empirical (position-residual-shape) distribution,
# so the only thing that differs between the two bands is how each model's mean/SD evolve
# during the game (R/pipeline.R's elapsed_fraction^3 / sqrt(remaining_fraction) vs
# R/bayesian.R's Normal-Normal update). See R/bayesian.R for the model itself.
#
# Usage (from the guillotine_app directory):
#   source("analysis/plot_projection_diagnostics.R")
#   plot_model_comparison(2026, 1, "Derrick Henry")
plot_model_comparison <- function(season, week, player, replay_dir = REPLAY_DIR, save_path = NULL,
                                  width = 9, height = 5.5, dpi = 150) {
  f <- file.path(replay_dir, sprintf("%s_w%02d_players.csv.gz", season, as.integer(week)))
  if (!file.exists(f)) {
    stop("No replay file at ", f, " -- generate it first with:\n",
        sprintf('  replay_week(%s, %d, step_minutes = 1)  # see R/replay.R', season, week))
  }
  raw <- read_csv(f, show_col_types = FALSE)
  d <- find_player(raw, as.character(player))

  final_row <- d |> filter(is_final_row)
  gd <- d |> filter(!is_final_row) |> arrange(grid_seconds) |> mutate(minutes = grid_seconds / 60)
  if (nrow(gd) == 0) stop(final_row$full_name[1], " has no in-game replay rows for week ", week, " (bye or inactive).")

  name <- final_row$full_name[1]; pos <- final_row$position[1]; matchup <- final_row$matchup_label[1]
  pre_proj <- final_row$pre_proj[1]; final_actual <- final_row$final_actual_points[1]
  step_min <- min(diff(sort(unique(gd$minutes))))

  current_q <- gd |> select(cur_q05, cur_q25, cur_q50, cur_q75, cur_q95) |>
    rename(lo90 = cur_q05, lo50 = cur_q25, median = cur_q50, hi50 = cur_q75, hi90 = cur_q95)
  bayes_q <- gd |> select(bayes_cur_q05, bayes_cur_q25, bayes_cur_q50, bayes_cur_q75, bayes_cur_q95) |>
    rename(lo90 = bayes_cur_q05, lo50 = bayes_cur_q25, median = bayes_cur_q50, hi50 = bayes_cur_q75, hi90 = bayes_cur_q95)

  fan <- bind_rows(
    bind_cols(minutes = gd$minutes, current_q, series = "Current model"),
    bind_cols(minutes = gd$minutes, bayes_q, series = "Bayesian model (experimental)")
  )

  ymax <- max(fan$hi90, final_actual, pre_proj, na.rm = TRUE)
  ymin <- min(fan$lo90, 0, na.rm = TRUE)
  label_x <- max(gd$minutes) * 1.03
  labels_df <- tibble(
    x = label_x, y = c(pre_proj, final_actual),
    text = c(sprintf("Pre-game\nprojection\n%.1f", pre_proj), sprintf("Actual\nfinal\n%.1f", final_actual)),
    bold = c("plain", "bold")
  )

  p <- ggplot(fan, aes(x = minutes)) +
    geom_ribbon(aes(ymin = lo90, ymax = hi90, fill = series), alpha = 0.16) +
    geom_ribbon(aes(ymin = lo50, ymax = hi50, fill = series), alpha = 0.30) +
    geom_line(aes(y = median, color = series), linewidth = 0.9) +
    geom_step(data = gd, aes(y = current_points), color = "grey25", linewidth = 0.5, linetype = "12") +
    geom_hline(yintercept = pre_proj, linetype = "dashed", color = "grey35", linewidth = 0.6) +
    geom_hline(yintercept = final_actual, color = "black", linewidth = 0.7) +
    geom_label(data = labels_df, aes(x = x, y = y, label = text, fontface = bold),
               hjust = 0, size = 2.9, linewidth = 0, fill = "white", color = "grey15", inherit.aes = FALSE) +
    scale_fill_brewer(palette = "Dark2") +
    scale_color_brewer(palette = "Dark2") +
    scale_x_continuous(breaks = seq(0, 60, 15), expand = expansion(mult = c(0.01, 0.16))) +
    coord_cartesian(ylim = c(ymin, ymax * 1.05), clip = "off") +
    labs(
      x = "Minutes into the game", y = "Projected final points for the week",
      title = sprintf("%s (%s, %s) -- Week %s, %s: Current vs Bayesian model", name, pos, matchup, week, season),
      subtitle = paste0("Bands = middle 50% / 90% of each model's belief about this player's FINAL weekly score,\n",
                        sprintf("updated every %g game-minute%s. Both use the same empirical position-residual shape --\n", step_min, if (step_min == 1) "" else "s"),
                        "only the mean/SD evolution differs. Dotted grey = points scored so far."),
      fill = NULL, color = NULL
    ) +
    theme_house(legend = "bottom") +
    theme(plot.margin = margin(10, 78, 10, 10))

  if (!is.null(save_path)) ggsave(save_path, p, width = width, height = height, dpi = dpi)
  p
}
