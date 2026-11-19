# --- Figure: Approximate tab vs Simulations tab, at a handful of moments per week ----------------
# Reads the cached results of analysis/simulation_spot_check.R. One row per week. Left: every team at every sampled
# moment, the Approximate tab's chance of being chopped (x) against the Simulations tab's (y); on the diagonal they
# agree. Right: the chopped team's chance under each tab at the sampled moments, in order.
#
# Usage: source("analysis/plot_sim_check.R"); plot_sim_check(2026, 1:3, save_path = "sim_check.png")

suppressMessages({ library(dplyr); library(readr); library(tidyr); library(ggplot2); library(patchwork) })
if (!file.exists("R/config.R") && file.exists("../R/config.R")) setwd("..")
source(file.path("analysis", "theme_sleeper.R"))
source(file.path("analysis", "simulation_spot_check.R"))

# headline numbers for the write-up
sim_check_summary <- function(sc) {
  per_moment <- sc |> group_by(week, step) |>
    summarise(top_agree = team_name[which.max(approx_prob)] == team_name[which.max(sim_prob)],
              max_gap = max(abs(approx_prob - sim_prob)), .groups = "drop")
  list(
    n_moments = nrow(per_moment), n_points = nrow(sc),
    mean_abs_gap_pp = 100 * mean(abs(sc$approx_prob - sc$sim_prob)),
    max_abs_gap_pp = 100 * max(abs(sc$approx_prob - sc$sim_prob)),
    cor = cor(sc$approx_prob, sc$sim_prob),
    top_pick_agree = sum(per_moment$top_agree),
    per_moment = per_moment
  )
}

plot_sim_check <- function(season, weeks, save_path = NULL, width = 12, height = 11, dpi = 150) {
  sc <- read_sim_check(season, weeks) |>
    mutate(when = format(as.POSIXct(wall_utc, tz = "UTC"), "%a %I:%M%p", tz = "America/New_York") |> sub(pattern = " 0", replacement = " ") |> sub(pattern = "AM$", replacement = "am") |> sub(pattern = "PM$", replacement = "pm"))
  wks <- sort(unique(sc$week))
  last_wk <- max(wks)

  # one row per week: left = every team at every sampled moment, right = the chopped team's chance at those moments
  scatter_week <- function(w) {
    ggplot(filter(sc, week == w), aes(approx_prob, sim_prob)) +
      geom_abline(slope = 1, intercept = 0, color = FIG$muted, linewidth = 0.5) +
      geom_point(data = filter(sc, week == w, !chopped), color = FIG$pts_other, alpha = FIG$pts_other_alpha, size = 1.6) +
      geom_point(data = filter(sc, week == w, chopped), color = FIG$chop, size = 2.8) +
      scale_x_continuous(labels = scales::percent, limits = c(0, 1)) + scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      labs(x = if (w == last_wk) "Approximate tab: chance of being chopped" else NULL, y = "Simulations tab:\nchance of being chopped",
           title = sprintf("Week %d: every team, every moment", w)) +
      theme_sleeper() + theme(plot.title = element_text(size = rel(1.0)))
  }
  ch <- sc |> filter(chopped) |>
    group_by(week) |> arrange(step, .by_group = TRUE) |> mutate(i = row_number()) |> ungroup() |>
    select(week, i, when, approx_prob, sim_prob) |>
    pivot_longer(c(approx_prob, sim_prob), names_to = "tab", values_to = "p") |>
    mutate(tab = recode(tab, approx_prob = "Approximate tab", sim_prob = "Simulations tab"))
  lines_week <- function(w) {
    d <- filter(ch, week == w)
    lab <- d |> distinct(i, when)
    ggplot(d, aes(i, p, color = tab, linetype = tab)) +
      geom_line(linewidth = 0.9) + geom_point(size = 1.8) +
      scale_color_manual(values = c("Approximate tab" = FIG$approx, "Simulations tab" = FIG$sims), name = NULL) +
      scale_linetype_manual(values = c("Approximate tab" = "solid", "Simulations tab" = "dashed"), name = NULL) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      scale_x_continuous(breaks = lab$i, labels = lab$when, expand = expansion(mult = 0.06)) +
      labs(x = NULL, y = "Chance given to the\nteam that was chopped", title = sprintf("Week %d: the team that was chopped", w)) +
      theme_sleeper(legend = "bottom") + theme(axis.text.x = element_text(angle = 40, hjust = 1, size = rel(0.85)), plot.title = element_text(size = rel(1.0)))
  }
  rows <- lapply(wks, function(w) if (isTRUE(getOption("sleeper.stack"))) (scatter_week(w) / lines_week(w)) else (scatter_week(w) + lines_week(w) + plot_layout(widths = c(1, 1.15))))
  stk <- isTRUE(getOption("sleeper.stack"))   # phone layout: the two panels of a week are stacked, so "left / right" becomes "upper / lower"
  p <- wrap_plots(rows, ncol = 1) + plot_layout(guides = "collect") +
    plot_annotation(
      title = wrap_txt("Approximate tab vs Simulations tab: two ways of computing the same odds", "annot"),
      subtitle = wrap_txt(paste0(if (stk) "Upper panel: " else "Left: ", "every team at every sampled moment (", FIG$w_chop, " = the team that was chopped); the grey diagonal is perfect agreement.\n",
                                 if (stk) "Lower panel: " else "Right: ", "the chopped team's chance under each tab at the sampled moments, from before the first game to the end of Monday night."), "annot"),
      caption = wrap_txt("Simulations tab: 5,000 simulated weeks per moment, so each point is good to about +/- 0.7 percentage points.", "annot"),
      theme = annot_theme()) & theme(legend.position = "bottom")
  if (!is.null(save_path)) ggsave(save_path, p, width = width, height = height, dpi = dpi, bg = FIG$bg, limitsize = FALSE)
  p
}
