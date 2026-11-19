# --- Season-overview figures: final scores week by week, and waiver budget spent vs score --------------------------------
#   plot_scores_over_time()   every team's final score each week (one thin line per team, a red cross where a team was chopped), with the league median over the
#                             teams that were alive that week. The Season overview shows an interactive version (collector/waivers_lib.R season_scores_interactive_html()).
#   plot_budget_vs_points()   one dot per team per week: share of the waiver budget spent going into the week against points above or below the league median.
# Inputs: accuracy_data/replay/{season}_wNN_league_teams_realtime.csv.gz (final_team_points) and recap/{season}/faab_history.csv.
suppressMessages({ library(dplyr); library(readr); library(tidyr); library(ggplot2); library(ggrepel) })
if (!exists("FIG")) source(file.path("analysis", "theme_sleeper.R"))

# One row per team per week it played: final score, and whether it was chopped that week.
season_scores <- function(season, weeks, replay_dir = file.path("accuracy_data", "replay")) {
  bind_rows(lapply(weeks, function(w) {
    f <- file.path(replay_dir, sprintf("%s_w%02d_league_teams_realtime.csv.gz", season, w)); if (!file.exists(f)) return(NULL)
    read_csv(f, show_col_types = FALSE) |> group_by(roster_id) |> slice_tail(n = 1) |> ungroup() |>
      transmute(week = w, roster_id, team_name, owner_name, score = final_team_points, chopped = as.logical(chopped))
  }))
}

plot_scores_over_time <- function(sc, save_path = NULL, width = 9, height = 6.4, dpi = 150) {
  W <- sort(unique(sc$week))
  lg <- sc |> group_by(week) |> summarise(score = median(score), .groups = "drop")
  bnd <- sc |> group_by(week) |> summarise(High = max(score), Low = min(score), .groups = "drop") |> pivot_longer(c(High, Low), names_to = "end", values_to = "score")
  out <- sc |> filter(chopped)
  p <- ggplot(sc, aes(week, score)) +
    geom_line(aes(group = team_name), color = FIG$other, alpha = 0.32, linewidth = 0.5) +
    geom_point(color = FIG$other, alpha = 0.4, size = 1.2) +
    geom_line(data = bnd, aes(group = end, color = "Highest and lowest score"), linetype = "dotted", linewidth = 1.1) +
    geom_line(data = lg, aes(color = "League median"), linewidth = 1.6) + geom_point(data = lg, aes(color = "League median"), size = 2.8) +
    geom_point(data = out, shape = 4, size = 3.4, stroke = 1.4, color = FIG$chop) +   # on top of the dotted line
    scale_color_manual(values = c(`League median` = FIG$text, `Highest and lowest score` = FIG$muted), name = NULL) +
    scale_x_continuous(breaks = W, expand = expansion(mult = c(0.03, 0.03))) +
    labs(x = "Week", y = "Final score (best ball)", title = "Final score by week",
         subtitle = "Grey lines are single teams; a red cross marks the week a team was chopped.\nDotted lines join the highest and the lowest score each week.") +
    theme_sleeper(legend = "top") + theme(plot.title.position = "plot", panel.grid.major.x = element_line(color = FIG$grid, linewidth = 0.25))
  if (!is.null(save_path)) ggsave(save_path, p, width = width, height = height, dpi = dpi, bg = FIG$bg, limitsize = FALSE)
  p
}

# One row per team per week (weeks 2 on: nothing can be claimed before week 1): the waiver budget spent going into the week (faab_history x = week - 1: everything spent up to the
# bidding for this week), and what that week's waiver claims added to the team's score (recap/{season}/waiver_points_by_week.csv from waiver_week_effect()).
budget_by_week <- function(sc, hist, effect, budget = 1000) {
  sc |> left_join(transmute(hist, team_name, week = x + 1L, pct_spent = 100 * (budget - remaining) / budget), by = c("team_name", "week")) |>
    left_join(effect[c("team_name", "week", "n_claims", "spent", "points_added", "claims")], by = c("team_name", "week")) |>
    filter(week >= 2, !is.na(pct_spent), !is.na(points_added)) |> select(team_name, owner_name, week, score, pct_spent, n_claims, spent, points_added, claims, chopped)
}

plot_budget_vs_points <- function(bw, save_path = NULL, width = 9, height = 6.4, dpi = 150) {
  p <- ggplot(bw, aes(pct_spent, points_added)) +
    geom_hline(yintercept = 0, color = FIG$dim, linetype = "22", linewidth = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = FIG$text, linetype = "22", linewidth = 0.9, formula = y ~ x) +
    geom_point(aes(color = week), size = 2.8, alpha = 0.85) +
    scale_color_gradient(low = FIG$survive, high = FIG$chop, name = "Week", breaks = pretty(bw$week, 4)) +
    scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100), expand = expansion(mult = c(0.02, 0.03))) +
    labs(x = "Waiver budget spent going into the week", y = "Points added by that week's waiver claims", title = "Waiver budget spent vs points added",
         subtitle = "One dot per team per week. The dashed line is the straight-line trend.") +
    theme_sleeper(legend = "top") + theme(plot.title.position = "plot")
  if (!is.null(save_path)) ggsave(save_path, p, width = width, height = height, dpi = dpi, bg = FIG$bg, limitsize = FALSE)
  p
}

# --- The pool each chop puts on the waivers, and what it sold for -----------------------------------------------------------------------------
# freed <- freed_pool_by_week.csv (collector/waivers_lib.R freed_pool_history()). One cluster of dots per chop: the six freed players with the highest average over their last three
# games played, best on the left. A filled dot was claimed and its size is the price paid (the price is printed under it); a hollow dot was not claimed; amber is the latest chop,
# still up for bid. The line is the average of the six.
plot_freed_pool <- function(freed, n_show = 6, save_path = NULL, width = 9, height = 6.4, dpi = 150) {
  d <- freed |> group_by(chop_week) |> slice_max(avg3, n = n_show, with_ties = FALSE) |> ungroup()
  d$yv <- d$avg3
  m <- d |> group_by(chop_week) |> summarise(yv = mean(yv), .groups = "drop")
  W <- sort(unique(d$chop_week))
  p <- ggplot(d, aes(chop_week, yv)) +
    geom_line(data = m, aes(chop_week, yv), color = FIG$text, linewidth = 1, alpha = 0.7, inherit.aes = FALSE) +
    geom_point(data = d[!d$claimed & !d$open, ], shape = 21, size = 3.4, color = FIG$dim, fill = NA, stroke = 1) +
    geom_point(data = d[d$open, ], shape = 21, size = 3.6, color = FIG$contenders[3], fill = NA, stroke = 1.3) +
    geom_point(data = d[d$claimed, ], aes(color = price), size = 3.6, alpha = 0.9) +
    scale_color_gradient(low = FIG$survive, high = FIG$chop, limits = c(0, 350), oob = scales::squish, breaks = c(0, 100, 200, 300), labels = c("$0", "$100", "$200", "$300+"), name = "Price paid") +
    scale_x_continuous(breaks = W, labels = W, expand = expansion(mult = c(0.04, 0.04))) +
    labs(x = "Week of the chop", y = "Average over his last 3 games played", title = "What each chop put on the waivers, and what it sold for",
         subtitle = "Six best freed players per chop. Colored = claimed (color = price), hollow grey = not claimed,\namber ring = up for bid now. The line is the average of the six.") +
    theme_sleeper(legend = "top") + theme(plot.title.position = "plot", panel.grid.major.x = element_line(color = FIG$grid, linewidth = 0.25), legend.key.width = grid::unit(1.6, "cm"))
  if (!is.null(save_path)) ggsave(save_path, p, width = width, height = height, dpi = dpi, bg = FIG$bg, limitsize = FALSE)
  p
}

# --- Spend curve: each team's claims in the order they were made ---------------------------------------------------------------------------
# cl <- waiver_claims.csv. x = waiver dollars spent so far, y = the points per game those claims add to the team's score (each claim's average per game since the pickup, summed), so an
# early claim, which has simply been held for more games, gets no head start. A steep climb is a team that bought well. Free ($0) claims count for the points only.
spend_curve_data <- function(cl) {
  cl |> mutate(apg = ifelse(coalesce(weeks_held, 0) > 0, coalesce(points_added, 0) / weeks_held, 0), bid = coalesce(bid, 0)) |> arrange(team_name, ts) |> group_by(team_name, owner_name) |>
    mutate(cum_spent = cumsum(bid), cum_apg = cumsum(apg), n = row_number()) |> ungroup()
}

plot_spend_curve <- function(sd, save_path = NULL, width = 9, height = 6.4, dpi = 150) {
  if (!"chopped" %in% names(sd)) sd$chopped <- FALSE
  z <- sd |> distinct(team_name, chopped) |> mutate(cum_spent = 0, cum_apg = 0, ts = -Inf)
  d <- bind_rows(sd |> select(team_name, chopped, cum_spent, cum_apg, ts), z) |> arrange(team_name, ts)
  d$status <- ifelse(d$chopped, "Chopped", "Still alive")
  slope <- sum(sd$apg) / sum(sd$bid)
  xm <- max(sd$cum_spent)
  p <- ggplot(d, aes(cum_spent, cum_apg, group = team_name)) +
    geom_abline(intercept = 0, slope = slope, color = FIG$text, linetype = "22", linewidth = 0.9) +
    geom_line(aes(color = status), alpha = 0.55, linewidth = 0.6) + geom_point(aes(color = status), alpha = 0.6, size = 1) +
    scale_color_manual(values = c(`Still alive` = FIG$other, Chopped = FIG$chop), name = NULL) +
    annotate("text", x = xm * 0.97, y = xm * 0.97 * slope, label = sprintf("League average: %.1f points per game per $100", 100 * slope), vjust = -0.7, hjust = 1, color = FIG$muted, size = 3.6) +
    scale_x_continuous(labels = scales::label_dollar(), expand = expansion(mult = c(0.01, 0.06))) +
    labs(x = "Waiver dollars spent so far", y = "Points per game the claims add", title = "Spend curve",
         subtitle = "One line per team, one step per claim in the order it was made.\nSteeper than the dashed line = more points per dollar than the league.") +
    theme_sleeper(legend = "top") + theme(plot.title.position = "plot")
  if (!is.null(save_path)) ggsave(save_path, p, width = width, height = height, dpi = dpi, bg = FIG$bg, limitsize = FALSE)
  p
}
