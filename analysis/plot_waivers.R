# --- Waiver figures for the Season overview (dark Sleeper look) -----------------------------------------------------
# Both read recap/{season}/waiver_claims.csv, written by collector/weekly_recap.R (collector/waivers_lib.R waiver_roi()).
#   plot_waiver_spending()  dollars won on waivers per surviving team between one chop and the next, split into the chopped team's players and everyone else.
#   plot_waiver_budgets()   every team's waiver budget over time (one line per team; chopped teams end with a red cross).
#   plot_waiver_value()     one dot per waiver claim: dollars paid against the AVERAGE POINTS PER GAME the player added to the team's best-ball score,
#                           with guide lines at 3 and 10 points per $100 (per $100 = average per game / price x 100). Hollow dots are claims whose player has not played for the team yet.
suppressMessages({ library(dplyr); library(readr); library(ggplot2); library(patchwork); library(ggrepel) })
if (!exists("FIG")) source(file.path("analysis", "theme_sleeper.R"))

plot_waiver_spending <- function(claims, save_path = NULL, width = 9, height = 5.2, dpi = 150) {
  K <- max(claims$n_chops)
  per <- claims |> group_by(period) |> summarise(alive = first(teams_alive), .groups = "drop")
  d <- claims |> mutate(grp = ifelse(from_chop, "Players freed by the chop", "Everyone else")) |> group_by(period, grp) |> summarise(spent = sum(bid), alive = first(teams_alive), .groups = "drop") |>
    filter(period >= 1) |> mutate(spent = spent / alive)   # dollars per team still alive, so the bars compare fairly as teams are chopped
  pe <- sort(unique(d$period)); alive <- per$alive[match(pe, per$period)]
  lab <- ifelse(pe == K, sprintf("After week %d chop\n(%d teams, so far)", pe, alive), sprintf("After week %d chop\n(%d teams)", pe, alive))
  d$period <- factor(d$period, levels = pe, labels = lab)
  tot <- d |> group_by(period) |> summarise(spent = sum(spent), .groups = "drop")
  cols <- c(`Players freed by the chop` = FIG$chop, `Everyone else` = FIG$approx); d$grp <- factor(d$grp, levels = names(cols))
  p <- ggplot(d, aes(period, spent, fill = grp)) + geom_col(width = 0.62) +
    geom_text(data = tot, aes(period, spent, label = scales::dollar(round(spent)), fill = NULL), vjust = -0.5, color = FIG$text, size = 4.6, fontface = "bold") +
    scale_fill_manual(values = cols, name = NULL) + scale_y_continuous(labels = scales::label_dollar(), expand = expansion(mult = c(0, 0.14))) +
    labs(x = NULL, y = "Waiver dollars spent per team", title = "League waiver spending per team",
         subtitle = "Dollars paid on winning claims between one chop and the next, divided by the\nnumber of teams still alive, by whether the player was freed by a chop.") +
    theme_sleeper(legend = "top") + theme(panel.grid.major.x = element_blank(), plot.title.position = "plot")
  if (!is.null(save_path)) ggsave(save_path, p, width = width, height = height, dpi = dpi, bg = FIG$bg)
  p
}

# Everyone's waiver budget over time (faab_history.csv): one line per team, a red cross where a team was chopped.
plot_waiver_budgets <- function(hist, save_path = NULL, width = 9, height = 7.6, dpi = 150) {
  K <- max(hist$x[is.na(hist$chopped_week)]); labs_x <- c("Start", sprintf("After week %d\nchop", seq_len(K)))
  ends <- hist |> group_by(team_name) |> slice_max(x, n = 1, with_ties = FALSE) |> ungroup()
  ends$out <- !is.na(ends$chopped_week); ends$status <- ifelse(ends$out, "Chopped", "Alive"); ends$lab <- sprintf("%s  %s", ends$team_name, scales::dollar(ends$remaining))
  hist$status <- ifelse(is.na(hist$chopped_week), "Alive", "Chopped")
  cols <- c(Alive = FIG$approx, Chopped = FIG$chop)
  p <- ggplot(hist, aes(x, remaining, group = team_name, color = status)) +
    geom_line(linewidth = 1.1, alpha = 0.85) + geom_point(size = 2.6) +
    geom_point(data = ends[ends$out, ], shape = 4, size = 4.6, stroke = 1.6) +
    geom_text_repel(data = ends[!ends$out, ], aes(label = lab), direction = "y", hjust = 0, nudge_x = 0.12, xlim = c(K + 0.1, K + 2.4), size = 4, segment.size = 0.3, segment.alpha = 0.5, min.segment.length = 0, box.padding = 0.15, seed = 3, show.legend = FALSE) +
    scale_color_manual(values = cols, guide = "none") +
    scale_x_continuous(breaks = 0:K, labels = labs_x, limits = c(0, K + 2.4), expand = expansion(mult = c(0.02, 0))) +
    scale_y_continuous(labels = scales::label_dollar(), limits = c(0, 1000), expand = expansion(mult = c(0.01, 0.03))) +
    labs(x = NULL, y = "Waiver budget left", title = "Waiver budget over time",
         subtitle = "Each line is one team, starting at $1,000. A red cross marks a team that was chopped.") +
    theme_sleeper() + theme(panel.grid.major.x = element_line(color = FIG$grid, linewidth = 0.25), plot.title.position = "plot")
  if (!is.null(save_path)) ggsave(save_path, p, width = width, height = height, dpi = dpi, bg = FIG$bg)
  p
}

plot_waiver_value <- function(claims, save_path = NULL, width = 9, height = 7.4, dpi = 150, guides = c(3, 10), n_label = 4) {
  d <- claims |> mutate(played = weeks_held > 0, grp = ifelse(from_chop, "Player freed by the chop", "Other player"))
  d$avg_pg <- ifelse(d$played, d$points_added / pmax(d$weeks_held, 1), 0)   # average points per game (0 until the player has played)
  d$p100 <- ifelse(d$bid > 0, 100 * d$avg_pg / d$bid, ifelse(d$avg_pg > 0, 1e6, 0))   # a free claim that scored is as good as it gets
  xmax <- max(d$bid) * 1.05; ymax <- max(c(d$avg_pg, 1)) * 1.12
  gl <- tibble::tibble(k = guides, lab = sprintf("%d points per $100", guides))
  gl$xend <- pmin(xmax, ymax / (gl$k / 100)); gl$yend <- gl$xend * gl$k / 100
  d$lab <- sprintf("%s\n%s (week %d)", d$player, d$team_name, d$for_week)
  pick <- d |> filter(played) |> arrange(desc(avg_pg)) |> head(n_label)
  pick <- bind_rows(pick, d |> filter(played, !player %in% pick$player) |> arrange(desc(bid)) |> head(2))
  d$grp <- factor(d$grp, levels = c("Player freed by the chop", "Other player"))
  pick$grp <- factor(pick$grp, levels = levels(d$grp))
  top <- 30   # the color scale stops here so one very cheap claim does not wash out the rest
  p <- ggplot(d, aes(bid, avg_pg)) +
    geom_segment(data = gl, aes(x = 0, y = 0, xend = xend, yend = yend), color = FIG$dim, linetype = "dashed", linewidth = 0.6, inherit.aes = FALSE) +
    geom_text(data = gl, aes(x = xend, y = yend, label = lab, hjust = ifelse(xend < xmax, -0.08, 1), vjust = ifelse(xend < xmax, 1, -0.7)), color = FIG$muted, size = 4, inherit.aes = FALSE) +
    geom_point(data = d[d$played, ], aes(fill = p100), shape = 21, size = 4.6, color = FIG$bg, stroke = 0.7) +
    geom_point(data = d[!d$played, ], shape = 21, size = 3.8, fill = NA, color = FIG$dim, stroke = 1.1) +
    geom_text_repel(data = pick, aes(label = lab), color = FIG$text, size = 3.9, lineheight = 0.95, fontface = "bold", show.legend = FALSE, seed = 11, box.padding = 0.9, force = 3, max.overlaps = Inf, min.segment.length = 0, segment.alpha = 0.6,
                    nudge_x = ifelse(pick$bid < 100, 75, 0), hjust = ifelse(pick$bid < 100, 0, 0.5)) +
    scale_fill_gradientn(colors = c("#FF6482", "#FFC857", "#4CD787", "#00FFF9"), limits = c(0, top), oob = scales::squish, name = "Points per $100",
                         breaks = c(0, 10, 20, top), labels = c("0", "10", "20", paste0(top, "+"))) +
    guides(fill = guide_colorbar(order = 1, barwidth = 11, barheight = 0.8, title.position = "left", title.vjust = 0.8)) +
    scale_x_continuous(labels = scales::label_dollar(), limits = c(0, xmax), expand = expansion(mult = c(0.01, 0))) +
    scale_y_continuous(limits = c(min(0, min(d$avg_pg) * 1.1), ymax), expand = expansion(0)) +
    labs(x = "Dollars paid", y = "Average points per game added\nto the team's best-ball score", title = "Was it worth it?",
         subtitle = "One dot per waiver claim, updated every week. Points added: the team's best-ball\nscore with the player minus without him, averaged over the games he was held. Color is\npoints per $100 (the average per game divided by the price, times 100; teal is good value);\ndashed lines mark 3 and 10 points per $100.\nHollow grey: no points yet (picked up for the coming week, or dropped\nbefore the week ended).") +
    theme_sleeper(legend = "top") + theme(plot.title.position = "plot")
  if (!is.null(save_path)) ggsave(save_path, p, width = width, height = height, dpi = dpi, bg = FIG$bg)
  p
}

# NOT USED on the page yet: the real question is whether early spending keeps a team alive longer, which needs many more chops. Kept for later.
# Money left against what the waivers bought, one dot per surviving team: x = waiver budget left now (faab_history.csv, latest point), y = points its waiver claims have added.
plot_waiver_money_vs_points <- function(claims, hist, save_path = NULL, width = 9, height = 6.4, dpi = 150) {
  now <- hist |> filter(is.na(chopped_week)) |> group_by(team_name) |> slice_max(x, n = 1, with_ties = FALSE) |> ungroup() |> select(team_name, remaining)
  pts <- claims |> group_by(team_name) |> summarise(added = sum(points_added), .groups = "drop")
  d <- left_join(now, pts, by = "team_name") |> mutate(added = coalesce(added, 0))
  p <- ggplot(d, aes(remaining, added)) + geom_point(size = 4.2, color = FIG$approx) +
    geom_text_repel(aes(label = team_name), size = 3.8, color = FIG$text, seed = 9, box.padding = 0.7, point.padding = 0.3, force = 4, max.overlaps = Inf, min.segment.length = 0, segment.alpha = 0.5) +
    scale_x_continuous(labels = scales::label_dollar(), limits = c(0, 1000), expand = expansion(mult = c(0.02, 0.04))) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0.02, 0.1))) +
    labs(x = "Waiver budget left", y = "Points added by waiver claims", title = "Money left against what the waivers bought",
         subtitle = "One dot per surviving team. Points added: the team's best-ball score with each claimed\nplayer minus without him, over the weeks he was held.") +
    theme_sleeper() + theme(plot.title.position = "plot")
  if (!is.null(save_path)) ggsave(save_path, p, width = width, height = height, dpi = dpi, bg = FIG$bg)
  p
}
