# --- How good were the app's team-level elimination probabilities, over each week? -------------
# Three figures built from the real-kickoff-time team timelines (analysis/build_team_elim_timeline.R
# -> R/replay.R::replay_league_week_realtime()): what the Approximate tab would have shown at every
# 5 minutes of real game time, with each game on its own real clock (Thursday / Sunday early / late /
# night / Monday), against who was actually chopped.
#
#   1. plot_team_race()           per week: every team's elimination probability (top) and points
#                                  scored so far (bottom); the chopped team and any other real
#                                  contenders highlighted.
#   2. plot_team_scorecard()      all weeks side by side: the probability given to the team that
#                                  was actually chopped (overlaid with that team's projected final
#                                  score and the league median) and that team's rank by probability --
#                                  with a strip of how much of each team's scoring had happened.
#                                  (A Brier-score panel exists in the code, commented out.)
#   3. plot_team_lowest_finishers()  the four lowest-scoring teams each week: points so far, projected
#                                  final with 50%/90% ranges and the actual final, with the elimination
#                                  probability on its own strip underneath each team.
#
# x-axis (all figures): real clock time in US Eastern, with the dead time between blocks of games
# (overnight, Friday-Saturday...) removed and shaded -- nothing happens there. Faint dotted verticals
# are kickoffs.
#
# Caveats: only 3 chops, so this shows how the probabilities BEHAVE far better than it can certify
# calibration. "Points scored so far" / the model's final scores come from nflverse play-by-play, which
# differs from Sleeper's own stat feed for a handful of players a week (the chopped team always agrees).
#
# Usage (from the guillotine_app directory):
#   source("analysis/plot_team_elimination.R")
#   plot_team_race(2026, 1)
#   plot_team_scorecard(2026, 1:3)
#   plot_team_lowest_finishers(2026, 1:3)
#   plot_team_elimination_all(2026, 1:3, out_dir = "somewhere")   # writes every figure as a png

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
source(file.path("analysis", "theme_sleeper.R")) # dark Sleeper look, matching the app

REPLAY_DIR <- file.path("accuracy_data", "replay")
CHOPPED_COLOR <- FIG$chop # the elimination color (Sleeper's red-pink in the dark theme)
CONTENDER_COLORS <- FIG$contenders

load_team_timeline <- function(season, week, replay_dir = REPLAY_DIR) {
  f <- file.path(replay_dir, sprintf("%s_w%02d_league_%s_realtime.csv.gz", season, as.integer(week), c("teams", "games")))
  if (!all(file.exists(f))) stop("Missing timeline for week ", week, " -- build it with analysis/build_team_elim_timeline.R")
  list(teams = read_csv(f[1], show_col_types = FALSE), games = read_csv(f[2], show_col_types = FALSE))
}

# Shared x-axis pieces: ET clock-time ticks inside each block of games, shaded gaps between blocks,
# faint kickoff lines (kickoff_lines = FALSE drops them), and (day_labels = TRUE, for the top panel of a figure) each block's day written
# along the top (short_days: weekday only, for narrow panels; min_tick_gap: minimum spacing between
# time labels, in axis minutes -- raise it for narrow panels; gap_labels: write "no games" inside the shaded gaps). Returns a list of ggplot components to add to a plot with x = x_min.
time_axis <- function(teams, games, day_labels = FALSE, short_days = FALSE, min_tick_gap = 90, gap_labels = FALSE, kickoff_lines = TRUE, day_in_ticks = FALSE) {
  tt <- teams |>
    distinct(block, block_label, x_min, wall_utc) |>
    mutate(wall = as.numeric(as.POSIXct(wall_utc, tz = "UTC")))
  blocks <- tt |>
    group_by(block, block_label) |>
    summarise(x0 = min(x_min), x1 = max(x_min), w0 = wall[which.min(x_min)], .groups = "drop")
  ticks <- bind_rows(lapply(seq_len(nrow(blocks)), function(i) {
    b <- blocks[i, ]
    span_h <- (b$x1 - b$x0) / 60
    step_h <- if (span_h > 5) 2 else 1
    w <- seq(ceiling(b$w0 / 3600) * 3600, b$w0 + (b$x1 - b$x0) * 60, by = 3600) # whole UTC hours = whole ET hours
    et <- as.POSIXct(w, origin = "1970-01-01", tz = "UTC")
    h <- as.integer(format(et, "%H", tz = "America/New_York"))
    keep <- h %% step_h == 0
    lab <- tolower(gsub("^0", "", format(et, "%I%p", tz = "America/New_York")))
    out <- data.frame(block = b$block, x = b$x0 + (w - b$w0) / 60, label = lab)[keep, ]
    # thin the ticks so labels never collide: keep one only if it is >= min_tick_gap (axis minutes) after the last kept
    kept <- integer(0)
    for (j in seq_len(nrow(out))) if (!length(kept) || out$x[j] - out$x[kept[length(kept)]] >= min_tick_gap) kept <- c(kept, j)
    out[kept, ]
  }))
  if (day_in_ticks) { # weekday under the first time label of each block (for panels that carry no day labels of their own)
    first <- !duplicated(ticks$block)
    ticks$label[first] <- paste0(ticks$label[first], "\n", substr(blocks$block_label[match(ticks$block[first], blocks$block)], 1, 3))
  }
  gaps <- if (nrow(blocks) > 1) data.frame(xmin = blocks$x1[-nrow(blocks)], xmax = blocks$x0[-1]) else NULL
  list(
    if (!is.null(gaps)) annotate("rect", xmin = gaps$xmin, xmax = gaps$xmax, ymin = -Inf, ymax = Inf, fill = FIG$gap_fill, alpha = FIG$gap_alpha),
    if (gap_labels && !is.null(gaps)) geom_text(data = data.frame(x = (gaps$xmin + gaps$xmax) / 2), aes(x = x), label = "no games", y = Inf,
                                                angle = 90, hjust = 1.1, vjust = 0.5, size = 4, color = FIG$muted, inherit.aes = FALSE),
    if (day_labels) geom_text(data = data.frame(x = blocks$x0, label = if (short_days) substr(blocks$block_label, 1, 3) else blocks$block_label), aes(x = x, label = label), y = Inf,
                              hjust = 0, vjust = 1.6, size = 4.4, color = FIG$muted, fontface = "bold", inherit.aes = FALSE),
    if (kickoff_lines) geom_vline(xintercept = sort(unique(round(games$x_start, 1))), color = FIG$kick, linetype = "dotted", linewidth = 0.3, alpha = FIG$kick_alpha),
    scale_x_continuous(breaks = ticks$x, labels = ticks$label, expand = expansion(mult = 0.01))
  )
}

# Which teams get a color: the chopped team plus any other team whose probability ever reached
# `contender_cut` (at most 5, highest peaks first); everyone else is grey.
highlight_groups <- function(teams, contender_cut = 0.2) {
  peaks <- teams |> filter(!chopped) |> group_by(team_name) |> summarise(peak = max(elimination_prob), .groups = "drop") |>
    filter(peak >= contender_cut) |> arrange(desc(peak)) |> slice_head(n = length(CONTENDER_COLORS))
  chopped_name <- teams$team_name[teams$chopped][1]
  lvl <- c(chopped_name, peaks$team_name)
  teams |> mutate(group = factor(if_else(team_name %in% lvl, team_name, "Other teams"), levels = c(lvl, "Other teams")))
}

group_scales <- function(groups, chopped_name) {
  lvl <- levels(groups)
  cols <- c(CHOPPED_COLOR, CONTENDER_COLORS)[seq_len(length(lvl) - 1)]
  labs <- lvl; labs[lvl == chopped_name] <- paste0(chopped_name, " (chopped)")
  list(
    scale_color_manual(values = setNames(c(cols, FIG$other), lvl), labels = setNames(labs, lvl), name = NULL),
    scale_linewidth_manual(values = setNames(c(1.5, rep(0.8, length(lvl) - 2), FIG$other_lw), lvl), guide = "none"),
    scale_alpha_manual(values = setNames(c(rep(1, length(lvl) - 1), FIG$other_alpha), lvl), guide = "none")
  )
}

# --- Figure 1 ---------------------------------------------------------------------------------
plot_team_race <- function(season, week, save_path = NULL, width = 11, height = 7.5, dpi = 150) {
  tl <- load_team_timeline(season, week)
  t <- highlight_groups(tl$teams)
  chopped_name <- t$team_name[t$chopped][1]
  final_pts <- t$final_team_points[t$chopped][1]
  n_teams <- n_distinct(t$roster_id)
  # draw the greys first so highlighted lines sit on top
  t <- t |> arrange(desc(group == "Other teams"), group)
  ax <- time_axis(tl$teams, tl$games)
  ax_top <- time_axis(tl$teams, tl$games, day_labels = TRUE, gap_labels = TRUE)

  p_top <- ggplot(t, aes(x_min, elimination_prob, group = roster_id, color = group, linewidth = group, alpha = group)) +
    ax_top + geom_line() +
    group_scales(t$group, chopped_name) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks = seq(0, 1, 0.25), expand = expansion(mult = c(0.01, 0.1))) + # headroom for the day labels
    labs(x = NULL, y = "Probability of being chopped",
         title = wrap_txt(sprintf("Week %d, %s: how the app's elimination odds moved", week, season)),
         subtitle = wrap_txt(sprintf("%s was chopped with %.1f points. %d teams; model = the Approximate tab.\nGrey bands = stretches with no games on (cut out of the time axis).",
                            chopped_name, final_pts, n_teams))) +
    theme_sleeper(legend = "bottom") + theme(axis.text.x = element_blank())

  p_bot <- ggplot(t, aes(x_min, current_score, group = roster_id, color = group, linewidth = group, alpha = group)) +
    ax + geom_line() +
    group_scales(t$group, chopped_name) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.04))) +
    labs(x = wrap_txt("Real clock time (US Eastern); gaps with no games removed (shaded)"), y = "Points scored so far",
         caption = wrap_txt("Points so far = best-ball lineup from points already scored. Faint dotted lines = kickoffs.")) +
    theme_sleeper(legend = "bottom")

  p <- (p_top / p_bot) + plot_layout(heights = c(1.1, 1), guides = "collect") & theme(legend.position = "bottom")
  if (!is.null(save_path)) ggsave(save_path, p, width = width, height = height, dpi = dpi, bg = FIG$bg, limitsize = FALSE)
  p
}

# --- Figure 2 ---------------------------------------------------------------------------------
# Projected final SCORE (points) of the chopped team -- the right-hand axis of the scorecard's second
# row. One shared range across weeks so the columns are comparable.
projection_points_limits <- function(season, weeks, step = 20) {
  v <- unlist(lapply(weeks, function(w) {
    t <- load_team_timeline(season, w)$teams
    t$projected_final[t$chopped]
  }))
  c(floor(min(v, na.rm = TRUE) / step) * step, ceiling(max(v, na.rm = TRUE) / step) * step)
}

PROJ_COLOR <- FIG$proj # the chopped team's projected score and its right-hand axis

scorecard_week <- function(season, week, proj_lims, show_y_titles = TRUE, show_right_title = TRUE) {
  tl <- load_team_timeline(season, week)
  n <- n_distinct(tl$teams$roster_id)
  ax <- time_axis(tl$teams, tl$games, min_tick_gap = 230)
  ax_top <- time_axis(tl$teams, tl$games, day_labels = TRUE, short_days = TRUE, min_tick_gap = 230)
  chopped_name <- tl$teams$team_name[tl$teams$chopped][1]
  chopped_final <- tl$teams$final_team_points[tl$teams$chopped][1]

  # each team's points so far as a share of ITS OWN final (modelled) total, so a team that ends on 70 and one that
  # ends on 170 are both 0 -> 100%; the trend is then across teams, not across point totals
  t <- tl$teams |>
    group_by(roster_id) |> mutate(final_modeled = current_score[which.max(x_min)]) |> ungroup() |>
    mutate(pct_total = if_else(final_modeled > 0, current_score / final_modeled, NA_real_))

  per_time <- t |>
    group_by(x_min) |>
    summarise(
      p_chopped = elimination_prob[chopped],
      rank_chopped = rank(-elimination_prob, ties.method = "min")[chopped],
      # brier = mean((elimination_prob - chopped)^2),            # Brier score: shelved for now (see the commented panel below)
      # brier_base = mean((1 / n - chopped)^2),
      pct_med = median(pct_total, na.rm = TRUE), pct_q25 = quantile(pct_total, 0.25, na.rm = TRUE),
      pct_q75 = quantile(pct_total, 0.75, na.rm = TRUE), pct_lo = min(pct_total, na.rm = TRUE), pct_hi = max(pct_total, na.rm = TRUE),
      proj_chopped = projected_final[chopped],
      .groups = "drop"
    )
  yl <- function(txt) if (show_y_titles) txt else NULL

  p_pts <- ggplot(per_time, aes(x_min)) + ax_top +
    geom_ribbon(aes(ymin = pct_lo, ymax = pct_hi), fill = FIG$muted, alpha = FIG$rb_outer) +
    geom_ribbon(aes(ymin = pct_q25, ymax = pct_q75), fill = FIG$muted, alpha = FIG$rb_inner) +
    geom_line(aes(y = pct_med), color = FIG$ink, linewidth = 0.8) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1), expand = expansion(mult = c(0, 0.02))) +
    labs(x = NULL, y = yl("% of final points\nscored so far"),
         title = sprintf("Week %d  (%d teams)", week, n)) +
    theme_sleeper() + theme(axis.text.x = element_blank())

  # second row: the probability on the left axis (red, matching its axis), and -- on a right axis in points, rescaled
  # onto the same panel -- the chopped team's own projected final score (teal)
  lo <- proj_lims[1]; hi <- proj_lims[2]; sc <- function(x) (x - lo) / (hi - lo)
  s_prob <- "Probability given to the chopped team (left axis)"
  s_team <- "Chopped team's projected final score (right axis)"
  lines_df <- per_time |>
    transmute(x_min, !!s_prob := p_chopped, !!s_team := sc(proj_chopped)) |>
    pivot_longer(-x_min, names_to = "series", values_to = "y") |>
    mutate(series = factor(series, levels = c(s_team, s_prob))) # probability drawn on top
  p_prob <- ggplot(lines_df, aes(x_min, y, color = series, linewidth = series)) + ax +
    geom_line() +
    scale_color_manual(values = setNames(c(PROJ_COLOR, CHOPPED_COLOR), levels(lines_df$series)), name = NULL, breaks = c(s_prob, s_team)) +
    scale_linewidth_manual(values = setNames(c(0.9, 1.2), levels(lines_df$series)), name = NULL, breaks = c(s_prob, s_team)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1), expand = expansion(mult = c(0.01, 0.02)),
                       sec.axis = sec_axis(~ . * (hi - lo) + lo, breaks = seq(lo, hi, by = 20),
                                           name = if (show_right_title) "Projected final\nscore (points)" else NULL)) +
    guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
    labs(x = NULL, y = yl("Probability given to\nthe chopped team"),
         title = sprintf("Chopped: %s (final %.1f)", chopped_name, chopped_final)) +
    theme_sleeper(legend = "bottom") + theme(axis.text.x = element_blank(),
                                           plot.title = element_text(color = CHOPPED_COLOR, face = "bold", size = rel(0.95)),
                                           axis.title.y.left = element_text(color = CHOPPED_COLOR), axis.text.y.left = element_text(color = CHOPPED_COLOR),
                                           axis.title.y.right = element_text(color = PROJ_COLOR), axis.text.y.right = element_text(color = PROJ_COLOR))

  # Brier score panel -- shelved for now (interesting, but not an easy one to explain to an audience). To restore: uncomment
  # this, the brier columns in per_time above, add p_brier into the list returned below, and set nrow = 4 / heights in
  # plot_team_scorecard().
  # p_brier <- ggplot(per_time, aes(x_min)) + ax +
  #   geom_line(aes(y = brier_base), linetype = "dashed", color = "grey45", linewidth = 0.5) +
  #   geom_line(aes(y = brier), color = "#1B9E77", linewidth = 1.1) +
  #   scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  #   labs(x = NULL, y = yl("Brier score\n(lower is better)")) +
  #   theme_sleeper() + theme(axis.text.x = element_blank())

  p_rank <- ggplot(per_time, aes(x_min, rank_chopped)) + ax +
    geom_step(color = FIG$ink, linewidth = 1.1) +
    scale_y_reverse(breaks = c(1, 5, 10, 15, 18), limits = c(18.5, 0.8)) +
    labs(x = "Real clock time (ET)", y = yl("Rank of chopped\nteam (1 = top pick)")) +
    theme_sleeper()

  list(p_pts, p_prob, p_rank)
}

plot_team_scorecard <- function(season, weeks, save_path = NULL, width = 17, height = 10.5, dpi = 150, per_row = 3) {
  lims <- projection_points_limits(season, weeks)
  # one column per week, `per_row` weeks to a row of panels; further weeks wrap onto another block below (the last block is padded so its columns keep their width)
  chunks <- split(seq_along(weeks), ceiling(seq_along(weeks) / per_row))
  blocks <- lapply(chunks, function(ix) {
    panels <- unlist(lapply(seq_along(ix), function(j)
      scorecard_week(season, weeks[ix[j]], lims, show_y_titles = (j == 1), show_right_title = (j == length(ix)))), recursive = FALSE)
    if (length(chunks) > 1 && length(ix) < per_row) panels <- c(panels, rep(list(patchwork::plot_spacer()), 3 * (per_row - length(ix))))
    # 3 rows x one column per week, filled column by column
    wrap_plots(panels, nrow = 3, byrow = FALSE) + plot_layout(heights = c(0.8, 1.2, 0.8), guides = "collect")
  })
  body <- if (length(blocks) == 1) blocks[[1]] else wrap_plots(blocks, ncol = 1) + plot_layout(guides = "collect")
  p <- (body +
    plot_annotation(
      title = sprintf("%s: did the app see the chop coming? (team elimination probabilities, real kickoff times)", season),
      subtitle = paste0("Row 1: share of each team's final points scored so far (line = median team; bands = middle half and full range).\nRow 2: ", FIG$w_chop, " = the probability the app gave the team that was actually chopped (left axis); ",
                        FIG$w_proj, " = that team's projected final score (right axis).\n",
                        "Row 3: where the app ranked the chopped team by probability (1 = its top pick for the chop)."),
      caption = paste0("Only ", length(weeks), " chops, so this is a behavior check, not a calibration test. ",
                       "Grey bands = stretches with no games on (cut out of the time axis); dotted verticals = kickoffs."),
      theme = annot_theme())) &
    theme(legend.position = "bottom")
  if (!is.null(save_path)) ggsave(save_path, p, width = width, height = height, dpi = dpi, bg = FIG$bg, limitsize = FALSE)
  p
}

# --- The simple version of Figure 1: "did the tracker see it coming?" ---------------------------------------------
# Just the bottom row of the scorecard: where the tracker ranked the team that was actually chopped, over the week (1 = its top pick for the chop).
# A line along the top means it saw the chop coming; a line that dives means it did not. One small panel per week, `per_row` panels to a row.
rank_week <- function(season, week, n_max, show_y_title = TRUE) {
  tl <- load_team_timeline(season, week)
  ax <- time_axis(tl$teams, tl$games, min_tick_gap = 230, day_in_ticks = TRUE)   # weekday under the time labels, so nothing sits on the line
  d <- tl$teams |> group_by(x_min) |> summarise(rank_chopped = rank(-elimination_prob, ties.method = "min")[chopped], .groups = "drop")
  top_share <- mean(d$rank_chopped == 1)
  ggplot(d, aes(x_min, rank_chopped)) + ax +
    geom_hline(yintercept = 1, linetype = "dotted", color = FIG$muted, linewidth = 0.4) +
    geom_step(color = CHOPPED_COLOR, linewidth = 1.2) +
    scale_y_reverse(breaks = c(1, 5, 10, 15)[c(1, 5, 10, 15) <= n_max], limits = c(n_max + 0.5, 0.7)) +
    labs(x = NULL, y = if (show_y_title) "Tracker's rank for\nthe chopped team\n(1 = top pick)" else NULL,
         title = sprintf("Week %d", week), subtitle = sprintf("Top pick for %s of game time", scales::percent(top_share, accuracy = 1))) +
    theme_sleeper() +
    theme(plot.subtitle = element_text(color = CHOPPED_COLOR, face = "bold", size = rel(0.9)), axis.title.y = element_text(size = rel(0.85)))
}

plot_chop_rank <- function(season, weeks, save_path = NULL, per_row = 3, panel_w = 3.7, panel_h = 2.6, dpi = 150) {
  n_max <- max(vapply(weeks, function(w) n_distinct(load_team_timeline(season, w)$teams$roster_id), numeric(1)))
  panels <- lapply(seq_along(weeks), function(j) rank_week(season, weeks[j], n_max, show_y_title = ((j - 1) %% per_row == 0)))
  nrow_ <- ceiling(length(weeks) / per_row)
  if (length(panels) %% per_row) panels <- c(panels, rep(list(ggplot() + theme_void() + theme(plot.background = element_rect(fill = FIG$bg, color = NA))), per_row - length(panels) %% per_row))
  p <- wrap_plots(panels, ncol = per_row) +
    plot_annotation(caption = wrap_txt("Rank 1 = the tracker's top pick for the chop. A line along the top means it saw the chop coming.\nGrey bands = stretches with no games on (cut out of the time axis).", "annot"), theme = annot_theme())
  if (!is.null(save_path)) ggsave(save_path, p, width = panel_w * per_row + 0.4, height = panel_h * nrow_ + 0.7, dpi = dpi, bg = FIG$bg, limitsize = FALSE)
  p
}

# --- Figure 3 ----------------------------------------------------------------------------------
# The four lowest-scoring teams each week, one column per team. The probability can go on its own short strip
# underneath each team's panel (plain 0-100% axis; show_prob = TRUE), or be left out (show_prob = FALSE) since
# Figures 1 and 2 already show it.
STATUS_COLORS <- c(Chopped = CHOPPED_COLOR, Survived = FIG$survive)

# the n_low lowest finishers of a week (by Sleeper's actual final score), with a panel label and a status
lowest_teams <- function(t, n_low = 4) {
  low <- t |> distinct(roster_id, team_name, final_team_points, chopped) |> arrange(final_team_points) |> slice_head(n = n_low)
  lab <- wrap_txt(sprintf("%s%s\nactual final %.1f", if_else(low$chopped, "CHOPPED: ", ""), low$team_name, low$final_team_points), keep_nl = TRUE)
  low |> mutate(panel = factor(lab, levels = lab), status = if_else(chopped, "Chopped", "Survived"))
}

# the elimination probability, on its own short strip under a team's panel (no dual axis, no overlap)
prob_strip <- function(d, ax, show_y_title = TRUE) {
  ggplot(d, aes(x_min, elimination_prob)) + ax +
    geom_area(fill = FIG$muted, alpha = FIG$area_alpha) +
    geom_line(color = FIG$ink, linewidth = 0.7) +
    scale_y_continuous(labels = scales::percent, breaks = c(0, 0.5, 1), limits = c(0, 1), expand = expansion(mult = c(0, 0.04))) +
    facet_wrap(~panel, nrow = 1) +
    labs(x = NULL, y = if (show_y_title) "Probability\nchopped" else NULL) +
    theme_sleeper() + theme(strip.text = element_blank(), strip.background = element_blank(),
                          axis.text.x = element_text(size = rel(0.85)),
                          panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
}

# a team's panel row = score panel (tall) over probability strip (short)
stack_with_strip <- function(p_main, p_strip) p_main / p_strip + plot_layout(heights = c(3.2, 1))

# --- Figure 3: points ---
lowest_week <- function(season, week, n_low = 4, show_y_title = TRUE, show_prob = TRUE) {
  tl <- load_team_timeline(season, week)
  low <- lowest_teams(tl$teams, n_low)
  d <- tl$teams |> inner_join(low |> select(roster_id, panel, status), by = "roster_id") |>
    mutate(lo90 = pmax(projected_final - 1.645 * projected_sd, 0), hi90 = projected_final + 1.645 * projected_sd,
           lo50 = pmax(projected_final - 0.6745 * projected_sd, 0), hi50 = projected_final + 0.6745 * projected_sd)
  ymax <- max(d$hi90, low$final_team_points) * 1.04
  finals <- low |> select(panel, final_team_points)
  ax <- time_axis(tl$teams, tl$games, min_tick_gap = 230, kickoff_lines = FALSE, day_in_ticks = TRUE)

  p_main <- ggplot(d, aes(x_min)) + ax +
    geom_ribbon(aes(ymin = lo90, ymax = hi90, fill = status), alpha = FIG$a90) +
    geom_ribbon(aes(ymin = lo50, ymax = hi50, fill = status), alpha = FIG$a50) +
    geom_line(aes(y = projected_final, color = status), linewidth = 0.9) +
    geom_step(aes(y = current_score), color = FIG$ink, linewidth = 0.7) +
    geom_hline(data = finals, aes(yintercept = final_team_points), color = FIG$ink, linewidth = 0.5, linetype = "dashed") +
    scale_fill_manual(values = STATUS_COLORS, guide = "none") +
    scale_color_manual(values = STATUS_COLORS, guide = "none") +
    coord_cartesian(ylim = c(0, ymax)) +
    { if (isTRUE(getOption("sleeper.stack"))) facet_wrap(~panel, ncol = 1) else facet_wrap(~panel, nrow = 1) } +   # phone layout: one team panel per row
    labs(x = NULL, y = if (show_y_title) sprintf("Week %d\nteam points", week) else NULL) +
    theme_sleeper() + theme(axis.text.x = if (show_prob) element_blank() else element_text(size = rel(0.75)),
                          panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
  if (show_prob) stack_with_strip(p_main, prob_strip(d, ax, show_y_title)) else p_main
}

# One team's week on its own: points scored so far, the projected final with 50% / 90% ranges, and the
# actual final -- the same picture as one panel of plot_team_lowest_finishers(), for ANY team. The weekly
# recap page (collector/recap_lib.R) shows one of these per team behind a drop-down. Smaller than the
# multi-week figures (a recap week carries one image per team), so it is drawn at a modest size.
plot_team_single <- function(season, week, roster_id, save_path = NULL, width = 7, height = 3.3, dpi = 110) {
  tl <- load_team_timeline(season, week)
  rid <- roster_id
  d <- tl$teams |> filter(roster_id == rid) |>
    mutate(lo90 = pmax(projected_final - 1.645 * projected_sd, 0), hi90 = projected_final + 1.645 * projected_sd,
           lo50 = pmax(projected_final - 0.6745 * projected_sd, 0), hi50 = projected_final + 0.6745 * projected_sd)
  if (!nrow(d)) stop("no timeline for roster ", rid, " in week ", week)
  info <- d |> distinct(team_name, final_team_points, chopped)
  # Colored by how the week went against the team's PRE-GAME projection (its projected final at the first
  # snapshot): green = finished at or above it, red = finished below it. (Not by chopped/survived, which the
  # title already says -- a team can be chopped and still beat a low projection, or survive while underperforming.)
  pregame <- d$pregame_projected[which.min(d$x_min)]
  diff_final <- info$final_team_points - pregame
  status <- if (diff_final >= 0) "Over" else "Under"
  d$status <- status
  cols <- c(Under = CHOPPED_COLOR, Over = if (identical(FIG$mode, "light")) "#1B9E77" else "#4CD787")
  ymax <- max(d$hi90, info$final_team_points) * 1.04
  ax <- time_axis(tl$teams, tl$games, min_tick_gap = 230, kickoff_lines = FALSE, day_in_ticks = TRUE)

  p <- ggplot(d, aes(x_min)) + ax +
    geom_ribbon(aes(ymin = lo90, ymax = hi90, fill = status), alpha = FIG$a90) +
    geom_ribbon(aes(ymin = lo50, ymax = hi50, fill = status), alpha = FIG$a50) +
    geom_line(aes(y = projected_final, color = status), linewidth = 0.9) +
    geom_step(aes(y = current_score), color = FIG$ink, linewidth = 0.7) +
    geom_hline(yintercept = info$final_team_points, color = FIG$ink, linewidth = 0.5, linetype = "dashed") +
    scale_fill_manual(values = cols, guide = "none") +
    scale_color_manual(values = cols, guide = "none") +
    coord_cartesian(ylim = c(0, ymax)) +
    labs(x = NULL, y = "Team points",
         title = sprintf("%s%s", if (isTRUE(info$chopped)) "CHOPPED: " else "", info$team_name),
         subtitle = wrap_txt(sprintf("Week %d: projected %.1f before the week, finished %.1f (%+.1f)", week, pregame, info$final_team_points, diff_final))) +
    theme_sleeper() + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
                            axis.text.x = element_text(size = rel(0.75)))
  if (!is.null(save_path)) ggsave(save_path, p, width = width, height = height, dpi = dpi, bg = FIG$bg, limitsize = FALSE)
  p
}

plot_team_lowest_finishers <- function(season, weeks, n_low = 4, show_prob = TRUE, save_path = NULL, width = 16,
                                       height = (if (show_prob) 4.7 else 3.6) * length(weeks) + 1.3, dpi = 150) {
  rows <- lapply(weeks, function(w) lowest_week(season, w, n_low, show_prob = show_prob))
  p <- wrap_plots(rows, ncol = 1) +
    plot_annotation(
      title = wrap_txt(sprintf("%s: the %d lowest-scoring teams each week", season, n_low), "annot"),
      subtitle = wrap_txt(paste0("Solid ", FIG$w_ink, " line = points scored so far; colored line = projected final\nwith 50% / 90% ranges; dashed ", FIG$w_final, " = the team's actual final score.",
                        if (show_prob) "\nShort strip underneath: the app's probability that the team is chopped (0-100%)." else ""), "annot"),
      caption = wrap_txt("Real kickoff times (US Eastern); grey bands = stretches with no games on (cut out of the time axis).", "annot"),
      theme = annot_theme())
  if (!is.null(save_path)) ggsave(save_path, p, width = width, height = height, dpi = dpi, bg = FIG$bg, limitsize = FALSE)
  p
}

# --- Figure 6: every team, projected against finished (slope chart) ---------------------------------
# `danger` = team(s) that saved themselves (began in danger), `close` = close calls, `beat` = teams that massively outperformed their projection.
pvf_data <- function(season, week, danger = character(0), close = character(0), beat = character(0)) {
  tl <- load_team_timeline(season, week)
  d <- tl$teams |> arrange(x_min) |> group_by(roster_id, team_name) |>
    summarise(proj = first(projected_final), final = first(final_team_points), chopped = any(chopped), .groups = "drop") |>
    arrange(final)
  lv <- c("Chopped", "Saved themselves", "Close call", "Massively outperformed", "Other teams")
  d <- d |> mutate(status = factor(case_when(chopped ~ lv[1], team_name %in% danger ~ lv[2], team_name %in% close ~ lv[3], team_name %in% beat ~ lv[4], TRUE ~ lv[5]), levels = lv),
                   team_f = factor(team_name, levels = team_name))     # lowest score at the bottom
  list(d = d, n = nrow(d), cut = d$final[d$chopped][1], lv = lv, cols = setNames(c(FIG$chop, FIG$survive, FIG$contenders[3], "#B58CFF", FIG$contenders[2]), lv))
}

# Slope chart: points on the y axis, two categorical columns (pregame projection, final score), one line per team,
# the team's name to the right of its final score. Labels are nudged apart so close finishes stay readable.
spread_labels <- function(y, gap) {          # y: label heights; returns heights at least `gap` apart, as close to y as possible
  o <- order(y); pos <- y[o]
  for (it in 1:500) {
    moved <- FALSE
    for (i in seq_len(length(pos) - 1)) if (pos[i + 1] - pos[i] < gap - 1e-9) {
      m <- (pos[i] + pos[i + 1]) / 2; pos[i] <- m - gap / 2; pos[i + 1] <- m + gap / 2; moved <- TRUE
    }
    if (!moved) break
  }
  out <- numeric(length(y)); out[o] <- pos; out
}

plot_team_projected_vs_final_slope <- function(season, week, danger = character(0), close = character(0), beat = character(0), save_path = NULL, width = 9, height = NULL, dpi = 150) {
  pv <- pvf_data(season, week, danger, close, beat); d <- pv$d; n <- pv$n; cut <- pv$cut; cols <- pv$cols
  rng <- range(c(d$proj, d$final))
  d$label_y <- spread_labels(d$final, gap = 0.052 * diff(rng))
  d$lab <- sprintf("%s  %.1f", d$team_name, d$final)
  d$bold <- d$status != "Other teams"
  long <- bind_rows(transmute(d, team_name, status, x = 1, y = proj), transmute(d, team_name, status, x = 2, y = final))
  p <- ggplot() +
    geom_line(data = long, aes(x, y, group = team_name, color = status, linewidth = status != "Other teams", alpha = status != "Other teams")) +
    geom_point(data = long, aes(x, y, color = status), size = 3.4) +
    geom_segment(data = d, aes(x = 2, xend = 2.1, y = final, yend = label_y, color = status), linewidth = 0.4, alpha = 0.7) +
    geom_text(data = d, aes(x = 2.13, y = label_y, label = lab, color = status, fontface = ifelse(bold, "bold", "plain")), hjust = 0, size = 4.4, show.legend = FALSE) +
    scale_color_manual(values = cols, name = NULL, drop = TRUE) +   # a category with no team in it is left out of the legend
    scale_linewidth_manual(values = c(`TRUE` = 1.6, `FALSE` = 0.9), guide = "none") +
    scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.65), guide = "none") +
    scale_x_continuous(breaks = c(1, 2), labels = c("Pregame projection", "Final score"), limits = c(0.6, 3.55), expand = expansion(0)) +
    scale_y_continuous(expand = expansion(mult = c(0.03, 0.04))) +
    labs(x = NULL, y = "Points",
         title = sprintf("Week %d, %s: projected before the week, and where each team finished", week, season),
         subtitle = "Each line is one team, from its pregame projected score to its final score.\nA line going up beat its projection.",
         caption = "Projected = the tracker's pregame projected score for the team's best lineup.") +
    guides(color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(linewidth = 1.6))) +
    theme_sleeper(legend = "bottom") + theme(panel.grid.major.x = element_blank(), axis.text.x = element_text(size = rel(1.05), color = FIG$text),
                                           plot.title.position = "plot", plot.caption.position = "plot")
  if (is.null(height)) height <- 0.3 * n + 3.4
  if (!is.null(save_path)) ggsave(save_path, p, width = width, height = height, dpi = dpi, bg = FIG$bg, limitsize = FALSE)
  p
}

# Every figure to disk in one go.
plot_team_elimination_all <- function(season, weeks, out_dir = ".") {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  for (w in weeks) plot_team_race(season, w, save_path = file.path(out_dir, sprintf("team_elim_1_race_w%d.png", w)))
  plot_team_scorecard(season, weeks, save_path = file.path(out_dir, "team_elim_2_scorecard.png"))
  plot_team_lowest_finishers(season, weeks, save_path = file.path(out_dir, "team_elim_3_lowest_finishers.png"))
  invisible(out_dir)
}
