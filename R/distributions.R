# --- Distribution visuals: helpers for the team ridgeline / range bars and the
# player pre-game-vs-current charts and sparklines -------------------------
#
# Two different curve constructions, on purpose (see the plan/discussion):
#   * PLAYERS use the same empirical noise shape the simulation uses (the
#     standardized per-position residual pool from get_position_shape_pool()),
#     evaluated exactly as a density -- a single player's score is boom/bust
#     (RB/WR/TE residual skew ~1.2-1.4), so a normal curve would visibly put
#     probability on negative points and look symmetric when it isn't.
#   * TEAMS use the simulation's own draws on the Simulations tab (exactly
#     what produced the elimination probabilities) and a normal curve on the
#     Approximate tab (exactly what its closed form assumes). After summing 9
#     players the skew is only ~0.1, so normal is a fair stand-in there.

library(ggplot2)

DIST_COLORS <- list(
  bg = "#05091D", card = "#131B38", border = "#25345D", teal = "#00FFF9",
  text = "#F4F6FB", muted = "#98B3D6", danger = "#FF6482", up = "#4CD787"
)

dist_trend_color <- function(trend) {
  unname(c(up = DIST_COLORS$up, down = DIST_COLORS$danger, even = DIST_COLORS$teal)[trend])
}

# --- Player curves -----------------------------------------------------------

.dist_cache <- new.env()

# One density function per position for the STANDARDIZED residual shape
# (mean 0, sd 1). Built once per R process from the same pool the simulation
# resamples; a smoothed KDE of ~400-1500 points per position.
position_shape_density <- function(position) {
  if (is.null(.dist_cache$shape)) {
    pool <- get_position_shape_pool()
    pool[["ALL"]] <- unlist(pool, use.names = FALSE)
    # Where 99.5% of each position's standardized shape lives -- used to size a
    # player's x-axis to the region that actually holds probability (see
    # player_x_range()), instead of a fixed number of SDs that stretches the
    # axis into a nearly-empty left tail.
    .dist_cache$bounds <- lapply(pool, function(z) stats::quantile(z, c(0.0025, 0.9975), names = FALSE))
    .dist_cache$shape <- lapply(pool, function(z) {
      d <- stats::density(z, n = 1024, cut = 0.5)
      stats::approxfun(d$x, d$y, yleft = 0, yright = 0)
    })
  }
  g <- .dist_cache$shape[[position]]
  if (is.null(g)) .dist_cache$shape[["ALL"]] else g
}

# Density of a player's total on `x`: location `mean`, scale `sd`, empirical
# shape. sd == 0 (finished game) has no density -- callers draw a spike.
player_density <- function(x, mean, sd, position) {
  position_shape_density(position)((x - mean) / sd) / sd
}

# x-range holding the central 99.5% of a player's distribution (from the
# position's own empirical shape), never below the simulation's floor. The
# model itself is unchanged -- a small (~1%) leak of probability below zero
# remains in the curve; this just stops it from dictating the axis.
player_x_range <- function(mean, sd, position) {
  position_shape_density(position) # ensures the cache (incl. bounds) exists
  b <- .dist_cache$bounds[[position]] %||% .dist_cache$bounds[["ALL"]]
  c(max(PLAYER_FLOOR, mean + sd * b[1]), mean + sd * b[2])
}

# Pre-game vs current curves for ONE player, on a shared grid.
#   pre:  mean = full_week_projection, sd = pregame_sd_underdog (full-game SD)
#   cur:  mean = locked + remaining_mean, sd = remaining_sd_underdog
player_curve_data <- function(pre_mean, pre_sd, cur_mean, cur_sd, position, n = 200) {
  ranges <- list(player_x_range(pre_mean, pre_sd, position))
  if (cur_sd > 0.05) ranges[[2]] <- player_x_range(cur_mean, cur_sd, position) else ranges[[2]] <- c(cur_mean, cur_mean)
  lo <- max(PLAYER_FLOOR, min(unlist(ranges), cur_mean - 1))
  hi <- max(unlist(ranges), cur_mean + 1)
  x <- seq(lo, hi, length.out = n)
  list(
    x = x,
    pre = if (pre_sd > 0.05) player_density(x, pre_mean, pre_sd, position) else rep(0, n),
    cur = if (cur_sd > 0.05) player_density(x, cur_mean, cur_sd, position) else NULL, # NULL = finished (spike)
    cur_final = cur_sd <= 0.05
  )
}

# --- Inline SVG cells for DT tables -----------------------------------------

svg_cell <- function(w, h, inner, title = NULL) {
  sprintf('<svg width="%d" height="%d" viewBox="0 0 %d %d" style="display:block;">%s%s</svg>',
          w, h, w, h, if (is.null(title)) "" else sprintf("<title>%s</title>", title), inner)
}

# Tiny muted axis label (kept small and only at the ends so the mini charts
# don't get busy).
svg_label <- function(x, y, text, anchor = "start") {
  sprintf('<text x="%s" y="%s" font-size="9" fill="%s" text-anchor="%s" font-family="Inter, sans-serif">%s</text>',
          x, y, DIST_COLORS$muted, anchor, text)
}

# Range bar for one team: thin line = middle 90%, thick bar = middle 50%,
# white tick = the table's projected final score, tiny numbers under the ends
# of the 90% line. `xmin`/`xmax` are SHARED across every team in the table so
# bars are comparable row to row.
svg_range_bar <- function(p5, p25, p75, p95, marker, xmin, xmax, w = 140, h = 32) {
  # 10px of side margin so a number centered on either end of the bar isn't clipped.
  sx <- function(v) round(10 + (v - xmin) / (xmax - xmin) * (w - 20), 1)
  cy <- 9
  labels <- if (sx(p95) - sx(p5) < 24) {
    # narrow bar: the two end labels would collide, so show "low-high" centered under it
    svg_label(min(max(sx((p5 + p95) / 2), 16), w - 16), h - 3,
              if (round(p5) == round(p95)) round(p5) else paste0(round(p5), "-", round(p95)), "middle")
  } else {
    # Each number is CENTERED on its end of the bar (not left/right-aligned
    # to it), so it sits directly under the point it labels.
    paste0(svg_label(sx(p5), h - 3, round(p5), "middle"),
           svg_label(sx(p95), h - 3, round(p95), "middle"))
  }
  inner <- paste0(
    sprintf('<line x1="%s" x2="%s" y1="%d" y2="%d" stroke="%s" stroke-width="2" stroke-linecap="round"/>',
            sx(p5), sx(p95), cy, cy, DIST_COLORS$muted),
    sprintf('<rect x="%s" y="%d" width="%s" height="8" rx="2" fill="%s" fill-opacity="0.85"/>',
            sx(p25), cy - 4, max(sx(p75) - sx(p25), 1), DIST_COLORS$danger),
    sprintf('<line x1="%s" x2="%s" y1="1" y2="%d" stroke="%s" stroke-width="2"/>',
            sx(marker), sx(marker), cy + 8, DIST_COLORS$text),
    labels
  )
  svg_cell(w, h, inner,
           sprintf("Middle 50%%: %.0f-%.0f | Middle 90%%: %.0f-%.0f | Projected: %.1f", p25, p75, p5, p95, marker))
}

# Sparkline for one player: pre-game outline (muted) behind the current
# distribution (filled, trend-colored). Own x-scale per row. A finished
# player has no spread, so their current score is a vertical line.
svg_sparkline <- function(pre_mean, pre_sd, cur_mean, cur_sd, position, trend, w = 120, h = 40) {
  d <- player_curve_data(pre_mean, pre_sd, cur_mean, cur_sd, position, n = 60)
  x <- d$x
  ymax <- max(d$pre, d$cur, 1e-9)
  sx <- function(v) round(1 + (v - min(x)) / (max(x) - min(x)) * (w - 2), 1)
  base_y <- h - 11 # curves sit above a strip reserved for the two end labels
  sy <- function(v) round(base_y - v / ymax * (base_y - 4), 1)
  poly <- function(y) paste(sprintf("%s,%s", sx(x), sy(y)), collapse = " ")
  col <- dist_trend_color(trend)
  inner <- sprintf('<polyline points="%s" fill="none" stroke="%s" stroke-width="1.2" stroke-dasharray="3 2"/>',
                   poly(d$pre), DIST_COLORS$muted)
  if (d$cur_final) {
    inner <- paste0(inner, sprintf('<line x1="%s" x2="%s" y1="2" y2="%s" stroke="%s" stroke-width="2.5"/>',
                                   sx(cur_mean), sx(cur_mean), base_y, col))
  } else {
    pts <- paste0(sprintf("%s,%s ", sx(x[1]), base_y), poly(d$cur), sprintf(" %s,%s", sx(x[length(x)]), base_y))
    inner <- paste0(inner, sprintf('<polygon points="%s" fill="%s" fill-opacity="0.5" stroke="%s" stroke-width="1.2"/>',
                                   pts, col, col))
  }
  inner <- paste0(inner,
                  svg_label(1, h - 1, round(min(x)), "start"),
                  svg_label(w - 1, h - 1, round(max(x)), "end"))
  svg_cell(w, h, inner,
           sprintf("Dashed: pre-game (%.1f). Filled: now (%.1f).", pre_mean, cur_mean))
}

# --- ggplot charts -----------------------------------------------------------

dist_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(fill = DIST_COLORS$bg, color = NA),
      panel.background = element_rect(fill = DIST_COLORS$bg, color = NA),
      panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = DIST_COLORS$border, linewidth = 0.3),
      text = element_text(color = DIST_COLORS$muted),
      axis.text = element_text(color = DIST_COLORS$muted),
      axis.text.y = element_text(color = DIST_COLORS$text),
      plot.title = element_text(color = DIST_COLORS$text, face = "bold", size = 14),
      plot.subtitle = element_text(color = DIST_COLORS$muted, size = 10.5),
      legend.position = "bottom", legend.title = element_blank(),
      legend.text = element_text(color = DIST_COLORS$muted),
      plot.margin = margin(8, 12, 8, 8)
    )
}

# Team curves on one shared grid.
#   teams: data.frame(roster_id, team_name, mean (projected final), sd, elim)
#   draws: NULL (normal curves from mean/sd) or n_sims x n_teams matrix with
#          colnames = roster_id (empirical density of the simulated totals)
team_curve_data <- function(teams, draws = NULL, n = 256) {
  spread <- if (is.null(draws)) teams$sd else apply(draws[, as.character(teams$roster_id), drop = FALSE], 2, sd)
  spread <- pmax(spread, 0.3) # finished teams (no variation left) get a narrow but drawable bump
  centers <- if (is.null(draws)) teams$mean else colMeans(draws[, as.character(teams$roster_id), drop = FALSE])
  lo <- min(centers - 3.2 * spread, teams$mean - 1)
  hi <- max(centers + 3.2 * spread, teams$mean + 1)
  x <- seq(lo, hi, length.out = n)
  dens <- lapply(seq_len(nrow(teams)), function(i) {
    if (is.null(draws)) {
      stats::dnorm(x, teams$mean[i], spread[i])
    } else {
      v <- draws[, as.character(teams$roster_id[i])]
      if (sd(v) < 0.3) stats::dnorm(x, mean(v), 0.3)
      else stats::density(v, from = lo, to = hi, n = n, adjust = 1.3)$y
    }
  })
  list(x = x, dens = dens)
}

# A single long word (no spaces) is forced onto two lines anyway: at a
# camelCase boundary (lower->Upper, e.g. "RobesPierre|Garcon") that balances
# the two lines if there is one, otherwise right in the middle. Short names are left alone.
break_long_word <- function(word, min_chars = 13) {
  n <- nchar(word)
  if (n < min_chars) return(word)
  starts <- as.integer(gregexpr("(?<=[a-z0-9])(?=[A-Z])", word, perl = TRUE)[[1]])
  starts <- starts[starts > 2 & starts < n - 2] # not a 1-2 letter orphan on either line
  # pick the boundary that leaves the longer line shortest
  cut <- if (length(starts)) starts[which.min(pmax(starts - 1, n - starts + 1))] else ceiling(n / 2)
  paste0(substr(word, 1, cut - 1 + (length(starts) == 0)), "\n", substr(word, cut + (length(starts) == 0), n))
}

# Team names split onto (at most) two balanced lines so they fit the narrow
# label column on a phone: the split point between words that makes the longer
# line as short as possible. One-word names stay on one line; a line still
# longer than `max_chars` is cut with an ellipsis.
wrap_team_label <- function(name, max_chars = 18) {
  words <- strsplit(trimws(name), "\\s+")[[1]]
  if (length(words) < 2) return(break_long_word(name))
  splits <- seq_len(length(words) - 1)
  longest <- vapply(splits, function(k) {
    max(nchar(paste(words[1:k], collapse = " ")), nchar(paste(words[-(1:k)], collapse = " ")))
  }, numeric(1))
  k <- splits[which.min(longest)]
  lines <- c(paste(words[1:k], collapse = " "), paste(words[-(1:k)], collapse = " "))
  lines <- ifelse(nchar(lines) > max_chars, paste0(substr(lines, 1, max_chars - 1), "\u2026"), lines)
  paste(lines, collapse = "\n")
}

# Team ridgeline: one stacked density per team, lowest projected score on top, shaded by elimination probability, white tick at the table's
# projected final score. Rows are the teams' own distributions of FINAL
# weekly score, so the left edge of the pile is where the guillotine falls.
ridgeline_plot <- function(teams, draws = NULL) {
  # Rows run from the lowest projected final score (top) to the highest
  # (bottom). Sorted here from whatever data is passed in, so the order
  # follows the numbers as they change through the week.
  teams <- teams[order(teams$mean, teams$roster_id), , drop = FALSE]
  cd <- team_curve_data(teams, draws)
  n <- nrow(teams)
  # Every curve is scaled to the SAME peak height (peak_h rows), not by its
  # density. Densities integrate to 1, so a team with lots of uncertainty left
  # has a low, wide curve and one whose games are nearly final has a tall
  # spike -- scaling by density left the wide ones almost flat and unreadable.
  # Equal peaks keep every shape legible; the WIDTH of a curve still shows
  # how uncertain that team is.
  peak_h <- 1.9
  label <- vapply(teams$team_name, wrap_team_label, character(1), USE.NAMES = FALSE)
  df <- do.call(rbind, lapply(seq_len(n), function(i) {
    data.frame(row = n - i + 1, team = label[i], x = cd$x, y = cd$dens[[i]] / max(cd$dens[[i]]) * peak_h, elim = teams$elim[i])
  }))
  df <- df[order(-df$row, df$x), ] # draw top row first so lower rows overlay it
  ticks <- data.frame(row = n - seq_len(n) + 1, x = teams$mean)
  ggplot() +
    lapply(unique(df$row)[order(-unique(df$row))], function(r) {
      d <- df[df$row == r, ]
      list(
        geom_ribbon(data = d, aes(x = x, ymin = row, ymax = row + y, fill = elim), color = NA, alpha = 0.92),
        geom_line(data = d, aes(x = x, y = row + y), color = DIST_COLORS$text, linewidth = 0.3)
      )
    }) +
    geom_segment(data = ticks, aes(x = x, xend = x, y = row, yend = row + 0.45),
                 color = DIST_COLORS$text, linewidth = 0.6) +
    scale_fill_gradient(low = "#2B3B6E", high = DIST_COLORS$danger, limits = c(0, max(0.05, max(teams$elim))),
                        guide = "none") +
    scale_y_continuous(breaks = n - seq_len(n) + 1, labels = label, expand = expansion(add = c(0.1, 1.6))) +
    scale_x_continuous(sec.axis = dup_axis(name = NULL)) + # numbers on top too, so upper rows are readable without hunting for the bottom axis
    labs(x = "Projected final score (points)", y = NULL) +
    dist_theme() +
    theme(axis.text.y = element_text(size = 9, lineheight = 0.85)) # two-line team names need a smaller font
}

# Player chart: pre-game vs current distribution of their weekly total.
#   row: one-row data.frame from roster_table with position, full_week_projection,
#        pregame_sd_underdog, locked_points, remaining_mean, remaining_sd_underdog,
#        full_name, matchup_label, nfl_team
player_dist_plot <- function(row) {
  pre_mean <- row$full_week_projection
  cur_mean <- row$locked_points + row$remaining_mean
  trend <- if (cur_mean > pre_mean + 1e-9) "up" else if (cur_mean < pre_mean - 1e-9) "down" else "even"
  col <- dist_trend_color(trend)
  d <- player_curve_data(pre_mean, row$pregame_sd_underdog, cur_mean, row$remaining_sd_underdog, row$position)

  long <- data.frame(x = d$x, y = d$pre, series = "Pre-game")
  if (!d$cur_final) long <- rbind(long, data.frame(x = d$x, y = d$cur, series = "Current"))
  long$series <- factor(long$series, levels = c("Pre-game", "Current"))
  ymax <- max(long$y)

  p <- ggplot(long, aes(x, y, fill = series, color = series)) +
    geom_area(position = "identity", alpha = 0.35, linewidth = 0.8) +
    geom_vline(xintercept = pre_mean, color = DIST_COLORS$muted, linetype = "dashed", linewidth = 0.5) +
    geom_vline(xintercept = cur_mean, color = col, linewidth = 0.7) +
    scale_fill_manual(values = c("Pre-game" = DIST_COLORS$muted, "Current" = col), drop = FALSE) +
    scale_color_manual(values = c("Pre-game" = DIST_COLORS$muted, "Current" = col), drop = FALSE) +
    scale_y_continuous(NULL, breaks = NULL, expand = expansion(mult = c(0, 0.1))) +
    labs(
      x = "Player's total points for the week",
      title = sprintf("%s (%s %s)", row$full_name, row$position, row$matchup_label),
      # Wrapped: the image is only as wide as the page (~360px on a phone) and ggplot clips long text.
      subtitle = paste(strwrap(if (d$cur_final) {
        sprintf("Game over: scored %.1f (pre-game projection was %.1f).", cur_mean, pre_mean)
      } else {
        sprintf("Pre-game %.1f (±%.1f)  →  now %.1f (±%.1f), incl. %.1f already scored.",
                pre_mean, row$pregame_sd_underdog, cur_mean, row$remaining_sd_underdog, row$locked_points)
      }, width = 46), collapse = "\n")
    ) +
    dist_theme()
  if (d$cur_final) {
    p <- p + annotate("segment", x = cur_mean, xend = cur_mean, y = 0, yend = ymax * 1.05, color = col, linewidth = 1.6) +
      annotate("text", x = cur_mean, y = ymax * 1.08, label = "Final", color = col, size = 3.5, vjust = 0)
  }
  p
}

# --- Range-bar column helpers (standings tables) ------------------------------
# q: n x 4 matrix of the 5th/25th/75th/95th percentiles; marker: projected
# final score per team. One x-scale for the whole column so rows compare.
range_cells_from_quantiles <- function(q, marker) {
  xmin <- min(q[, 1], marker) - 2
  xmax <- max(q[, 4], marker) + 2
  vapply(seq_len(nrow(q)), function(i) {
    svg_range_bar(q[i, 1], q[i, 2], q[i, 3], q[i, 4], marker[i], xmin, xmax)
  }, character(1))
}

# Approximate tab: normal quantiles (same assumption as its closed form).
range_cells_normal <- function(mean, sd) {
  sd <- pmax(sd, 0.1)
  q <- cbind(stats::qnorm(0.05, mean, sd), stats::qnorm(0.25, mean, sd),
             stats::qnorm(0.75, mean, sd), stats::qnorm(0.95, mean, sd))
  range_cells_from_quantiles(q, mean)
}

# Simulations tab: empirical quantiles of that team's simulated totals.
range_cells_draws <- function(roster_id, marker, draws) {
  q <- t(vapply(as.character(roster_id), function(r) {
    stats::quantile(draws[, r], c(0.05, 0.25, 0.75, 0.95), names = FALSE)
  }, numeric(4)))
  range_cells_from_quantiles(q, marker)
}

# --- Player sparkline column ---------------------------------------------------
# df: player rows with position, full_week_projection, pregame_sd_underdog,
#     remaining_sd_underdog; total = current projected total (locked +
#     remaining); trend = "up"/"down"/"even" (same rule as the table's color).
player_sparkline_cells <- function(df, total, trend) {
  vapply(seq_len(nrow(df)), function(i) {
    svg_sparkline(
      dplyr::coalesce(df$full_week_projection[i], 0), dplyr::coalesce(df$pregame_sd_underdog[i], 0),
      dplyr::coalesce(total[i], 0), dplyr::coalesce(df$remaining_sd_underdog[i], 0),
      df$position[i], trend[i]
    )
  }, character(1))
}
