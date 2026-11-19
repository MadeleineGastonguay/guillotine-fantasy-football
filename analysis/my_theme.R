# my_theme.R
# House ggplot2 theme.
# Source this at the top of any analysis script:
#   source("my_theme.R")
# Requires: ggplot2
#
# Design intent:
#   - Built on theme_minimal(): clean, low-ink, presentation- and print-ready.
#   - Palette and facet conventions (Dark2 for categorical, viridis for
#     continuous/ordered, facet_wrap scale defaults) are documented in
#     SKILL.md and references/plot_templates.md rather than wrapped in
#     helper functions — they're one-line calls to base ggplot2/RColorBrewer/
#     viridis, so a wrapper just adds a name to remember without adding
#     behavior. This file holds theme_house(), which is genuinely doing
#     something (a dozen+ styling decisions in one call) and is worth
#     naming.

library(ggplot2)

#' House theme, built on theme_minimal()
#'
#' @param base_size base font size (pt); scales all text proportionally
#' @param legend legend position: "right" (default), "bottom", or "none"
theme_house <- function(base_size = 11, legend = "right") {
  theme_minimal(base_size = base_size) +
    theme(
      # Panel / gridlines: keep major gridlines faint, drop minor entirely —
      # minor gridlines rarely carry information and just add visual noise.
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey88", linewidth = 0.3),

      # Axis
      axis.title = element_text(size = rel(1.0), face = "plain"),
      axis.text = element_text(size = rel(0.9), color = "grey20"),
      axis.line = element_line(color = "grey40", linewidth = 0.3),

      # Legend
      legend.position = legend,
      legend.title = element_text(size = rel(0.95), face = "plain"),
      legend.text = element_text(size = rel(0.85)),
      legend.key = element_blank(),

      # Facets (facet_wrap / facet_grid) — this is the block that matters
      # most given how often facet_wrap gets used here. Strip text should
      # be readable at a glance across many small panels without dominating
      # the panel itself.
      strip.text = element_text(size = rel(0.9), face = "bold", color = "grey20"),
      strip.background = element_rect(fill = "grey95", color = NA),
      panel.spacing = unit(1, "lines"),

      # Title / caption
      plot.title = element_text(size = rel(1.15), face = "bold"),
      plot.subtitle = element_text(size = rel(1.0), color = "grey30"),
      plot.caption = element_text(size = rel(0.75), color = "grey50"),

      plot.margin = margin(10, 10, 10, 10)
    )
}
