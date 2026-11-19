# --- Figure look for the team-elimination figures: dark "Sleeper" (default) or light ---------------
# analysis/plot_team_elimination.R and analysis/plot_sim_check.R draw every color and alpha from FIG below, so
# one switch re-skins all the figures:
#   options(guillotine.fig_theme = "light")   # before sourcing the plot scripts; default is "dark"
# Dark uses the app's own tokens (app.R's :root and POSITION_COLORS) with the text, grid and "other teams" lines
# pushed brighter and the transparent bands made more opaque, so it reads clearly on the navy ground.
suppressMessages(library(ggplot2))

.fig_mode <- getOption("guillotine.fig_theme", "dark")

FIG <- if (identical(.fig_mode, "light")) {
  list(
    mode = "light", bg = "#FFFFFF", strip = "#F2F2F2", grid = "#E0E0E0", axis_line = "#8A8A8A",
    text = "#151A23", muted = "#566070", dim = "#808080", ink = "#222222",
    chop = "#D95F02", survive = "#1B9E77", proj = "#7570B3",
    contenders = c("#1B9E77", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"),
    approx = "#7570B3", sims = "#1B9E77",
    other = "#B8B8B8", other_lw = 0.35, other_alpha = 1, pts_other = "#8C8C8C", pts_other_alpha = 0.55,
    gap_fill = "#808080", gap_alpha = 0.12, kick = "#999999", kick_alpha = 0.7,
    a90 = 0.15, a50 = 0.30, rb_outer = 0.25, rb_inner = 0.35, area_alpha = 0.18,
    w_chop = "orange", w_proj = "purple", w_ink = "dark", w_final = "black"
  )
} else {
  list(
    mode = "dark", bg = "#05091D", strip = "#1B2547", grid = "#2B3A66", axis_line = "#5A6FA3",
    text = "#FFFFFF", muted = "#C9D6EE", dim = "#A9B8D6", ink = "#FFFFFF",
    chop = "#FF6482", survive = "#00FFF9", proj = "#00FFF9", # survive: teams that were NOT chopped in the four-lowest figure (Sleeper teal)
    contenders = c("#4CD787", "#5B9DFF", "#FFA84C", "#FFC857", "#00FFF9"),
    approx = "#5B9DFF", sims = "#4CD787",
    other = "#FFFFFF", other_lw = 0.4, other_alpha = 0.4, # white, but dimmed so the colored lines stay the focus
    pts_other = "#C9D6EE", pts_other_alpha = 0.75,
    gap_fill = "#FFFFFF", gap_alpha = 0.10, kick = "#C9D6EE", kick_alpha = 0.6,
    a90 = 0.30, a50 = 0.50, rb_outer = 0.28, rb_inner = 0.45, area_alpha = 0.30,
    w_chop = "red", w_proj = "teal", w_ink = "white", w_final = "white"
  )
}

# base_size 15 (was 11): the figures are shown scaled down on the page, so everything is set large
# Phone layout switches (all default to the desktop look; analysis/plot_mobile.R sets them for the phone versions):
#   options(sleeper.base_size = 12)   text size for a canvas drawn at phone width (a narrower canvas needs relatively bigger text)
#   options(sleeper.wrap = 58)        wrap titles / subtitles / captions at this many characters
#   options(sleeper.annot_scale = .7) scale the overall title / subtitle / caption of the multi-panel figures
#   kind = "annot": the overall title/subtitle/caption of multi-panel figures (smaller text, so a longer line: sleeper.wrap_annot)
#   keep_nl = TRUE: keep the text's own line breaks (labels that are deliberately two lines); otherwise hard breaks are joined and re-wrapped
wrap_txt <- function(x, kind = "plot", keep_nl = FALSE) {
  n <- if (kind == "annot") getOption("sleeper.wrap_annot", getOption("sleeper.wrap")) else getOption("sleeper.wrap")
  if (is.null(n) || is.null(x)) return(x)
  vapply(x, function(s) {
    if (keep_nl) paste(vapply(strsplit(s, "\n", fixed = TRUE)[[1]], function(l) paste(strwrap(l, n), collapse = "\n"), character(1)), collapse = "\n")
    else paste(strwrap(gsub("\n", " ", s, fixed = TRUE), n), collapse = "\n")
  }, character(1), USE.NAMES = FALSE)
}

theme_sleeper <- function(base_size = getOption("sleeper.base_size", 15), legend = "right") {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(color = FIG$text),
      plot.background = element_rect(fill = FIG$bg, color = NA),
      panel.background = element_rect(fill = FIG$bg, color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = FIG$grid, linewidth = 0.35),
      axis.title = element_text(size = rel(1.0), color = FIG$text),
      axis.text = element_text(size = rel(0.9), color = FIG$muted),
      axis.line = element_line(color = FIG$axis_line, linewidth = 0.3),
      legend.position = legend,
      legend.title = element_text(size = rel(0.95), color = FIG$text),
      legend.text = element_text(size = rel(0.85), color = FIG$text),
      legend.key = element_blank(),
      legend.background = element_rect(fill = FIG$bg, color = NA),
      strip.text = element_text(size = rel(0.9), face = "bold", color = FIG$text),
      strip.background = element_rect(fill = FIG$strip, color = NA),
      panel.spacing = unit(1, "lines"),
      plot.title = element_text(size = rel(1.15), face = "bold", color = FIG$text),
      plot.subtitle = element_text(size = rel(1.0), color = FIG$muted),
      plot.caption = element_text(size = rel(0.75), color = FIG$dim),
      plot.margin = margin(10, 10, 10, 10)
    )
}

# theme for patchwork's plot_annotation(): the overall title/subtitle/caption and the ground behind every panel
annot_theme <- function() {
  k <- getOption("sleeper.annot_scale", 1)
  theme(plot.background = element_rect(fill = FIG$bg, color = NA),
        plot.title = element_text(face = "bold", color = FIG$text, size = 20 * k),
        plot.subtitle = element_text(color = FIG$muted, size = 14 * k),
        plot.caption = element_text(color = FIG$dim, size = 12 * k))
}
