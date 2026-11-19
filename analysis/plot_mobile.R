# --- Phone versions of the recap figures ---------------------------------------------------------------------
# The desktop figures are drawn on a wide canvas (11 x 7.5 in, 12 x 11 in ...) and then shrunk into a ~364 px
# phone column, which also shrinks their text to ~22%. These versions are drawn at phone proportions instead: a
# 6.5 in wide canvas (the text ends up ~1.6x larger relative to the page column), taller than wide, long
# titles/subtitles wrapped, and side-by-side panels stacked into one column.
#
#   source("analysis/plot_mobile.R"); plot_mobile_figures(2026, 1:5, out_dir = "somewhere")
#
# Uses the layout switches in analysis/theme_sleeper.R (sleeper.base_size / sleeper.wrap / sleeper.annot_scale)
# and analysis/plot_team_elimination.R / plot_sim_check.R (sleeper.stack). The slope chart, the preview chart and the waiver
# figures are not redrawn: they are already about 1.2:1 and read fine at phone width.

if (!exists("plot_team_race")) source(file.path("analysis", "plot_team_elimination.R"))
if (!exists("plot_sim_check")) source(file.path("analysis", "plot_sim_check.R"))

MOBILE_W <- 6.5   # inches

with_mobile_layout <- function(code, base_size = 12, wrap = 62, annot_scale = 0.72) {
  old <- options(sleeper.base_size = base_size, sleeper.wrap = wrap, sleeper.wrap_annot = round(wrap * 1.3), sleeper.annot_scale = annot_scale, sleeper.stack = TRUE)
  on.exit(options(old))
  force(code)
}

# One team's week, phone layout (used by build_team_week_charts() in collector/recap_lib.R)
plot_mobile_team <- function(season, week, roster_id, save_path) {
  with_mobile_layout(plot_team_single(season, week, roster_id, save_path = save_path, width = MOBILE_W, height = 4.0, dpi = 130), base_size = 13, wrap = 68)
}

# what = which figures: "race" (one per week), "chop_rank", "lowest_two", "sim_check", "team" (one per team per week)
plot_mobile_figures <- function(season, weeks, out_dir, what = c("race", "chop_rank", "lowest_two", "sim_check", "team"), team_weeks = weeks, log = message) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  if ("race" %in% what) for (w in weeks) with_mobile_layout(plot_team_race(season, w, save_path = file.path(out_dir, sprintf("race_w%d.png", w)), width = MOBILE_W, height = 8.2))
  if ("chop_rank" %in% what) with_mobile_layout(plot_chop_rank(season, weeks, save_path = file.path(out_dir, "chop_rank.png"), per_row = 2, panel_w = (MOBILE_W - 0.4) / 2, panel_h = 2.6), base_size = 10.5)
  if ("lowest_two" %in% what) with_mobile_layout(plot_team_lowest_finishers(season, weeks, n_low = 2, show_prob = FALSE, width = MOBILE_W, height = 2.6 * 2 * length(weeks) + 1.9,
                                                                            save_path = file.path(out_dir, "lowest_two.png")))
  if ("sim_check" %in% what) { sw <- weeks[file.exists(vapply(weeks, function(x) sim_check_path(season, x), character(1)))]
    if (length(sw)) with_mobile_layout(plot_sim_check(season, sw, width = MOBILE_W, height = 2.9 * 2 * length(sw) + 2.4, save_path = file.path(out_dir, "sim_check.png"))) }
  if ("team" %in% what) for (w in team_weeks) {
    tf <- teams_path(season, w); if (!file.exists(tf)) next
    for (r in unique(readr::read_csv(tf, show_col_types = FALSE, col_select = "roster_id")$roster_id))
      plot_mobile_team(season, w, r, file.path(out_dir, sprintf("team_w%d_r%d.png", w, r)))
  }
  invisible(out_dir)
}
