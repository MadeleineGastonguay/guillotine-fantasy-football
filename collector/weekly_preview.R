#!/usr/bin/env Rscript
# Week preview: published Wednesday morning, AFTER the waivers have run and before any game of the week.
#   Rscript collector/weekly_preview.R                 # the NFL week that is current now (needs this morning's waiver claims to be on Sleeper)
#   Rscript collector/weekly_preview.R 2026 5          # a specific season and week
#   Rscript collector/weekly_preview.R --force         # redo a preview that is already done (reusing the data saved by its first build)
#   Rscript collector/weekly_preview.R --refresh       # fetch fresh data (projections move through the day) and rebuild; also needed if the draft was written from an older prompt
#   Rscript collector/weekly_preview.R --no-publish    # build locally even if RECAP_PAGES_DIR is set
#   Rscript collector/weekly_preview.R --narrative-file=path.txt   # testing: use this text instead of looking for a draft
#
# For every team still alive: its projected score and chance of being chopped BEFORE the waivers and AFTER them (both with today's projections, so a change
# is the waiver moves alone), the players the waivers added, the players on a bye, and the claims it lost. The text works like the recaps': the script writes
# preview_wNN.facts.txt and preview_wNN.prompt.txt; paste the prompt into Claude and save the reply as preview_wNN.draft.txt, and the next run (or
# `Rscript collector/weekly_preview.R`) rebuilds the page with it; until then the page shows an automatic summary.
#
# Output (under recap/{season}/): preview_wNN.data.rds (the saved data, see --refresh), preview_wNN.csv (one row per team), preview_pickups_wNN.csv, preview_byes_wNN.csv, preview_lost_wNN.csv,
# preview_wNN.{txt,kind,facts.txt,prompt.txt}, img/preview_wN.png, and the page itself (index.html gets a "Week N preview" tab).
# If no waiver claim has been processed yet (the run is Wednesday ~3am Eastern) it stops without publishing, so a later scheduled run picks it up.

started <- Sys.time()
args <- commandArgs(trailingOnly = TRUE)
force <- "--force" %in% args
refresh <- "--refresh" %in% args
narr_arg <- grep("^--narrative-file=", args, value = TRUE)
narrative_file <- if (length(narr_arg)) sub("^--narrative-file=", "", narr_arg) else NULL
if ("--no-publish" %in% args) Sys.setenv(RECAP_PAGES_DIR_OFF = "1")
pos <- args[!grepl("^--", args)]

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) setwd(normalizePath(file.path(dirname(sub("^--file=", "", script_arg[1])), "..")))

suppressMessages({ library(dplyr); library(purrr); library(tidyr); library(tibble); library(httr2); library(readr); library(scales) })
for (f in c("config", "sleeper_api", "sleeper_projections", "espn_api", "stat_scoring", "bestball", "simulation",
            "pipeline", "bayesian", "nflverse", "collector", "replay", "fallback")) source(file.path("R", paste0(f, ".R")))
source(file.path("collector", "recap_lib.R")); source(file.path("collector", "waivers_lib.R")); source(file.path("collector", "preview_lib.R"))
suppressMessages(source(file.path("analysis", "plot_team_elimination.R"))); suppressMessages(source(file.path("analysis", "plot_sim_check.R")))

log_msg <- function(...) message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", sprintf(...))

last_trace <- ""
result <- tryCatch(withCallingHandlers({
  state <- get_nfl_state()
  season <- if (length(pos) >= 1) as.integer(pos[1]) else as.integer(state$season)
  week <- if (length(pos) >= 2) as.integer(pos[2]) else as.integer(state$week)
  if (!identical(state$season_type, "regular") && length(pos) < 2) stop("it is not the regular season; give the season and week explicitly")
  if (week < 2) stop("a preview needs a chop and waivers before it (week 2 or later)")
  out_dir <- file.path("recap", season); img_dir <- file.path(out_dir, "img")
  dir.create(img_dir, showWarnings = FALSE, recursive = TRUE)
  prefix <- file.path(out_dir, sprintf("preview_w%02d", week)); done_flag <- paste0(prefix, ".done")
  pending <- recap_pending_draft(prefix)
  if (file.exists(done_flag) && !force && !pending) {
    log_msg("season %s week %s preview is already done (%s); nothing to rebuild. Use --force to redo it.", season, week, done_flag)
    if (!nzchar(Sys.getenv("RECAP_PAGES_DIR_OFF"))) publish_if_needed(season, week, log = log_msg, what = "Preview")
    quit(status = 0)
  }
  if (pending) log_msg("found a new %s.draft.txt; rebuilding the preview with it", basename(prefix))
  log_msg("week preview: season %s week %s", season, week)

  # the data is saved at the first build and reused by later runs of the same week, so a draft written from preview_wNN.prompt.txt still matches the facts it was written from
  # (projections move through the day, and a changed number would make the draft fail its check); --refresh fetches new data
  rds <- paste0(prefix, ".data.rds")
  if (file.exists(rds) && !refresh) {
    pv <- readRDS(rds); log_msg("reusing the data saved at the first build of this preview (%s); use --refresh for new data", basename(rds))
    # data saved before the injury news existed: add just that block (fresh statuses, the day it was pulled is recorded); nothing else in the saved data changes
    if (is.null(pv$injuries) || !("sleeper_proj" %in% names(pv$injuries))) { log_msg("adding the injury news to the saved data (statuses as of now)"); pv <- preview_add_injuries(pv, log = log_msg); saveRDS(pv, rds) }
  }
  else { pv <- build_preview(season, week, log = log_msg); if (pv$claims_won > 0) saveRDS(pv, rds) }
  if (pv$claims_won == 0 && !force) { log_msg("no waiver claims have been processed yet; nothing published, will retry on the next run"); quit(status = 0) }
  # self-test: the rebuilt projected totals must reproduce the app's own
  if (any(abs(pv$teams$check_proj - pv$teams$proj_after) > 0.05)) log_msg("WARNING: projected totals do not match the app's snapshot for some team")

  readr::write_csv(pv$teams[, c("roster_id", "team_name", "owner_name", "chance_before", "chance_after", "proj_before", "proj_after", "n_pickups", "spent_run", "budget_before", "spent_share", "n_bye", "bye_cost", "n_lost", "lost_gain")], paste0(prefix, ".csv"))
  readr::write_csv(pv$pickups, file.path(out_dir, sprintf("preview_pickups_w%02d.csv", week)))
  readr::write_csv(pv$byes, file.path(out_dir, sprintf("preview_byes_w%02d.csv", week)))
  readr::write_csv(pv$lost, file.path(out_dir, sprintf("preview_lost_w%02d.csv", week)))
  if (!is.null(pv$injuries)) readr::write_csv(pv$injuries, file.path(out_dir, sprintf("preview_injuries_w%02d.csv", week)))
  # when the data was pulled, for the page's disclaimer (a preview saved before this was recorded was built on a Wednesday morning)
  readr::write_csv(data.frame(projections = if (!is.null(pv$built_at)) day_part(pv$built_at) else "Wednesday morning",
                              injuries = if (!is.null(pv$injuries_asof)) pv$injuries_asof else if (!is.null(pv$injuries) && nrow(pv$injuries) && "asof" %in% names(pv$injuries)) pv$injuries$asof[1] else NA_character_),
                   file.path(out_dir, sprintf("preview_meta_w%02d.csv", week)))
  plot_preview_chances(pv$teams, week, save_path = file.path(img_dir, sprintf("preview_w%d.png", week)))

  fact_sheet <- preview_fact_sheet(pv, pv$bye_teams)
  text <- make_preview_text(fact_sheet, template_preview(pv, pv$bye_teams), prefix, narrative_file = narrative_file, log = log_msg)

  rebuild_recap_page(season, out_dir)
  newly_done <- !file.exists(done_flag)
  writeLines(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), done_flag)
  if (newly_done && identical(text$kind, "template")) {
    notify_mac("Guillotine preview", sprintf("The week %d preview is built with the automatic summary. To add a Claude-written version, paste %s.prompt.txt into Claude and save the reply as %s.draft.txt.", week, basename(prefix), basename(prefix)))
  }
  if (!nzchar(Sys.getenv("RECAP_PAGES_DIR_OFF"))) publish_if_needed(season, week, log = log_msg, what = "Preview")
  log_msg("done in %.1f minutes: %s", as.numeric(difftime(Sys.time(), started, units = "mins")), normalizePath(file.path(out_dir, "index.html")))
  0L
}, error = function(e) last_trace <<- paste(rev(vapply(utils::tail(sys.calls(), 10), function(x) substr(deparse(x, nlines = 1)[1], 1, 70), character(1))), collapse = "  <-  ")),
error = function(e) {
  log_msg("PREVIEW FAILED: %s", conditionMessage(e))
  if (nzchar(last_trace)) log_msg("  where: %s", last_trace)
  1L
})
quit(status = result)
