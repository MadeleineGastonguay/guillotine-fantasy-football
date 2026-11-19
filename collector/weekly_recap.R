#!/usr/bin/env Rscript
# Weekly recap: replay the week that just finished on real kickoff times, make the figures and a static page.
#   Rscript collector/weekly_recap.R                  # the latest week in which every game has a final score
#   Rscript collector/weekly_recap.R 2026 4           # a specific season and week
#   Rscript collector/weekly_recap.R --force          # redo a week that is already done
#   Rscript collector/weekly_recap.R --window=4       # limit the Projection performance figures to the last N weeks (default: every week)
#   Rscript collector/weekly_recap.R --no-publish     # build locally even if RECAP_PAGES_DIR is set
#   Rscript collector/weekly_recap.R --no-sim         # skip the Simulations-tab cross-check (saves ~4-5 minutes); --redo-sim recomputes it
#
# Output (all under recap/{season}/, a plain static folder you can host anywhere):
#   index.html                       the recap page (table of every chop so far, this week's story, the figures)
#   img/race_wNN.png                 one race chart per week; img/chop_rank.png and img/lowest_two.png cover the last few weeks
#   img/team_wN_rID.png, teamcharts_wNN.csv   one chart per team per week behind the "Every team's projection through the week" drop-down at the bottom of each week's recap
#   scorecard.png                    (in recap/{season}, not img/) the detailed three-row figure for every week; kept in the repo, not published
#   facts.csv                        one row of headline numbers per week
#   saved_wNN.csv, saved_players_wNN.csv   the teams that saved themselves (and the players who carried them) for the page; img/proj_slope_wN.png = every team, projected vs finished
#   narrative_wNN.txt                the description on the page (automatic text, or a draft that passed the checks)
#   narrative_wNN.facts.txt          the fact sheet every description is written from; .prompt.txt = the same plus instructions, ready to paste into Claude
#   narrative_wNN.draft.txt          YOURS: save Claude's reply here and the next run rebuilds the page with it (.rejected.txt = a draft that failed its checks)
#   week_NN.done                     marker that a week finished, so scheduled re-runs do nothing
# Also writes accuracy_data/replay/{season}_wNN_players.csv.gz and ..._league_*_realtime.csv.gz (R/replay.R).
#
# Safe to schedule: it checks the replay holds EVERY game on the week's schedule (nflverse can lag a night behind the last
# game) and stops without publishing anything if not, so a later scheduled run picks the week up. Exit status is 1 on failure.
# Takes a few minutes. Needs the network (Sleeper, nflverse).

started <- Sys.time()
args <- commandArgs(trailingOnly = TRUE)
force <- "--force" %in% args
window_arg <- grep("^--window=", args, value = TRUE)
window <- if (length(window_arg)) as.integer(sub("^--window=", "", window_arg)) else 999L   # every week so far
narr_arg <- grep("^--narrative-file=", args, value = TRUE)   # testing: use this text file instead of calling the model
narrative_file <- if (length(narr_arg)) sub("^--narrative-file=", "", narr_arg) else NULL
no_sim <- "--no-sim" %in% args
redo_sim <- "--redo-sim" %in% args
if ("--no-publish" %in% args) Sys.setenv(RECAP_PAGES_DIR_OFF = "1")
pos <- args[!grepl("^--", args)]

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) setwd(normalizePath(file.path(dirname(sub("^--file=", "", script_arg[1])), "..")))

suppressMessages({ library(dplyr); library(purrr); library(tidyr); library(tibble); library(httr2); library(readr); library(scales) })
for (f in c("config", "sleeper_api", "sleeper_projections", "espn_api", "stat_scoring", "bestball", "simulation",
            "pipeline", "bayesian", "nflverse", "collector", "replay")) source(file.path("R", paste0(f, ".R")))
source(file.path("collector", "recap_lib.R"))
source(file.path("collector", "waivers_lib.R"))
source(file.path("collector", "preview_lib.R"))   # the page draws the week preview tab (preview_section_html)
suppressMessages(source(file.path("analysis", "plot_team_elimination.R"))) # the figure functions (dark Sleeper look)
suppressMessages(source(file.path("analysis", "plot_waivers.R")))
suppressMessages(source(file.path("analysis", "plot_season_scores.R")))   # final score by week, budget spent vs score             # waiver spending + value figures
suppressMessages(source(file.path("analysis", "plot_sim_check.R")))         # Approximate-vs-Simulations spot check + its figure
suppressMessages(source(file.path("analysis", "plot_mobile.R")))             # the phone-proportioned versions of those figures (used on every device)

log_msg <- function(...) message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", sprintf(...))

last_trace <- ""
result <- tryCatch(withCallingHandlers({
  season <- if (length(pos) >= 1) as.integer(pos[1]) else as.integer(get_nfl_state()$season)
  week <- if (length(pos) >= 2) as.integer(pos[2]) else latest_complete_week(season)
  out_dir <- file.path("recap", season)
  img_dir <- file.path(out_dir, "img")
  dir.create(img_dir, showWarnings = FALSE, recursive = TRUE)
  done_flag <- file.path(out_dir, sprintf("week_%02d.done", week))
  narr_prefix <- file.path(out_dir, sprintf("narrative_w%02d", week))
  pending_draft <- recap_pending_draft(narr_prefix) # a person dropped narrative_wNN.draft.txt after the page was built
  if (file.exists(done_flag) && !force && !pending_draft) {
    log_msg("season %s week %s is already done (%s); nothing to rebuild. Use --force to redo it.", season, week, done_flag)
    if (!nzchar(Sys.getenv("RECAP_PAGES_DIR_OFF"))) publish_if_needed(season, week, log = log_msg) # retries a publish that failed last time
    quit(status = 0)
  }
  if (pending_draft) log_msg("found a new %s.draft.txt; rebuilding the page with it", basename(narr_prefix))
  log_msg("weekly recap: season %s week %s", season, week)

  # 1. player-by-player replay of the week, then a guard that no game is missing
  pr <- replay_week(season, week, step_minutes = 1, exclude_season_from_shape = FALSE)
  n_have <- dplyr::n_distinct(pr$game_id)
  n_want <- scheduled_games(season, week)
  if (n_have < n_want) {
    stop(sprintf("the replay has %d of the %d games on the schedule (nflverse is probably still missing the last game); nothing published, will retry on the next run", n_have, n_want))
  }

  # 2. the league's team timeline on real kickoff times
  replay_league_week_realtime(season, week, pr, write = TRUE)

  # 2b. make sure collector/managers.csv lists every manager (adds new user names with a blank real_name; never overwrites yours)
  tt <- readr::read_csv(teams_path(season, week), show_col_types = FALSE, col_select = c("team_name", "owner_name"))
  if (ensure_manager_key(tt)) log_msg("added new user names to %s; fill in real_name to have the text use real names", MANAGER_KEY_FILE)

  # 3. headline numbers and the chopped team's biggest shortfalls
  facts <- recap_facts(season, week)
  pl <- recap_players(season, week, pr)
  players <- pl$players
  readr::write_csv(players, file.path(out_dir, sprintf("players_w%02d.csv", week))) # the page shows every week's table
  facts_path <- file.path(out_dir, "facts.csv")
  facts_all <- if (file.exists(facts_path)) readr::read_csv(facts_path, show_col_types = FALSE) else facts[0, ]
  facts_all <- dplyr::bind_rows(dplyr::filter(facts_all, week != !!week), facts) |> dplyr::arrange(week)
  readr::write_csv(facts_all, facts_path)

  # 3b. the written description: a model writes it from a fact sheet; it is checked, else the page keeps the automatic summary
  saved <- pl$saved # teams that were in danger / beat projection and were not chopped (+ their carrying players)
  readr::write_csv(saved$teams[, c("team_name", "owner_name", "why", "category", "pre_prob", "peak", "peak_time", "score_at_peak", "pre_proj", "final", "beat", "league_median_beat", "beat_vs_league", "above_chop", "place", "place_text")],
                   file.path(out_dir, sprintf("saved_w%02d.csv", week)))
  readr::write_csv(saved$players, file.path(out_dir, sprintf("saved_players_w%02d.csv", week)))
  first_ko <- min(as.numeric(as.POSIXct(as.character(readr::read_csv(teams_path(season, week), show_col_types = FALSE, col_select = "wall_utc")$wall_utc), tz = "UTC")))
  prev_owner <- facts_all$chopped_owner[facts_all$week == week - 1]
  perf <- pr |> dplyr::filter(is_final_row) |> dplyr::transmute(player_id = as.character(player_id), pre_proj, scored = final_actual_points) |> dplyr::distinct(player_id, .keep_all = TRUE)
  spender <- tryCatch(chop_spender_tile(week, first_ko, if (length(prev_owner)) prev_owner[1] else NA, perf = perf), error = function(e) { log_msg("biggest-spender tile skipped: %s", conditionMessage(e)); NULL })
  hl_all <- dplyr::bind_rows(pl$highlights, spender)
  hl_all <- hl_all[order(match(hl_all$label, c("Top team score", "Top player score", "Biggest boom", "Biggest bust", "Biggest Waiver Spender", "Most expensive player", "Waiver spending", "Best swap after waivers", "Worst swap after waivers"))), ]
  readr::write_csv(hl_all, file.path(out_dir, sprintf("highlights_w%02d.csv", week)))   # the tiles at the top of the week
  tryCatch(readr::write_csv(faab_table(), file.path(out_dir, "faab.csv")), error = function(e) log_msg("FAAB budgets skipped: %s", conditionMessage(e)))   # waiver budgets as of now, for the scoreboard
  sw_out <- dplyr::left_join(saved$swaps, saved$cf_all[, c("roster_id", "team_name", "owner_name")], by = "roster_id")   # every swap with its effect, for the season ledger
  readr::write_csv(sw_out[, c("team_name", "owner_name", "type", "bid", "when", "added", "dropped", "effect", "replaced")], file.path(out_dir, sprintf("swaps_w%02d.csv", week)))
  readr::write_csv(saved$cf_all[saved$cf_all$n_moves > 0, ], file.path(out_dir, sprintf("moves_w%02d.csv", week))) # every team with in-game roster moves, and its score with none of them
  # 3c. waivers: the auction for last week's chopped players, the chopped team's lost bids, and every claim's return (waivers_wNN.csv, waiver_claims.csv)
  wv <- tryCatch({
    weeks_so_far <- sort(unique(facts_all$week))
    ctx <- waiver_context(season, weeks_so_far)
    wk <- waiver_week(ctx, week, perf)
    if (!is.null(wk$results)) readr::write_csv(wk$results, file.path(out_dir, sprintf("waivers_w%02d.csv", week))) else unlink(file.path(out_dir, sprintf("waivers_w%02d.csv", week)))
    readr::write_csv(faab_history(ctx), file.path(out_dir, "faab_history.csv"))
    readr::write_csv(waiver_habits(ctx), file.path(out_dir, "waiver_habits.csv"))
    tryCatch(readr::write_csv(freed_pool_history(ctx, weeks_so_far), file.path(out_dir, "freed_pool_by_week.csv")), error = function(e) log_msg("freed pool history skipped: %s", conditionMessage(e)))
    tryCatch(readr::write_csv(waiver_week_effect(ctx, weeks_so_far), file.path(out_dir, "waiver_points_by_week.csv")), error = function(e) log_msg("waiver points by week skipped: %s", conditionMessage(e)))
    roi <- waiver_roi(season, weeks_so_far, ctx$claims, ctx$chops, ctx$win, ctx$db, ctx$pos_tbl, dplyr::distinct(dplyr::bind_rows(lapply(weeks_so_far, function(w) readr::read_csv(teams_path(season, w), show_col_types = FALSE, col_select = c("roster_id", "team_name", "owner_name")))), roster_id, .keep_all = TRUE))
    if (!is.null(roi)) readr::write_csv(roi, file.path(out_dir, "waiver_claims.csv"))
    wk$roi <- roi   # for the "paid for, then sidelined" block of the fact sheet
    wk
  }, error = function(e) { log_msg("waiver analysis skipped: %s", conditionMessage(e)); NULL })
  sidelined <- tryCatch(sidelined_for_week(wv$roi, week), error = function(e) NULL)
  payoff <- tryCatch(paying_off_for_week(wv$roi, week), error = function(e) NULL)   # earlier claims that finally scored this week
  bare_bench <- tryCatch(bare_bench_teams(season, week), error = function(e) { log_msg("bare-bench check skipped: %s", conditionMessage(e)); NULL })   # teams whose whole bench could not play
  upforbid <- tryCatch(waiver_watch(season, week), error = function(e) { log_msg("up-for-bid table skipped: %s", conditionMessage(e)); NULL })   # the chopped team's freed players
  if (!is.null(upforbid) && any(upforbid$show)) readr::write_csv(upforbid[upforbid$show, ], file.path(out_dir, sprintf("upforbid_w%02d.csv", week)))
  fact_sheet <- recap_fact_sheet(season, week, facts_all, players, pl$last_games, saved = saved, chopped_info = pl$chopped_info, lost = wv$lost, waivers = wv$results, sidelined = sidelined, payoff = payoff, out_dir = out_dir, upforbid = upforbid, bare_bench = bare_bench)
  draft_path <- paste0(narr_prefix, ".draft.txt")
  use_file <- if (!is.null(narrative_file)) narrative_file else if (file.exists(draft_path)) draft_path else NULL
  narrative <- make_narrative(fact_sheet, need_compare = any(facts_all$week < week), out_prefix = narr_prefix,
                              template = template_narrative(week, facts_all, players, pl$last_games, saved = saved, chopped_info = pl$chopped_info, lost = wv$lost, waivers = wv$results, sidelined = sidelined, payoff = payoff, out_dir = out_dir, upforbid = upforbid, bare_bench = bare_bench),
                              narrative_file = use_file, file_label = if (is.null(narrative_file)) "a chat with Claude" else "supplied text file", log = log_msg)

  # 4. figures: this week's race chart, plus the scorecard and two-lowest-teams figures over the last `window` weeks
  weeks_all <- sort(unique(facts_all$week))
  weeks_window <- tail(weeks_all, window)
  plot_mobile_figures(season, week, img_dir, what = "race")
  # one chart per team for the drop-down at the bottom of every week's recap (older weeks are backfilled the first time; this week is always redrawn)
  tryCatch(build_team_week_charts(season, weeks_all, out_dir, img_dir, redo_weeks = week, log = log_msg),
           error = function(e) log_msg("team charts skipped: %s", conditionMessage(e)))
  plot_team_projected_vs_final_slope(season, week, danger = saved$teams$team_name[saved$teams$category == "danger"], close = saved$teams$team_name[saved$teams$category == "close"], beat = saved$teams$team_name[saved$teams$category == "beat"], save_path = file.path(img_dir, sprintf("proj_slope_w%d.png", week)))
  tryCatch({ wc <- file.path(out_dir, "waiver_claims.csv")
    if (file.exists(wc) && file.exists(file.path(out_dir, "faab_history.csv"))) {
      cl <- readr::read_csv(wc, show_col_types = FALSE)
      plot_waiver_spending(cl, save_path = file.path(img_dir, "waiver_spending.png")); plot_waiver_value(cl, save_path = file.path(img_dir, "waiver_value.png"))
      fh <- readr::read_csv(file.path(out_dir, "faab_history.csv"), show_col_types = FALSE)
      plot_waiver_budgets(fh, save_path = file.path(img_dir, "waiver_budgets.png"))
      plot_spend_curve(dplyr::mutate(spend_curve_data(cl), chopped = team_name %in% fh$team_name[!is.na(fh$chopped_week)]), save_path = file.path(img_dir, "spend_curve.png"))
      pfp <- file.path(out_dir, "freed_pool_by_week.csv"); if (file.exists(pfp)) plot_freed_pool(readr::read_csv(pfp, show_col_types = FALSE), save_path = file.path(img_dir, "freed_pool.png"))
    } }, error = function(e) log_msg("waiver figures skipped: %s", conditionMessage(e)))
  tryCatch({ fhp <- file.path(out_dir, "faab_history.csv"); sc_all <- season_scores(season, weeks_all)
    if (nrow(sc_all) && file.exists(fhp)) {
      readr::write_csv(sc_all, file.path(out_dir, "scores_by_week.csv"))
      plot_scores_over_time(sc_all, save_path = file.path(img_dir, "scores_by_week.png"))
      wpw <- file.path(out_dir, "waiver_points_by_week.csv")
      if (file.exists(wpw)) plot_budget_vs_points(budget_by_week(sc_all, readr::read_csv(fhp, show_col_types = FALSE), readr::read_csv(wpw, show_col_types = FALSE)), save_path = file.path(img_dir, "budget_vs_score.png"))
    } }, error = function(e) log_msg("score-over-time figures skipped: %s", conditionMessage(e)))
  # up to 6 weeks side by side in a row; more weeks wrap into further blocks below (one row of 9+ weeks passed ggsave's 50-inch limit and failed the week 9 build).
  # Not shown on the website, so wrapping costs nothing; for 6 weeks or fewer this is the same single row as before.
  sc_per_row <- min(length(weeks_window), 6L); sc_blocks <- ceiling(length(weeks_window) / sc_per_row)
  plot_team_scorecard(season, weeks_window, width = 5.7 * sc_per_row + 0.5, height = 10.5 * sc_blocks, per_row = sc_per_row,
                      save_path = file.path(out_dir, "scorecard.png"))   # detailed version: kept in the repo, not on the website
  plot_mobile_figures(season, weeks_window, img_dir, what = c("chop_rank", "lowest_two"))
  # The two long figures also get their original wide (desktop) versions, shown on screens 760 px and wider; the phone versions above are shown below that
  # (<picture> in collector/recap_lib.R).
  plot_team_lowest_finishers(season, weeks_window, n_low = 2, show_prob = FALSE, width = 10, height = 3.7 * length(weeks_window) + 1.4,
                             save_path = file.path(img_dir, "lowest_two_wide.png"))

  # 4b. cross-check against the Simulations tab (about 4-5 minutes a week; cached per week, so only new weeks are computed)
  # --no-sim only skips COMPUTING this week's check; the section is always built from whatever is cached, so a
  # rebuild never drops the Approximate-vs-Simulations comparison from the page.
  if (!no_sim) simulation_spot_check(season, week, redo = redo_sim)
  sim_html <- ""
  sim_weeks <- sort(weeks_all[file.exists(vapply(weeks_all, function(x) sim_check_path(season, x), character(1)))])
  if (length(sim_weeks)) {
    fig_weeks <- tail(sim_weeks, window)
    plot_mobile_figures(season, fig_weeks, img_dir, what = "sim_check")
    plot_sim_check(season, fig_weeks, width = 12, height = 3.4 * length(fig_weeks) + 0.9, save_path = file.path(img_dir, "sim_check_wide.png"))
    sim_html <- sim_section_html(season, sim_weeks, fig_weeks)
  }

  # 5. the page, then mark the week done
  write_recap_page(season, week, facts_all, players, weeks_window, weeks_all, file.path(out_dir, "index.html"), narrative = narrative, sim_html = sim_html)
  newly_done <- !file.exists(done_flag)
  writeLines(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), done_flag)
  if (newly_done && identical(narrative$kind, "template")) {
    notify_mac("Guillotine recap", sprintf("Week %d recap is built with the automatic summary. To add a Claude-written version, paste %s.prompt.txt into Claude and save the reply as %s.draft.txt.", week, basename(narr_prefix), basename(narr_prefix)))
  }
  if (!nzchar(Sys.getenv("RECAP_PAGES_DIR_OFF"))) publish_if_needed(season, week, log = log_msg)
  log_msg("done in %.1f minutes: %s", as.numeric(difftime(Sys.time(), started, units = "mins")), normalizePath(file.path(out_dir, "index.html")))
  0L
}, error = function(e) last_trace <<- paste(rev(vapply(utils::tail(sys.calls(), 10), function(x) substr(deparse(x, nlines = 1)[1], 1, 70), character(1))), collapse = "  <-  ")),
error = function(e) {
  log_msg("RECAP FAILED: %s", conditionMessage(e))
  if (nzchar(last_trace)) log_msg("  where: %s", last_trace)
  1L
})
quit(status = result)
