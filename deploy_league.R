#!/usr/bin/env Rscript
# Stage (and optionally deploy) a copy of the app for another Sleeper guillotine league.
#
#   Rscript deploy_league.R leagues/my_league.R                    # check the league, stage a clean folder, print the deploy command
#   Rscript deploy_league.R leagues/my_league.R --name my-app      # same, with the shinyapps.io app name (default: the config file's name)
#   Rscript deploy_league.R leagues/my_league.R --deploy           # also run rsconnect::deployApp() (publishes; account from --account or SHINYAPPS_ACCOUNT)
#   Rscript deploy_league.R leagues/my_league.R --deploy --update  # same, over an app of that name that already exists (the redeploy case)
#
# What it does:
#   1. Reads the config, asks Sleeper for the league and prints what the app will see (lineup, team count, best ball, guillotine settings). It stops if the
#      lineup has a slot the app cannot score (K, DEF, IDP).
#   2. Builds ../guillotine_app_<name>_deploy/ from ONLY the files the app needs at run time: app.R, the runtime R/ files, www/, the chosen config as
#      league_config.R, and the cached files the app reads (the residual-shape file is required). It leaves out rsconnect/ (so an existing app's link and
#      app ID are untouched), logs/, data/ (this league's history), recap/, collector/, analysis/ and accuracy_data/.
#   3. Prints the rsconnect::deployApp() command. Nothing is published unless --deploy is given.
# An existing app keeps deploying from this folder exactly as before (its link is tied to rsconnect/ here).

args <- commandArgs(trailingOnly = TRUE)
flag <- function(name) { i <- match(name, args); if (is.na(i)) NULL else if (i < length(args)) args[i + 1] else TRUE }
pos <- args[!startsWith(args, "--") & !(seq_along(args) %in% (match(c("--name", "--account"), args) + 1))]
if (!length(pos)) stop("usage: Rscript deploy_league.R leagues/<config>.R [--name app-name] [--account name] [--deploy]")
cfg_path <- normalizePath(pos[1], mustWork = TRUE)
app_dir <- normalizePath(".")
if (!file.exists(file.path(app_dir, "app.R")) || !file.exists(file.path(app_dir, "R", "config.R"))) stop("run this from the guillotine_app folder")
name <- flag("--name"); if (is.null(name) || isTRUE(name)) name <- gsub("_", "-", tools::file_path_sans_ext(basename(cfg_path)))
account <- flag("--account"); if (is.null(account) || isTRUE(account)) account <- Sys.getenv("SHINYAPPS_ACCOUNT")
do_deploy <- !is.null(flag("--deploy"))
do_update <- !is.null(flag("--update"))   # needed to redeploy over an app of that name that already exists on shinyapps.io (the same app: its link stays the same)

# 1. read the config and the league ---------------------------------------------------------------
cfg <- new.env(); sys.source(cfg_path, envir = cfg)
lid <- get0("LEAGUE_ID", envir = cfg)
if (is.null(lid) || !grepl("^[0-9]{10,25}$", lid)) stop("LEAGUE_ID in ", cfg_path, " is not a Sleeper league ID (digits only): ", lid)
suppressMessages({ library(httr2); library(dplyr); library(purrr); library(tibble) })
source(file.path(app_dir, "R", "config.R")); source(file.path(app_dir, "R", "sleeper_api.R"))
lg <- get_league(lid)
if (is.null(lg$name)) stop("Sleeper has no league with ID ", lid)
cat(sprintf("League: %s (%s), season %s, status %s\n", lg$name, lid, lg$season, lg$status))
cat(sprintf("  teams at the start: %s | best ball: %s | roster_positions: %s\n", lg$total_rosters, if (isTRUE(lg$settings$best_ball == 1)) "yes" else "NO", paste(unlist(lg$roster_positions), collapse = " ")))
cat(sprintf("  guillotine settings: last_chopped_leg=%s, last_scored_leg=%s, disable_elimination=%s\n", lg$settings$last_chopped_leg %||% "none", lg$settings$last_scored_leg %||% "none", lg$settings$disable_elimination %||% "none"))
# chops per week, from how many rosters were eliminated in each leg so far (Sleeper stores the leg number in roster settings$eliminated)
ros <- get_rosters(lid)
el <- vapply(ros, function(r) as.numeric(r$settings$eliminated %||% 0), numeric(1)); el <- el[el > 0]
detected <- if (length(el)) as.integer(names(which.max(table(table(el)))) ) else NA_integer_   # the most common number of eliminations in a leg
cfg_chops <- as.integer(get0("N_CHOPS_PER_WEEK", envir = cfg, ifnotfound = 1L))
cat(sprintf("  eliminated so far: %d rosters over %d legs; chops per week seen: %s | config N_CHOPS_PER_WEEK = %d\n", length(el), length(unique(el)), if (is.na(detected)) "none yet" else detected, cfg_chops))
if (!is.na(detected) && detected != cfg_chops) stop(sprintf("the league chops %d team(s) a week but the config says N_CHOPS_PER_WEEK <- %dL: fix %s", detected, cfg_chops, basename(cfg_path)))
ld <- derive_lineup(lg$roster_positions)           # stops with a clear message for K / DEF / IDP slots
cat(sprintf("  the app will use %d lineup slots: %s\n", length(ld$slots), paste(ld$slots, collapse = " ")))
if (!isTRUE(lg$settings$best_ball == 1)) warning("this league is not best ball; the app assumes best ball")
if (!length(el)) warning("no team has been eliminated yet in this league, so the chops per week cannot be checked")

# 2. stage ----------------------------------------------------------------------------------------
stage <- file.path(dirname(app_dir), paste0("guillotine_app_", name, "_deploy"))
marker <- file.path(stage, ".staged_by_deploy_league")
if (dir.exists(stage)) { if (!file.exists(marker)) stop("refusing to overwrite ", stage, ": it was not created by this script"); unlink(stage, recursive = TRUE) }
dir.create(file.path(stage, "R"), recursive = TRUE)
runtime_r <- c("config", "sleeper_api", "sleeper_projections", "espn_api", "stat_scoring", "bestball", "simulation", "distributions", "history", "fallback", "pipeline", "logging")
copy1 <- function(from, to) { dir.create(dirname(to), showWarnings = FALSE, recursive = TRUE); ok <- file.copy(from, to, copy.date = TRUE); if (!ok) stop("could not copy ", from); invisible(ok) }
copy1(file.path(app_dir, "app.R"), file.path(stage, "app.R"))
for (f in runtime_r) copy1(file.path(app_dir, "R", paste0(f, ".R")), file.path(stage, "R", paste0(f, ".R")))
if (dir.exists(file.path(app_dir, "www"))) for (f in list.files(file.path(app_dir, "www"), full.names = TRUE)) copy1(f, file.path(stage, "www", basename(f)))
copy1(cfg_path, file.path(stage, "league_config.R"))
need <- "cache/backtest/calibration_summary_topn.rds"            # required by R/simulation.R
if (!file.exists(file.path(app_dir, need))) stop("missing ", need, " (the residual-shape file the simulation needs)")
copy1(file.path(app_dir, need), file.path(stage, need))
for (f in c("cache/sleeper_players.rds")) if (file.exists(file.path(app_dir, f))) copy1(file.path(app_dir, f), file.path(stage, f))   # saves a cold-start download
writeLines(c("Staged by deploy_league.R from the app folder. Do not edit: re-run the script instead.", format(Sys.time())), marker)
files <- list.files(stage, recursive = TRUE, all.files = TRUE)
cat(sprintf("\nStaged %d files (%.1f MB) in %s\n", length(files), sum(file.size(file.path(stage, files))) / 1e6, stage))
bad <- grep("^(rsconnect|logs|data|recap|collector|analysis|accuracy_data)/", files, value = TRUE)
if (length(bad)) stop("staging copied files that must not be there: ", paste(head(bad, 5), collapse = ", "))

# 3. deploy command ---------------------------------------------------------------------------------
cmd <- sprintf('rsconnect::deployApp(appDir = "%s", appName = "%s", account = "%s", forceUpdate = %s)', stage, name, account, if (do_update) "TRUE" else "FALSE")
if (do_deploy) { cat("\nDeploying:\n  ", cmd, "\n"); eval(parse(text = cmd)) } else cat("\nNothing was published. To publish, run in R:\n  ", cmd, "\n", sep = "")
