# --- Helpers for the scheduled pre-kickoff snapshot job -------------------
# (collector/snapshot_pregame.R). One job run freezes what Sleeper/ESPN say
# about every QB/RB/WR/TE in this week's games BEFORE kickoff -- the true
# pre-game projection, depth chart, injury status and betting/weather
# context -- so later analysis doesn't have to trust an archived projection
# that might have been revised after the games. Files are append-only (a new
# timestamped file per run), never overwritten.

PREGAME_DIR <- file.path("accuracy_data", "pregame")

# Retry a flaky network call a couple of times before giving up (Wi-Fi drops,
# transient 5xx). Returns the value, or stops with the last error.
with_retry <- function(fn, tries = 3, wait_seconds = 4) {
  last_err <- NULL
  for (i in seq_len(tries)) {
    res <- tryCatch(fn(), error = function(e) { last_err <<- e; NULL })
    if (!is.null(res)) return(res)
    if (i < tries) Sys.sleep(wait_seconds * i)
  }
  stop(last_err)
}

# One row per QB/RB/WR/TE on an NFL team, with the fields the accuracy work
# needs: depth chart, injury/practice, and the ids that link Sleeper to
# nflverse (gsis_id) and ESPN (espn_id).
slim_players_table <- function(players_db = get_players_db()) {
  keep <- c("QB", "RB", "WR", "TE")
  purrr::map_dfr(players_db, function(p) {
    if (!(p$position %||% "") %in% keep || is.null(p$team)) return(NULL)
    tibble::tibble(
      player_id = as.character(p$player_id %||% NA_character_),
      gsis_id = as.character(p$gsis_id %||% NA_character_),
      espn_id = as.character(p$espn_id %||% NA_character_),
      full_name = p$full_name %||% paste(p$first_name, p$last_name),
      position = p$position,
      nfl_team = p$team,
      depth_chart_order = as.integer(p$depth_chart_order %||% NA_integer_),
      depth_chart_position = as.character(p$depth_chart_position %||% NA_character_),
      injury_status = as.character(p$injury_status %||% NA_character_),
      practice_participation = as.character(p$practice_participation %||% NA_character_),
      sleeper_status = as.character(p$status %||% NA_character_)
    )
  })
}

# Raw projected stat lines to keep alongside the rescored points, so a later
# analysis can see *why* a projection was what it was (Sleeper's projection
# feed uses the same field names as the league's scoring settings).
PROJECTED_STAT_FIELDS <- c("pass_att", "pass_cmp", "pass_yd", "pass_td", "pass_int",
                           "rush_att", "rush_yd", "rush_td", "rec", "rec_yd", "rec_td", "fum_lost")

build_pregame_snapshot <- function(league_id = LEAGUE_ID) {
  state <- with_retry(get_nfl_state)
  season <- state$season
  week <- state$week
  scoring <- with_retry(function() get_league(league_id))$scoring_settings

  games <- with_retry(function() espn_get_scoreboard_detail(season, week, state$season_type))
  projections <- with_retry(function() get_sleeper_projections(season, week))
  players <- slim_players_table()
  # Sleeper's own gsis_id is missing for most players; fill from the crosswalk
  # so the snapshot can be joined to nflverse later.
  xw <- tryCatch(load_id_crosswalk(), error = function(e) NULL)
  if (!is.null(xw)) {
    players <- players |>
      dplyr::left_join(dplyr::select(xw, player_id = sleeper_id, gsis_xw = gsis_id, pfr_id), by = "player_id") |>
      dplyr::mutate(gsis_id = dplyr::coalesce(gsis_id, gsis_xw)) |>
      dplyr::select(-gsis_xw)
  }

  now <- Sys.time()
  proj_tbl <- tibble::tibble(
    player_id = names(projections),
    pre_proj = purrr::map_dbl(projections, function(s) pmax(score_stats(s, scoring), 0))
  )
  for (f in PROJECTED_STAT_FIELDS) {
    proj_tbl[[paste0("proj_", f)]] <- purrr::map_dbl(projections, function(s) {
      v <- s[f]; if (length(v) == 0 || is.na(v)) NA_real_ else unname(v)
    })
  }

  snap <- players |>
    dplyr::inner_join(dplyr::distinct(games, nfl_team, .keep_all = TRUE), by = "nfl_team") |>
    dplyr::left_join(proj_tbl, by = "player_id") |>
    dplyr::mutate(
      has_projection = !is.na(pre_proj),
      pre_proj = dplyr::coalesce(pre_proj, 0),
      pre_sd = full_game_sd_underdog(position, pre_proj),
      pre_cv = dplyr::if_else(pre_proj > 0, pre_sd / pre_proj, NA_real_),
      snapshot_utc = format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      season = season, week = week,
      taken_after_kickoff = state != "pre" | (!is.na(kickoff_utc) & now >= as.POSIXct(kickoff_utc, format = "%Y-%m-%dT%H:%MZ", tz = "UTC"))
    ) |>
    dplyr::select(
      snapshot_utc, season, week, player_id, gsis_id, espn_id, dplyr::any_of("pfr_id"), full_name, position, nfl_team,
      opponent, home_away, matchup, matchup_label, game_id, kickoff_utc, game_state = state, game_clock,
      taken_after_kickoff, has_projection, pre_proj, pre_sd, pre_cv, dplyr::starts_with("proj_"),
      depth_chart_order, depth_chart_position, injury_status, practice_participation, sleeper_status,
      team_spread, game_total, implied_team_total, weather, temperature, indoor
    )
  list(snapshot = snap, season = season, week = week, games = games, now = now)
}

write_pregame_snapshot <- function(res, dir = PREGAME_DIR) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  stamp <- format(res$now, "%Y%m%dT%H%M%SZ", tz = "UTC")
  path <- file.path(dir, sprintf("%s_w%02d_%s.csv.gz", res$season, res$week, stamp))
  if (file.exists(path)) stop("refusing to overwrite existing snapshot: ", path)
  # Write to a temp name first so an interrupted run never leaves a
  # half-written file that looks like a real snapshot.
  tmp <- paste0(path, ".tmp")
  readr::write_csv(res$snapshot, tmp)
  file.rename(tmp, path)
  path
}
