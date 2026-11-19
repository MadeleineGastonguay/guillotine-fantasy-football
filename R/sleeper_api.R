# --- Sleeper API -------------------------------------------------------
# Public, unauthenticated API. Docs: https://docs.sleeper.com/
# Sleeper computes each player's fantasy points live during games using the
# league's own scoring_settings, so "points so far" never needs to be
# recomputed from raw stats -- we only need Sleeper for that part.

library(httr2)
library(dplyr)
library(purrr)

SLEEPER_BASE <- "https://api.sleeper.app/v1"

sleeper_get <- function(path) {
  resp <- request(paste0(SLEEPER_BASE, path)) |>
    req_user_agent("guillotine-tracker/0.1") |>
    req_perform()
  resp_body_json(resp, simplifyVector = FALSE)
}

get_nfl_state <- function() {
  sleeper_get("/state/nfl")
}

get_league <- function(league_id = LEAGUE_ID) {
  sleeper_get(paste0("/league/", league_id))
}

get_rosters <- function(league_id = LEAGUE_ID) {
  sleeper_get(paste0("/league/", league_id, "/rosters"))
}

get_users <- function(league_id = LEAGUE_ID) {
  sleeper_get(paste0("/league/", league_id, "/users"))
}

get_matchups <- function(league_id = LEAGUE_ID, week) {
  sleeper_get(paste0("/league/", league_id, "/matchups/", week))
}

get_transactions <- function(league_id = LEAGUE_ID, week) {
  sleeper_get(paste0("/league/", league_id, "/transactions/", week))
}

# Full NFL player dictionary (several MB). Sleeper asks that this only be
# pulled a few times a day, so we cache it to disk.
get_players_db <- function(force_refresh = FALSE) {
  dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

  is_stale <- TRUE
  if (file.exists(PLAYERS_CACHE_FILE)) {
    age_hours <- as.numeric(difftime(Sys.time(), file.info(PLAYERS_CACHE_FILE)$mtime, units = "hours"))
    is_stale <- age_hours > PLAYERS_CACHE_MAX_AGE_HOURS
  }

  if (force_refresh || is_stale || !file.exists(PLAYERS_CACHE_FILE)) {
    message("Refreshing Sleeper player dictionary (this is a big download)...")
    raw <- sleeper_get("/players/nfl")
    saveRDS(raw, PLAYERS_CACHE_FILE)
  }
  readRDS(PLAYERS_CACHE_FILE)
}

# owner_id -> owner_name/team_name/avatar_url, shared by build_roster_table()
# and get_eliminated_teams() below.
build_user_lookup <- function(users) {
  map_dfr(users, function(u) {
    # metadata$avatar (when set) is already a full CDN URL for a custom
    # uploaded avatar image; otherwise fall back to Sleeper's default avatar
    # hash (top-level `avatar`), which needs the thumbs/ CDN path prefixed.
    meta_avatar <- u$metadata$avatar
    avatar_url <- if (!is.null(meta_avatar) && nzchar(meta_avatar)) {
      meta_avatar
    } else if (!is.null(u$avatar) && nzchar(u$avatar)) {
      paste0("https://sleepercdn.com/avatars/thumbs/", u$avatar)
    } else {
      NA_character_
    }

    tibble::tibble(
      owner_id = u$user_id,
      owner_name = u$display_name %||% u$user_id,
      team_name = u$metadata$team_name %||% u$display_name %||% paste("Team", u$user_id),
      avatar_url = avatar_url
    )
  })
}

# Teams already chopped from the guillotine. Sleeper marks this via
# roster$settings$eliminated == 1 -- an eliminated roster's player list is
# also emptied out at the same time, which is why build_roster_table() below
# never sees it (and why an eliminated team's roster_id can seem to just
# vanish from every other table in this app). final_points is that roster's
# settings$fpts(.fpts_decimal) as of elimination -- the score that got them
# chopped, not a live number that updates afterward.
get_eliminated_teams <- function(league_id = LEAGUE_ID, current_week = 1) {
  rosters <- get_rosters(league_id)
  user_lookup <- build_user_lookup(get_users(league_id))

  # roster$settings$eliminated is NOT a 0/1 flag -- confirmed directly against
  # a real week-2 chop that this original `== 1` check silently missed: it was
  # 2 for that roster, matching the week they were chopped (leg 2 below). Any
  # nonzero value means "eliminated"; the actual chop WEEK still comes from
  # the "chopped" transaction walk below, which is the authoritative source
  # (this field looks like it happens to mirror that week, but transactions
  # are what Sleeper's own UI is built from).
  elim_rows <- map_dfr(rosters, function(r) {
    if (!isTRUE((r$settings$eliminated %||% 0) > 0)) return(NULL)
    tibble::tibble(
      roster_id = r$roster_id,
      owner_id = r$owner_id,
      # settings$fpts/fpts_decimal are the roster's CUMULATIVE season points,
      # not the score they were chopped with -- fine for a roster eliminated
      # in week 1 (cumulative == that one week), silently wrong from week 2
      # on (confirmed: a real roster's cumulative 195.12 vs. their actual
      # chop-week score of ~87.7). The real chop-week score is filled in
      # below, once chopped_week is known, from that week'"'"'s matchup points.
      cumulative_points = (r$settings$fpts %||% 0) + (r$settings$fpts_decimal %||% 0) / 100
    )
  })

  # map_dfr(all-NULL) collapses to a 0-column tibble, which left_join() below
  # would choke on (no owner_id column to join by) -- return the properly
  # shaped empty result directly instead.
  if (nrow(elim_rows) == 0) {
    return(tibble::tibble(
      roster_id = integer(), owner_id = character(), final_points = double(),
      chopped_week = integer(),
      owner_name = character(), team_name = character(), avatar_url = character()
    ))
  }

  # Roster settings say WHO was eliminated but not WHEN -- that's only
  # recorded as a "chopped"-type transaction, tagged with the week ("leg")
  # it happened in. Walk transaction history week by week (each call is a
  # small, cheap payload) until every eliminated roster found above has a
  # matching chop week, or every week played so far has been checked.
  chop_weeks <- setNames(rep(NA_integer_, nrow(elim_rows)), elim_rows$roster_id)
  for (wk in seq_len(max(current_week, 1))) {
    if (!any(is.na(chop_weeks))) break
    tx <- tryCatch(get_transactions(league_id, wk), error = function(e) NULL)
    for (t in tx) {
      if (identical(t$type, "chopped")) {
        for (rid in unlist(t$roster_ids)) {
          key <- as.character(rid)
          if (key %in% names(chop_weeks)) chop_weeks[[key]] <- t$leg %||% wk
        }
      }
    }
  }
  elim_rows$chopped_week <- unname(chop_weeks[as.character(elim_rows$roster_id)])

  # The actual score they were chopped with: that roster's points in their
  # own chop week's matchup, not the cumulative season total above. Falls
  # back to the cumulative figure only if the chop week itself is unknown.
  elim_rows$final_points <- vapply(seq_len(nrow(elim_rows)), function(i) {
    wk <- elim_rows$chopped_week[i]
    if (is.na(wk)) return(elim_rows$cumulative_points[i])
    wk_matchups <- tryCatch(get_matchups(league_id, wk), error = function(e) NULL)
    pts <- purrr::detect(wk_matchups, ~ .x$roster_id == elim_rows$roster_id[i])$points
    pts %||% elim_rows$cumulative_points[i]
  }, numeric(1))

  # Newest chop first (the graveyard reads like a timeline); a roster whose chop
  # week couldn't be determined sorts last.
  elim_rows |>
    left_join(user_lookup, by = "owner_id") |>
    arrange(desc(chopped_week), final_points)
}

# Tidy table: one row per rostered player, joined to team/owner info.
# Columns: roster_id, team_name, owner_name, player_id, full_name, position,
#          nfl_team
build_roster_table <- function(league_id = LEAGUE_ID, players_db = get_players_db(), membership = NULL) {
  rosters <- get_rosters(league_id)
  users <- get_users(league_id)
  user_lookup <- build_user_lookup(users)

  # membership (optional): a data frame of roster_id + player_id to use INSTEAD of the rosters' current players, e.g. the week preview's
  # reconstruction of the rosters before this morning's waivers. Left NULL (the app's case) it is the current rosters, as always.
  map_dfr(rosters, function(r) {
    player_ids <- if (is.null(membership)) unlist(r$players %||% list()) else membership$player_id[membership$roster_id == r$roster_id]
    if (length(player_ids) == 0) return(NULL)

    tibble::tibble(
      roster_id = r$roster_id,
      owner_id = r$owner_id,
      player_id = player_ids
    )
  }) |>
    left_join(user_lookup, by = "owner_id") |>
    mutate(
      full_name = map_chr(player_id, function(pid) {
        p <- players_db[[pid]]
        if (is.null(p)) return(pid)
        p$full_name %||% paste(p$first_name, p$last_name)
      }),
      # Sleeper's main position, unless that is not a fantasy position here but another listed one is: Travis Hunter is
      # DB first and WR second, and counts as a WR in this league (his old behavior was to drop off the roster entirely).
      position = map_chr(player_id, function(pid) {
        p <- players_db[[pid]]
        opts <- c(p$position, unlist(p$fantasy_positions))
        opts <- opts[opts %in% c("QB", "RB", "WR", "TE")]
        if (length(opts)) opts[[1]] else p$position %||% NA_character_
      }),
      nfl_team = map_chr(player_id, function(pid) players_db[[pid]]$team %||% NA_character_),
      # Sleeper's injury designation (Questionable / Doubtful / Out / IR ...);
      # NA when healthy. Comes from the cached players DB, so it is up to
      # PLAYERS_CACHE_MAX_AGE_HOURS old -- so in-game "Out" news comes from ESPN (espn_live_injuries()), not from here.
      injury_status = map_chr(player_id, function(pid) players_db[[pid]]$injury_status %||% NA_character_),
      # Sleeper's depth-chart rank within his position slot (1 = starter); NA if not on one.
      depth_chart_order = map_int(player_id, function(pid) as.integer(players_db[[pid]]$depth_chart_order %||% NA_integer_))
    ) |>
    filter(position %in% c("QB", "RB", "WR", "TE"))
}

`%||%` <- function(a, b) if (is.null(a)) b else a
