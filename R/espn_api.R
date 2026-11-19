# --- ESPN scoreboard (game clock / status) ---------------------------------
# Sleeper provides actual scoring and projections (see sleeper_api.R and
# sleeper_projections.R) but not fine-grained live game clock. This is the
# one thing we still pull from ESPN: which NFL games are upcoming, live, or
# final, and -- while live -- roughly how much game time is left, so we can
# tell how far through a player's game we are. Team-level only; no
# per-player ESPN id crosswalk needed anymore.

library(httr2)

ESPN_SCOREBOARD_URL <- "https://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard"
ESPN_SUMMARY_URL <- "https://site.api.espn.com/apis/site/v2/sports/football/nfl/summary"
ESPN_INJURIES_URL <- "https://site.api.espn.com/apis/site/v2/sports/football/nfl/injuries"   # league-wide, ~800 rows, one light call
ESPN_TEAMS_URL <- "https://site.api.espn.com/apis/site/v2/sports/football/nfl/teams"

# Common ESPN <-> Sleeper team abbreviation mismatches.
TEAM_ABBR_ALIASES <- c(WSH = "WAS", JAX = "JAX", LAR = "LAR", LAC = "LAC", LV = "LV")
normalize_team_abbr <- function(x) {
  x <- toupper(x)
  ifelse(x %in% names(TEAM_ABBR_ALIASES), TEAM_ABBR_ALIASES[x], x)
}

ESPN_SEASON_TYPE <- c(pre = 1, regular = 2, post = 3)

# Returns a tibble: nfl_team, status ("STATUS_SCHEDULED"/"STATUS_IN_PROGRESS"/
# "STATUS_HALFTIME"/"STATUS_FINAL"/...), state (ESPN's own broader
# "pre"/"in"/"post" bucket -- use THIS, not the granular status name, for any
# "is this game currently live" check: `status` has more values than just
# STATUS_IN_PROGRESS while a game is underway -- STATUS_HALFTIME is the
# common one, confirmed directly against a real halftime game, where it
# silently made a team disappear from Live Games and could stop autorefresh
# entirely if every live game happened to be at halftime at once, since
# every "is it live" check here used to match STATUS_IN_PROGRESS by exact
# string), elapsed_fraction (0 = hasn't started, 1 = final, in-between =
# live estimate based on clock/period -- this part already handled halftime
# correctly: period=2/clock=0:00 at halftime evaluates to exactly 0.5, so no
# change needed there), and matchup_label -- e.g. "vs DAL" (home) or "@ DAL"
# (away), the same home/away notation Sleeper's own app uses -- replacing a
# bare team abbreviation with who they're actually playing this week.
#
# season/week/season_type (from Sleeper's own get_nfl_state(), the source of
# truth for what week the league is actually on) MUST be passed explicitly.
# Called with no query params, this endpoint's own "current week" default is
# not reliable -- observed returning the prior, fully-completed week on a
# game-less day (e.g. a Tuesday) instead of the upcoming week. That silently
# marked every player as STATUS_FINAL/elapsed_fraction=1 a week early,
# zeroing every team's remaining projection and turning the elimination sim
# into an 18-way tie (which which.min() resolves to whichever team happens
# to be first in the roster list, not a real result).
# Share of the game played, from ESPN's period and clock (0 = not started, 1 = over). Overtime (period 5
# or later) used to push the plain formula past 60 minutes, which was capped to exactly 1: a game still being
# played showed 100% complete, its players lost their remaining spread (their distribution collapsed to a
# line) and a team whose players were all in overtime counted as finished. A game that is not final never
# reaches 1 now. In overtime the time left is the overtime clock itself (ESPN counts it down from the real
# length, 10 minutes in the regular season and 15 in the playoffs), as a share of a 60-minute game, which is
# the scale the player rates are on. It is never taken below 0.5% of a game, so a game still waiting for ESPN
# to mark it final is not treated as over.
espn_elapsed_fraction <- function(status, state, period, clock) {
  if (identical(status, "STATUS_FINAL")) return(1)
  if (status %in% c("STATUS_SCHEDULED", "STATUS_POSTPONED") || identical(state, "pre") || period == 0) return(0)
  mins_sec <- suppressWarnings(as.numeric(strsplit(clock, ":")[[1]]))
  clock_min <- if (length(mins_sec) == 2 && !anyNA(mins_sec)) mins_sec[1] + mins_sec[2] / 60 else 15
  if (period > 4) return(1 - max(0.005, min(clock_min, 15) / 60))
  pmax(0, pmin(0.999, ((period - 1) * 15 + (15 - clock_min)) / 60))
}

# Time actually played, in regulation games (1 = 60 minutes), for judging a player's pace: points so far divided
# by this gives points per 60 minutes, the unit the pre-game projection is in. Same as espn_elapsed_fraction()
# until overtime, then it keeps growing past 1 (65 minutes played = 1.083) while espn_elapsed_fraction() stays
# below 1 because it is what sets the time LEFT. Overtime periods are 10 minutes in the regular season and 15 in
# the playoffs (ESPN's clock starts from that length).
espn_played_fraction <- function(status, state, period, clock, season_type = "regular") {
  if (identical(status, "STATUS_FINAL") || status %in% c("STATUS_SCHEDULED", "STATUS_POSTPONED") || identical(state, "pre") || period == 0) {
    return(espn_elapsed_fraction(status, state, period, clock))   # nothing left to judge pace for / not started
  }
  if (period <= 4) return(espn_elapsed_fraction(status, state, period, clock))
  mins_sec <- suppressWarnings(as.numeric(strsplit(clock, ":")[[1]]))
  clock_min <- if (length(mins_sec) == 2 && !anyNA(mins_sec)) mins_sec[1] + mins_sec[2] / 60 else 0
  ot_len <- if (identical(season_type, "post")) 15 else 10
  (60 + (period - 5) * ot_len + (ot_len - min(clock_min, ot_len))) / 60
}

espn_get_game_status <- function(season, week, season_type = "regular") {
  # Note: this endpoint's bot-detection seems to key off a browser-like
  # User-Agent (paradoxically blocks it) but allows a plain/default one, so
  # we deliberately don't set req_user_agent() here.
  resp <- request(ESPN_SCOREBOARD_URL) |>
    req_url_query(
      year = season,
      week = week,
      seasontype = unname(ESPN_SEASON_TYPE[[season_type]] %||% ESPN_SEASON_TYPE[["regular"]])
    ) |>
    req_perform()
  body <- resp_body_json(resp, simplifyVector = FALSE)

  rows <- purrr::map_dfr(body$events, function(ev) {
    comp <- ev$competitions[[1]]
    status <- comp$status$type$name %||% ev$status$type$name
    state <- comp$status$type$state %||% ev$status$type$state
    period <- comp$status$period %||% 1
    clock <- comp$status$displayClock %||% "15:00"

    elapsed_fraction <- espn_elapsed_fraction(status, state, as.numeric(period), clock)
    played_fraction <- espn_played_fraction(status, state, as.numeric(period), clock, season_type)

    teams <- purrr::map_dfr(comp$competitors, function(c) {
      tibble::tibble(
        nfl_team = normalize_team_abbr(c$team$abbreviation),
        home_away = c$homeAway
      )
    })
    # Exactly 2 competitors per game; each team's opponent is just the other row.
    teams$opponent <- rev(teams$nfl_team)
    teams$matchup_label <- ifelse(teams$home_away == "home",
                                    paste("vs", teams$opponent),
                                    paste("@", teams$opponent))
    teams$home_away <- NULL
    teams$opponent <- NULL
    teams$game_id <- ev$id
    teams$status <- status
    teams$state <- state
    teams$elapsed_fraction <- elapsed_fraction
    teams$played_fraction <- played_fraction
    teams$period <- as.numeric(period)   # 5 or more = overtime (the Live Games tab shows "OT" and the clock)
    teams$clock <- clock
    teams$kickoff_utc <- ev$date %||% NA_character_   # ISO time ESPN schedules the game for, e.g. "2026-10-30T00:15Z"; the app shows it for games not yet started
    teams
  })

  rows
}

# --- ESPN in-game injury list --------------------------------------------------
# Sleeper's player dictionary (where its injury_status lives) is a ~14 MB file it asks apps to pull
# about once a day, so it can't carry "he just went Out" news during a game. ESPN's per-game summary
# (~600 KB) has an `injuries` list per team (status Out / Questionable, body part, timestamp) that is
# updated during the game -- in a live check ESPN flagged a receiver Out ~18 minutes before Sleeper
# did. Only live games are fetched.
#
# Returns a tibble: nfl_team, player_name, espn_status, injury_type, reported_at (UTC string).
# Empty on any failure (the caller then just relies on Sleeper's status).
empty_espn_injuries <- function() {
  tibble::tibble(nfl_team = character(), player_name = character(), espn_status = character(),
                 injury_type = character(), reported_at = character())
}

espn_get_game_injuries <- function(game_id) {
  resp <- request(ESPN_SUMMARY_URL) |> req_url_query(event = game_id) |> req_timeout(15) |> req_perform()
  body <- resp_body_json(resp, simplifyVector = FALSE)
  purrr::map_dfr(body$injuries %||% list(), function(tm) {
    purrr::map_dfr(tm$injuries %||% list(), function(i) {
      tibble::tibble(
        nfl_team = normalize_team_abbr(tm$team$abbreviation %||% NA_character_),
        player_name = i$athlete$fullName %||% i$athlete$displayName %||% NA_character_,
        espn_status = i$status %||% NA_character_,
        injury_type = i$details$type %||% NA_character_,
        reported_at = i$date %||% NA_character_
      )
    })
  })
}

# ESPN's league-wide injury list, for the games that are NOT live yet. Sleeper's own status comes from a player dictionary that is
# only re-pulled once a day (and on shinyapps.io the copy bundled with a deploy counts as fresh for the next 24 hours), so a player
# ruled out in the afternoon can still show Questionable for most of a day -- and a team's projection keeps counting him. ESPN's list
# is updated within minutes, so any report NEWER than Sleeper's copy (`since`, when that copy was downloaded) is taken from ESPN.
# Reports older than the copy are left to Sleeper, which already has them (or has since superseded them), so an old ESPN row can
# never override a fresher Sleeper status. Only statuses that matter are kept (Out, Doubtful, Questionable, IR, suspended); an ESPN
# "Active" is ignored -- the stale Sleeper tag then stays, which is display-only (only Out changes a projection).
# Same shape as espn_live_injuries(); empty on any failure.
ESPN_STATUS_MAP <- c(Out = "Out", Doubtful = "Doubtful", Questionable = "Questionable", `Injured Reserve` = "IR", `Day-To-Day` = "Questionable", Suspension = "Sus")

espn_team_abbr_lookup <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    r <- request(ESPN_TEAMS_URL) |> req_timeout(15) |> req_perform() |> resp_body_json(simplifyVector = FALSE)
    tms <- r$sports[[1]]$leagues[[1]]$teams
    cache <<- stats::setNames(vapply(tms, function(x) normalize_team_abbr(x$team$abbreviation), character(1)), vapply(tms, function(x) as.character(x$team$id), character(1)))
    cache
  }
})

espn_recent_injuries <- function(since) {
  if (is.null(since) || is.na(since)) return(empty_espn_injuries())
  r <- request(ESPN_INJURIES_URL) |> req_timeout(15) |> req_perform() |> resp_body_json(simplifyVector = FALSE)
  abbr <- espn_team_abbr_lookup()
  out <- purrr::map_dfr(r$injuries %||% list(), function(tm) {
    purrr::map_dfr(tm$injuries %||% list(), function(i) {
      tibble::tibble(
        nfl_team = unname(abbr[as.character(tm$id)]),
        player_name = i$athlete$displayName %||% NA_character_,
        espn_status = unname(ESPN_STATUS_MAP[i$status %||% ""]),
        injury_type = i$details$type %||% NA_character_,
        reported_at = i$date %||% NA_character_
      )
    })
  })
  at <- as.POSIXct(out$reported_at, format = "%Y-%m-%dT%H:%MZ", tz = "UTC")
  out[!is.na(out$espn_status) & !is.na(at) & at > since, , drop = FALSE]
}

# The plays of one game with their real-world time, quarter and game clock (ESPN's summary: drives -> plays carry `wallclock`, `period` and `clock`), in order.
# Used to say WHEN in the game a time-stamped report was made: "early in the 3rd quarter" reads better than "112 minutes after kickoff".
espn_game_plays <- function(game_id) {
  s <- request("https://site.api.espn.com/apis/site/v2/sports/football/nfl/summary") |> req_url_query(event = game_id) |> req_timeout(20) |> req_perform() |> resp_body_json(simplifyVector = FALSE)
  dr <- c(s$drives$previous %||% list(), if (!is.null(s$drives$current)) list(s$drives$current) else list())
  d <- purrr::map_dfr(dr, function(x) purrr::map_dfr(x$plays %||% list(), function(p)
    tibble::tibble(at = as.POSIXct(p$wallclock %||% NA_character_, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), period = as.integer(p$period$number %||% NA), clock = p$clock$displayValue %||% NA_character_)))
  d <- d[!is.na(d$at) & !is.na(d$period), ]
  d[order(d$at), ]
}

# Where in the game the time `at` falls, in words: the last play before it gives the quarter and clock ("early in the 3rd quarter" = first five minutes, "late" = last
# five), the gap between the 2nd and 3rd quarters is "at halftime", the gap between other quarters "between the 1st and 2nd quarters", and 5+ is "in overtime".
espn_game_moment <- function(at, plays) {
  if (is.null(plays) || !nrow(plays) || is.na(at)) return(NA_character_)
  i <- which(plays$at <= at); if (!length(i)) return("before the game")
  i <- max(i); p <- plays[i, ]; nx <- if (i < nrow(plays)) plays[i + 1, ] else NULL
  ord <- function(n) c("1st", "2nd", "3rd", "4th")[n]
  if (!is.null(nx) && nx$period > p$period && p$period <= 4) return(if (p$period == 2) "at halftime" else sprintf("between the %s and %s quarters", ord(p$period), ord(p$period + 1L)))
  if (p$period > 4) return("in overtime")
  secs <- tryCatch({ z <- as.numeric(strsplit(p$clock, ":")[[1]]); z[1] * 60 + z[2] }, error = function(e) NA_real_)
  pre <- if (is.na(secs)) "" else if (secs >= 600) "early in the " else if (secs <= 300) "late in the " else "in the "
  paste0(pre, ord(p$period), " quarter")
}

# Injury reports ESPN timestamped DURING a game (from kickoff to `window_min` minutes after it): players who went down or were ruled out mid-game.
# ESPN keeps these on a finished game's summary, so a recap can be built afterward with no extra logging. Pregame reports (made before kickoff) are
# excluded -- those are the ordinary Out / Questionable designations. One row per report: game_id, matchup, nfl_team, player_name, status (Out /
# Doubtful / Questionable / IR), reported_at (UTC), minutes_after_kickoff. Cached on disk per week once every game of the week is final.
# The injury TYPE ESPN gives is deliberately not returned: the recaps never name an injury, and sources disagree on it anyway.
espn_game_exit_reports <- function(season, week, season_type = "regular", window_min = 240) {
  cache <- file.path("cache", sprintf("espn_exits_%s_wk%02d.rds", season, as.integer(week)))
  if (file.exists(cache)) return(readRDS(cache))
  sb <- request(ESPN_SCOREBOARD_URL) |>
    req_url_query(year = season, week = week, seasontype = unname(ESPN_SEASON_TYPE[[season_type]] %||% ESPN_SEASON_TYPE[["regular"]])) |>
    req_timeout(20) |> req_perform() |> resp_body_json(simplifyVector = FALSE)
  all_final <- length(sb$events) > 0 && all(vapply(sb$events, function(e) identical((e$competitions[[1]]$status$type$state %||% e$status$type$state), "post"), logical(1)))
  out <- purrr::map_dfr(sb$events, function(e) {
    ko <- as.POSIXct(e$date, format = "%Y-%m-%dT%H:%MZ", tz = "UTC")
    r <- tryCatch(espn_get_game_injuries(e$id), error = function(err) empty_espn_injuries())
    if (!nrow(r)) return(NULL)
    at <- as.POSIXct(r$reported_at, format = "%Y-%m-%dT%H:%MZ", tz = "UTC")
    mins <- as.numeric(difftime(at, ko, units = "mins"))
    st <- unname(ESPN_STATUS_MAP[r$espn_status])
    keep <- !is.na(st) & !is.na(mins) & mins >= 0 & mins <= window_min
    if (!any(keep)) return(NULL)
    plays <- tryCatch(espn_game_plays(e$id), error = function(err) NULL)
    when <- vapply(at[keep], function(a) espn_game_moment(a, plays), character(1))   # e.g. "late in the 3rd quarter"
    tibble::tibble(game_id = e$id, matchup = e$shortName, nfl_team = r$nfl_team[keep], player_name = r$player_name[keep], status = st[keep],
                   reported_at = r$reported_at[keep], minutes_after_kickoff = round(mins[keep]), when = when)
  })
  if (!nrow(out)) out <- tibble::tibble(game_id = character(), matchup = character(), nfl_team = character(), player_name = character(), status = character(),
                                         reported_at = character(), minutes_after_kickoff = numeric(), when = character())
  if (all_final) { dir.create("cache", showWarnings = FALSE, recursive = TRUE); saveRDS(out, cache) }
  out
}

# game_status: the espn_get_game_status() tibble (needs game_id and state).
espn_live_injuries <- function(game_status) {
  ids <- unique(game_status$game_id[game_status$state == "in"])
  if (length(ids) == 0) return(empty_espn_injuries())
  purrr::map_dfr(ids, function(id) tryCatch(espn_get_game_injuries(id), error = function(e) empty_espn_injuries()))
}

# "Ja'Marr Chase" / "Kenneth Walker III" -> "jamarrchase" / "kennethwalker": the key used to match an ESPN
# athlete to a Sleeper player (Sleeper's espn_id is missing for over half of active skill players).
name_match_key <- function(x) {
  x <- tolower(iconv(x, to = "ASCII//TRANSLIT"))
  x <- gsub("\\b(jr|sr|ii|iii|iv|v)\\b\\.?", "", x)
  gsub("[^a-z]", "", x)
}

# Cheap check used to gate the app's auto-refresh: is any NFL game live
# right now? A single lightweight scoreboard call, no roster/projection
# data involved -- used to skip the expensive full pipeline + Monte Carlo
# refresh when nothing could have changed.
any_game_live <- function(season, week, season_type = "regular") {
  status <- tryCatch(espn_get_game_status(season, week, season_type), error = function(e) NULL)
  if (is.null(status) || nrow(status) == 0) return(FALSE)
  any(status$state == "in")
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# Game-level detail, one row per TEAM (so it joins to players on nfl_team): the
# basics espn_get_game_status() returns plus what the accuracy-measurement work
# needs -- game id, kickoff time, period/clock, both scores, matchup text
# ("NYG @ LAR"), and the betting/weather context ESPN attaches to each
# scoreboard event (spread, total, implied team total, weather, dome).
# Odds/weather can be missing for a game; those columns are then NA.
espn_get_scoreboard_detail <- function(season, week, season_type = "regular") {
  resp <- request(ESPN_SCOREBOARD_URL) |>
    req_url_query(
      year = season, week = week,
      seasontype = unname(ESPN_SEASON_TYPE[[season_type]] %||% ESPN_SEASON_TYPE[["regular"]])
    ) |>
    req_perform()
  body <- resp_body_json(resp, simplifyVector = FALSE)

  na_or <- function(x) if (is.null(x) || length(x) == 0) NA else x

  purrr::map_dfr(body$events, function(ev) {
    comp <- ev$competitions[[1]]
    status <- na_or(comp$status$type$name %||% ev$status$type$name)
    state <- na_or(comp$status$type$state %||% ev$status$type$state)
    period <- as.numeric(comp$status$period %||% 0)
    clock <- comp$status$displayClock %||% "0:00"
    elapsed_fraction <- espn_elapsed_fraction(status, state, period, clock)
    played_fraction <- espn_played_fraction(status, state, period, clock, season_type)

    odds <- if (length(comp$odds) > 0) comp$odds[[1]] else NULL
    home_spread <- as.numeric(na_or(odds$spread)) # negative = home favored
    total <- as.numeric(na_or(odds$overUnder))

    teams <- purrr::map_dfr(comp$competitors, function(c) {
      tibble::tibble(
        nfl_team = normalize_team_abbr(c$team$abbreviation),
        home_away = c$homeAway,
        score = as.numeric(c$score %||% 0)
      )
    })
    home <- teams$nfl_team[teams$home_away == "home"]
    away <- teams$nfl_team[teams$home_away == "away"]
    teams$opponent <- rev(teams$nfl_team)
    teams$opp_score <- rev(teams$score)
    teams$matchup <- paste(away, "@", home) # same text for both teams in the game
    teams$matchup_label <- ifelse(teams$home_away == "home", paste("vs", teams$opponent), paste("@", teams$opponent))
    teams$team_spread <- if (is.na(home_spread)) NA_real_ else ifelse(teams$home_away == "home", home_spread, -home_spread)
    teams$implied_team_total <- if (is.na(total) || is.na(home_spread)) NA_real_ else total / 2 - teams$team_spread / 2
    teams$game_id <- ev$id
    teams$kickoff_utc <- ev$date
    teams$status <- status
    teams$state <- state
    teams$period <- period
    teams$clock <- clock
    teams$game_clock <- if (period == 0) "Pre" else if (period > 4) sprintf("OT %s", clock) else sprintf("Q%d %s", as.integer(period), clock)
    teams$elapsed_fraction <- elapsed_fraction
    teams$played_fraction <- played_fraction
    teams$game_total <- total
    teams$weather <- na_or(ev$weather$displayValue)
    teams$temperature <- as.numeric(na_or(ev$weather$temperature))
    teams$indoor <- as.logical(na_or(comp$venue$indoor))
    teams
  })
}
