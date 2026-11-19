# --- Data pipeline: Sleeper (actuals + projections) + ESPN (game clock) ---
# Combines into per-roster data.frames ready for simulate_team_totals().
#
# For a player whose game hasn't started: remaining estimate = their
# pregame (Sleeper/RotoWire) projection, re-scored under this league's
# rules.
#
# For a player mid-game: rather than just decaying that frozen pregame
# number by time remaining (which ignores how the game is actually going),
# we blend the pregame rate with the pace they've actually shown so far,
# giving increasing weight to the observed pace as more of the game plays
# out (a simple shrinkage estimate) -- so a player who's blowing past their
# projection gets a bumped-up rest-of-game estimate, and one who's been
# quiet gets scaled down, instead of both just following the original
# pregame number on a clock-based glide path.
#
# For a player whose game is over: remaining = 0; their Sleeper actual is
# locked in as-is.
#
# Four variance models are computed side by side (see R/config.R for the
# sources): "backtest" (this league's own backtested a + b*projection fit),
# "underdog" and "playerprofiler" (flat published CVs per position), and
# "player" (the backtest model, adjusted per-player by a shrunk multiplier
# from that player's own history -- see analysis/backtest_player_variance.R).
# All four use the same sqrt(remaining_fraction) in-game decay assumption.

full_game_sd_backtest <- function(position, full_game_rate) {
  a <- vapply(position, function(p) (PROJECTION_ERROR_MODEL[[p]] %||% DEFAULT_ERROR_MODEL)[["a"]], numeric(1))
  b <- vapply(position, function(p) (PROJECTION_ERROR_MODEL[[p]] %||% DEFAULT_ERROR_MODEL)[["b"]], numeric(1))
  pmax(a + b * full_game_rate, 0)
}

full_game_sd_underdog <- function(position, full_game_rate) {
  cv <- dplyr::coalesce(UNDERDOG_CV[position], DEFAULT_UNDERDOG_CV)
  pmax(full_game_rate, 0) * unname(cv)
}

full_game_sd_playerprofiler <- function(position, full_game_rate) {
  cv <- dplyr::coalesce(PLAYERPROFILER_CV[position], DEFAULT_PLAYERPROFILER_CV)
  pmax(full_game_rate, 0) * unname(cv)
}

get_player_variance_lookup <- function() {
  if (!file.exists(PLAYER_VARIANCE_LOOKUP_FILE)) return(c())
  readRDS(PLAYER_VARIANCE_LOOKUP_FILE)
}

full_game_sd_player <- function(player_id, position, full_game_rate, lookup) {
  # no lookup file at all (a fresh deploy, another league's app) -> every player gets the plain position-level model; indexing NULL used to return nothing and break the refresh
  ratio <- if (length(lookup)) unname(lookup[player_id]) else rep(NA_real_, length(player_id))
  ratio[is.na(ratio)] <- 1.0 # no history (e.g. rookies) -> plain position-level model
  ratio * full_game_sd_backtest(position, full_game_rate)
}

# The per-player live-projection math, shared by the app (build_live_snapshot()
# below) and the retrospective replay (R/replay.R) so a replayed minute is
# computed EXACTLY the way the live app computes it.
#
# `df` needs: player_id, position, locked_points (points scored so far),
# elapsed_fraction (0 = not started, 1 = over) and full_week_projection (the
# pregame projection re-scored under this league's rules). Optional: played_fraction
# (time actually played in regulation games; goes past 1 in overtime, where elapsed_fraction stays below 1 and
# only sets the time left). The pace and its weight use it, so overtime counts as time played; default = elapsed_fraction. Returns df with
# remaining_fraction, has_live_signal, observed_rate, blend_weight,
# blended_rate, remaining_mean, the full-game / pregame / remaining SDs for
# each variance model, added.
add_live_projection_columns <- function(df, player_variance_lookup = get_player_variance_lookup()) {
  if (!"played_fraction" %in% names(df)) df$played_fraction <- df$elapsed_fraction
  df$played_fraction <- dplyr::coalesce(df$played_fraction, df$elapsed_fraction)
  df |>
    dplyr::mutate(
      remaining_fraction = 1 - elapsed_fraction,
      has_live_signal = elapsed_fraction > 0.02,
      observed_rate = dplyr::if_else(
        has_live_signal, locked_points / pmax(played_fraction, 0.02), NA_real_
      ),
      # Cubed rather than linear in elapsed_fraction: observed_rate is
      # extrapolated from a small, noisy in-game sample (a hot or cold
      # quarter), and a straight linear weight let that noise swing the
      # blended rate further than real-world live projections do -- checked
      # directly against 7 real Sleeper live projections (Sept 2026): most
      # wanted heavy damping (weight near 0, i.e. mostly trust the pregame
      # number even after a hot start), only one wanted near-full trust in
      # the observed pace, and two were literally impossible to hit under
      # ANY weight in [0,1] (Sleeper's own number sat below even 100%
      # pregame trust) -- so no fixed formula reproduces Sleeper exactly.
      # Cubing biases toward the heavier-damping side that most of those
      # real examples wanted, without pretending to match Sleeper's own
      # (more sophisticated, e.g. game-script-aware) methodology.
      blend_weight = pmin(played_fraction, 1)^3,   # full weight on the pace once a full game has been played (overtime)
      blended_rate = dplyr::if_else(
        has_live_signal,
        (1 - blend_weight) * full_week_projection + blend_weight * observed_rate,
        full_week_projection
      ),
      remaining_mean = blended_rate * remaining_fraction,
      # Full-game SD under each model, evaluated at the LIVE full-game rate
      # (blended_rate) rather than the frozen pregame projection -- so a
      # player running hot or cold gets a correspondingly wider or
      # narrower band, not just a smaller one as the clock runs down. Then
      # scaled down as the game plays out. There's no backtest data on
      # *how* variance decays within a game, so this scaling is a
      # documented assumption, not a fitted one: treating points as
      # accumulating from a roughly time-homogeneous process (whether many
      # small increments or a Poisson-like process of scoring events)
      # implies Var(remaining) scales with remaining time, i.e. SD scales
      # with sqrt(remaining_fraction) -- decaying slower than remaining_mean
      # itself does, so a game that's half over still carries most of its
      # uncertainty, not half of it. The weak point of that assumption is
      # time-homogeneity itself -- real NFL scoring isn't uniform across a
      # game (game script, garbage time, red-zone clustering) -- but it's
      # at least as defensible as any other simple shape absent real
      # intra-game backtest data, and it's the shape we've deliberately
      # decided to keep (see README).
      sd_full_game_backtest = full_game_sd_backtest(position, blended_rate),
      sd_full_game_underdog = full_game_sd_underdog(position, blended_rate),
      # Pre-game (frozen) full-game SD -- the "before any games" distribution
      # the player charts compare the current one against (R/distributions.R).
      pregame_sd_underdog = full_game_sd_underdog(position, full_week_projection),
      sd_full_game_playerprofiler = full_game_sd_playerprofiler(position, blended_rate),
      sd_full_game_player = full_game_sd_player(player_id, position, blended_rate, player_variance_lookup),
      remaining_sd_backtest = sd_full_game_backtest * sqrt(remaining_fraction),
      remaining_sd_underdog = sd_full_game_underdog * sqrt(remaining_fraction),
      remaining_sd_playerprofiler = sd_full_game_playerprofiler * sqrt(remaining_fraction),
      remaining_sd_player = sd_full_game_player * sqrt(remaining_fraction)
    )
}

# Injury status, merged from both sources, and the rest-of-week zeroing for Out players.
#   * Status shown (injury_status): for a player ESPN lists in a LIVE game (`espn_injuries`, see
#     espn_live_injuries()), ESPN's current status wins -- whether it is worse or better than Sleeper's
#     (e.g. Sleeper Out -> ESPN Questionable) -- so any in-game change shows up within a pull. The same goes for any
#     ESPN report NEWER than the Sleeper dictionary's download time, for every game, live or not (espn_recent_injuries():
#     a player ruled out in the afternoon showed Questionable all day otherwise). A player ESPN
#     does not list keeps Sleeper's status (refreshed daily): ESPN's list is NOT complete (checked: it omits
#     some players Sleeper has Out, e.g. long-term absences), so "not listed" must never mean healthy.
#   * A player whose resulting status is Out (ruled out pregame or hurt mid-game) will not score again: his
#     rest-of-week projection and its spread go to 0 and points he already scored (locked_points) stay.
#     Every other status is display-only and does not change the projection.
# Only the live app applies this; the replay (R/replay.R) has no historical injury feed.
merge_injury_status <- function(df, espn_injuries = empty_espn_injuries()) {
  e <- espn_injuries[!is.na(espn_injuries$espn_status) & nzchar(espn_injuries$espn_status), , drop = FALSE]
  e <- e[order(e$reported_at), , drop = FALSE]          # latest report last, so it is the one kept
  e <- e[!duplicated(paste(e$nfl_team, name_match_key(e$player_name)), fromLast = TRUE), , drop = FALSE]
  espn_status <- e$espn_status[match(paste(df$nfl_team, name_match_key(df$full_name)), paste(e$nfl_team, name_match_key(e$player_name)))]
  df$injury_status <- dplyr::coalesce(espn_status, df$injury_status)
  df
}

zero_out_players <- function(df, espn_injuries = empty_espn_injuries()) {
  df <- merge_injury_status(df, espn_injuries)
  out <- !is.na(df$injury_status) & df$injury_status %in% c("Out", "IR")   # a player on injured reserve cannot play, so he is zeroed like an Out player
  sd_cols <- c("remaining_sd_backtest", "remaining_sd_underdog", "remaining_sd_playerprofiler", "remaining_sd_player")
  df |>
    dplyr::mutate(
      ruled_out = out,
      remaining_mean = dplyr::if_else(out, 0, remaining_mean),
      dplyr::across(dplyr::all_of(sd_cols), ~ dplyr::if_else(out, 0, .x))
    )
}

build_live_snapshot <- function(league_id = LEAGUE_ID, membership = NULL) {   # membership: see build_roster_table(); NULL = the current rosters (the app)
  state <- get_nfl_state()
  week <- state$week
  season <- state$season

  league <- get_league(league_id)
  scoring_settings <- league$scoring_settings

  players_db <- get_players_db()
  roster_table <- build_roster_table(league_id, players_db, membership = membership)
  # players_db is Sleeper's full ~12,000-player dictionary (~100MB parsed in
  # R), of which this league only ever needs ~270 players' worth. It's
  # already out of scope after this point, but on a memory-constrained host
  # (this is what caused shinyapps.io OOM crashes) it's worth forcing R to
  # actually reclaim it now rather than waiting for a lazy GC trigger.
  rm(players_db)
  gc(full = TRUE)

  matchups <- get_matchups(league_id, week)
  points_lookup <- purrr::map_dfr(matchups, function(m) {
    pts <- m$players_points %||% list()
    if (length(pts) == 0) return(NULL)
    tibble::tibble(
      roster_id = m$roster_id,
      player_id = names(pts),
      locked_points = as.numeric(unlist(pts))
    )
  })

  game_status <- espn_get_game_status(season, week, state$season_type)
  # in-game injury news (live games only), plus any ESPN injury report NEWER than the Sleeper player dictionary (pulled once a day; see
  # espn_recent_injuries()); Sleeper's own status is only refreshed daily
  espn_injuries <- dplyr::bind_rows(
    tryCatch(espn_live_injuries(game_status), error = function(e) empty_espn_injuries()),
    tryCatch(espn_recent_injuries(file.info(PLAYERS_CACHE_FILE)$mtime), error = function(e) empty_espn_injuries())
  )
  projections <- get_sleeper_projections(season, week)
  player_variance_lookup <- get_player_variance_lookup()

  roster_table <- roster_table |>
    dplyr::left_join(points_lookup, by = c("roster_id", "player_id")) |>
    dplyr::mutate(locked_points = tidyr::replace_na(locked_points, 0)) |>
    dplyr::left_join(
      dplyr::distinct(game_status, nfl_team, .keep_all = TRUE),
      by = "nfl_team"
    ) |>
    dplyr::mutate(
      # No game found this week (bye) => treat as already-complete, 0 more points.
      elapsed_fraction = tidyr::replace_na(elapsed_fraction, 1),
      played_fraction = tidyr::replace_na(played_fraction, 1),
      matchup_label = tidyr::replace_na(matchup_label, "BYE"),
      full_week_projection = purrr::map_dbl(player_id, function(pid) {
        pmax(score_stats(projections[[pid]], scoring_settings), 0)
      }),
      # Sleeper sometimes has only a placeholder (no real stat projection) for a
      # player who is about to play; see R/fallback.R.
      has_sleeper_projection = purrr::map_lgl(player_id, function(pid) has_real_projection(projections[[pid]]))
    ) |>
    apply_fallback_projections(season, week, scoring_settings) |>
    # A player on injured reserve cannot play, so his pre-game projection is 0, not whatever stale number Sleeper still carries (Jordan Mason was IR with 9.1).
    # Done before the live columns so his pre-game spread, the team's pre-game total and the "pre-game X" annotation all start from 0.
    dplyr::mutate(full_week_projection = dplyr::if_else(!is.na(injury_status) & injury_status == "IR", 0, full_week_projection)) |>
    add_live_projection_columns(player_variance_lookup) |>
    zero_out_players(espn_injuries)
    # Bayesian comparison model disabled for now (tab commented out in app.R). To restore, also
    # re-source R/bayesian.R in app.R and chain it here (it needs the has_live_signal/observed_rate/
    # remaining_fraction columns add_live_projection_columns() adds, so it runs after):
    #   |> add_bayesian_projection_columns()

  # Deterministic "naive" projected total: the optimal lineup solved directly
  # from mean projections (locked_points + remaining_mean), no simulation
  # involved. This is what Sleeper's own UI shows as a team's projected
  # score. It is NOT the same as the Monte Carlo sim's projected_mean (see
  # run_elimination_simulation() in R/simulation.R): that figure averages,
  # across many noisy draws, whichever lineup happens to be optimal *in each
  # draw* -- since a team can swap in a bench player whenever they outscore a
  # starter, that per-draw flexibility is a call option on the roster's own
  # variance, and its average payout runs well above the value of the single
  # lineup that's optimal at the means (confirmed empirically: ~15pt gap on
  # this league's real rosters). Sleeper's number is the latter (no
  # optionality credit), so that's what's shown as "Projected Final Score" --
  # the simulated mean stays a real number, just an internal one, useful for
  # relative comparisons but not one to put next to Sleeper's own figure.
  # pregame_projected_total: the same optimal-lineup calculation, but using
  # ONLY each player's pure pregame projection (full_week_projection) --
  # no locked points, no live-pace blending. This is "what the team was
  # projected to score before any games this week started," used elsewhere
  # (app.R) purely as the reference point for red/green trend coloring on
  # team-level projected scores -- is naive_projected_total (the current,
  # live-updated figure) now above or below where it started the week?
  naive_projected <- roster_table |>
    dplyr::group_by(roster_id) |>
    dplyr::summarise(
      naive_projected_total = optimal_lineup_points(position, locked_points + remaining_mean),
      # Bayesian model's team total (disabled with its tab; restore together with R/bayesian.R):
      # bayesian_projected_total = optimal_lineup_points(position, locked_points + bayes_remaining_mean),
      pregame_projected_total = optimal_lineup_points(position, full_week_projection),
      # (current_score below is the "current score" shown in the Standings tables
      # and Team detail header.)
      # How many rostered players' games have started (in progress or final;
      # bye-week and not-yet-started players don't count) out of the roster players
      # who have a game this week (a bye player has no ESPN game, so state is NA).
      players_started = sum(state %in% c("in", "post")),
      players_total = sum(!is.na(state)),
      # NOT limited to the projected lineup above: best-ball lineup from points
      # ALREADY SCORED only, so a player who has scored but sits on the projected
      # bench (an unplayed teammate's projection is higher) still counts.
      current_score = optimal_lineup_points(position, locked_points),
      .groups = "drop"
    )

  team_meta <- dplyr::distinct(roster_table, roster_id, team_name, owner_name, avatar_url) |>
    dplyr::left_join(naive_projected, by = "roster_id")

  roster_data <- split(
    roster_table[, c("position", "locked_points", "remaining_mean",
                      "remaining_sd_backtest", "remaining_sd_underdog",
                      "remaining_sd_playerprofiler", "remaining_sd_player")],
    roster_table$roster_id
  )

  list(
    week = week,
    season = season,
    roster_table = roster_table,
    roster_data = roster_data,
    team_meta = team_meta,
    eliminated_teams = get_eliminated_teams(league_id, current_week = week),
    fetched_at = Sys.time()
  )
}
