# --- Waiver data for the weekly recap -------------------------------------------------------------------------
# Sleeper's transaction log keeps every waiver claim, the lost ones too (status "failed", with the bid and a reason such as "This player was claimed by
# another owner"). This file turns that into: the results of the auction for last week's chopped players (price, number of bids, runner-up bid), the claims
# the chopped team LOST and what winning them would have meant, and every claim's value (points it added to the team's best-ball score per dollar).
# Needs R/sleeper_api.R, R/bestball.R and collector/recap_lib.R (teams_path, manager_label). League budget: $1000 FAAB per team.

FAAB_BUDGET <- 1000

# Every waiver claim (won and lost), one row per claim. Sleeper files the Wednesday run under the week that just ended, so the list for the week after the
# last one built is read too. `run` buckets claims by the hour they were processed (a waiver run processes all claims at once).
waiver_claims_raw <- function(weeks, league_id = LEAGUE_ID) {
  tx <- do.call(c, lapply(c(weeks, max(weeks) + 1), function(w) get_transactions(league_id, w)))
  ids <- vapply(tx, function(z) z$transaction_id %||% NA_character_, character(1)); tx <- tx[!duplicated(ids)]
  rows <- lapply(tx, function(y) {
    if (!identical(y$type, "waiver")) return(NULL)
    add <- names(y$adds %||% list()); if (!length(add)) return(NULL)
    d <- names(y$drops %||% list())
    tibble::tibble(id = y$transaction_id, ts = as.numeric(y$status_updated %||% y$created) / 1000, status = y$status, rid = as.integer(unlist(y$roster_ids))[1],
                   add = add[1], drop = if (length(d)) d[1] else NA_character_, bid = suppressWarnings(as.numeric(y$settings$waiver_bid %||% NA)),
                   note = paste(unlist(y$metadata$notes %||% ""), collapse = " "))
  })
  out <- dplyr::bind_rows(rows); out$run <- floor(out$ts / 3600)
  # A team can file several claims on one player (each with a different player to drop). Those are not rival bids: a failed claim by a team that WON that player in the same run is
  # dropped, and several failed claims by one team on one player in one run count as one bid (the highest).
  won_key <- paste(out$rid, out$add, out$run)[out$status == "complete"]
  out <- out[!(out$status != "complete" & paste(out$rid, out$add, out$run) %in% won_key), ]
  out <- out[order(out$status != "complete", -out$bid), ]
  out <- out[!duplicated(ifelse(out$status == "complete", out$id, paste(out$rid, out$add, out$run))), ]
  out[order(out$ts), ]
}

# The chop of each week: when it happened, which roster, and the players it freed.
chop_txns <- function(weeks, league_id = LEAGUE_ID) {
  dplyr::bind_rows(lapply(weeks, function(w) {
    ch <- Filter(function(z) identical(z$type, "chopped"), get_transactions(league_id, w)); if (!length(ch)) return(NULL)
    z <- ch[[1]]
    tibble::tibble(week = w, ts = as.numeric(z$status_updated %||% z$created) / 1000, rid = as.integer(unlist(z$roster_ids))[1], freed = list(names(z$drops %||% list())))
  }))
}

# Start and end of each week's games on the clock (UTC seconds), from the replayed team timelines.
week_windows <- function(season, weeks) {
  dplyr::bind_rows(lapply(weeks, function(w) {
    u <- as.numeric(as.POSIXct(as.character(readr::read_csv(teams_path(season, w), show_col_types = FALSE, col_select = "wall_utc")$wall_utc), tz = "UTC"))
    tibble::tibble(week = w, start = min(u), end = max(u))
  }))
}

# Best-ball lineup score for a set of players, from a points table (player_id, scored) and a positions table (player_id, position).
lineup_value <- function(ids, pts_tbl, pos_tbl) {
  d <- tibble::tibble(player_id = ids) |> dplyr::left_join(pts_tbl[, c("player_id", "scored")], by = "player_id") |> dplyr::left_join(pos_tbl, by = "player_id")
  d <- d[!is.na(d$position), ]; d$scored[is.na(d$scored)] <- 0
  v <- optimal_lineup_points(d$position, d$scored)
  if (is.na(v)) {   # the roster cannot fill every slot (for example its only tight end is the player being removed): an empty slot scores 0, so pad with zero-point players
    pad <- rep(c("QB", "RB", "WR", "TE"), each = length(ROSTER_SLOTS))
    v <- optimal_lineup_points(c(d$position, pad), c(d$scored, rep(0, length(pad))))
  }
  v
}

# The auction for last week's chopped players: one row per freed player who was claimed, with the price, how many bids it drew, the runner-up bid (the
# highest losing bid) and how the player did this week. NULL for week 1 (no earlier chop) or if nobody was claimed.
waiver_results <- function(week, first_kickoff_secs, claims, chops, perf, team_lab, db, team_tbl) {
  ch <- chops[chops$week == week - 1, ]
  if (week < 2 || !nrow(ch)) return(NULL)
  freed <- ch$freed[[1]]
  cl <- claims[claims$add %in% freed & claims$ts > ch$ts & claims$ts < first_kickoff_secs, ]
  won <- cl[cl$status == "complete", ]
  if (!nrow(won)) return(NULL)
  tm <- function(rid, col) team_tbl[[col]][match(rid, team_tbl$roster_id)]
  out <- dplyr::bind_rows(lapply(seq_len(nrow(won)), function(i) {
    w <- won[i, ]
    same <- cl[cl$add == w$add & cl$run == w$run & (cl$id == w$id | grepl("claimed by another", cl$note, fixed = TRUE)), ]   # the competing bids in the same waiver run
    lose <- same[same$id != w$id, ]; lose <- lose[order(-lose$bid), ]
    p <- db[[w$add]]; pf <- perf[match(w$add, perf$player_id), ]
    tibble::tibble(player = sprintf("%s, %s (%s)", p$full_name %||% w$add, p$position %||% "?", p$team %||% "FA"), winner = team_lab(w$rid), winner_team = tm(w$rid, "team_name"), winner_owner = tm(w$rid, "owner_name"),
                   price = w$bid, bids = nrow(same), runner_up = if (nrow(lose)) lose$bid[1] else NA_real_,
                   runner_up_team = if (nrow(lose)) tm(lose$rid[1], "team_name") else NA_character_, runner_up_owner = if (nrow(lose)) tm(lose$rid[1], "owner_name") else NA_character_,
                   scored = if (is.na(pf$scored)) 0 else pf$scored, projected = pf$pre_proj)
  }))
  out$overpay <- out$price - out$runner_up
  out <- out[order(-out$price), ]
  attr(out, "freed") <- length(freed); out
}

# Claims the CHOPPED team lost to another owner during the weeks that mattered (after the previous week ended, before this week ended): for each, the
# team's own bid, the winning bid, the team's budget left at the time (could it have outbid?), and the points winning would have added to its final
# score (the claim's own drop applied), against the gap it needed to survive. Also the combined effect of winning all of them. NULL if none matter.
lost_bids <- function(chopped_rid, gap, prev_end_secs, week_end_secs, claims, end_roster, perf, pos_tbl, team_lab, db) {
  lost <- claims[claims$rid == chopped_rid & claims$status == "failed" & claims$ts > prev_end_secs & claims$ts < week_end_secs &
                   grepl("claimed by another", claims$note, fixed = TRUE), ]
  if (!nrow(lost)) return(NULL)
  base <- lineup_value(end_roster, perf, pos_tbl)
  gain_of <- function(adds, drops) lineup_value(union(setdiff(end_roster, drops[!is.na(drops)]), adds), perf, pos_tbl) - base
  lost$gain <- vapply(seq_len(nrow(lost)), function(i) gain_of(lost$add[i], lost$drop[i]), numeric(1))
  won <- claims[claims$status == "complete", ]
  w <- lapply(seq_len(nrow(lost)), function(i) { x <- won[won$add == lost$add[i] & won$run == lost$run[i], ]; if (nrow(x)) x[which.max(x$bid), ] else NULL })
  lost$win_bid <- vapply(w, function(x) if (is.null(x)) NA_real_ else x$bid, numeric(1))
  lost$winner <- vapply(w, function(x) if (is.null(x)) NA_character_ else team_lab(x$rid), character(1))
  lost$left <- vapply(lost$ts, function(t) FAAB_BUDGET - sum(claims$bid[claims$rid == chopped_rid & claims$status == "complete" & claims$ts < t], na.rm = TRUE), numeric(1))
  lost <- lost[order(-lost$gain), ]; lost <- lost[!duplicated(lost$add) & lost$gain >= 0.5, ]
  if (!nrow(lost)) return(NULL)
  p <- lapply(lost$add, function(i) db[[i]])
  out <- tibble::tibble(player = vapply(seq_along(p), function(k) sprintf("%s, %s", p[[k]]$full_name %||% lost$add[k], p[[k]]$position %||% "?"), character(1)),
                        their_bid = lost$bid, win_bid = lost$win_bid, winner = lost$winner, budget_left = lost$left, could_outbid = lost$left > lost$win_bid,
                        gain = round(lost$gain, 1), saves = lost$gain > gap)
  comb <- gain_of(lost$add, lost$drop)
  attr(out, "combined_gain") <- round(comb, 1); attr(out, "combined_saves") <- comb > gap
  out
}

# Every won waiver claim, with the points it added: the player's contribution to the claiming team's best-ball score in each week he was on its end-of-week
# roster after the claim (the team's score with him minus its score without him, so the next-best option replaces him). `from_chop` marks players freed by a chop.
waiver_roi <- function(season, weeks, claims, chops, windows, db, pos_tbl, team_tbl, league_id = LEAGUE_ID) {
  won <- claims[claims$status == "complete", ]
  if (!nrow(won)) return(NULL)
  pts <- lapply(weeks, function(w) { f <- file.path(REPLAY_DIR, sprintf("%s_w%02d_players.csv.gz", season, w))
    readr::read_csv(f, show_col_types = FALSE) |> dplyr::filter(is_final_row) |> dplyr::transmute(player_id = as.character(player_id), scored = final_actual_points) |> dplyr::distinct(player_id, .keep_all = TRUE) })
  names(pts) <- weeks
  rost <- lapply(weeks, function(w) dplyr::bind_rows(lapply(get_matchups(league_id, w), function(m) tibble::tibble(roster_id = m$roster_id, player_id = as.character(unlist(m$players))))))
  names(rost) <- weeks
  freed_all <- unlist(chops$freed)
  # Availability of each claimed player: the NFL's official weekly injury reports (a rostered week in which he was reported Out or Doubtful AND did not play
  # counts as a missed game), and his status right now (Sleeper's player list, plus any ESPN report newer than it -- see espn_recent_injuries()), for players
  # still on the claiming team's roster. No injury types and no return dates are kept: the recaps do not name injuries, and sources disagree on them.
  inj_rep <- load_nflverse_injuries(season); xw <- tryCatch(load_id_crosswalk(), error = function(e) NULL)
  espn_new <- tryCatch(espn_recent_injuries(file.info(PLAYERS_CACHE_FILE)$mtime), error = function(e) empty_espn_injuries())
  now_ros <- lapply(get_rosters(league_id), function(r) list(rid = r$roster_id, players = as.character(unlist(r$players))))
  dplyr::bind_rows(lapply(seq_len(nrow(won)), function(i) {
    cl <- won[i, ]; ws <- windows$week[windows$end > cl$ts]; held <- 0L; added <- 0; missed_wks <- integer(0); added_last <- NA_real_; scored_last <- NA_real_; last_wk <- NA_integer_; scored_sum <- 0
    gsis <- if (is.null(xw)) NA_character_ else xw$gsis_id[match(cl$add, xw$sleeper_id)]
    for (w in ws) {
      r <- rost[[as.character(w)]]$player_id[rost[[as.character(w)]]$roster_id == cl$rid]
      if (cl$add %in% r) {
        ctrb <- lineup_value(r, pts[[as.character(w)]], pos_tbl) - lineup_value(setdiff(r, cl$add), pts[[as.character(w)]], pos_tbl)
        held <- held + 1L; added <- added + ctrb
        added_last <- ctrb; last_wk <- w   # the latest week he was held: what he added that week (for "finally paying off")
        scored_last <- pts[[as.character(w)]]$scored[match(cl$add, pts[[as.character(w)]]$player_id)]
        scored_sum <- scored_sum + dplyr::coalesce(scored_last, 0)   # the player's own points over the weeks he was held
        rep_w <- if (is.null(inj_rep) || is.na(gsis)) NULL else inj_rep[inj_rep$week == w & !is.na(inj_rep$gsis_id) & inj_rep$gsis_id == gsis & inj_rep$game_type == "REG", ]
        sc <- pts[[as.character(w)]]$scored[match(cl$add, pts[[as.character(w)]]$player_id)]
        if (!is.null(rep_w) && nrow(rep_w) && isTRUE(rep_w$report_status[1] %in% c("Out", "Doubtful")) && (is.na(sc) || sc == 0)) missed_wks <- c(missed_wks, w)
      }
    }
    p_now <- db[[cl$add]]
    status_now <- merge_injury_status(tibble::tibble(nfl_team = p_now$team %||% NA_character_, full_name = p_now$full_name %||% NA_character_,
                                                     injury_status = p_now$injury_status %||% NA_character_), espn_new)$injury_status
    on_now <- any(vapply(now_ros, function(z) identical(as.integer(z$rid), as.integer(cl$rid)) && cl$add %in% z$players, logical(1)))
    p <- db[[cl$add]]
    fr <- chops[vapply(chops$freed, function(z) cl$add %in% z, logical(1)) & chops$ts < cl$ts, ]   # the chop that freed him (the latest one before the claim)
    tibble::tibble(player = p$full_name %||% cl$add, position = p$position %||% "?", team_name = team_tbl$team_name[match(cl$rid, team_tbl$roster_id)],
                   owner_name = team_tbl$owner_name[match(cl$rid, team_tbl$roster_id)], bid = cl$bid, ts = cl$ts,
                   for_week = if (length(ws)) ws[1] else max(weeks) + 1L, period = sum(chops$ts < cl$ts), n_chops = nrow(chops), teams_alive = nrow(team_tbl) - sum(chops$ts < cl$ts), weeks_held = held, points_added = round(added, 1), from_chop = nrow(fr) > 0,
                   missed_inj = length(missed_wks), missed_weeks = paste(missed_wks, collapse = ","), added_last = round(added_last, 1), scored_last = scored_last, scored_total = round(scored_sum, 1), last_week_held = last_wk, status_now = status_now, on_roster_now = on_now,  # freed by a chop that came BEFORE this claim (a player who was only freed by a later chop is not a chop pickup)
                   freed_owner = if (nrow(fr)) team_tbl$owner_name[match(fr$rid[nrow(fr)], team_tbl$roster_id)] else NA_character_, freed_week = if (nrow(fr)) as.integer(fr$week[nrow(fr)]) else NA_integer_)
  }))
}

# --- Text -----------------------------------------------------------------------------------------------------
# The "WAIVER BIDS THE CHOPPED TEAM LOST" block of the fact sheet (f = this week's row of facts_all). Only decisive bids are written up: the claims that on their own
# would have saved the team, or, when no single claim would have, the combined effect if that was enough. NULL when winning would not have mattered.
lost_bid_lines <- function(lost, f) {
  alone <- lost[lost$saves, ]; comb_ok <- isTRUE(attr(lost, "combined_saves"))
  if (!nrow(alone) && !comb_ok) return(NULL)
  nm <- function(x) sub(", .*$", "", x)
  c(sprintf("WAIVER BIDS THE CHOPPED TEAM LOST (waiver claims the chopped team made that another team won, each scored on its own: the points winning that claim would have added to its final score, against the %s points it needed to finish above the next lowest team. Write about these in 'The team that got chopped' and nowhere else. Summarise the bids lost in one sentence, then say which would have been enough, and leave out any claim that would not have mattered):", fmt_gap(f$gap)),
    sprintf("Bids lost to other owners: %s.", paste(sprintf("%s ($%.0f vs the winning $%.0f, won by %s)", nm(lost$player), lost$their_bid, lost$win_bid, lost$winner), collapse = "; ")),
    if (nrow(alone)) sprintf("Enough to survive on its own: %s. The chopped team had $%.0f of its budget left at the time of the bids, so it %s.",
                             paste(sprintf("%s would have added %.1f points", nm(alone$player), alone$gain), collapse = ", and "), lost$budget_left[1],
                             if (all(alone$could_outbid)) "could have outbid the winner" else "could not have outbid every winner"),
    if (!nrow(alone)) sprintf("No single claim was enough, but winning all of them would have added %.1f points, enough to finish above %s, the next lowest team. The chopped team had $%.0f of its budget left at the time of the bids.",
                              attr(lost, "combined_gain"), f$next_team, lost$budget_left[1]))
}

# The same as plain sentences, for the automatic text (used only when no draft exists)
lost_bid_text <- function(lost, f, mgr) {
  alone <- lost[lost$saves, ]; comb_ok <- isTRUE(attr(lost, "combined_saves"))
  if (!nrow(alone) && !comb_ok) return(NULL)
  nm <- function(x) sub(", .*$", "", x)
  n <- nrow(lost); words <- c("one", "two", "three", "four", "five")
  s1 <- sprintf("%s lost %s waiver %s: %s.", mgr, if (n <= 5) words[n] else n, if (n == 1) "bid" else "bids", paste(sprintf("%s ($%.0f vs $%.0f)", nm(lost$player), lost$their_bid, lost$win_bid), collapse = "; "))
  s2 <- if (nrow(alone)) sprintf("%s would have added %s, enough on its own to survive. %s still had $%.0f in the budget, so a bigger bid was possible.",
                                 paste(sprintf("%s alone", nm(alone$player[1])), "", sep = ""), "", mgr, lost$budget_left[1]) else NULL
  if (nrow(alone)) {
    items <- sprintf("%s %.1f", nm(alone$player), alone$gain)
    s2 <- sprintf("%s alone would have added %.1f points%s, enough on its own to survive. %s still had $%.0f in the budget, so a bigger bid was possible.", nm(alone$player[1]), alone$gain[1],
                  if (nrow(alone) > 1) paste0(", and ", paste(items[-1], collapse = ", ")) else "", mgr, lost$budget_left[1])
  } else s2 <- sprintf("Neither claim would have saved the team alone, but winning all of them would have added %.1f points, enough to finish above %s. %s still had $%.0f in the budget.", attr(lost, "combined_gain"), f$next_team, mgr, lost$budget_left[1])
  paste(s1, s2)
}

# --- Putting it together ----------------------------------------------------------------------------------------
# Everything that is fetched once per build: every claim, every chop, each week's clock window, names and positions.
waiver_context <- function(season, weeks, league_id = LEAGUE_ID) {
  users <- build_user_lookup(get_users(league_id))
  ro <- dplyr::bind_rows(lapply(get_rosters(league_id), function(r) tibble::tibble(roster_id = r$roster_id, owner_id = r$owner_id))) |> dplyr::left_join(users, by = "owner_id")
  list(claims = waiver_claims_raw(weeks, league_id), chops = chop_txns(weeks, league_id), win = week_windows(season, weeks), weeks = weeks, season = season,
       db = get_players_db(), pos_tbl = slim_players_table()[, c("player_id", "position")] |> dplyr::mutate(player_id = as.character(player_id)),
       team_tbl = ro,
       team_lab = function(rid) { w <- ro[match(rid, ro$roster_id), ]; sprintf("%s (%s)", w$team_name, manager_label(w$owner_name)) })
}

# One week's waiver pieces: `results` (the auction for last week's chopped players) and `lost` (bids the chopped team lost). perf = player_id, pre_proj, scored.
waiver_week <- function(ctx, week, perf, league_id = LEAGUE_ID) {
  win <- ctx$win; w_now <- win[win$week == week, ]
  res <- waiver_results(week, w_now$start, ctx$claims, ctx$chops, perf, ctx$team_lab, ctx$db, ctx$team_tbl)
  t <- readr::read_csv(teams_path(ctx$season, week), show_col_types = FALSE) |> dplyr::distinct(roster_id, final_team_points, chopped) |> dplyr::arrange(final_team_points)
  ch <- t[t$chopped, ]; gap <- t$final_team_points[2] - t$final_team_points[1]
  end_ros <- unlist(lapply(get_matchups(league_id, week), function(m) if (identical(as.integer(m$roster_id), as.integer(ch$roster_id[1]))) as.character(unlist(m$players))))
  prev_end <- if (week > 1 && any(win$week == week - 1)) win$end[win$week == week - 1] else 0
  lost <- lost_bids(ch$roster_id[1], gap, prev_end, w_now$end, ctx$claims, end_ros, perf, ctx$pos_tbl, ctx$team_lab, ctx$db)
  list(results = res, lost = lost)
}

# What each team's waiver claims were worth in each week, in the spirit of baseball's wins above replacement: for every team and week, the team's best-ball score minus the score it
# would have had if that week's waiver claims had not been made. "That week's claims" are the winning claims whose first week on the roster is that week (the Wednesday run before
# kickoff and any run during the week). The counterfactual roster takes those players off and puts back the players each claim dropped to make room, so the comparison is against
# the team's real alternative, not against an empty slot; a negative number means the team dropped someone who scored more than it got. Points come from the matchup's own
# per-player points, and from the week's stat lines under the league's scoring for the players who were dropped. One row per team per week (n_claims 0 when it made none).
waiver_week_effect <- function(ctx, weeks, league_id = LEAGUE_ID) {
  ss <- get_league(league_id)$scoring_settings
  won <- ctx$claims[ctx$claims$status == "complete", ]
  won$for_week <- vapply(won$ts, function(t) { ws <- ctx$win$week[ctx$win$end > t]; if (length(ws)) as.integer(ws[1]) else NA_integer_ }, integer(1))
  nm <- function(p) ctx$db[[p]]$full_name %||% p
  dplyr::bind_rows(lapply(weeks, function(w) {
    mm <- get_matchups(league_id, w); st <- tryCatch(fetch_week_stats(ctx$season, w), error = function(e) NULL)
    dplyr::bind_rows(lapply(mm, function(m) {
      r <- as.character(unlist(m$players)); rid <- as.integer(m$roster_id)
      cl <- won[won$rid == rid & won$for_week %in% w & won$add %in% r, ]
      if (!nrow(cl)) return(tibble::tibble(week = w, roster_id = rid, n_claims = 0L, spent = 0, points_added = 0, claims = NA_character_))
      cf <- union(setdiff(r, cl$add), setdiff(stats::na.omit(cl$drop), union(r, cl$add)))
      ids <- union(r, cf)
      pts <- vapply(ids, function(p) { x <- m$players_points[[p]]; if (!is.null(x)) as.numeric(x) else if (!is.null(st) && !is.null(st[[p]])) score_stats(st[[p]], ss) else 0 }, numeric(1))
      tb <- tibble::tibble(player_id = ids, scored = pts)
      tibble::tibble(week = w, roster_id = rid, n_claims = nrow(cl), spent = sum(cl$bid, na.rm = TRUE), points_added = round(lineup_value(r, tb, ctx$pos_tbl) - lineup_value(cf, tb, ctx$pos_tbl), 1),
                     claims = paste(sprintf("%s ($%.0f)", vapply(cl$add, nm, character(1)), cl$bid), collapse = "; "))
    }))
  })) |> dplyr::left_join(ctx$team_tbl[, c("roster_id", "team_name", "owner_name")], by = "roster_id")
}

# The pool each chop puts on the waivers, week by week: for every player the chopped team freed, his average over his last three games played up to that week (league scoring),
# whether he was claimed, and the price. One row per freed offensive player who has played. `open` marks the latest chop, whose bidding has not run yet.
freed_pool_history <- function(ctx, weeks, league_id = LEAGUE_ID) {
  sc <- get_league(league_id)$scoring_settings; db <- ctx$db; OFF <- c("QB", "RB", "WR", "TE")
  st <- lapply(weeks, function(w) tryCatch(fetch_week_stats(ctx$season, w), error = function(e) NULL)); names(st) <- weeks
  fpos <- function(pid) { p <- db[[pid]]; o <- c(p$position, unlist(p$fantasy_positions)); o <- o[o %in% OFF]; if (length(o)) o[[1]] else NA_character_ }
  won <- ctx$claims[ctx$claims$status == "complete", ]
  dplyr::bind_rows(lapply(seq_len(nrow(ctx$chops)), function(i) {
    ch <- ctx$chops[i, ]; ids <- as.character(unlist(ch$freed)); ids <- ids[!is.na(vapply(ids, fpos, character(1)))]
    dplyr::bind_rows(lapply(ids, function(pid) {
      pts <- unlist(lapply(weeks[weeks <= ch$week], function(w) { x <- st[[as.character(w)]][[pid]]; if (is.null(x) || is.na(x["off_snp"]) || x["off_snp"] < 1) NULL else score_stats(x, sc) }))
      if (!length(pts)) return(NULL)
      cl <- won[won$add == pid & won$ts > ch$ts, ]; cl <- cl[order(cl$ts), ][1, ]
      tibble::tibble(chop_week = ch$week, player = db[[pid]]$full_name %||% pid, position = fpos(pid), avg3 = mean(utils::tail(pts, 3)), games = length(pts),
                     claimed = nrow(cl) == 1 && !is.na(cl$add), price = if (nrow(cl) == 1 && !is.na(cl$add)) cl$bid else NA_real_,
                     claimed_by = if (nrow(cl) == 1 && !is.na(cl$add)) ctx$team_tbl$team_name[match(cl$rid, ctx$team_tbl$roster_id)] else NA_character_,
                     open = ch$week == max(ctx$chops$week))
    }))
  }))
}

# Season table for the Season overview: waiver claims won, FAAB spent, points added, points per $100, best and worst claim, per manager.
waiver_roi_table <- function(roi) {
  if (is.null(roi) || !nrow(roi)) return(NULL)
  roi |> dplyr::group_by(team_name, owner_name) |> dplyr::summarise(
    claims = dplyr::n(), spent = sum(bid), added = sum(points_added),
    # each claim's average per game is its points divided by the games it was held (weeks_held); added up across the manager's claims it is the points per game the claims add to
    # the team together. Points per $100 then divides that by the money paid on the claims that have played (claims not yet played have no games and are left out).
    avg_pg = sum(ifelse(weeks_held > 0, points_added / pmax(weeks_held, 1), 0)),
    per100 = if (sum(bid[weeks_held > 0]) > 0) 100 * avg_pg / sum(bid[weeks_held > 0]) else NA_real_,
    # best / worst claim are judged on the same per-game basis as the figure and the text: a claim's average per game is its points divided by the games it was held
    best = { ok <- which(weeks_held > 0); a <- points_added / pmax(weeks_held, 1); k <- if (length(ok)) ok[which.max(a[ok])] else NA_integer_
             if (!is.na(k) && a[k] > 0) sprintf("%s, %+.1f per game", player[k], round(a[k], 1) + 0) else "-" },
    worst = { a <- points_added / pmax(weeks_held, 1); okb <- which(weeks_held > 0); kb <- if (length(okb)) okb[which.max(a[okb])] else NA_integer_
              ok <- which(weeks_held > 0 & bid >= 10); k <- if (length(ok)) ok[which.min(a[ok] / bid[ok])] else NA_integer_   # claims of $10 or more that have played, lowest points per $100
              if (!is.na(k) && !(a[k] > 0 && identical(k, kb))) sprintf("%s, $%.0f for %+.1f per game", player[k], bid[k], round(a[k], 1) + 0) else "-" },
    .groups = "drop")
}

# --- Page pieces --------------------------------------------------------------------------------------------------
# The "Waiver results" table at the end of a weekly page (waivers_wNN.csv from weekly_recap.R). "" if the week has none (week 1, or nobody was claimed).
waiver_results_html <- function(out_dir, w, text_html = "") {
  f <- file.path(out_dir, sprintf("waivers_w%02d.csv", w)); if (!file.exists(f)) return("")
  d <- readr::read_csv(f, show_col_types = FALSE); if (!nrow(d)) return("")
  money <- function(x) ifelse(is.na(x), "-", sprintf("$%.0f", x))
  # claimed by: the team, with the manager underneath in light gray (falls back to the old "Team (Manager)" text for a csv written before those columns existed)
  esc1 <- function(x) vapply(as.character(x), h, character(1))
  claimed <- if (all(c("winner_team", "winner_owner") %in% names(d))) sprintf('%s<span class="t2">(%s)</span>', esc1(d$winner_team), esc1(vapply(d$winner_owner, manager_label, character(1)))) else esc1(d$winner)
  df <- data.frame(Player = d$player, `Claimed by` = claimed, Price = money(d$price), Bids = d$bids, `Runner-up bid` = money(d$runner_up), Overpay = money(d$overpay),
                   Scored = sprintf("%.1f", d$scored), Projected = ifelse(is.na(d$projected), "-", sprintf("%.1f", d$projected)), check.names = FALSE, stringsAsFactors = FALSE)
  paste0(sprintf('<h3 id="wk%d-waivers" class="wvh h-waivers">Waiver results</h3>', w), text_html,
         sprintf('<p class="fine">Waiver claims on the players freed by the week %d chop, %d of them claimed. Price is what the winner paid. Bids counts the claims on that player in the same waiver run. Runner-up bid is the highest losing bid, and overpay is the price minus the runner-up bid, the money that was not needed to win. Scored and projected are the player\'s points this week.</p>', w - 1, nrow(d)),
         html_table(df, num_cols = c("Price", "Bids", "Runner-up bid", "Overpay", "Scored", "Projected"), sortable = TRUE, raw_cols = "Claimed by",
                    sort_vals = list(Price = d$price, Bids = d$bids, `Runner-up bid` = d$runner_up, Overpay = d$overpay, Scored = d$scored, Projected = d$projected)))
}

# --- Up for bid this week ----------------------------------------------------------------------------------------
# The chopped team's players are freed when it is chopped and go up on the waivers early Wednesday, so the Tuesday recap can tell managers who is
# available. For each QB/RB/WR/TE on the chopped team's end-of-week roster: his average points a game over his LAST THREE games where he played at
# least one offensive snap (league scoring), the week-to-week spread of his points over all his games played (standard deviation), his best game, how many
# of those games he finished in the top 12 at his position (the upside: a "boom" is a top-12 week), his points this week, Sleeper's projection for next
# week, whether his NFL team is on a bye next week, and Sleeper's current status. `show` marks who the page lists: last-three-game average of 8.0 or more
# (players on a bye included, flagged in the Status column), best first, at most 10 and at least 3. In the past weeks 7 to 13 of the 15 freed players were claimed.
# Each NFL team's bye week this season (named integer vector, team -> week): the week in which the team has no game on ESPN's weekly scoreboard.
# The schedule never changes, so it is fetched once (18 calls) and cached; NULL if ESPN cannot be reached.
nfl_bye_weeks <- function(season, season_type = "regular") {
  f <- file.path("cache", sprintf("nfl_byes_%s.rds", season))
  if (file.exists(f)) return(readRDS(f))
  played <- vector("list", 18)
  for (w in 1:18) { gs <- tryCatch(espn_get_game_status(season, w, season_type), error = function(e) NULL); if (is.null(gs) || !nrow(gs)) return(NULL); played[[w]] <- unique(gs$nfl_team) }
  teams <- unique(unlist(played))
  by <- vapply(teams, function(tm) { b <- which(!vapply(played, function(p) tm %in% p, logical(1))); if (length(b)) b[1] else NA_integer_ }, integer(1))
  dir.create("cache", showWarnings = FALSE, recursive = TRUE); saveRDS(by, f)
  by
}
WATCH_MIN_AVG3 <- 8; WATCH_MAX <- 10; WATCH_MIN <- 3; WATCH_BOOM_RANK <- 12
waiver_watch <- function(season, week, league_id = LEAGUE_ID, db = get_players_db(), season_type = "regular") {
  sc <- get_league(league_id)$scoring_settings
  t <- readr::read_csv(teams_path(season, week), show_col_types = FALSE) |> dplyr::distinct(roster_id, team_name, final_team_points, chopped)
  ch <- t[t$chopped, ]; if (!nrow(ch)) return(NULL)
  m <- Filter(function(x) identical(as.integer(x$roster_id), as.integer(ch$roster_id[1])), get_matchups(league_id, week))
  if (!length(m)) return(NULL)
  ids <- as.character(unlist(m[[1]]$players))
  OFF <- c("QB", "RB", "WR", "TE")
  fpos <- function(pid) { p <- db[[pid]]; o <- c(p$position, unlist(p$fantasy_positions)); o <- o[o %in% OFF]; if (length(o)) o[[1]] else NA_character_ }
  pos <- vapply(ids, fpos, character(1)); keep <- !is.na(pos); ids <- ids[keep]; pos <- pos[keep]
  if (!length(ids)) return(NULL)
  # every player who took an offensive snap in each finished week, with his league points and his rank among his position that week
  hist <- dplyr::bind_rows(lapply(seq_len(week), function(w) {
    st <- tryCatch(fetch_week_stats(season, w), error = function(e) NULL); if (is.null(st)) return(NULL)
    played <- names(st)[vapply(st, function(x) !is.na(x["off_snp"]) && x["off_snp"] >= 1, logical(1))]
    ps <- vapply(played, fpos, character(1)); played <- played[!is.na(ps)]; ps <- ps[!is.na(ps)]
    pts <- vapply(played, function(pid) score_stats(st[[pid]], sc), numeric(1))
    rk <- stats::ave(-pts, ps, FUN = function(x) rank(x, ties.method = "min"))
    tibble::tibble(player_id = played, week = w, pts = pts, rank = rk)
  }))
  proj <- tryCatch(get_sleeper_projections(season, week + 1L), error = function(e) NULL)
  gs <- tryCatch(espn_get_game_status(season, week + 1L, season_type), error = function(e) NULL)
  playing <- if (is.null(gs)) NULL else unique(gs$nfl_team)
  byes <- nfl_bye_weeks(season, season_type)
  out <- dplyr::bind_rows(lapply(seq_along(ids), function(i) {
    pid <- ids[i]; p <- db[[pid]]; h <- hist[hist$player_id == pid, ]
    team <- normalize_team_abbr(p$team %||% NA_character_); status <- p$injury_status %||% NA_character_
    bye <- !is.null(playing) && !is.na(team) && !(team %in% playing)
    pn <- if (is.null(proj) || is.null(proj[[pid]])) NA_real_ else max(score_stats(proj[[pid]], sc), 0)
    if (isTRUE(status == "IR") || bye) pn <- 0   # an IR player cannot play and a bye player has no game
    tibble::tibble(player_id = pid, player = p$full_name %||% pid, position = pos[i], nfl_team = team, status = status, games = nrow(h),
                   avg3 = if (nrow(h)) mean(utils::tail(h$pts[order(h$week)], 3)) else NA_real_, n3 = min(nrow(h), 3L),   # average over his last three games played
                   swing = if (nrow(h) >= 2) stats::sd(h$pts) else NA_real_,   # week-to-week spread: standard deviation of his points over the games he played
                   avg_ppg = if (nrow(h)) mean(h$pts) else NA_real_, best_game = if (nrow(h)) max(h$pts) else NA_real_,
                   boom_games = sum(h$rank <= WATCH_BOOM_RANK), this_week = if (any(h$week == week)) h$pts[h$week == week][1] else 0,
                   proj_next = pn, bye = bye, bye_week = if (is.null(byes) || is.na(team) || is.na(byes[team])) NA_integer_ else as.integer(byes[[team]]))
  }))
  # shown: his average over his last three games played is 8.0 or more (bye weeks included: a bye player is flagged in the Status column), best average
  # first, at most 10 and at least 3 (filled with the next best averages)
  out <- out[order(-dplyr::coalesce(out$avg3, -1), -dplyr::coalesce(out$proj_next, 0)), ]
  ok <- which(!is.na(out$avg3) & out$avg3 >= WATCH_MIN_AVG3)
  show <- head(ok, WATCH_MAX)
  if (length(show) < WATCH_MIN) show <- head(which(!is.na(out$avg3)), WATCH_MIN)
  out$show <- seq_len(nrow(out)) %in% show
  out$chopped_team <- ch$team_name[1]; out$next_week <- as.integer(week) + 1L
  out
}

week_next <- function(d) if ('next_week' %in% names(d)) d$next_week else rep(Inf, nrow(d))
upforbid_status_word <- function(x) switch(as.character(x), IR = "on injured reserve", Out = "out", Doubtful = "doubtful", Questionable = "questionable", PUP = "out", NA_character_)

upforbid_fact_lines <- function(d) {
  d <- if (is.null(d)) NULL else d[d$show, ]
  if (is.null(d) || !nrow(d)) return(NULL)
  sw <- vapply(d$status, upforbid_status_word, character(1))
  c(sprintf("UP FOR BID THIS WEEK (the chopped team's players, %s's, who are freed and go up on the waivers; the waiver run is early Wednesday morning, so managers can still bid after this recap. Write one short section called 'Up for bid this week' after 'Waiver results': two to four lively sentences about the top three or four players only, and say a table under the text lists them all. Use only the figures below. The average is over his last three games played, not the whole season. Upside means the number of games he finished in the top 12 at his position, and the spread is how much his scores move from week to week. A player on a bye next week is still listed and may be called a bye-week pickup, but do not say he adds anything next week. A line may also give his team's bye week when it is still to come, which you may mention. Say a player's status only as the plain word given (for example out or on injured reserve), never the injury or a return date. Do not predict who will win a bid or tell anyone how to bid):", d$chopped_team[1]),
    sprintf("- %s, %s (%s): averaged %.1f points over his last %d %s played; week-to-week spread %s; best game %.1f; top-12 at his position in %d of %d games; %s; scored %.1f this week%s%s.",
            d$player, d$position, ifelse(is.na(d$nfl_team), "no team", d$nfl_team), d$avg3, d$n3, ifelse(d$n3 == 1, "game", "games"),
            ifelse(is.na(d$swing), "not enough games", sprintf("%.1f", d$swing)), d$best_game, d$boom_games, d$games,
            ifelse(d$bye, "on a bye next week", ifelse(is.na(d$proj_next), "no Sleeper projection yet", ifelse(d$proj_next == 0, "Sleeper projects 0.0 next week", sprintf("Sleeper projects %.1f next week", d$proj_next)))),
            d$this_week, ifelse(is.na(sw), "", paste0("; Sleeper lists him as ", sw)),
            ifelse(!is.na(d$bye_week) & d$bye_week > week_next(d) , sprintf("; his team's bye is in week %d", d$bye_week), "")),
    "")
}

upforbid_template_text <- function(d) {
  d <- if (is.null(d)) NULL else d[d$show, ]
  if (is.null(d) || !nrow(d)) return(NULL)
  top <- head(d, 4)
  c(sprintf("%s's players are up for bid. The best of them over the last three games: %s. The waiver run is early Wednesday morning.", d$chopped_team[1],
            paste(sprintf("%s, %s, %.1f points a game%s", top$player, top$position, top$avg3, ifelse(top$bye, " (bye next week)", "")), collapse = "; ")))
}

upforbid_html <- function(out_dir, w, text_html = "") {
  f <- file.path(out_dir, sprintf("upforbid_w%02d.csv", w)); if (!file.exists(f)) return("")
  d <- readr::read_csv(f, show_col_types = FALSE); if (!nrow(d)) return("")
  nx <- if ("next_week" %in% names(d)) d$next_week else rep(Inf, nrow(d)); bw <- if ("bye_week" %in% names(d)) d$bye_week else rep(NA_integer_, nrow(d))
  stat <- vapply(seq_len(nrow(d)), function(i) {
    bye_txt <- if (isTRUE(d$bye[i])) sprintf("On a bye next week (week %d)", nx[i]) else if (!is.na(bw[i]) && bw[i] > nx[i]) sprintf("Bye in week %d", bw[i]) else NA_character_   # byes still to come; a bye already played is not mentioned
    x <- c(d$status[i], bye_txt); x <- x[!is.na(x)]; if (length(x)) paste(x, collapse = "; ") else "-" }, character(1))
  # position in the app's colors (QB red, RB green, WR blue, TE orange) in its own column so a reader can sort or scan for a position
  pcol <- c(QB = "#FF5757", RB = "#4CD787", WR = "#5B9DFF", TE = "#FFA84C")
  pos_html <- sprintf('<span style="color:%s;font-weight:600">%s</span>', ifelse(is.na(pcol[d$position]), "#98B3D6", pcol[d$position]), d$position)
  # short headers so the table stays narrow (headers do not wrap on phones); the note above the table spells each one out
  df <- data.frame(Player = sprintf("%s (%s)", d$player, ifelse(is.na(d$nfl_team), "-", d$nfl_team)), Pos = pos_html,
                   `Last 3 avg` = ifelse(is.na(d$avg3), "-", sprintf("%.1f", d$avg3)), Spread = ifelse(is.na(d$swing), "-", sprintf("%.1f", d$swing)), Games = d$games,
                   Best = ifelse(is.na(d$best_game), "-", sprintf("%.1f", d$best_game)), `Top-12` = ifelse(d$games == 0, "-", sprintf("%d of %d", d$boom_games, d$games)),
                   `Next proj` = ifelse(d$bye | is.na(d$proj_next), "-", sprintf("%.1f", d$proj_next)), `This wk` = sprintf("%.1f", d$this_week),
                   Status = stat, check.names = FALSE, stringsAsFactors = FALSE)
  paste0(sprintf('<h3 id="wk%d-upforbid" class="wvh h-waivers">Up for bid this week</h3>', w), text_html,
         sprintf('<p class="fine">Players from the chopped team (%s) who are now free agents and go up on the waivers early Wednesday morning, best average first. Last 3 avg is his average over the last three games where he played at least one offensive snap this season. Spread is how much his points move from week to week over all the games he played (a standard deviation: a bigger number means a less predictable player). Best is his best game. Top-12 is the number of those games in which he finished among the 12 highest scorers at his position, a measure of how often he booms. Next proj is Sleeper\'s projection for next week under this league\'s scoring, and This wk is what he scored this week. Status gives Sleeper\'s designation as of now (for example out) and the week of his team\'s bye if it is still to come.</p>', h(d$chopped_team[1])),
         html_table(df, num_cols = c("Last 3 avg", "Spread", "Games", "Best", "Next proj", "This wk"), cls = "small", sortable = TRUE, raw_cols = "Pos",
                    sort_vals = list(Pos = match(d$position, c("QB", "RB", "WR", "TE")), `Last 3 avg` = d$avg3, Spread = d$swing, Games = d$games, Best = d$best_game, `Top-12` = d$boom_games,
                                     `Next proj` = ifelse(d$bye, 0, d$proj_next), `This wk` = d$this_week)))
}

# --- Bare bench ------------------------------------------------------------------------------------------------
# Teams alive that week whose WHOLE bench could not play: after the best-ball lineup is picked from the week's actual points, every player left on the
# bench took no offensive snap AND was either on a bye (`nfl_bye_weeks()`) or ruled out (Out/Doubtful on the NFL's weekly injury report in nflverse, or
# listed Out/IR/PUP by Sleeper now). A healthy backup who simply did not play does not count: the bench has to be unavailable, not just unused. A team
# with more good players than lineup spots never has this; a team with exactly as many available players as slots (or fewer) does, and one more absence
# leaves a hole in the lineup. Returns one row per bench player of each such team (NULL when there is none): team_name, owner_name, player, position,
# nfl_team, reason ("bye" or "ruled out"), n_bench.
bare_bench_teams <- function(season, week, league_id = LEAGUE_ID, db = get_players_db()) {
  t <- readr::read_csv(teams_path(season, week), show_col_types = FALSE) |> dplyr::distinct(roster_id, team_name, owner_name)
  m <- get_matchups(league_id, week)
  st <- tryCatch(fetch_week_stats(season, week), error = function(e) NULL); if (is.null(st)) return(NULL)
  byes <- nfl_bye_weeks(season)
  inj_rep <- tryCatch(load_nflverse_injuries(season), error = function(e) NULL); xw <- tryCatch(load_id_crosswalk(), error = function(e) NULL)
  OFF <- c("QB", "RB", "WR", "TE")
  fpos <- function(pid) { p <- db[[pid]]; o <- c(p$position, unlist(p$fantasy_positions)); o <- o[o %in% OFF]; if (length(o)) o[[1]] else NA_character_ }
  ruled_out <- function(pid) {   # on the weekly injury report as Out or Doubtful that week, or Sleeper lists him Out / IR / PUP
    gsis <- if (is.null(xw)) NA_character_ else xw$gsis_id[match(pid, xw$sleeper_id)]
    rep_w <- if (is.null(inj_rep) || is.na(gsis)) NULL else inj_rep[inj_rep$week == week & !is.na(inj_rep$gsis_id) & inj_rep$gsis_id == gsis & inj_rep$game_type == "REG", ]
    isTRUE(!is.null(rep_w) && nrow(rep_w) > 0 && rep_w$report_status[1] %in% c("Out", "Doubtful")) || isTRUE(db[[pid]]$injury_status %in% c("Out", "IR", "PUP"))
  }
  out <- lapply(seq_len(nrow(t)), function(i) {
    mm <- Filter(function(x) identical(as.integer(x$roster_id), as.integer(t$roster_id[i])), m); if (!length(mm)) return(NULL)
    ids <- as.character(unlist(mm[[1]]$players)); pos <- vapply(ids, fpos, character(1)); keep <- !is.na(pos); ids <- ids[keep]; pos <- pos[keep]
    if (length(ids) <= length(ROSTER_SLOTS)) return(NULL)   # no bench at all
    pts <- vapply(ids, function(pid) { x <- mm[[1]]$players_points[[pid]]; if (is.null(x)) 0 else as.numeric(x) }, numeric(1))
    played <- vapply(seq_along(ids), function(k) { x <- st[[ids[k]]]; (!is.null(x) && !is.na(x["off_snp"]) && x["off_snp"] >= 1) || pts[k] > 0 }, logical(1))   # a snap, or any points (the snap data can miss a short appearance)
    asg <- optimal_lineup_assignment(pos, ifelse(played, pts, -1))   # a player who did not play can only fill a slot nobody else can
    bench <- which(asg == "BN")
    if (!length(bench)) return(NULL)
    team <- vapply(ids[bench], function(pid) normalize_team_abbr(db[[pid]]$team %||% NA_character_), character(1))
    is_bye <- !is.na(team) & !is.na(byes[team]) & byes[team] == week
    is_out <- vapply(ids[bench], ruled_out, logical(1))
    unavail <- (is_bye | is_out) & !played[bench]   # on a bye or ruled out, and did not play; a healthy backup who sat is an unused bench, not an unavailable one
    n_ok <- length(bench) - sum(unavail)
    kind <- if (n_ok == 0) "bare" else if (sum(unavail) >= 2 && n_ok <= 1) "thin" else return(NULL)
    nm <- function(pid) db[[pid]]$full_name %||% pid
    tibble::tibble(team_name = t$team_name[i], owner_name = t$owner_name[i], player = vapply(ids[bench][unavail], nm, character(1)),
                   position = pos[bench][unavail], nfl_team = team[unavail], reason = ifelse(is_bye[unavail], "bye", "ruled out"), n_bench = length(bench), kind = kind,
                   usable = if (n_ok == 1) sprintf("%s, %s (%.1f points)", nm(ids[bench][!unavail]), pos[bench][!unavail], pts[bench][!unavail]) else NA_character_)
  })
  d <- dplyr::bind_rows(out)
  if (!nrow(d)) NULL else d
}

bare_bench_fact_lines <- function(bb, chopped_team = NA_character_) {
  if (!is.null(bb) && nrow(bb)) bb <- bb[bb$kind == "bare" | bb$team_name %in% chopped_team, ]   # a thin bench counts only on the chopped team
  if (is.null(bb) || !nrow(bb)) return(NULL)
  desc <- function(d) paste(sprintf("%s, %s (%s)", d$player, d$position, ifelse(d$reason == "bye", "on a bye", "ruled out")), collapse = "; ")
  bare <- unique(bb$team_name[bb$kind == "bare"]); thin <- unique(bb$team_name[bb$kind == "thin"])
  c(if (length(bare)) c("BARE BENCH (teams whose whole bench could not play this week: every player left on the bench was on a bye, ruled out or inactive, so the team had no depth at all. This is embarrassing for the team and a gift for the roast. Put ONE sentence about each team below in the second paragraph of '### What happened', just before its last sentence, in the roast of the chopped team: if the chopped team is not that team, aim the joke at the chopped team, which had backups who could play and still got chopped, while this team had no usable bench and survived anyway; if the chopped team IS that team, give it as a reason the week went wrong. Say only 'on a bye' or 'ruled out' for the players, never the injury or a return date; use only the names and numbers below.):",
    unlist(lapply(bare, function(tn) { d <- bb[bb$team_name == tn, ]
      sprintf("- %s (managed by %s): all %d bench players could not play: %s.", tn, manager_label(d$owner_name[1]), d$n_bench[1], desc(d)) })), ""),
    if (length(thin)) c("THIN BENCH (the CHOPPED team's bench: all but one bench player were on a bye or ruled out, so the team had almost no depth, and that is part of why the week went wrong. Put ONE sentence about it in '### Why it fell short', after the sentences about a sidelined player and before the position-group sentences, saying 'most of the bench' and naming the one backup who could play with his points. Never put it in '### What happened'; use only the names and numbers below.):",
    unlist(lapply(thin, function(tn) { d <- bb[bb$team_name == tn, ]
      sprintf("- %s (managed by %s): %d of %d bench players could not play: %s. The only bench player who could play: %s.", tn, manager_label(d$owner_name[1]), nrow(d), d$n_bench[1], desc(d), d$usable[1]) })), ""))
}

bare_bench_template_text <- function(bb, kind = "bare") {
  if (!is.null(bb) && nrow(bb)) bb <- bb[bb$kind == kind, ]
  if (is.null(bb) || !nrow(bb)) return(NULL)
  paste(vapply(unique(bb$team_name), function(tn) { d <- bb[bb$team_name == tn, ]
    if (d$kind[1] == "bare") sprintf("%s's bench was empty of players who could play: all %d bench players were on a bye or ruled out.", manager_label(d$owner_name[1]), d$n_bench[1])
    else sprintf("%s's bench was nearly empty: %d of %d bench players were on a bye or ruled out.", manager_label(d$owner_name[1]), nrow(d), d$n_bench[1]) }, character(1)), collapse = " ")
}

# --- Interactive "Final score by week" ----------------------------------------------------------------------------------------
# Drawn with plotly.js (the same library and pattern as the waiver figure): hover or tap a team's line to highlight it and see who it is, pick a team in the drop-down,
# switch to "points above or below the median" to take the week-to-week swings out, and color the lines by early spenders vs savers.
# The page shows the PNG until plotly has loaded and the Season overview tab is open.
SCORES_PLOTLY_JS <- r"--(
(function(){
var D=__DATA__, M=__MEDIAN__, el=null, drawn=false, picked='', hov='';
var WH='#FFFFFF', AMB='#FFC857', GREY='#98A6C8', CHOP='#FF6482', BND='#C9D6EE';
function esc(s){return String(s).replace(/[&<>"]/g,function(c){return {'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;'}[c];});}
function med(w){var i=M.week.indexOf(w);return i<0?null:M.score[i];}
function yv(t,k){return t.score[k];}
function base(t){return GREY;}
function style(t){var on=(t.team===picked||t.team===hov),any=!!(picked||hov);
  return {color:on?AMB:GREY,width:on?4:1.4,op:on?1:(any?0.16:0.45)};}
function tip(t,k){var d=t.score[k]-med(t.week[k]);return '<b>'+esc(t.team)+'</b> ('+esc(t.manager)+')<br>Week '+t.week[k]+': '+t.score[k].toFixed(1)+' ('+(d>=0?'+':'')+d.toFixed(1)+' vs the median)'+(t.chop===t.week[k]?'<br>Chopped this week':'');}
function build(){
  var tr=D.map(function(t){var st=style(t);
    return {type:'scatter',mode:'lines+markers',x:t.week,y:t.week.map(function(w,k){return yv(t,k);}),name:t.team,
      line:{color:st.color,width:st.width},marker:{color:st.color,size:7},opacity:st.op,
      hovertext:t.week.map(function(w,k){return tip(t,k);}),hoverinfo:'text',customdata:t.week.map(function(){return t.team;}),showlegend:false};});
  var cx=[],cy=[];D.forEach(function(t){if(t.chop){var k=t.week.indexOf(t.chop);if(k>=0){cx.push(t.chop);cy.push(yv(t,k));}}});
  var HI=[],LO=[],HT=[],LT=[];
  M.week.forEach(function(w){var hi=null,lo=null;D.forEach(function(t){var k=t.week.indexOf(w);if(k<0)return;var v=t.score[k];if(hi===null||v>hi.v)hi={v:v,t:t.team};if(lo===null||v<lo.v)lo={v:v,t:t.team};});
    HI.push(hi.v);HT.push('<b>Highest score</b><br>Week '+w+': '+hi.v.toFixed(1)+' ('+esc(hi.t)+')');LO.push(lo.v);LT.push('<b>Lowest score</b><br>Week '+w+': '+lo.v.toFixed(1)+' ('+esc(lo.t)+')');});
  [[HI,HT],[LO,LT]].forEach(function(z){tr.push({type:'scatter',mode:'lines+markers',x:M.week,y:z[0],hovertext:z[1],hoverinfo:'text',showlegend:false,line:{color:BND,width:2.5,dash:'dot'},marker:{color:BND,size:6}});});
  tr.push({type:'scatter',mode:'markers',x:cx,y:cy,hoverinfo:'skip',showlegend:false,marker:{symbol:'x',size:11,color:CHOP,line:{width:2.5,color:CHOP}}});
  tr.push({type:'scatter',mode:'lines+markers',x:M.week,y:M.score,name:'League median',
    line:{color:WH,width:4},marker:{color:WH,size:8},hovertext:M.week.map(function(w,i){return '<b>League median</b><br>Week '+w+': '+M.score[i].toFixed(1);}),hoverinfo:'text',showlegend:false});
  return {data:tr,layout:{paper_bgcolor:'#05091D',plot_bgcolor:'#05091D',font:{color:'#F4F6FB',size:12},margin:{l:56,r:12,t:12,b:44},
    xaxis:{title:{text:'Week'},tickmode:'array',tickvals:M.week,gridcolor:'#25345D',zerolinecolor:'#5A6FA3',fixedrange:true},
    yaxis:{title:{text:'Final score (best ball)'},gridcolor:'#25345D',zerolinecolor:'#5A6FA3',fixedrange:true},
    showlegend:false,hovermode:'closest',hoverdistance:25,hoverlabel:{bgcolor:'#131B38',bordercolor:'#00FFF9',font:{color:'#F4F6FB',size:12},align:'left'}}};
}
function restyleLines(){
  var idx=[],c=[],w=[],o=[],mc=[];
  D.forEach(function(t,i){var st=style(t);idx.push(i);c.push(st.color);w.push(st.width);o.push(st.op);});
  Plotly.restyle(el,{'line.color':c,'line.width':w,opacity:o,'marker.color':c},idx);
}
function legend(){
  document.getElementById('scores-legend').innerHTML='<span><i style="background:'+GREY+'"></i>One team</span><span><i style="background:'+AMB+'"></i>Highlighted team</span><span><i style="background:'+WH+'"></i>League median</span><span><i style="background:none;border-top:3px dotted '+BND+';height:0"></i>Highest and lowest score</span>';
}
function setPick(name){picked=name||'';var s=document.getElementById('scores-team');if(s)s.value=picked;restyleLines();}
function draw(){var b=build();legend();Plotly.react(el,b.data,b.layout,{responsive:true,displaylogo:false,displayModeBar:false});}
window.initScorePlot=function(){
  var v=document.getElementById('view-season');
  if(!window.Plotly||!v||v.hidden)return;
  el=document.getElementById('scores-plotly');if(!el)return;
  if(drawn){Plotly.Plots.resize(el);return;}
  drawn=true;
  document.getElementById('scores-static').hidden=true;document.getElementById('scores-interactive').hidden=false;
  draw();
  el.on('plotly_hover',function(e){var p=e.points&&e.points[0];if(!p||!p.data.customdata)return;hov=p.data.customdata[0];restyleLines();});
  el.on('plotly_unhover',function(){hov='';restyleLines();});
  el.on('plotly_click',function(e){var p=e.points&&e.points[0];if(!p||!p.data.customdata)return;var n=p.data.customdata[0];setPick(picked===n?'':n);});
  document.getElementById('scores-team').addEventListener('change',function(){setPick(this.value);});
};
if(window.Plotly)window.initScorePlot();
})();
)--"

season_scores_interactive_html <- function(sc, static_src = "img/scores_by_week.png") {
  W <- sort(unique(sc$week))
  teams <- lapply(split(sc[order(sc$week), ], sc$team_name[order(sc$week)]), function(d) {
    list(team = d$team_name[1], manager = manager_label(d$owner_name[1]),
         chop = if (any(d$chopped)) d$week[d$chopped][1] else NULL, week = d$week, score = round(d$score, 1))
  })
  teams <- unname(teams[order(tolower(names(teams)))])
  md <- sc |> dplyr::group_by(week) |> dplyr::summarise(score = round(median(score), 1), .groups = "drop")
  tj <- function(x) gsub("</", "<\\/", as.character(jsonlite::toJSON(x, auto_unbox = FALSE, na = "null", null = "null")), fixed = TRUE)
  tj1 <- function(x) gsub("</", "<\\/", as.character(jsonlite::toJSON(x, auto_unbox = TRUE, na = "null", null = "null")), fixed = TRUE)
  # one-element vectors must stay arrays in the JSON (the page indexes them), so each team's fields are written by hand
  tj_team <- function(t) sprintf('{"team":%s,"manager":%s,"chop":%s,"week":%s,"score":%s}', tj1(t$team), tj1(t$manager), if (is.null(t$chop)) "null" else tj1(t$chop), tj(t$week), tj(t$score))
  data_js <- paste0("[", paste(vapply(teams, tj_team, character(1)), collapse = ","), "]")
  med_js <- sprintf('{"week":%s,"score":%s}', tj(md$week), tj(md$score))
  js <- sub("__DATA__", data_js, SCORES_PLOTLY_JS, fixed = TRUE)
  js <- sub("__MEDIAN__", med_js, js, fixed = TRUE)
  opts <- paste0('<option value="">No one</option>', paste0('<option value="', h(vapply(teams, `[[`, "", "team")), '">', h(vapply(teams, `[[`, "", "team")), " (", h(vapply(teams, `[[`, "", "manager")), ")</option>", collapse = ""))
  paste0('<figure id="scores-fig">',
         '<div id="scores-static" class="plate" style="max-width:820px"><img src="', static_src, '" loading="lazy" alt="Each team\'s final score in every week as a thin grey line, a red cross where a team was chopped, with the league median over the teams still alive drawn on top."></div>',
         '<div id="scores-interactive" style="max-width:820px" hidden>',
         '<p class="note" style="max-width:76ch"><b>Hover or tap a team\'s line</b> to highlight it and see who it is. Each grey line is one team and a red cross marks the week it was chopped; the white line is the league median over the teams still alive that week, and the dotted lines join the highest and the lowest score each week. ',
         'Or pick a team from the Highlight list. Tap a highlighted line again to clear it.</p>',
         '<div style="display:flex;flex-wrap:wrap;gap:6px 14px">',
         '<div class="picker" style="margin:6px 0 4px"><label for="scores-team">Highlight</label><select id="scores-team">', opts, "</select></div></div>",
         '<style>#scores-legend{display:flex;flex-wrap:wrap;gap:4px 16px;color:var(--muted);font-size:.8rem;margin:4px 0}#scores-legend i{display:inline-block;width:18px;height:4px;border-radius:2px;margin-right:6px;vertical-align:middle}</style><div id="scores-legend"></div>',
         '<div class="plate" style="overflow:hidden"><div id="scores-plotly" style="width:100%;height:520px"></div></div>',
         "</div></figure>", "<script>", js, "</script>")
}

# --- Interactive "Waiver budget spent vs points added" ----------------------------------------------------------------------------------
# One dot per team per week: the share of the $1,000 still unspent going into the week against the points above or below the league median. Hover or tap a dot to see
# which team and week it is and join that team's dots in week order; pick a team from the Highlight list. Same plotly.js pattern as the other season figures.
BUDGET_PLOTLY_JS <- r"--(
(function(){
var D=__DATA__, FIT=__FIT__, el=null, drawn=false, picked='', hov='';
var AMB='#FFC857', WH='#FFFFFF', MAXW=Math.max.apply(null,D.map(function(t){return Math.max.apply(null,t.week);}));
function esc(s){return String(s).replace(/[&<>"]/g,function(c){return {'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;'}[c];});}
function sg(x){return (x>=0?'+':'')+x.toFixed(1);}
function on(t){return t.team===picked||t.team===hov;}
function tip(t,k){var c=t.n[k]?('<br>'+t.n[k]+(t.n[k]===1?' claim':' claims')+' that week ($'+Math.round(t.sp[k])+'): '+esc(t.cl[k])):'<br>No claims that week';return '<b>'+esc(t.team)+'</b> ('+esc(t.manager)+')<br>Week '+t.week[k]+': waiver claims added '+sg(t.add[k])+' points'+c+'<br>'+t.pct[k].toFixed(0)+'% of the budget spent going into the week'+(t.chop===t.week[k]?'<br>Chopped this week':'');}
function build(){
  var tr=D.map(function(t){var o=on(t),any=!!(picked||hov);
    return {type:'scatter',mode:'lines+markers',x:t.pct,y:t.add,name:t.team,line:{color:AMB,width:o?2.5:0},opacity:o?1:(any?0.14:0.85),
      marker:{size:o?11:8,color:t.week,colorscale:[[0,'#00FFF9'],[0.5,'#FFC857'],[1,'#FF6482']],cmin:1,cmax:Math.max(MAXW,2),line:{width:o?2:0,color:WH}},
      hovertext:t.week.map(function(w,k){return tip(t,k);}),hoverinfo:'text',customdata:t.week.map(function(){return t.team;}),showlegend:false};});
  tr.push({type:'scatter',mode:'lines',x:[0,100],y:[FIT.b,FIT.b+FIT.m*100],hoverinfo:'skip',showlegend:false,line:{color:WH,width:2,dash:'dash'}});
  return {data:tr,layout:{paper_bgcolor:'#05091D',plot_bgcolor:'#05091D',font:{color:'#F4F6FB',size:12},margin:{l:56,r:14,t:12,b:48},
    xaxis:{title:{text:'Waiver budget spent going into the week'},ticksuffix:'%',range:[-3,103],gridcolor:'#25345D',zerolinecolor:'#5A6FA3',fixedrange:true},
    yaxis:{title:{text:'Points added by that week\'s waiver claims'},gridcolor:'#25345D',zerolinecolor:'#98B3D6',zerolinewidth:1,fixedrange:true},
    showlegend:false,hovermode:'closest',hoverdistance:20,hoverlabel:{bgcolor:'#131B38',bordercolor:'#00FFF9',font:{color:'#F4F6FB',size:12},align:'left'}}};
}
function restyle(){
  var m=[],w=[],op=[],ms=[],any=!!(picked||hov),idx=[];
  D.forEach(function(t,i){var o=on(t);idx.push(i);w.push(o?2.5:0);op.push(o?1:(any?0.14:0.85));ms.push(o?11:8);});
  Plotly.restyle(el,{'line.width':w,opacity:op,'marker.size':ms},idx);
}
function setPick(n){picked=n||'';var s=document.getElementById('budget-team');if(s)s.value=picked;restyle();}
window.initBudgetPlot=function(){
  var v=document.getElementById('view-season');
  if(!window.Plotly||!v||v.hidden)return;
  el=document.getElementById('budget-plotly');if(!el)return;
  if(drawn){Plotly.Plots.resize(el);return;}
  drawn=true;
  document.getElementById('budget-static').hidden=true;document.getElementById('budget-interactive').hidden=false;
  var b=build();Plotly.react(el,b.data,b.layout,{responsive:true,displaylogo:false,displayModeBar:false});
  el.on('plotly_hover',function(e){var p=e.points&&e.points[0];if(!p||!p.data.customdata)return;hov=p.data.customdata[0];restyle();});
  el.on('plotly_unhover',function(){hov='';restyle();});
  el.on('plotly_click',function(e){var p=e.points&&e.points[0];if(!p||!p.data.customdata)return;var n=p.data.customdata[0];setPick(picked===n?'':n);});
  document.getElementById('budget-team').addEventListener('change',function(){setPick(this.value);});
};
if(window.Plotly)window.initBudgetPlot();
})();
)--"

season_budget_interactive_html <- function(bw, static_src = "img/budget_vs_score.png") {
  fit <- stats::lm(points_added ~ pct_spent, data = bw)
  tj <- function(x) gsub("</", "<\\/", as.character(jsonlite::toJSON(x, auto_unbox = FALSE, na = "null")), fixed = TRUE)
  tj1 <- function(x) gsub("</", "<\\/", as.character(jsonlite::toJSON(x, auto_unbox = TRUE, na = "null")), fixed = TRUE)
  teams <- lapply(split(bw[order(bw$week), ], bw$team_name[order(bw$week)]), function(d)
    sprintf('{"team":%s,"manager":%s,"chop":%s,"week":%s,"score":%s,"add":%s,"pct":%s,"n":%s,"sp":%s,"cl":%s}', tj1(d$team_name[1]), tj1(manager_label(d$owner_name[1])), if (any(d$chopped)) tj1(d$week[d$chopped][1]) else "null",
            tj(d$week), tj(round(d$score, 1)), tj(round(d$points_added, 1)), tj(round(d$pct_spent, 1)), tj(d$n_claims), tj(round(d$spent)), tj(ifelse(is.na(d$claims), "", d$claims))))
  teams <- unname(teams[order(tolower(names(teams)))])
  js <- sub("__DATA__", paste0("[", paste(teams, collapse = ","), "]"), BUDGET_PLOTLY_JS, fixed = TRUE)
  js <- sub("__FIT__", sprintf('{"b":%s,"m":%s}', format(unname(coef(fit)[1]), digits = 8), format(unname(coef(fit)[2]), digits = 8)), js, fixed = TRUE)
  nm <- sort(unique(bw$team_name)); mg <- bw$owner_name[match(nm, bw$team_name)]
  opts <- paste0('<option value="">No one</option>', paste0('<option value="', h(nm), '">', h(nm), " (", h(vapply(mg, manager_label, character(1))), ")</option>", collapse = ""))
  paste0('<figure id="budget-fig">',
         '<div id="budget-static" class="plate" style="max-width:820px"><img src="', static_src, '" loading="lazy" alt="One dot per team per week: the share of the waiver budget already spent going into the week against the points that week\'s waiver claims added to the team\'s score, colored by week, with a dashed straight-line trend."></div>',
         '<div id="budget-interactive" style="max-width:820px" hidden>',
         '<p class="note" style="max-width:76ch"><b>Hover or tap a dot</b> to see the team and the week. The dots are colored by week (teal early, pink late), the dashed line is the straight-line trend, and picking a team joins its dots in week order.</p>',
         '<div class="picker" style="margin:6px 0 4px"><label for="budget-team">Highlight</label><select id="budget-team">', opts, "</select></div>",
         '<div class="plate" style="overflow:hidden"><div id="budget-plotly" style="width:100%;height:520px"></div></div>',
         "</div></figure>", "<script>", js, "</script>")
}

# --- Interactive "What each chop put on the waivers" and "Spend curve" ----------------------------------------------------------------------
# plotly.js, same pattern as the other season figures: the page shows the PNG until plotly has loaded and the Season overview tab is open.
POOL_PLOTLY_JS <- r"--(
(function(){
var D=__DATA__, PCT=__PCT__, YT='__YT__', el=null, drawn=false;
function esc(s){return String(s).replace(/[&<>"]/g,function(c){return {'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;'}[c];});}
function tip(r){var n=Math.min(r.games,3);return '<b>'+esc(r.player)+'</b> ('+esc(r.pos)+')<br>Freed by the week '+r.week+' chop<br>Average over his last '+n+(n===1?' game: ':' games: ')+r.avg3.toFixed(1)+'<br>'+(r.open?'Up for bid now':(r.claimed?('Claimed for $'+Math.round(r.price)+' by '+esc(r.by)):'Not claimed'));}
function build(){
  var n=D.filter(function(r){return r.rk===1;}).length;
  var M={};D.forEach(function(r){(M[r.week]=M[r.week]||[]).push(r.y);});
  var mw=Object.keys(M).map(Number).sort(function(a,b){return a-b;}),mv=mw.map(function(w){return M[w].reduce(function(a,b){return a+b;},0)/M[w].length;});
  function pts(f,style){var q=D.filter(f);return Object.assign({type:'scatter',mode:'markers',x:q.map(function(r){return r.x;}),y:q.map(function(r){return r.y;}),hovertext:q.map(tip),hoverinfo:'text',showlegend:false},style(q));}
  var tr=[{type:'scatter',mode:'lines',x:mw,y:mv,hoverinfo:'skip',showlegend:false,line:{color:'#FFFFFF',width:2},opacity:0.7},
    pts(function(r){return r.claimed&&!r.open;},function(q){return {marker:{color:q.map(function(r){return r.price;}),colorscale:[[0,'#00FFF9'],[1,'#FF6482']],cmin:0,cmax:350,size:11,opacity:0.9,line:{width:0.5,color:'#05091D'},colorbar:{title:{text:'Price paid',side:'top'},thickness:10,len:0.5,x:0.5,xanchor:'center',y:1.12,yanchor:'bottom',orientation:'h',tickvals:[0,100,200,300],ticktext:['$0','$100','$200','$300+'],tickfont:{size:10}}}};}),
    pts(function(r){return !r.claimed&&!r.open;},function(q){return {marker:{symbol:'circle-open',color:'#98A6C8',size:11,line:{width:2}}};}),
    pts(function(r){return r.open;},function(q){return {marker:{symbol:'circle-open',color:'#FFC857',size:12,line:{width:2.5}}};})];
  var W=D.map(function(r){return r.week;}).filter(function(v,i,a){return a.indexOf(v)===i;}).sort(function(a,b){return a-b;});
  return {data:tr,layout:{paper_bgcolor:'#05091D',plot_bgcolor:'#05091D',font:{color:'#F4F6FB',size:12},margin:{l:56,r:12,t:56,b:48},
    xaxis:{title:{text:'Week of the chop'},tickmode:'array',tickvals:W,gridcolor:'#25345D',zerolinecolor:'#5A6FA3',fixedrange:true},
    yaxis:Object.assign({title:{text:YT},gridcolor:'#25345D',zerolinecolor:'#5A6FA3',fixedrange:true},PCT?{tickformat:'.0%',range:[-0.04,1.06]}:{rangemode:'tozero'}),
    showlegend:false,hovermode:'closest',hoverdistance:18,hoverlabel:{bgcolor:'#131B38',bordercolor:'#00FFF9',font:{color:'#F4F6FB',size:12},align:'left'}}};
}
window.__FN__=function(){
  var v=document.getElementById('view-season');
  if(!window.Plotly||!v||v.hidden)return;
  el=document.getElementById('__ID__-plotly');if(!el)return;
  if(drawn){Plotly.Plots.resize(el);return;}
  drawn=true;document.getElementById('__ID__-static').hidden=true;document.getElementById('__ID__-interactive').hidden=false;
  var b=build();Plotly.react(el,b.data,b.layout,{responsive:true,displaylogo:false,displayModeBar:false});
};
if(window.Plotly)window.__FN__();
})();
)--"

SPEND_PLOTLY_JS <- r"--(
(function(){
var D=__DATA__, SL=__SLOPE__, XM=__XMAX__, el=null, drawn=false, picked='', hov='';
var AMB='#FFC857', GREY='#98A6C8', RED='#FF6482', WH='#FFFFFF';
function on(t){return t.team===picked||t.team===hov;}
function sty(t){var o=on(t),any=!!(picked||hov);return {w:o?4:1.4,op:o?1:(any?0.16:0.6),c:o?AMB:(t.chopped?RED:GREY),s:o?9:5};}
function build(){
  var tr=D.map(function(t){var s=sty(t);
    return {type:'scatter',mode:'lines+markers',x:[0].concat(t.x),y:[0].concat(t.y),name:t.team,line:{color:s.c,width:s.w},marker:{color:s.c,size:s.s},opacity:s.op,
      hovertext:['<b>'+t.team+'</b> ('+t.manager+')<br>Start'].concat(t.tip),hoverinfo:'text',customdata:[t.team].concat(t.x.map(function(){return t.team;})),showlegend:false};});
  tr.push({type:'scatter',mode:'lines',x:[0,XM],y:[0,XM*SL],hoverinfo:'skip',showlegend:false,line:{color:WH,width:2,dash:'dash'}});
  return {data:tr,layout:{paper_bgcolor:'#05091D',plot_bgcolor:'#05091D',font:{color:'#F4F6FB',size:12},margin:{l:56,r:12,t:12,b:48},
    xaxis:{title:{text:'Waiver dollars spent so far'},tickprefix:'$',gridcolor:'#25345D',zerolinecolor:'#5A6FA3',fixedrange:true},
    yaxis:{title:{text:'Points per game the claims add'},gridcolor:'#25345D',zerolinecolor:'#5A6FA3',fixedrange:true},
    annotations:[{x:XM*0.97,y:XM*0.97*SL,text:'League average: '+(100*SL).toFixed(1)+' points per game per $100',showarrow:false,xanchor:'right',yanchor:'bottom',yshift:6,font:{size:11,color:'#C9D6EE'}}],
    showlegend:false,hovermode:'closest',hoverdistance:25,hoverlabel:{bgcolor:'#131B38',bordercolor:'#00FFF9',font:{color:'#F4F6FB',size:12},align:'left'}}};
}
function restyle(){var c=[],w=[],o=[],s=[],idx=[];D.forEach(function(t,i){var z=sty(t);idx.push(i);c.push(z.c);w.push(z.w);o.push(z.op);s.push(z.s);});Plotly.restyle(el,{'line.color':c,'line.width':w,opacity:o,'marker.color':c,'marker.size':s},idx);}
function setPick(n){picked=n||'';var s=document.getElementById('spend-team');if(s)s.value=picked;restyle();}
window.initSpendPlot=function(){
  var v=document.getElementById('view-season');
  if(!window.Plotly||!v||v.hidden)return;
  el=document.getElementById('spend-plotly');if(!el)return;
  if(drawn){Plotly.Plots.resize(el);return;}
  drawn=true;document.getElementById('spend-static').hidden=true;document.getElementById('spend-interactive').hidden=false;
  var b=build();Plotly.react(el,b.data,b.layout,{responsive:true,displaylogo:false,displayModeBar:false});
  el.on('plotly_hover',function(e){var p=e.points&&e.points[0];if(!p||!p.data.customdata)return;hov=p.data.customdata[0];restyle();});
  el.on('plotly_unhover',function(){hov='';restyle();});
  el.on('plotly_click',function(e){var p=e.points&&e.points[0];if(!p||!p.data.customdata)return;var n=p.data.customdata[0];setPick(picked===n?'':n);});
  document.getElementById('spend-team').addEventListener('change',function(){setPick(this.value);});
};
if(window.Plotly)window.initSpendPlot();
})();
)--"

season_pool_interactive_html <- function(freed, n_show = 6, static_src = "img/freed_pool.png", id = "pool", fn = "initPoolPlot") {
  tj1 <- function(x) gsub("</", "<\\/", as.character(jsonlite::toJSON(x, auto_unbox = TRUE, na = "null")), fixed = TRUE)
  d <- freed |> dplyr::group_by(chop_week) |> dplyr::slice_max(avg3, n = n_show, with_ties = FALSE) |> dplyr::mutate(rk = dplyr::row_number(), x = chop_week) |> dplyr::ungroup()
  d$yv <- d$avg3
  rows <- sprintf('{"week":%d,"x":%s,"rk":%d,"player":%s,"pos":%s,"avg3":%s,"y":%s,"games":%d,"claimed":%s,"price":%s,"by":%s,"open":%s}', d$chop_week, format(round(d$x, 3)), d$rk, vapply(d$player, tj1, ""), vapply(d$position, tj1, ""),
                  format(round(d$avg3, 2)), format(round(d$yv, 4)), d$games, tolower(as.character(d$claimed)), ifelse(is.na(d$price), "null", format(round(d$price))), vapply(ifelse(is.na(d$claimed_by), "", d$claimed_by), tj1, ""), tolower(as.character(d$open)))
  js <- sub("__DATA__", paste0("[", paste(rows, collapse = ","), "]"), POOL_PLOTLY_JS, fixed = TRUE)
  js <- sub("__PCT__", "false", js, fixed = TRUE)
  js <- sub("__YT__", "Average over his last 3 games played", js, fixed = TRUE)
  js <- gsub("__ID__", id, js, fixed = TRUE); js <- gsub("__FN__", fn, js, fixed = TRUE)
  paste0('<figure id="', id, '-fig"><div id="', id, '-static" class="plate" style="max-width:820px"><img src="', static_src, '" loading="lazy" alt="The six best players each chop freed, ', "by their average over their last three games", ', colored by the price paid, with hollow dots for players nobody claimed."></div>',
         '<div id="', id, '-interactive" style="max-width:820px" hidden><p class="note" style="max-width:76ch"><b>Hover or tap a dot</b> for the player, his numbers and what he sold for. A colored dot was claimed and its color is the price (teal cheap, pink expensive), a hollow grey dot was not claimed, an amber ring is a player up for bid now, and the white line is the average of the six.</p>',
         '<div class="plate" style="overflow:hidden"><div id="', id, '-plotly" style="width:100%;height:520px"></div></div></div></figure>', "<script>", js, "</script>")
}

season_spend_interactive_html <- function(sd, static_src = "img/spend_curve.png") {
  tj <- function(x) gsub("</", "<\\/", as.character(jsonlite::toJSON(x, auto_unbox = FALSE, na = "null")), fixed = TRUE)
  tj1 <- function(x) gsub("</", "<\\/", as.character(jsonlite::toJSON(x, auto_unbox = TRUE, na = "null")), fixed = TRUE)
  sd$tip <- sprintf("<b>%s</b> (%s)<br>%s, %s: $%.0f for week %d<br>Adds %.1f points per game%s<br>Spent so far $%.0f, claims add %.1f points per game", vapply(sd$team_name, h, ""), vapply(vapply(sd$owner_name, manager_label, ""), h, ""),
                    vapply(sd$player, h, ""), sd$position, sd$bid, sd$for_week, sd$apg, ifelse(coalesce(sd$weeks_held, 0) == 0, " (has not played for the team yet)", ""), sd$cum_spent, sd$cum_apg)
  teams <- lapply(split(sd, sd$team_name), function(d) sprintf('{"team":%s,"manager":%s,"chopped":%s,"x":%s,"y":%s,"tip":%s}', tj1(d$team_name[1]), tj1(manager_label(d$owner_name[1])), tolower(as.character(isTRUE(d$chopped[1]))), tj(round(d$cum_spent)), tj(round(d$cum_apg, 2)), tj(d$tip)))
  teams <- unname(teams[order(tolower(names(teams)))])
  slope <- sum(sd$apg) / sum(sd$bid)
  js <- sub("__DATA__", paste0("[", paste(teams, collapse = ","), "]"), SPEND_PLOTLY_JS, fixed = TRUE)
  js <- sub("__SLOPE__", format(slope, digits = 8), js, fixed = TRUE); js <- sub("__XMAX__", format(max(sd$cum_spent)), js, fixed = TRUE)
  nm <- sort(unique(sd$team_name)); mg <- sd$owner_name[match(nm, sd$team_name)]
  opts <- paste0('<option value="">No one</option>', paste0('<option value="', h(nm), '">', h(nm), " (", h(vapply(mg, manager_label, "")), ")</option>", collapse = ""))
  paste0('<figure id="spend-fig"><div id="spend-static" class="plate" style="max-width:820px"><img src="', static_src, '" loading="lazy" alt="One line per team: waiver dollars spent so far across, the points per game those claims add up the side, one step per claim, with a dashed line for the league average."></div>',
         '<div id="spend-interactive" style="max-width:820px" hidden><p class="note" style="max-width:76ch"><b>Hover or tap a line</b> to highlight a team, or pick one from the list. Red lines are teams that have been chopped. Each step is one claim, in the order it was made; a team whose line climbs faster than the dashed league-average line got more points per dollar. Tap a highlighted team again to clear it.</p>',
         '<div class="picker" style="margin:6px 0 4px"><label for="spend-team">Highlight</label><select id="spend-team">', opts, "</select></div>",
         '<div class="plate" style="overflow:hidden"><div id="spend-plotly" style="width:100%;height:520px"></div></div></div></figure>', "<script>", js, "</script>")
}

# The "Scores over the season" section of the Season overview: two figures from analysis/plot_season_scores.R.
season_scores_html <- function(out_dir) {
  if (!file.exists(file.path(out_dir, "img", "scores_by_week.png"))) return("")
  fig <- function(src, alt) sprintf('<figure><div class="plate" style="max-width:820px"><img src="%s" loading="lazy" alt="%s"></div></figure>', src, alt)
  sf <- file.path(out_dir, "scores_by_week.csv"); hf <- file.path(out_dir, "faab_history.csv")
  by_week <- if (file.exists(sf) && file.exists(hf)) tryCatch(season_scores_interactive_html(readr::read_csv(sf, show_col_types = FALSE)), error = function(e) NULL) else NULL
  bw_html <- if (file.exists(sf) && file.exists(hf) && file.exists(file.path(out_dir, "img", "budget_vs_score.png")) && file.exists(file.path(out_dir, "waiver_points_by_week.csv"))) tryCatch({
    bw <- budget_by_week(readr::read_csv(sf, show_col_types = FALSE), readr::read_csv(hf, show_col_types = FALSE), readr::read_csv(file.path(out_dir, "waiver_points_by_week.csv"), show_col_types = FALSE)); r <- stats::cor(bw$pct_spent, bw$points_added)
    paste0('<h4 class="sublabel fighead">Waiver budget spent vs points added</h4>', season_budget_interactive_html(bw),
           sprintf('<p class="fine">Each dot is one team in one week, from week 2 on (nothing can be claimed before week 1). Across is the share of the team\'s $1,000 already spent going into the week. Up is what that week\'s waiver claims added to the team\'s score: its best-ball score minus the score it would have had if those claims had not been made, with the players it dropped to make room put back on the roster. A negative number means the team dropped someone who scored more than the player it picked up, and a team that made no claim that week sits at 0. A claim counts for the week it first took effect. Across the %d team-weeks so far the correlation is %.2f. The lowest scorers are chopped along the way, so the late-week dots come from the teams that survived; treat the trend as a description, not a cause.</p>', nrow(bw), r))
  }, error = function(e) "") else ""
  if (is.null(by_week)) by_week <- fig("img/scores_by_week.png", "Each team's final score in every week as a thin grey line, a red cross where a team was chopped, with the league median over the teams still alive drawn on top.")
  paste0('<h2 id="season-scores">Scores over the season</h2>',
         '<h4 class="sublabel fighead">Every team, week by week</h4>',
         by_week,
         '<p class="fine">The median covers only the teams still alive that week, so it rises partly because the lowest scorer is chopped every week, not only because rosters improve. Scores also move with the schedule: bye weeks pull a week down for everyone.</p>',
         bw_html)
}

# The "Is it worth waiting?" block of the Waivers section: what each chop put on the waivers and what it sold for, and the spend curve. "" when its data files are missing.
wait_html <- function(out_dir) {
  pf <- file.path(out_dir, "freed_pool_by_week.csv"); cf <- file.path(out_dir, "waiver_claims.csv")
  if (!file.exists(pf) || !file.exists(cf) || !file.exists(file.path(out_dir, "img", "freed_pool.png")) || !file.exists(file.path(out_dir, "img", "spend_curve.png"))) return("")
  tryCatch({
    freed <- readr::read_csv(pf, show_col_types = FALSE); sd <- spend_curve_data(readr::read_csv(cf, show_col_types = FALSE))
    fhf <- file.path(out_dir, "faab_history.csv")
    ch_names <- if (file.exists(fhf)) { fh <- readr::read_csv(fhf, show_col_types = FALSE); unique(fh$team_name[!is.na(fh$chopped_week)]) } else character(0)
    sd$chopped <- sd$team_name %in% ch_names
    paste0('<h3 id="season-wait" class="wvh h-worth">Is it worth waiting?</h3>',
           '<h4 class="sublabel fighead">What each chop put on the waivers, and what it sold for</h4>', season_pool_interactive_html(freed),
           '<p class="fine">For every chop, the six players the chopped team freed with the highest average over their last three games played, placed in the week of the chop, with the price each one fetched in the bidding that followed, shown by color. A player with fewer than three games either was early in the season or missed games; byes do not count against him, because the average is over his last three games actually played. A player with a similar average tended to cost far less in the later weeks than in the first ones, when every team still had most of its $1,000, though the price still swings a lot from one player to the next.</p>',
           '<h4 class="sublabel fighead">Spend curve</h4>', season_spend_interactive_html(sd),
           '<p class="fine">Each line is one team, with a step for every claim it won, in the order it made them (red lines are teams that have been chopped). Across is the total it had paid; up is the points per game those claims add to its score, counting each claim\'s average per game since the pickup, so a claim made early gets no extra credit for having been held longer. The dashed line is the league average, so a line steeper than it is a team that got more for its money.</p>')
  }, error = function(e) "")
}

# The waiver section of the Season overview: the two figures and the return-on-investment table by manager (from waiver_claims.csv).
season_waivers_html <- function(out_dir) {
  f <- file.path(out_dir, "waiver_claims.csv"); if (!file.exists(f) || !file.exists(file.path(out_dir, "img", "waiver_spending.png"))) return("")
  tb <- waiver_roi_table(readr::read_csv(f, show_col_types = FALSE)); if (is.null(tb)) return("")
  tb <- tb[order(-tb$added), ]
  df <- data.frame(Team = tb$team_name, Manager = vapply(tb$owner_name, manager_label, character(1)), Claims = tb$claims, `FAAB spent` = sprintf("$%.0f", tb$spent),
                   `Total points` = sprintf("%.1f", tb$added), `Average per game` = sprintf("%.1f", tb$avg_pg), `Points per $100` = ifelse(is.na(tb$per100), "-", sprintf("%.1f", tb$per100)), `Best claim` = tb$best, `Worst claim` = tb$worst, check.names = FALSE, stringsAsFactors = FALSE)
  fig <- function(src, alt) sprintf('<figure><div class="plate" style="max-width:820px"><img src="%s" loading="lazy" alt="%s"></div></figure>', src, alt)
  hd <- function(id, cls, text) sprintf('<h3 id="season-%s" class="wvh h-%s">%s</h3>', id, cls, text)
  paste0('<h2 id="waivers">Waivers</h2>',
         hd("money", "money", "League wallet"),
         '<h4 class="sublabel fighead">Spent after each chop</h4>',
         fig("img/waiver_spending.png", "Bars of the waiver dollars teams spent on winning claims between each chop and the next, per team still alive, split into players freed by the chop and everyone else."),
         '<p class="fine">Each bar is the waiver money teams spent on the claims they won between one chop and the next (the bidding on the chopped team\'s players, plus the waiver runs that follow during the week\'s games), divided by the number of teams still alive, so it is dollars per team. Nobody wins money here: it is the budget each team spends. The last bar is still open.</p>',
         if (file.exists(file.path(out_dir, "img", "waiver_budgets.png"))) paste0('<h4 class="sublabel fighead">Budget left, team by team</h4>', fig("img/waiver_budgets.png", "One line per team: the waiver budget remaining after each waiver period, starting at $1,000. A chopped team's line ends with a red cross.")),
         wait_html(out_dir),
         hd("worth", "worth", "Was it worth it?"),
         waiver_value_interactive_html(readr::read_csv(f, show_col_types = FALSE)),
         '<p class="fine"><b>This figure updates every week.</b> Dots move up as the players keep scoring for their new teams, and hollow dots fill in once the players have played. Click or tap a dot for the details, or pick a team to see only its claims.</p>',
         waiver_value_text(readr::read_csv(f, show_col_types = FALSE), out_dir),
         waiver_habits_html(out_dir),
         hd("roi", "roi", "Waiver return on investment, by manager"),
         '<p class="fine">Total points is, for each claim, the team\'s best-ball score with the player minus the score without him, summed over the weeks he was on the roster after the claim; the next best player on the roster takes his spot in the second score. Average per game divides each claim\'s points by the games it was held and adds the claims up: the points per game a manager\'s claims add to the team together. Points per $100 is that average per game divided by the money paid on claims whose player has played, times 100. Claims for a player who has not played yet have no games and are left out of it. Best claim is the claim with the highest average per game. Worst claim is the lowest points per $100 among claims of $10 or more whose player has played. Both are per game, like the figure and the lists above. Click a heading to sort.</p>',
         html_table(df, num_cols = c("Claims", "FAAB spent", "Total points", "Average per game", "Points per $100"), sortable = TRUE,
                    sort_vals = list(Claims = tb$claims, `FAAB spent` = tb$spent, `Total points` = round(tb$added, 2), `Average per game` = round(tb$avg_pg, 2), `Points per $100` = round(tb$per100, 2))))
}

# Each team's FAAB budget after each waiver period: x = 0 is the start ($1,000), x = p is the end of the period after the week p chop (all claims up to the next chop).
# A chopped team's line stops at the last point before its chop. Returns team_name, owner_name, x, remaining, chopped_week.
faab_history <- function(ctx, budget = FAAB_BUDGET) {
  won <- ctx$claims[ctx$claims$status == "complete", ]
  won$period <- vapply(won$ts, function(t) sum(ctx$chops$ts < t), numeric(1))
  K <- nrow(ctx$chops); tt <- ctx$team_tbl
  dplyr::bind_rows(lapply(seq_len(nrow(tt)), function(i) {
    rid <- tt$roster_id[i]; cw <- ctx$chops$week[ctx$chops$rid == rid]; cw <- if (length(cw)) cw[1] else NA_integer_
    x <- 0:(if (is.na(cw)) K else cw - 1)
    spent <- vapply(x, function(p) if (p == 0) 0 else sum(won$bid[won$rid == rid & won$period <= p], na.rm = TRUE), numeric(1))
    tibble::tibble(team_name = tt$team_name[i], owner_name = tt$owner_name[i], x = x, remaining = budget - spent, chopped_week = cw)
  }))
}


# The sidelined, paid-for claims that belong to THIS week's recap: the player missed this week's game with an injury, or he was claimed for next week and is ruled out
# for it as of now. Biggest price first, at most four. NULL when there are none. roi = the waiver_roi() table.
sidelined_for_week <- function(roi, week) {
  if (is.null(roi) || !nrow(roi)) return(NULL)
  s <- roi[roi$bid > 0 & !is.na(roi$bid), ]
  if (!"missed_weeks" %in% names(s) || !nrow(s)) return(NULL)
  s$missed_this <- vapply(strsplit(as.character(s$missed_weeks), ","), function(z) week %in% suppressWarnings(as.integer(z[nzchar(z)])), logical(1))
  s$out_next <- dplyr::coalesce(as.logical(s$on_roster_now), FALSE) & s$status_now %in% c("Out", "IR", "PUP") & s$for_week == week + 1L
  s <- s[s$missed_this | s$out_next, ]
  if (!nrow(s)) return(NULL)
  head(s[order(-s$bid), ], 4)
}

# For the waiver recap only the week's fresh pickups count: a claim picked up for this week that missed this week's game, or one claimed for next week that is
# already ruled out. An older claim that sits out a game is not waiver news (the chopped team's own block still uses the full list).
sidelined_fresh <- function(sd, week) {
  if (is.null(sd) || !nrow(sd)) return(sd)
  sd[(sd$missed_this & sd$for_week == week) | sd$out_next, , drop = FALSE]
}

# The fact-sheet block. Plain facts only: no injury type, no return date.
sidelined_fact_lines <- function(sd, week) {
  sd <- sidelined_fresh(sd, week)
  if (is.null(sd) || !nrow(sd)) return(NULL)
  c("PAID FOR, THEN SIDELINED (waiver claims that cost $1 or more, picked up for this week or claimed for next week, where the player then went out. Older claims are left out on purpose. Say it in plain words: the player was ruled out, missed the game, or is ruled out for the coming week. Do not name the injury or the body part, do not say how it happened, and do not give or guess a return date or how long he will be out. Mention at most two, the biggest prices first):",
    sprintf("- %s, %s: claimed by %s, managed by %s, for $%.0f, picked up for week %d; %s.", sub(", .*$", "", sd$player), sd$position, sd$team_name, manager_label(sd$owner_name), sd$bid, sd$for_week,
            ifelse(sd$missed_this, "was ruled out for this week's game and did not play", sprintf("is ruled out for week %d, the coming week", sd$for_week))))
}

# The chopped team's own sidelined players, for 'Why it fell short': paid-for claims of the chopped team that missed this week's game, with what Sleeper projected for the
# player when the week's preview was built (the preview only lists players who would have been in the lineup if healthy). Plain facts; no injury type, no return date.
chopped_sidelined_lines <- function(sd, chopped_team, out_dir, week) {
  if (is.null(sd) || !nrow(sd) || is.null(chopped_team) || is.na(chopped_team) || is.null(out_dir)) return(NULL)
  d <- sd[sd$team_name == chopped_team & sd$missed_this, ]
  if (!nrow(d)) return(NULL)
  f <- file.path(out_dir, sprintf("preview_injuries_w%02d.csv", week))
  pv <- if (file.exists(f)) readr::read_csv(f, show_col_types = FALSE) else NULL
  nm <- sub(", .*$", "", d$player)
  k <- if (is.null(pv)) rep(NA_integer_, nrow(d)) else match(paste(nm, chopped_team), paste(pv$player, pv$team_name))
  c("SIDELINED PLAYER ON THE CHOPPED TEAM (a waiver claim of the chopped team that was ruled out for this week's game and did not play. Say it in plain words under 'Why it fell short', first, in one or two sentences: the manager, the player, that he was ruled out and did not play, the price paid, and, where given, what Sleeper projected for him and that he would have been in the lineup if healthy. Call it a big part of why the team fell short. Do not name the injury or the body part, do not give or guess a return date, and do not say what the team's score or result would have been with him):",
    sprintf("- %s, %s: claimed by %s, managed by %s, for $%.0f; he was ruled out for this week's game and did not play.%s", nm, d$position, d$team_name, manager_label(d$owner_name), d$bid,
            ifelse(is.na(k), "", sprintf(" Sleeper projected him for %.1f points as of %s, and he would have been in the team's starting lineup if healthy.", pv$healthy_proj[k], pv$asof[k]))),
    "")
}

# Players who finally paid off: a claim made in an EARLIER week (held at least two games before this one) that added very little until this week and added a lot this week.
# Biggest jump first, at most six. NULL when none. roi = waiver_roi() table; `week` = the recap week.
paying_off_for_week <- function(roi, week) {
  if (is.null(roi) || !nrow(roi) || !all(c("added_last", "scored_last", "scored_total", "last_week_held") %in% names(roi))) return(NULL)
  s <- roi[!is.na(roi$last_week_held) & roi$last_week_held == week & roi$for_week < week & roi$weeks_held >= 3 & !is.na(roi$added_last), ]
  if (!nrow(s)) return(NULL)
  s$prior_avg <- (s$points_added - s$added_last) / (s$weeks_held - 1)   # what he added to the team's score per game before this week (selection only; never shown)
  s <- s[s$prior_avg < 4 & s$added_last >= 6, ]
  if (!nrow(s)) return(NULL)
  s$games_before <- s$weeks_held - 1L
  s$scored_before_avg <- (s$scored_total - s$scored_last) / s$games_before   # the player's own average points scored in those games (what the text shows)
  # the text talks about the PLAYER's points, so he must really have jumped: at least 8 points this week and 5 or more above his own earlier average
  # (a player who added a lot to the team's score only because the next man up was weak did not "finally pay off")
  s <- s[!is.na(s$scored_last) & s$scored_last >= 8 & s$scored_last >= s$scored_before_avg + 5, ]
  if (!nrow(s)) return(NULL)
  head(s[order(-s$added_last), ], 6)
}

payoff_fact_lines <- function(po) {
  if (is.null(po) || !nrow(po)) return(NULL)
  c("FINALLY PAYING OFF (earlier waiver claims that scored very little for the claiming team until this week and then scored real points. List every player below in 'Waiver results' in one short paragraph, and add a sentence in the team's own paragraph too where it has one. Say only how many points the PLAYER scored, never how many points he added to the team's score, and use only the figures below):",
    sprintf("- %s, %s: claimed by %s, managed by %s, for $%.0f in week %d; over his first %d games with the team he averaged only %.1f points a game, then scored %.1f this week.",
            sub(", .*$", "", po$player), po$position, po$team_name, manager_label(po$owner_name), po$bid, po$for_week, po$games_before, po$scored_before_avg, po$scored_last),
    "")
}

payoff_template_text <- function(po) {
  if (is.null(po) || !nrow(po)) return(NULL)
  paste(sprintf("%s finally paid off for %s: after averaging only %.1f points a game over his first %d games, he scored %.1f this week.", sub(", .*$", "", po$player), manager_label(po$owner_name),
                po$scored_before_avg, po$games_before, po$scored_last), collapse = " ")
}

sidelined_template_text <- function(sd, chopped = FALSE, week = NULL) {
  if (!chopped && !is.null(week)) sd <- sidelined_fresh(sd, week)
  if (is.null(sd) || !nrow(sd)) return(NULL)
  if (chopped) return(sprintf("%s paid $%.0f for %s, who was ruled out and did not play this week, and that is a big part of why the team fell short.", manager_label(sd$owner_name[1]), sd$bid[1], sub(", .*$", "", sd$player[1])))
  sd <- head(sd, 2)
  paste(sprintf("%s paid $%.0f for %s, who %s.", manager_label(sd$owner_name), sd$bid, sub(", .*$", "", sd$player),
                ifelse(sd$missed_this, "was ruled out and did not play this week", sprintf("is ruled out for week %d", sd$for_week))), collapse = " ")
}

# Plain-language reading of the "Was it worth it?" figure, from the data: who paid off, who did not, and who is still waiting for a first game.
waiver_value_text <- function(cl, out_dir = NULL) {
  cl$played <- cl$weeks_held > 0; cl$avg_pg <- ifelse(cl$played, cl$points_added / pmax(cl$weeks_held, 1), 0)   # average points per game
  cl$p100 <- ifelse(cl$bid > 0, 100 * cl$avg_pg / cl$bid, NA_real_)
  who <- function(d) sprintf("%s, %s (week %d)", d$player, d$team_name, d$for_week)
  line <- function(d) sprintf("%s: %.1f points per $100", who(d), d$p100)   # one line per claim
  good <- cl[cl$played & cl$bid >= 10, ]; good <- head(good[order(-good$p100), ], 3)
  big <- cl[cl$played & cl$bid >= 100, ]; big_best <- head(big[order(-big$p100), ], 2); big_worst <- head(big[order(big$p100), ], 2)
  done_wk <- max(cl$n_chops, na.rm = TRUE)   # weeks played so far (there is one chop a week)
  waiting <- cl[!cl$played & cl$for_week > done_wk, ]; waiting_big <- head(waiting[order(-waiting$bid), ], 3)     # picked up for a week that has not been played yet
  # projected points per $100 for the players still waiting: the pickup's projected gain to the team's score for the coming week (from that week's preview
  # data, saved at its first build) divided by the price. Claims the preview did not cover (made after it was built) just show no projection.
  pvf <- if (!is.null(out_dir)) file.path(out_dir, sprintf("preview_pickups_w%02d.csv", done_wk + 1L)) else ""
  pv <- if (nzchar(pvf) && file.exists(pvf)) readr::read_csv(pvf, show_col_types = FALSE) else NULL
  proj100 <- function(d) {
    if (is.null(pv) || !nrow(d)) return(rep("", nrow(d)))
    vapply(seq_len(nrow(d)), function(i) {
      k <- which(pv$player == d$player[i] & pv$team_name == d$team_name[i]); if (!length(k) || is.na(pv$adds[k[1]])) return("")
      if (d$bid[i] > 0) sprintf(": projected %.1f points per $100", 100 * pv$adds[k[1]] / d$bid[i]) else if (pv$adds[k[1]] > 0) ": projected gain with no cost, a free claim" else ""
    }, character(1))
  }
  cnt <- function(d, what) sprintf("%d %s worth $%s in all, %s The largest:", nrow(d), if (nrow(d) == 1) "claim" else "claims", format(round(sum(d$bid)), big.mark = ","), what)
  sub <- function(lead, items, intro = NULL) paste0('<h4 class="sublabel">', h(lead), "</h4>", if (!is.null(intro)) paste0('<p class="note">', h(intro), "</p>") else "",
                                                     '<ul class="wvlist">', paste0("<li>", h(items), "</li>", collapse = ""), "</ul>")
  paste0('<h3 id="season-reading" class="wvh h-reading">Best and worst claims</h3>',
    '<p class="fine">Everything below is in points per $100: the average points per game a claim has added, divided by its price, times 100. The dashed lines on the figure are 3 and 10. Because it is per game, a claim from an earlier week is not favored for having had more games. The week is the week the player was picked up.</p>',
    if (nrow(good)) sub("Best value (claims of $10 or more)", line(good)),
    if (nrow(big_best)) sub("Big spenders ($100 or more) that paid off", line(big_best)),
    if (nrow(big_worst)) sub("Big spenders that did not", line(big_worst)),
    if (nrow(waiting)) sub("Waiting for a first game", sprintf("%s, %s ($%.0f)%s", waiting_big$player, waiting_big$team_name, waiting_big$bid, proj100(waiting_big)),
                           intro = cnt(waiting, paste0(sprintf("picked up for week %d, which has not been played yet.", done_wk + 1L),
                                                       if (!is.null(pv)) " Projected points per $100 is the pickup's projected gain to the team's score for that week, divided by its price." else "")))
)
}

# --- Waiver results text -----------------------------------------------------------------------------------------
# Rules the written section and the page both use: a massive overbid is a winning price at least $50 and at least 25% of the price above the runner-up bid;
# a close miss is a runner-up bid within $5 or 5% of the price (or tied with it) on a player who cost at least $10.
overbids <- function(res) { d <- res[!is.na(res$overpay) & res$overpay >= 50 & res$overpay >= 0.25 * res$price, ]; head(d[order(-d$overpay), ], 3) }
close_misses <- function(res) { d <- res[!is.na(res$runner_up) & res$price >= 10 & res$overpay <= pmax(5, 0.05 * res$price), ]; head(d[order(d$overpay, -d$price), ], 3) }

# The "WAIVER RESULTS" block of the fact sheet. res = waiver_results() for the week (NULL for week 1 or when nothing was claimed).
waiver_fact_lines <- function(res) {
  if (is.null(res) || !nrow(res)) return(NULL)
  who <- function(t, o) sprintf("%s (managed by %s)", t, manager_label(o))
  ob <- overbids(res); cm <- close_misses(res)
  ob_l <- if (nrow(ob)) vapply(seq_len(nrow(ob)), function(i) { r <- ob[i, ]
    sprintf("- %s: %s paid $%.0f, the runner-up bid was $%.0f (%d bids in all), so the winner paid $%.0f above the runner-up. The player scored %.1f this week against a projection of %s.",
            r$player, who(r$winner_team, r$winner_owner), r$price, r$runner_up, r$bids, r$overpay, r$scored, if (is.na(r$projected)) "none" else sprintf("%.1f", r$projected)) }, character(1)) else "none this week."
  cm_l <- if (nrow(cm)) vapply(seq_len(nrow(cm)), function(i) { r <- cm[i, ]
    if (r$overpay == 0) sprintf("- %s: %s and %s both bid $%.0f (%d bids in all); %s won the tie and %s missed out.", r$player, who(r$winner_team, r$winner_owner), who(r$runner_up_team, r$runner_up_owner), r$price, r$bids, r$winner_team, r$runner_up_team)
    else sprintf("- %s: %s paid $%.0f and %s bid $%.0f (%d bids in all), missing out by $%.0f.", r$player, who(r$winner_team, r$winner_owner), r$price, who(r$runner_up_team, r$runner_up_owner), r$runner_up, r$bids, r$overpay) }, character(1)) else "none this week."
  c(sprintf("WAIVER RESULTS (the waiver claims on the %d players freed by last week's chop, processed before this week's games; %d were claimed, for $%s in all. The price is what the winner paid; the runner-up bid is the highest losing bid. Write the 'Waiver results' section from this block only):",
            attr(res, "freed"), nrow(res), format(round(sum(res$price)), big.mark = ",")),
    "MASSIVE OVERBIDS (the winner paid at least $50 and at least 25% of the price above the runner-up bid):", ob_l,
    "JUST MISSED OUT (the runner-up bid was within $5 or 5% of the price, or tied it, on a player who cost at least $10):", cm_l,
    "End the FIRST paragraph of the section (the one about the auction) with this exact sentence: Will it pay off? Follow along in the season overview.")
}

# The same as plain sentences, for the automatic text
waiver_template_text <- function(res) {
  if (is.null(res) || !nrow(res)) return(NULL)
  ob <- overbids(res); cm <- close_misses(res)
  m <- function(o) manager_label(o)
  c(sprintf("%d of the players freed by last week's chop were claimed, for $%s in all.", nrow(res), format(round(sum(res$price)), big.mark = ",")),
    if (nrow(ob)) paste(sprintf("%s paid $%.0f for %s when the runner-up bid was $%.0f.", m(ob$winner_owner), ob$price, sub(", .*$", "", ob$player), ob$runner_up), collapse = " "),
    if (nrow(cm)) paste(ifelse(cm$overpay == 0, sprintf("%s tied the winning bid of $%.0f for %s and lost the tie.", m(cm$runner_up_owner), cm$price, sub(", .*$", "", cm$player)),
                               sprintf("%s missed out on %s by $%.0f.", m(cm$runner_up_owner), sub(", .*$", "", cm$player), cm$overpay)), collapse = " "),
    "Will it pay off? Follow along in the season overview.")
}

# --- Manager habits ---------------------------------------------------------------------------------------------------
# One row per team, from every real claim (won, or lost to another owner; claims rejected for a full roster are ignored): claims placed and won, the money
# left on the table (price minus the runner-up bid, summed over wins that had competition), near-misses (lost by $5 or 5% of the winning price or less, on a
# winning price of $10 or more), dollars spent and left. Written to waiver_habits.csv for the Season overview.
waiver_habits <- function(ctx, budget = FAAB_BUDGET) {
  cl <- ctx$claims; cl <- cl[cl$status == "complete" | grepl("claimed by another", cl$note, fixed = TRUE), ]
  won <- cl[cl$status == "complete", ]; lost <- cl[cl$status != "complete", ]
  lost$win_bid <- vapply(seq_len(nrow(lost)), function(i) { w <- won[won$add == lost$add[i] & won$run == lost$run[i], ]; if (nrow(w)) max(w$bid) else NA_real_ }, numeric(1))
  won$runner <- vapply(seq_len(nrow(won)), function(i) { l <- lost[lost$add == won$add[i] & lost$run == won$run[i], ]; if (nrow(l)) max(l$bid) else NA_real_ }, numeric(1))
  tt <- ctx$team_tbl
  dplyr::bind_rows(lapply(seq_len(nrow(tt)), function(i) {
    rid <- tt$roster_id[i]; w <- won[won$rid == rid, ]; l <- lost[lost$rid == rid & !is.na(lost$win_bid), ]
    cw <- ctx$chops$week[ctx$chops$rid == rid]
    tibble::tibble(team_name = tt$team_name[i], owner_name = tt$owner_name[i], chopped_week = if (length(cw)) cw[1] else NA_integer_,
                   placed = nrow(w) + nrow(l), won = nrow(w), spent = sum(w$bid, na.rm = TRUE), left = budget - sum(w$bid, na.rm = TRUE),
                   contested_wins = sum(!is.na(w$runner)), left_on_table = sum(w$bid - w$runner, na.rm = TRUE),
                   near_misses = sum(l$win_bid >= 10 & (l$win_bid - l$bid) <= pmax(5, 0.05 * l$win_bid)))
  }))
}

# A few plain sentences about how the managers have used waivers (from waiver_habits.csv). "" until there is something to say.
waiver_habits_html <- function(out_dir) {
  f <- file.path(out_dir, "waiver_habits.csv"); if (!file.exists(f)) return("")
  d <- readr::read_csv(f, show_col_types = FALSE); d <- d[d$placed > 0 | d$spent > 0, ]; if (!nrow(d)) return("")
  nm <- function(r) sprintf("%s (%s%s)", r$team_name, manager_label(r$owner_name), ifelse(is.na(r$chopped_week), "", sprintf(", chopped in week %d", r$chopped_week)))
  top <- function(col, n = 1, min_val = -Inf, dec = TRUE, ok = rep(TRUE, nrow(d))) { v <- d[[col]]; i <- which(ok & v >= min_val); if (!length(i)) return(NULL); i <- i[order(if (dec) -v[i] else v[i])]; d[head(i, n), ] }
  li <- character(0)
  add <- function(label, txt) li <<- c(li, paste0('<h4 class="sublabel">', label, "</h4>", '<p class="note">', txt, "</p>"))
  a <- top("spent"); if (!is.null(a) && a$spent > 0) add("Biggest spender", sprintf("%s has spent $%s of $%s and has $%s left.", h(nm(a)), format(a$spent, big.mark = ","), format(FAAB_BUDGET, big.mark = ","), format(a$left, big.mark = ",")))
  a <- top("placed"); if (!is.null(a)) add("Most active", sprintf("%s has placed %d claims and won %d.", h(nm(a)), a$placed, a$won))
  a <- top("won", ok = d$placed >= 5); if (!is.null(a)) { d$rate <- d$won / pmax(d$placed, 1)
    b <- d[d$placed >= 5, ]; b <- b[order(-b$rate, -b$placed), ][1, ]; c0 <- d[d$placed >= 5, ]; c0 <- c0[order(c0$rate, -c0$placed), ][1, ]
    li <- c(li, paste0('<h4 class="sublabel">Hit rate (teams with five or more claims)</h4><ul class="wvlist">',
                       sprintf("<li><b>Highest:</b> %s has won %d of %d (%.0f%%).</li>", h(nm(b)), b$won, b$placed, 100 * b$rate),
                       sprintf("<li><b>Lowest:</b> %s has won %d of %d (%.0f%%).</li>", h(nm(c0)), c0$won, c0$placed, 100 * c0$rate), "</ul>")) }
  a <- top("left_on_table"); if (!is.null(a) && a$left_on_table > 0) add("Most money left on the table", sprintf("%s, who paid $%s more than the runner-up bid across %d contested %s.", h(nm(a)), format(a$left_on_table, big.mark = ","), a$contested_wins, if (a$contested_wins == 1) "win" else "wins"))
  a <- top("near_misses"); if (!is.null(a) && a$near_misses >= 1) add("Most near misses", sprintf("%s has lost %d %s by $5 or 5%% of the winning price or less.", h(nm(a)), a$near_misses, if (a$near_misses == 1) "bid" else "bids"))
  idle <- d[is.na(d$chopped_week) & d$won == 0, ]
  if (nrow(idle)) add("Still waiting for a first win", sprintf("%s.", paste(h(vapply(seq_len(nrow(idle)), function(k) sprintf("%s (%s)", idle$team_name[k], manager_label(idle$owner_name[k])), character(1))), collapse = "; ")))
  if (!length(li)) return("")
  paste0('<h3 id="season-habits" class="wvh h-habits">Manager habits</h3>', paste(li, collapse = ""))
}

# --- Interactive "Was it worth it?" ----------------------------------------------------------------------------------------
# The same chart as img/waiver_value.png drawn with plotly.js (loaded from cdn.plot.ly): hover or tap a dot for the details, pick a team in the drop-down to see only its
# claims. The page shows the PNG until plotly has loaded and the Season overview tab is open, so a blocked or slow library never leaves a gap.
WAIVER_PLOTLY_JS <- r"--(
(function(){
var D=__DATA__, el=null, drawn=false;
var CS=[[0,'#FF6482'],[0.33,'#FFC857'],[0.66,'#4CD787'],[1,'#00FFF9']], TOP=30, GUIDES=[3,10];
function esc(s){return String(s).replace(/[&<>"]/g,function(c){return {'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;'}[c];});}
function money(x){return '$'+Math.round(x).toLocaleString('en-US');}
function avgv(r){return r.held>0?r.added/r.held:0;}   // average points per game (0 until the player has played)
function p100(r){return r.bid>0?100*avgv(r)/r.bid:null;}
function p100txt(r){if(r.held===0)return r.waiting?'waiting for a first game (week '+r.week+')':'dropped before the week ended';if(r.bid>0)return (100*avgv(r)/r.bid).toFixed(1);return r.added>0?'free claim':'0.0';}
function avg(r){return r.held>0?(r.added/r.held).toFixed(1)+' points':(r.waiting?'waiting for a first game':'dropped before the week ended');}
function tip(r){return '<b>'+esc(r.player)+'</b> ('+esc(r.position)+')<br>'+esc(r.team)+' ('+esc(r.manager)+')<br>Picked up for week '+r.week+'<br>Source: '+(r.freed?('freed by '+esc(r.freed)+'\u2019s team when it was chopped'+(r.freedweek?' (week '+r.freedweek+')':'')):'not from the chopped team')+'<br>Cost: '+money(r.bid)+'<br>Points per $100: '+p100txt(r)+'<br>Total points added: '+(r.added||0).toFixed(1)+'<br>Average per game: '+avg(r)+(r.held>0?' (over '+r.held+(r.held===1?' game)':' games)'):'');}
D.forEach(function(r,i){r.i=i;});
function rng(s){var bids=s.map(function(r){return r.bid;}),pts=s.map(avgv);return {xmax:Math.max(10,Math.max.apply(null,bids)*1.08),ymax:Math.max(2,Math.max.apply(null,pts))*1.15,ymin:Math.min(0,Math.min.apply(null,pts))*1.1};}   // axis limits follow the claims on show
function col(r){var v=p100(r);return v===null?(r.added>0?TOP:0):Math.min(v,TOP);}
function dots(s,played){
  var q=s.filter(function(r){return (r.held>0)===played;});
  return {type:'scatter',mode:'markers',x:q.map(function(r){return r.bid;}),y:q.map(avgv),customdata:q.map(function(r){return r.i;}),
    hovertext:q.map(tip),hoverinfo:'text',showlegend:false,
    marker:played?{symbol:'circle',size:12,color:q.map(col),colorscale:CS,cmin:0,cmax:TOP,line:{color:'#05091D',width:1}}
                 :{symbol:'circle-open',size:11,color:'#98B3D6',line:{width:2}}};
}
function key(name,symbol,color){return {type:'scatter',mode:'markers',x:[null],y:[null],name:name,hoverinfo:'skip',marker:{symbol:symbol,size:10,color:color}};}
function build(team){
  var s=D.filter(function(r){return !team||r.team===team;}),R=rng(s),xmax=R.xmax,ymax=R.ymax,ymin=R.ymin;
  var tr=[dots(s,true),dots(s,false)];   // the legend and color scale are plain HTML above the chart (they wrap on a phone)
  var pick=team?s.slice().sort(function(a,b){return b.bid-a.bid;}).slice(0,8)
    :s.filter(function(r){return r.held>0;}).sort(function(a,b){return avgv(b)-avgv(a);}).slice(0,4).concat(s.filter(function(r){return r.held>0;}).sort(function(a,b){return b.bid-a.bid;}).filter(function(r){return true;}).slice(0,2));
  // labels: placed greedily around each dot, at the first spot that clears the other labels, the dots, the guide-line labels and the plot edge
  var W=(el&&el._fullLayout&&el._fullLayout.width)||(el&&el.clientWidth)||820,H=540,ML=54,MR=12,MT=14,MB=46,PW=W-ML-MR,PH=H-MT-MB;
  function px(x){return ML+x/xmax*PW;}
  function py(y){return MT+(1-(y-ymin)/(ymax-ymin))*PH;}
  var ann=[],boxes=[];
  var shapes=GUIDES.map(function(k){var x1=Math.min(xmax,ymax/(k/100));return {type:'line',x0:0,y0:0,x1:x1,y1:x1*k/100,line:{color:'#98B3D6',width:1,dash:'dash'}};});
  GUIDES.forEach(function(k){var x1=Math.min(xmax,ymax/(k/100)),f=x1<xmax?1:0.78,gx=x1*f,gy=gx*k/100,X=px(gx),Y=py(gy);
    ann.push({x:gx,y:gy,text:k+' per $100',showarrow:false,xanchor:x1<xmax?'left':'center',yanchor:'top',yshift:x1<xmax?0:-6,font:{size:10,color:'#C9D6EE'}});
    boxes.push(x1<xmax?[X,Y,X+64,Y+14]:[X-32,Y+6,X+32,Y+20]);});
  var dotPx=s.map(function(r){return [px(r.bid),py(avgv(r))];});
  function free(b){
    if(b[0]<ML+2||b[2]>ML+PW-2||b[1]<MT+2||b[3]>MT+PH-2)return false;
    for(var i=0;i<boxes.length;i++){var o=boxes[i];if(b[0]<o[2]+3&&b[2]>o[0]-3&&b[1]<o[3]+3&&b[3]>o[1]-3)return false;}
    for(var j=0;j<dotPx.length;j++){var cx=Math.max(b[0],Math.min(dotPx[j][0],b[2])),cy=Math.max(b[1],Math.min(dotPx[j][1],b[3]));if(Math.sqrt(Math.pow(cx-dotPx[j][0],2)+Math.pow(cy-dotPx[j][1],2))<9)return false;}
    return true;}
  var seen={},angles=[-40,-140,40,140,-90,90,0,180,-15,-165,15,165,-65,-115,65,115],gaps=[14,30,52,80,115,150];
  pick.forEach(function(r){
    if(seen[r.i])return;seen[r.i]=1;
    var lines=team?[r.player]:[r.player,r.team+' (week '+r.week+')'];
    var w=Math.max.apply(null,lines.map(function(l){return l.length;}))*5.7+10,h=lines.length*12.5+5,x=px(r.bid),y=py(avgv(r)),best=null;
    for(var gi=0;gi<gaps.length&&!best;gi++)for(var ai=0;ai<angles.length&&!best;ai++){
      var t=angles[ai]*Math.PI/180,dx=Math.cos(t),dy=Math.sin(t),ext=Math.abs(dx)*w/2+Math.abs(dy)*h/2,cx=x+(gaps[gi]+ext)*dx,cy=y+(gaps[gi]+ext)*dy,bx=[cx-w/2,cy-h/2,cx+w/2,cy+h/2];
      if(free(bx))best={cx:cx,cy:cy,bx:bx};}
    if(!best)return;   // no clear spot: leave the dot unlabeled (tap it for the details) rather than cover another label
    boxes.push(best.bx);
    ann.push({x:r.bid,y:avgv(r),text:lines.map(esc).join('<br>'),showarrow:true,arrowhead:0,arrowcolor:'#98B3D6',arrowwidth:1,axref:'pixel',ayref:'pixel',ax:best.cx-x,ay:best.cy-y,xanchor:'center',yanchor:'middle',font:{size:10,color:'#F4F6FB'},bgcolor:'rgba(5,9,29,.75)',borderpad:2});});
  return {data:tr,layout:{paper_bgcolor:'#05091D',plot_bgcolor:'#05091D',font:{color:'#F4F6FB',size:12},margin:{l:54,r:12,t:14,b:46},
    xaxis:{title:{text:'Dollars paid'},tickprefix:'$',range:[0,xmax],gridcolor:'#25345D',zerolinecolor:'#5A6FA3',fixedrange:true},
    yaxis:{title:{text:"Average points per game added to the team's best-ball score"},range:[ymin,ymax],gridcolor:'#25345D',zerolinecolor:'#5A6FA3',fixedrange:true},
    showlegend:false,hoverlabel:{bgcolor:'#131B38',bordercolor:'#00FFF9',font:{color:'#F4F6FB',size:12},align:'left'},
    annotations:ann,shapes:shapes},sum:s,W:W};
}
function summary(s,team){
  if(!team)return '';
  var spent=s.reduce(function(a,r){return a+r.bid;},0),pts=s.reduce(function(a,r){return a+r.added;},0),apg=s.reduce(function(a,r){return a+avgv(r);},0),ps=s.filter(function(r){return r.held>0;}).reduce(function(a,r){return a+r.bid;},0);
  return esc(team)+' ('+esc(s[0]?s[0].manager:'')+'): '+s.length+(s.length===1?' claim, ':' claims, ')+money(spent)+' spent, '+pts.toFixed(1)+' total points added ('+apg.toFixed(1)+' per game across the claims)'+(ps>0?', '+(100*apg/ps).toFixed(1)+' points per $100 on the claims that have played.':'.');
}
function draw(keep,again){
  var sel=document.getElementById('waiver-team'),team=sel?sel.value:'',b=build(team);
  var cfg={responsive:true,displaylogo:false,displayModeBar:false};
  document.getElementById('waiver-summary').innerHTML=summary(b.sum,team);
  if(!keep)document.getElementById('waiver-detail').innerHTML='Click or tap a dot for the details.';
  Plotly.react(el,b.data,b.layout,cfg).then(function(){var w=el._fullLayout&&el._fullLayout.width;if(!again&&w&&Math.abs(w-b.W)>2)draw(true,true);});
}
window.initWaiverPlot=function(){
  var view=document.getElementById('view-season');
  if(!window.Plotly||!view||view.hidden)return;
  el=document.getElementById('waiver-plotly'); if(!el)return;
  if(drawn){Plotly.Plots.resize(el);return;}
  drawn=true;
  document.getElementById('waiver-static').hidden=true;document.getElementById('waiver-interactive').hidden=false;
  draw(false);
  el.on('plotly_click',function(e){var p=e.points&&e.points[0];if(!p||p.customdata===undefined)return;document.getElementById('waiver-detail').innerHTML=tip(D[p.customdata]);});
  document.getElementById('waiver-team').addEventListener('change',function(){draw(false);});
  var rt=null;window.addEventListener('resize',function(){clearTimeout(rt);rt=setTimeout(function(){if(drawn)draw(true);},250);});
};
})();
)--"

waiver_value_interactive_html <- function(cl, static_src = "img/waiver_value.png") {
  d <- data.frame(player = cl$player, position = cl$position, team = cl$team_name, manager = vapply(cl$owner_name, manager_label, character(1)), bid = cl$bid,
                  added = round(dplyr::coalesce(cl$points_added, 0), 1), held = cl$weeks_held, week = cl$for_week, chop = as.logical(cl$from_chop), waiting = cl$weeks_held == 0 & cl$for_week > max(cl$n_chops, na.rm = TRUE),
                  freed = if ("freed_owner" %in% names(cl)) vapply(cl$freed_owner, function(x) if (is.na(x)) NA_character_ else manager_label(x), character(1)) else NA_character_,
                  freedweek = if ("freed_week" %in% names(cl)) cl$freed_week else NA_integer_, stringsAsFactors = FALSE)
  json <- gsub("</", "<\\/", as.character(jsonlite::toJSON(d, dataframe = "rows", auto_unbox = TRUE, na = "null")), fixed = TRUE)
  js <- sub("__DATA__", json, WAIVER_PLOTLY_JS, fixed = TRUE)
  tm <- d[!duplicated(d$team), c("team", "manager")]; tm <- tm[order(tolower(tm$team)), ]
  opts <- paste0('<option value="">All teams</option>', paste0('<option value="', h(tm$team), '">', h(tm$team), " (", h(tm$manager), ")</option>", collapse = ""))
  paste0('<figure id="waiver-value-fig">',
         '<div id="waiver-static" class="plate" style="max-width:820px"><img src="', static_src, '" loading="lazy" alt="One dot per waiver claim: dollars paid against the average points per game added to the team\'s best-ball score, colored by points per $100, with guide lines at 3 and 10 points per $100."></div>',
         '<div id="waiver-interactive" style="max-width:820px" hidden>',
         '<p class="note" style="max-width:76ch"><b>Click or tap any dot</b> to see the player, the team he went to, the week he was picked up, whose chopped team freed him (if he came from one), what he cost, his points per $100, the total points he has added and his average per game. ',
         'Each dot is one waiver claim, with dollars paid across and the average points per game it has added up the side; the color is points per $100 (the average per game divided by the price, times 100; teal is good value) and the dashed lines mark 3 and 10 points per $100. Hollow dots have no points yet: the player was picked up for a week that has not been played, or was dropped before the week ended. Pick a team from the menu to see only its claims.</p>',
         '<div class="picker" style="margin:6px 0 4px"><label for="waiver-team">Show</label><select id="waiver-team">', opts, "</select></div>",
         '<p id="waiver-summary" class="fine" style="margin:4px 0"></p>',
         '<style>.wv-legend{display:flex;flex-wrap:wrap;gap:6px 18px;align-items:center;color:var(--muted);font-size:.8rem;margin:6px 0}.wv-legend .g{display:flex;flex-direction:column;min-width:180px}.wv-grad{height:9px;border-radius:5px;background:linear-gradient(90deg,#FF6482 0%,#FFC857 33%,#4CD787 66%,#00FFF9 100%)}.wv-ticks{display:flex;justify-content:space-between;font-size:.7rem;color:var(--dim)}.wv-dot{display:inline-block;width:10px;height:10px;border-radius:50%;background:#C9D6EE;margin-right:6px;vertical-align:-1px}.wv-dot.o{background:transparent;border:2px solid #98B3D6;width:8px;height:8px}</style>',
         '<div class="wv-legend"><div class="g"><span>Points per $100</span><span class="wv-grad"></span><span class="wv-ticks"><span>0</span><span>10</span><span>20</span><span>30+</span></span></div>',
         '<span><span class="wv-dot"></span>Has played for the team</span><span><span class="wv-dot o"></span>No points yet</span></div>',
         '<div class="plate" style="overflow:hidden"><div id="waiver-plotly" style="width:100%;height:540px"></div></div>',
         '<div id="waiver-detail" class="ai" style="border-left-color:var(--accent);max-width:820px">Click or tap a dot for the details.</div>',
         "</div></figure>",
         "<script>", js, "</script>",
         '<script src="https://cdn.plot.ly/plotly-basic-2.35.2.min.js" async onload="window.initWaiverPlot&&window.initWaiverPlot();window.initScorePlot&&window.initScorePlot();window.initBudgetPlot&&window.initBudgetPlot();window.initPoolPlot&&window.initPoolPlot();window.initSpendPlot&&window.initSpendPlot()"></script>')
}
