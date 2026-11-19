# --- Helpers for the weekly recap (collector/weekly_recap.R) -----------------------------------
# Not part of the Shiny app. Everything here reads the replay files that replay_week() and
# replay_league_week_realtime() (R/replay.R) write under accuracy_data/replay/, and turns them into the
# numbers, tables and static page that go in recap/{season}/.

# "Sun 4:43pm" in US Eastern, from a UTC timestamp
fmt_et <- function(wall_utc, fmt = "%a %I:%M%p") {
  x <- format(as.POSIXct(wall_utc, tz = "UTC"), fmt, tz = "America/New_York")
  x <- sub(" 0", " ", x)
  x <- sub("AM$", "am", x)
  sub("PM$", "pm", x)
}

pct0 <- function(x) sprintf("%.0f%%", 100 * x)

# point gaps: two decimals when tiny (a 0.04 finish must not read as 0.0), one otherwise
fmt_gap <- function(x) ifelse(x < 0.1, sprintf("%.2f", x), sprintf("%.1f", x))

# The most recent regular-season week in which every game has a final score (per nflverse's schedule).
latest_complete_week <- function(season) {
  s <- load_nflverse_schedules()
  s <- s[s$season == season & s$game_type == "REG", ]
  done <- tapply(!is.na(s$home_score) & !is.na(s$away_score), s$week, all)
  if (!any(done)) stop("no completed regular-season week found for ", season)
  max(as.integer(names(done)[done]))
}

# How many regular-season games the schedule has for a week (to catch a replay that is missing a game)
scheduled_games <- function(season, week) {
  s <- load_nflverse_schedules()
  sum(s$season == season & s$week == week & s$game_type == "REG")
}

teams_path <- function(season, week) file.path(REPLAY_DIR, sprintf("%s_w%02d_league_teams_realtime.csv.gz", season, week))

# One row of headline numbers for a week, from its team timeline.
recap_facts <- function(season, week) {
  t <- readr::read_csv(teams_path(season, week), show_col_types = FALSE)
  n <- dplyr::n_distinct(t$roster_id)
  fin <- t |> dplyr::distinct(roster_id, team_name, owner_name, final_team_points) |> dplyr::arrange(final_team_points)
  ch <- t |> dplyr::filter(chopped) |> dplyr::arrange(x_min)
  first <- t |> dplyr::filter(x_min == min(x_min)) |> dplyr::arrange(dplyr::desc(elimination_prob))
  last_blk_x <- min(t$x_min[t$block == max(t$block)])
  is_top <- t |> dplyr::group_by(x_min) |>
    dplyr::summarise(top = team_name[which.max(elimination_prob)] == team_name[which(chopped)[1]], .groups = "drop")
  before_last <- ch |> dplyr::filter(block < max(t$block))
  if (nrow(before_last) == 0) before_last <- ch
  pk <- before_last[which.max(before_last$elimination_prob), ]
  lo <- ch[which.min(ch$projected_final), ]
  tibble::tibble(
    season = season, week = week, n_teams = n,
    chopped_team = ch$team_name[1], chopped_owner = ch$owner_name[1], chopped_final = fin$final_team_points[1],
    next_team = fin$team_name[2], next_owner = fin$owner_name[2], next_final = fin$final_team_points[2],
    gap = fin$final_team_points[2] - fin$final_team_points[1],
    pregame_fav = first$team_name[1], pregame_fav_owner = first$owner_name[1], pregame_fav_chance = first$elimination_prob[1], pregame_fav_was_chopped = first$chopped[1],
    chopped_pregame_chance = ch$elimination_prob[1], chopped_pregame_proj = ch$projected_final[1],
    chopped_low_proj = lo$projected_final, low_time = fmt_et(lo$wall_utc),
    peak_chance_before_last = pk$elimination_prob, peak_time = fmt_et(pk$wall_utc),
    chance_at_last_kickoff = ch$elimination_prob[match(last_blk_x, ch$x_min)],
    top_pick_share = mean(is_top$top)
  )
}

ordinal <- function(n) paste0(n, ifelse(n %% 100 %in% 11:13, "th", c("th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th")[n %% 10 + 1]))

# "the 2nd lowest score of the N teams" / "the 6th highest score of the N teams" (place 1 = the lowest score = the chopped team)
place_text <- function(place, n) {
  if (place <= ceiling(n / 2)) sprintf("the %s lowest score of the %d teams", ordinal(place), n)
  else sprintf("the %s highest score of the %d teams", ordinal(n - place + 1), n)
}

# Teams that were NOT chopped but are worth a mention, from the team timeline `t`. Each team gets ONE category; a team that fits
# more than one takes the first in this order:
#   1. "danger" = SAVED THEMSELVES: the survivor with the highest chance of being chopped before any game, if that chance was at least
#      `danger_ratio` times the even split (1 in n). At most one team a week.
#   2. "close" = CLOSE CALL: a survivor whose chance of being chopped climbed to at least `close_peak` once games were on, and at
#      least 5 points above its pregame chance; up to `n_close`, highest peaks first.
#   3. "beat" = MASSIVELY OUTPERFORMED its projection: a survivor (not in 1 or 2) whose finish above its pregame projected score is at least
#      `beat_over_median` points MORE than the typical team's (the median over all teams that week, so a week in which the whole league beats its
#      projections does not make everyone a "massive" outperformer); up to `n_beat`, biggest first.
# Returns one row per team in that order. peak_x is the timeline position of the team's highest chance of being chopped.
saved_selection <- function(t, danger_ratio = 2, close_peak = 0.25, n_close = 2, beat_over_median = 20, n_beat = 2) {
  n <- dplyr::n_distinct(t$roster_id)
  per <- t |> dplyr::arrange(x_min) |> dplyr::group_by(roster_id, team_name, owner_name) |>
    dplyr::summarise(pre_prob = dplyr::first(elimination_prob), pre_proj = dplyr::first(projected_final),
                     peak = max(elimination_prob), peak_wall = wall_utc[which.max(elimination_prob)], peak_x = x_min[which.max(elimination_prob)],
                     score_at_peak = current_score[which.max(elimination_prob)],
                     final = dplyr::first(final_team_points), chopped = any(chopped), .groups = "drop") |>
    dplyr::arrange(final) |> dplyr::mutate(place = dplyr::row_number())
  chop_final <- per$final[per$chopped][1]
  med_beat <- stats::median(per$final - per$pre_proj)   # how far above its projection the typical team finished this week
  sv <- per[!per$chopped, ]
  if (!nrow(sv)) return(sv[0, ])
  sv$category <- NA_character_
  d <- which.max(sv$pre_prob)
  if (sv$pre_prob[d] >= danger_ratio / n) sv$category[d] <- "danger"
  cl <- which(is.na(sv$category) & sv$peak >= close_peak & sv$peak - sv$pre_prob >= 0.05)
  sv$category[utils::head(cl[order(-sv$peak[cl])], n_close)] <- "close"
  bt <- which(is.na(sv$category) & (sv$final - sv$pre_proj) - med_beat >= beat_over_median)
  sv$category[utils::head(bt[order(-(sv$final[bt] - sv$pre_proj[bt]))], n_beat)] <- "beat"
  sv <- sv[!is.na(sv$category), ]
  sv$why <- c(danger = "Saved themselves (began in danger)", close = "Close call", beat = "Massively outperformed")[sv$category]
  sv <- sv[order(match(sv$category, c("danger", "close", "beat")), if (nrow(sv)) ifelse(sv$category == "beat", -(sv$final - sv$pre_proj), -sv$peak) else numeric(0)), ]
  sv$n_teams <- n
  sv$beat <- round(round(sv$final, 1) - round(sv$pre_proj, 1), 1)
  sv$league_median_beat <- round(med_beat, 1)
  sv$beat_vs_league <- round(sv$beat - sv$league_median_beat, 1)
  sv$above_chop <- round(sv$final - chop_final, 2)   # unrounded gap, shown with fmt_gap() (0.04 stays 0.04), same as the week's "gap"
  sv$place_text <- vapply(sv$place, place_text, character(1), n = n)
  sv$peak_time <- fmt_et(sv$peak_wall)
  sv
}

# Two things from the team replay, found in one pass:
#  * players: the chopped team's biggest shortfalls against projection, at the moment its projected score bottomed out
#  * last_games: the three most likely teams when the last block of games kicked off, with their score so far and the
#    players still to play (projected vs. what they actually scored)
#  * saved: the teams that were in danger (or beat projection the most) and were not chopped, with the players in each team's scoring
#    lineup who beat their projection by the most (saved$teams, saved$players)
recap_players <- function(season, week, pr, n_show = 4) {
  t <- readr::read_csv(teams_path(season, week), show_col_types = FALSE)
  ch <- t |> dplyr::filter(chopped) |> dplyr::arrange(x_min)
  xs <- sort(unique(t$x_min))
  lo_i <- match(ch$x_min[which.min(ch$projected_final)], xs)
  last_x <- min(t$x_min[t$block == max(t$block)])
  last_i <- match(last_x, xs)
  sel <- saved_selection(t)
  peak_i <- match(sel$peak_x[sel$category == "close"], xs)
  r <- replay_league_week_realtime(season, week, pr, write = FALSE, keep_rt_steps = unique(c(1, lo_i, last_i, peak_i)))
  nm <- slim_players_table()[, c("player_id", "full_name")] |> dplyr::mutate(player_id = as.character(player_id))
  fin <- pr |> dplyr::filter(is_final_row) |> dplyr::mutate(player_id = as.character(player_id)) |>
    dplyr::select(player_id, nfl_team, final_actual_points, off_snp, snap_share)
  rid <- ch$roster_id[1]
  get_step <- function(si, id = rid) r$rt[[as.character(si)]]$rt |>
    dplyr::filter(roster_id == id) |> dplyr::mutate(proj_final = locked_points + remaining_mean)
  a <- get_step(1) |> dplyr::select(player_id, position, pre_proj, start_final = proj_final)
  b <- get_step(lo_i) |> dplyr::select(player_id, locked_points, proj_final)
  players <- a |> dplyr::left_join(b, by = "player_id") |> dplyr::left_join(fin, by = "player_id") |> dplyr::left_join(nm, by = "player_id") |>
    dplyr::mutate(delta = proj_final - start_final) |>
    dplyr::filter(!is.na(pre_proj)) |> dplyr::arrange(delta) |> dplyr::slice_head(n = n_show) |>
    dplyr::transmute(
      player = paste0(full_name, ", ", position), team = nfl_team,
      projected = round(pre_proj, 1), scored_by_then = round(locked_points, 1), final = round(final_actual_points, 1),
      snaps = ifelse(is.na(off_snp), "", sprintf("%d (%d%%)", as.integer(off_snp), round(100 * snap_share)))
    )

  # who was still to play when the last games kicked off, for the three most likely teams then
  at_last <- t |> dplyr::filter(x_min == last_x) |> dplyr::arrange(dplyr::desc(elimination_prob)) |> dplyr::slice_head(n = 3)
  rt_last <- r$rt[[as.character(last_i)]]
  last_games <- lapply(seq_len(nrow(at_last)), function(i) {
    id <- at_last$roster_id[i]
    todo <- rt_last$rt |> dplyr::filter(roster_id == id, elapsed_fraction < 0.5) |>
      dplyr::left_join(nm, by = "player_id") |> dplyr::left_join(fin, by = "player_id") |>
      dplyr::filter(pre_proj > 0 | final_actual_points > 0) |>
      dplyr::transmute(player = paste0(full_name, ", ", position), projected = round(pre_proj, 1), scored = round(final_actual_points, 1)) |>
      dplyr::arrange(dplyr::desc(projected))
    list(team = at_last$team_name[i], owner = at_last$owner_name[i], chance = at_last$elimination_prob[i], score_so_far = at_last$current_score[i],
         final = at_last$final_team_points[i], chopped = at_last$chopped[i], still_to_play = todo)
  })

  # For each selected team: the players in its final best-ball lineup who beat their pregame projection by the most; the players in the
  # final scoring lineup who were not in the pregame lineup (bench players who broke in, or players picked up during the week, with the
  # time of the pickup); the mid-week roster moves; and, for a close call, who scored after the moment its chance of being chopped peaked.
  # Rosters are the time-aware ones from the replay (R/replay.R roster_intervals()), so a player picked up on Sunday is not credited to the
  # team before then.
  iv_all <- r$ros_iv |> dplyr::mutate(player_id = as.character(player_id))
  pre <- pr |> dplyr::mutate(player_id = as.character(player_id)) |> dplyr::group_by(player_id) |>
    dplyr::summarise(pre_proj = dplyr::first(pre_proj[!is.na(pre_proj)]), .groups = "drop")
  nm2 <- slim_players_table()[, c("player_id", "position")] |> dplyr::mutate(player_id = as.character(player_id))
  say_when <- function(utc_secs) say_time(fmt_et(as.POSIXct(utc_secs, origin = "1970-01-01", tz = "UTC")))
  move_note <- function(from, to) vapply(seq_along(from), function(k) {
    if (is.finite(from[k]) && is.finite(to[k])) paste("added to the roster on", say_when(from[k]), "and dropped from it on", say_when(to[k]))
    else if (is.finite(from[k])) paste("added to the roster on", say_when(from[k]))
    else paste("dropped from the roster on", say_when(to[k]))
  }, character(1))
  build_x <- function(rid) {   # every player the roster had at any point of the games, with the end-of-week and start-of-week flags
    x <- iv_all |> dplyr::filter(roster_id == rid) |>
      dplyr::left_join(pre, by = "player_id") |> dplyr::left_join(fin, by = "player_id") |>
      dplyr::left_join(nm, by = "player_id") |> dplyr::left_join(nm2, by = "player_id")
    x <- x[!is.na(x$position), ]
    x$final_actual_points[is.na(x$final_actual_points)] <- 0
    x$in_end <- is.infinite(x$to); x$in_start <- is.infinite(x$from)
    x
  }
  # --- What if a team had made none of its moves during the games? ---
  # Undoing a team's drop also undoes any later pickup of that player by ANOTHER team (that team could not have had him), and so on down
  # the chain; those teams' rosters change too. Every changed roster is re-scored from the same replay points (best-ball lineup) and the
  # difference is applied to the team's official final score, then the lowest score of the league is found again.
  finals_tbl <- t |> dplyr::distinct(roster_id, team_name, owner_name, final_team_points, chopped)
  win <- range(r$steps$wall)
  tx <- Filter(function(z) identical(z$status, "complete") && z$type %in% c("free_agent", "waiver", "trade", "commissioner"), r$transactions %||% list())
  tx_rows <- dplyr::bind_rows(lapply(seq_along(tx), function(k) {
    z <- tx[[k]]; ts <- as.numeric(z$status_updated %||% z$created %||% NA) / 1000
    if (is.na(ts) || ts < win[1] || ts > win[2]) return(NULL)
    bid <- { v <- z$settings$waiver_bid; if (is.null(v)) NA_real_ else suppressWarnings(as.numeric(v)) }   # FAAB dollars of a waiver claim (NA for a free-agent pickup)
    mk1 <- function(l, act) if (length(l)) tibble::tibble(tx = k, type = z$type, bid = bid, ts = ts, act = act, player_id = names(l), roster_id = as.integer(unlist(l))) else NULL
    dplyr::bind_rows(mk1(z$adds, "ADD"), mk1(z$drops, "DROP"))
  }))
  if (!nrow(tx_rows)) tx_rows <- tibble::tibble(tx = integer(), type = character(), bid = numeric(), ts = numeric(), act = character(), player_id = character(), roster_id = integer())
  end_ros <- iv_all[is.infinite(iv_all$to), c("roster_id", "player_id")]
  pts_tbl <- fin[, c("player_id", "final_actual_points")]
  lineup_pts <- function(ros_df) {
    d <- ros_df |> dplyr::left_join(pts_tbl, by = "player_id") |> dplyr::left_join(nm2, by = "player_id")
    d <- d[!is.na(d$position), ]; d$final_actual_points[is.na(d$final_actual_points)] <- 0
    optimal_lineup_points(d$position, d$final_actual_points)
  }
  lineup_ids <- function(ros_df) {   # who is in the best-ball scoring lineup for this roster
    d <- ros_df |> dplyr::left_join(pts_tbl, by = "player_id") |> dplyr::left_join(nm2, by = "player_id")
    d <- d[!is.na(d$position), ]; d$final_actual_points[is.na(d$final_actual_points)] <- 0
    d$player_id[optimal_lineup_assignment(d$position, d$final_actual_points) != "BN"]
  }
  # The end-of-week rosters with the transactions in S undone, newest first (so a player added and dropped by the same team inside the week nets out
  # to "never on the roster"). If that leaves one player on two rosters, the other team's pickup of him is impossible in that world: undo it too,
  # and repeat until no player is on two rosters.
  cf_rosters <- function(S) {
    repeat {
      ops <- tx_rows[tx_rows$tx %in% S, ]; ops <- ops[order(-ops$ts), ]
      ros <- end_ros
      for (k in seq_len(nrow(ops))) {
        if (ops$act[k] == "ADD") ros <- ros[!(ros$roster_id == ops$roster_id[k] & ros$player_id == ops$player_id[k]), ]
        else ros <- dplyr::bind_rows(ros, tibble::tibble(roster_id = ops$roster_id[k], player_id = ops$player_id[k]))
      }
      ros <- dplyr::distinct(ros)
      key <- paste(ros$roster_id, ros$player_id)
      dup <- ros[ros$player_id %in% ros$player_id[duplicated(ros$player_id)], ]
      bad <- tx_rows$tx[tx_rows$act == "ADD" & !(tx_rows$tx %in% S) & paste(tx_rows$roster_id, tx_rows$player_id) %in% paste(dup$roster_id, dup$player_id)]
      if (!length(bad)) return(list(ros = ros, S = S))
      S <- union(S, bad)
    }
  }
  finals_cur <- finals_tbl[, c("roster_id", "team_name", "owner_name", "final_team_points")]
  # Each of a team's swaps (one transaction: players added and/or dropped), scored on its own: effect = the final score minus what it would
  # have been had that one swap not been made (positive = the swap helped, negative = it hurt).
  pdb <- get_players_db()   # Sleeper's full dictionary: names for players (e.g. one released from every roster) that the slim table lacks
  nm_of <- function(pid) vapply(pid, function(id) {
    v <- nm$full_name[match(id, nm$player_id)]; p <- nm2$position[match(id, nm2$player_id)]
    if (is.na(v)) { q <- pdb[[id]]; v <- q$full_name %||% id; if (is.na(p)) p <- q$position %||% NA_character_ }
    if (is.na(p)) v else paste0(v, ", ", p)
  }, character(1), USE.NAMES = FALSE)
  pts_of <- function(pid) { v <- fin$final_actual_points[match(pid, fin$player_id)]; round(ifelse(is.na(v), 0, v), 1) }
  swaps_all <- dplyr::bind_rows(lapply(finals_tbl$roster_id, function(rid) {
    ks <- unique(tx_rows$tx[tx_rows$roster_id == rid]); if (!length(ks)) return(NULL)
    ks <- ks[order(vapply(ks, function(k) min(tx_rows$ts[tx_rows$tx == k]), numeric(1)))]   # in time order
    act <- lineup_pts(end_ros[end_ros$roster_id == rid, ])
    dplyr::bind_rows(lapply(ks, function(k) {
      cfr <- cf_rosters(k); eff <- act - lineup_pts(cfr$ros[cfr$ros$roster_id == rid, ])
      a_ids <- lineup_ids(end_ros[end_ros$roster_id == rid, ]); c_ids <- lineup_ids(cfr$ros[cfr$ros$roster_id == rid, ])
      enter <- setdiff(a_ids, c_ids); leave <- setdiff(c_ids, a_ids)   # in the scoring lineup with the swap but not without it, and the reverse
      fmt_p <- function(ids) paste(sprintf("%s (%.1f)", nm_of(ids), pts_of(ids)), collapse = " and ")
      replaced <- if (length(enter) && length(leave)) sprintf("%s was replaced by %s", fmt_p(leave), fmt_p(enter)) else ""
      ops <- tx_rows[tx_rows$tx == k & tx_rows$roster_id == rid, ]
      ad <- ops$player_id[ops$act == "ADD"]; dr <- ops$player_id[ops$act == "DROP"]
      tibble::tibble(roster_id = rid, tx = k, type = ops$type[1], bid = ops$bid[1], ts = ops$ts[1], when = say_when(ops$ts[1]),
                     added = paste(sprintf("%s (scored %.1f)", nm_of(ad), pts_of(ad)), collapse = ", "),
                     dropped = paste(sprintf("%s (scored %.1f in the week)", nm_of(dr), pts_of(dr)), collapse = ", "),
                     effect = round(eff, 1), effect_raw = eff, replaced = replaced)
    }))
  }))
  if (!nrow(swaps_all)) swaps_all <- tibble::tibble(roster_id = integer(), tx = integer(), type = character(), bid = numeric(), ts = numeric(), when = character(), added = character(), dropped = character(), effect = numeric(), effect_raw = numeric(), replaced = character())
  chop_team_all <- finals_tbl[finals_tbl$chopped, ][1, ]
  cf_for <- function(rid, is_chopped) {
    sw <- swaps_all[swaps_all$roster_id == rid, ]
    none <- tibble::tibble(n_moves = nrow(sw), cf_final = NA_real_, move_effect = 0, low_team = chop_team_all$team_name, low_owner = chop_team_all$owner_name,
                           low_final = chop_team_all$final_team_points, cascade = "", undone = "", n_undone = 0L)
    if (!nrow(sw)) return(none)
    # what-if: the chopped team skips only the swaps that HURT it (its best possible score from its own moves); any other team skips only the swaps that HELPED it
    S0 <- if (is_chopped) sw$tx[sw$effect_raw < -0.05] else sw$tx[sw$effect_raw > 0.05]
    if (!length(S0)) { none$cf_final <- finals_cur$final_team_points[match(rid, finals_cur$roster_id)]; return(none) }
    cfr <- cf_rosters(S0); cf_ros <- cfr$ros; ops <- tx_rows[tx_rows$tx %in% cfr$S, ]
    aff <- unique(ops$roster_id)
    fin_cf <- finals_cur
    for (a_id in aff) {
      k <- match(a_id, fin_cf$roster_id)
      fin_cf$final_team_points[k] <- fin_cf$final_team_points[k] - lineup_pts(end_ros[end_ros$roster_id == a_id, ]) + lineup_pts(cf_ros[cf_ros$roster_id == a_id, ])
    }
    me <- match(rid, fin_cf$roster_id); lo <- which.min(fin_cf$final_team_points)
    others <- setdiff(aff, rid)
    others <- others[abs(fin_cf$final_team_points[match(others, fin_cf$roster_id)] - finals_cur$final_team_points[match(others, finals_cur$roster_id)]) >= 0.05]   # only teams whose score actually changes
    cascade <- if (length(others)) paste(sprintf("%s would also have been affected, because it picked up a player this team dropped, and would have finished with %.1f instead of %.1f",
                                                  fin_cf$team_name[match(others, fin_cf$roster_id)], fin_cf$final_team_points[match(others, fin_cf$roster_id)], finals_cur$final_team_points[match(others, finals_cur$roster_id)]), collapse = "; ") else ""
    tibble::tibble(n_moves = nrow(sw), cf_final = fin_cf$final_team_points[me], move_effect = finals_cur$final_team_points[me] - fin_cf$final_team_points[me],
                   low_team = fin_cf$team_name[lo], low_owner = fin_cf$owner_name[lo], low_final = fin_cf$final_team_points[lo], cascade = cascade,
                   undone = paste(sw$when[sw$tx %in% S0], collapse = " and "), n_undone = length(S0))
  }
  cf_all <- dplyr::bind_cols(finals_tbl, dplyr::bind_rows(lapply(seq_len(nrow(finals_tbl)), function(k) cf_for(finals_tbl$roster_id[k], finals_tbl$chopped[k]))))
  chop_final_all <- finals_tbl$final_team_points[finals_tbl$chopped][1]
  cf_all$cf_gap <- cf_all$cf_final - chop_final_all
  sel <- dplyr::left_join(sel, cf_all[, c("roster_id", "n_moves", "cf_final", "move_effect", "cf_gap", "low_team", "low_owner", "low_final", "cascade", "undone", "n_undone")], by = "roster_id")
  # the chopped team's own in-game moves (and, from cf_all, who would have been chopped had it made none)
  moves_tbl <- function(rid) {
    x <- build_x(rid); mv <- x[is.finite(x$from) | is.finite(x$to), ]
    if (!nrow(mv)) return(NULL)
    mv <- mv[order(ifelse(is.finite(mv$from), mv$from, mv$to)), ]
    tibble::tibble(player = paste0(mv$full_name, ", ", mv$position), nfl_team = mv$nfl_team, scored = round(mv$final_actual_points, 1),
                   note = move_note(mv$from, mv$to))
  }
  chop_row <- cf_all[cf_all$chopped, ][1, ]
  # Why the chopped team fell short, as a PATTERN (the table on the page has every player): its pregame starters (the best lineup on pregame
  # projections) combined and by position group, how many fell far short, and whether the shortfall was about playing time or production.
  ch_x <- build_x(chop_row$roster_id); ch_x <- ch_x[ch_x$in_start & !is.na(ch_x$pre_proj), ]
  ch_x$in_pre <- optimal_lineup_assignment(ch_x$position, ch_x$pre_proj) != "BN"
  st_x <- ch_x[ch_x$in_pre, ]
  st_x$final_actual_points <- ifelse(st_x$in_end, st_x$final_actual_points, 0)   # a starter dropped before his game scored nothing for this team
  grp_name <- c(QB = "quarterbacks", RB = "running backs", WR = "wide receivers", TE = "tight ends")
  by_pos <- dplyr::bind_rows(lapply(intersect(names(grp_name), unique(st_x$position)), function(ps) {
    g <- st_x[st_x$position == ps, ]
    tibble::tibble(group = grp_name[[ps]], n = nrow(g), projected = sum(g$pre_proj), scored = sum(g$final_actual_points))
  }))
  short <- st_x[st_x$final_actual_points < 0.5 * st_x$pre_proj, ]
  diag <- list(n_starters = nrow(st_x), projected = sum(st_x$pre_proj), scored = sum(st_x$final_actual_points), by_pos = by_pos,
               n_under_half = nrow(short), n_under_1 = sum(st_x$final_actual_points < 1),
               n_barely_played = sum(!is.na(short$snap_share) & short$snap_share < 0.30),
               n_played_most = sum(!is.na(short$snap_share) & short$snap_share >= 0.30))
  act_stats <- tryCatch(load_sleeper_archive("stats", season, week), error = function(e) list())
  lost <- vapply(ch_x$player_id, function(id) { sv <- act_stats[[id]]$stats; if (!is.null(sv) && "fum_lost" %in% names(sv)) as.numeric(sv[["fum_lost"]]) else 0 }, numeric(1))
  fumbles <- ch_x[lost > 0, ]
  fumbles <- dplyr::transmute(fumbles, player = paste0(full_name, ", ", position), nfl_team, scored = round(final_actual_points, 1), starter = in_pre)
  # ---- weekly highlights across ALL teams: top team score, top player, biggest boom and bust against projection, best and worst roster swap
  team_lab <- function(rid) { k <- match(rid, finals_tbl$roster_id); sprintf("%s (%s)", finals_tbl$team_name[k], manager_label(finals_tbl$owner_name[k])) }
  all_x <- dplyr::bind_rows(lapply(finals_tbl$roster_id, function(rid) {
    x <- build_x(rid); x$roster_id <- rid
    x$in_pre <- FALSE; ok <- x$in_start & !is.na(x$pre_proj); x$in_pre[ok] <- optimal_lineup_assignment(x$position[ok], x$pre_proj[ok]) != "BN"
    x$in_fin <- FALSE; x$in_fin[x$in_end] <- optimal_lineup_assignment(x$position[x$in_end], x$final_actual_points[x$in_end]) != "BN"
    x }))
  plr <- function(x) sprintf("%s (%s)", paste0(x$full_name, ", ", x$position), x$nfl_team)
  hl <- list()
  tp <- finals_tbl[which.max(finals_tbl$final_team_points), ]
  hl[[1]] <- tibble::tibble(label = "Top team score", headline = sprintf("%s, %.1f", team_lab(tp$roster_id), tp$final_team_points), detail = "")
  sc <- all_x[all_x$in_fin, ]; sc <- sc[which.max(sc$final_actual_points), ]
  hl[[2]] <- tibble::tibble(label = "Top player score", headline = sprintf("%s, %.1f points", plr(sc), sc$final_actual_points), detail = sprintf("For %s.", team_lab(sc$roster_id)))
  bm <- all_x[all_x$in_fin & !is.na(all_x$pre_proj), ]; bm <- bm[which.max(bm$final_actual_points - bm$pre_proj), ]
  hl[[3]] <- tibble::tibble(label = "Biggest boom", headline = sprintf("%s, %.1f points", plr(bm), bm$final_actual_points), detail = sprintf("Projected for %.1f, for %s.", bm$pre_proj, team_lab(bm$roster_id)))
  bs <- all_x[all_x$in_pre & all_x$in_end & !is.na(all_x$pre_proj), ]; bs <- bs[which.min(bs$final_actual_points - bs$pre_proj), ]
  hl[[4]] <- tibble::tibble(label = "Biggest bust", headline = sprintf("%s, %.1f points", plr(bs), bs$final_actual_points), detail = sprintf("A pregame starter projected for %.1f, for %s.", bs$pre_proj, team_lab(bs$roster_id)))
  # when the top player score is also the biggest boom, one tile does both jobs: keep the boom tile and say so in its text
  if (identical(sc$roster_id, bm$roster_id) && identical(sc$full_name, bm$full_name) && isTRUE(all.equal(sc$final_actual_points, bm$final_actual_points))) {
    hl[[3]]$detail <- paste(hl[[3]]$detail, "Also the top player score of the week.")
    hl[[2]] <- NULL
  }
  if (nrow(swaps_all)) {
    bw <- swaps_all[which.max(swaps_all$effect_raw), ]; ww <- swaps_all[which.min(swaps_all$effect_raw), ]
    strip <- function(x) sub(" \\(scored.*$", "", x)
    after <- if (week >= 2) ", after the waivers on the chopped team's players were processed" else ", during the games"   # the swaps counted are made once the week's first game has started
    if (bw$effect >= 1) hl[[length(hl) + 1]] <- tibble::tibble(label = "Best swap after waivers", headline = sprintf("%s, +%.1f points", manager_label(finals_tbl$owner_name[match(bw$roster_id, finals_tbl$roster_id)]), bw$effect),
                                                              detail = sprintf("%s%s: added %s and dropped %s%s.", bw$when, after, strip(bw$added), strip(bw$dropped), if (identical(bw$type, "waiver") && !is.na(bw$bid)) sprintf(", for $%.0f", bw$bid) else ""))
    if (ww$effect <= -1) hl[[length(hl) + 1]] <- tibble::tibble(label = "Worst swap after waivers", headline = sprintf("%s, %.1f points", manager_label(finals_tbl$owner_name[match(ww$roster_id, finals_tbl$roster_id)]), ww$effect),
                                                              detail = sprintf("%s%s: added %s and dropped %s%s.", ww$when, after, strip(ww$added), strip(ww$dropped), if (identical(ww$type, "waiver") && !is.na(ww$bid)) sprintf(", for $%.0f", ww$bid) else ""))
  }
  highlights <- dplyr::bind_rows(hl)
  # Players on the chopped team that ESPN's injury report flagged DURING their game (so there is evidence he went down, not just that he played little)
  left_game <- tryCatch({
    ex <- espn_game_exit_reports(season, week)
    k <- match(paste(ch_x$nfl_team, name_match_key(ch_x$full_name)), paste(ex$nfl_team, name_match_key(ex$player_name)))
    hit <- !is.na(k)
    if (any(hit)) tibble::tibble(player = ch_x$full_name[hit], position = ch_x$position[hit], starter = ch_x$in_pre[hit], status = ex$status[k[hit]], minutes = ex$minutes_after_kickoff[k[hit]], when = if ("when" %in% names(ex)) ex$when[k[hit]] else NA_character_) else NULL
  }, error = function(e) NULL)
  chopped_info <- list(cf = chop_row, moves = moves_tbl(chop_row$roster_id), swaps = swaps_all[swaps_all$roster_id == chop_row$roster_id, ], diag = diag, fumbles = fumbles, left = left_game)
  sv_players <- dplyr::bind_rows(lapply(seq_len(nrow(sel)), function(i) {
    x <- build_x(sel$roster_id[i]); x <- x[!is.na(x$pre_proj), ]
    x$in_final <- FALSE; x$in_pre <- FALSE
    x$in_final[x$in_end] <- optimal_lineup_assignment(x$position[x$in_end], x$final_actual_points[x$in_end]) != "BN"
    x$in_pre[x$in_start] <- optimal_lineup_assignment(x$position[x$in_start], x$pre_proj[x$in_start]) != "BN"
    x$projected <- round(x$pre_proj, 1); x$scored <- round(x$final_actual_points, 1)
    mk <- function(d, kind, scored, above = NA_real_, note = "") if (!nrow(d)) NULL else
      dplyr::transmute(d, team_name = sel$team_name[i], kind = kind, player = paste0(full_name, ", ", position), nfl_team, projected,
                       scored = scored[seq_len(nrow(d))], above = above[seq_len(nrow(d))], note = note)
    ab <- x[x$in_final & x$scored - x$projected > 0, ]; ab <- utils::head(ab[order(-(ab$scored - ab$projected)), ], 3)
    sb <- x[x$in_final & !x$in_pre, ]; sb <- utils::head(sb[order(-sb$scored), ], 2)
    sb_note <- ifelse(sb$in_start, "on the bench in the pregame lineup", paste("picked up on", vapply(sb$from, say_when, character(1))))
    mv <- x[is.finite(x$from) | is.finite(x$to), ]; mv <- mv[order(ifelse(is.finite(mv$from), mv$from, mv$to)), ]
    mv_note <- move_note(mv$from, mv$to)
    out <- list(mk(ab, "above", ab$scored, round(ab$scored - ab$projected, 1)),
                mk(sb, "sub", sb$scored, note = sb_note),
                mk(mv, "move", mv$scored, note = mv_note))
    if (identical(sel$category[i], "close")) {
      pk <- r$rt[[as.character(match(sel$peak_x[i], xs))]]$rt |> dplyr::filter(roster_id == sel$roster_id[i]) |>
        dplyr::mutate(player_id = as.character(player_id)) |> dplyr::select(player_id, locked_peak = locked_points)
      y <- dplyr::left_join(x, pk, by = "player_id")
      y$on_peak <- y$player_id %in% pk$player_id; y$locked_peak[is.na(y$locked_peak)] <- 0
      y$post <- round(y$final_actual_points - y$locked_peak, 1)
      y$in_peak <- FALSE; y$in_peak[y$on_peak] <- optimal_lineup_assignment(y$position[y$on_peak], y$locked_peak[y$on_peak]) != "BN"
      y <- y[y$in_final & y$post > 0, ]; y <- utils::head(y[order(-y$post), ], 3)
      out <- c(out, list(mk(y, "after_peak", y$post, note = ifelse(y$in_peak, "already in the scoring lineup at that point", "not in the scoring lineup at that point"))))
    }
    dplyr::bind_rows(out)
  }))
  list(players = players, last_games = last_games, saved = list(teams = sel, players = sv_players, cf_all = cf_all, swaps = swaps_all), chopped_info = chopped_info, highlights = highlights)
}

# --- Manager names ---------------------------------------------------------------------------------------
# Text refers to a manager by name instead of "they/their". For now that name is the manager's Sleeper user name (what the app shows as
# @name). collector/managers.csv maps user names to real names: fill in the real_name column and the text uses it from the next
# build (a blank real_name keeps the user name). The script adds any user name it has not seen to the file, never overwriting yours.
MANAGER_KEY_FILE <- file.path("collector", "managers.csv")

read_manager_key <- function(path = MANAGER_KEY_FILE) {
  if (!file.exists(path)) return(character(0))
  k <- readr::read_csv(path, show_col_types = FALSE, col_types = readr::cols(.default = readr::col_character()))
  if (!all(c("username", "real_name") %in% names(k))) return(character(0))
  k <- k[!is.na(k$real_name) & nzchar(trimws(k$real_name)) & !is.na(k$username), ]
  stats::setNames(trimws(k$real_name), trimws(k$username))
}

# every user name in the key file (a user name can carry digits, which must not count as numbers in the text)
all_usernames <- function(path = MANAGER_KEY_FILE) {
  if (!file.exists(path)) return(character(0))
  k <- readr::read_csv(path, show_col_types = FALSE, col_types = readr::cols(.default = readr::col_character()))
  u <- trimws(k$username[!is.na(k$username)])
  u[order(-nchar(u))]
}

manager_label <- function(username) {
  key <- read_manager_key()
  ifelse(!is.na(username) & username %in% names(key), unname(key[username]), as.character(username))
}

# Append user names not yet in the key (username, team_name, blank real_name); existing rows are never touched.
ensure_manager_key <- function(owners, path = MANAGER_KEY_FILE) {
  owners <- unique(owners[!is.na(owners$owner_name), c("owner_name", "team_name")])
  have <- if (file.exists(path)) readr::read_csv(path, show_col_types = FALSE, col_types = readr::cols(.default = readr::col_character())) else
    tibble::tibble(username = character(), team_name = character(), real_name = character())
  new <- owners[!(owners$owner_name %in% have$username), ]
  if (!nrow(new)) return(invisible(FALSE))
  readr::write_csv(dplyr::bind_rows(have, tibble::tibble(username = new$owner_name, team_name = new$team_name, real_name = NA_character_)), path, na = "")
  invisible(TRUE)
}

# Swap user names for real names in text written earlier (e.g. a saved draft). Team names are masked first, since several teams are
# named after their manager and must stay as they are.
apply_manager_key <- function(text, team_names = character(0)) {
  key <- read_manager_key()
  if (!length(key) || !length(text)) return(text)
  tn <- unique(team_names[order(-nchar(team_names))])
  mask <- function(x, from, to) { for (i in seq_along(from)) x <- gsub(from[i], to[i], x, fixed = TRUE); x }
  tokens <- sprintf("\001T%d\001", seq_along(tn))
  out <- text
  if (length(tn)) out <- mask(out, tn, tokens)
  for (u in names(key)[order(-nchar(names(key)))]) {
    out <- gsub(paste0("(?<![A-Za-z0-9_])", gsub("([.|()\\^{}+$*?\\[\\]])", "\\\\\\1", u), "(?![A-Za-z0-9_])"), key[[u]], out, perl = TRUE)
  }
  if (length(tn)) out <- mask(out, tokens, tn)
  out
}

# --- AI-written descriptions ---------------------------------------------------------------------
# The weekly page gets the same kind of write-up as the hand-made post: a short version, the week's story, who fell short.
# An AI assistant writes it from a FACT SHEET of numbers computed here, and nothing it writes is published unchecked:
#   * every number in the text must appear in the fact sheet (it may not do its own arithmetic or invent figures),
#   * no injury talk, and the required sections must be there,
# and if the text fails a check, or there is no draft, the page falls back to the plain automatic summary.
# The text comes from your own draft (paste the saved prompt into an AI assistant) or, failing that, the automatic summary.

# KEY=VALUE lines from ~/.guillotine_recap_env (kept OUTSIDE the project so settings such as the publish folder can never be committed or deployed)
load_recap_env <- function(path = path.expand("~/.guillotine_recap_env")) {
  if (!file.exists(path)) return(invisible(FALSE))
  for (ln in readLines(path, warn = FALSE)) {
    ln <- trimws(ln)
    if (!nzchar(ln) || startsWith(ln, "#") || !grepl("=", ln, fixed = TRUE)) next
    k <- trimws(sub("=.*$", "", ln)); v <- trimws(sub("^[^=]*=", "", ln))
    if (nzchar(k) && !nzchar(Sys.getenv(k))) do.call(Sys.setenv, stats::setNames(list(gsub("^[\"']|[\"']$", "", v)), k))
  }
  invisible(TRUE)
}

fmt_players_line <- function(df) paste(sprintf("%s (%s) projected %.1f, had scored %.1f by then, finished the week with %.1f", df$player, df$team, df$projected, df$scored_by_then, df$final), collapse = "; ")

fmt_list <- function(d) paste(sprintf("%s (%s) projected %.1f, scored %.1f", d$player, d$nfl_team, d$projected, d$scored), collapse = "; ")

# One swap as a sentence, with its effect on the final score ("raised ... by 4.7" / "lowered ... by 22.6" / "did not change ...")
swap_sentence <- function(w) {
  eff <- if (abs(w$effect) < 0.05) "That did not change the final score." else
    sprintf("That %s the final score by %s%s.", if (w$effect > 0) "raised" else "lowered", fmt_gap(abs(w$effect)),
            if (!is.null(w$replaced) && nzchar(w$replaced)) paste0(": in the scoring lineup, ", w$replaced) else "")
  # a waiver claim is processed by the waiver run (about 3am), so its hour says nothing about when the manager decided
  how <- if (identical(w$type, "waiver")) " (a waiver claim, processed in the waiver run at that time)" else ""
  sprintf("%s%s: added %s%s. %s", w$when, how, if (nzchar(w$added)) w$added else "no one", if (nzchar(w$dropped)) paste0(" and dropped ", w$dropped) else "", eff)
}

# "so it would have been chopped" etc. from a what-if world: self = the team asked about, low_team / low_final = the team with the lowest score
# in that world, actual = the team actually chopped.
whatif_verdict <- function(self, low_team, low_final, actual, self_chopped) {
  if (identical(low_team, self)) {
    sprintf("The lowest score in the league would have been its own %.1f, so it would have been chopped%s.", low_final, if (!self_chopped) sprintf(" instead of %s", actual) else "")
  } else if (identical(low_team, actual)) {
    sprintf("The lowest score in the league would still have been %s's %.1f, so it would %s.", low_team, low_final, if (self_chopped) "still have been chopped" else "still have survived")
  } else {
    sprintf("The lowest score in the league would have been %s's %.1f, so %s would have been chopped.", low_team, low_final, low_team)
  }
}

# The chopped team's own roster moves during the games, and who would have been chopped without them (NULL if it made none)
chopped_whatif_lines <- function(ci, f) {
  cf <- ci$cf; sw <- ci$swaps
  if (is.null(sw) || !nrow(sw)) return(NULL)
  # only a swap that sealed the chop is written about: skipping the swaps that lowered the score would have meant someone else was chopped
  if (cf$n_undone == 0 || identical(cf$low_team, f$chopped_team)) return(NULL)
  out <- c(sprintf("ROSTER SWAPS THAT DECIDED THE CHOP (the only roster moves to write about this week). The chopped team made these swaps during the games, each scored on its own (the added player's points and the points the dropped player scored in the week are shown): %s",
                   paste(vapply(seq_len(nrow(sw)), function(i) swap_sentence(sw[i, ]), character(1)), collapse = " ")),
           if (!nzchar(cf$cascade) && any(nzchar(sw$dropped))) "No other team picked up a player the chopped team dropped during the week.")
  if (cf$n_undone == 0) return(c(out, "None of those swaps lowered the chopped team's score, so skipping swaps could not have saved it."))
  c(out, paste0(sprintf("What if the chopped team had skipped only the swap that lowered its score, the one on %s (and kept the other swaps): its final score would have been %.1f instead of its actual %.1f. %s",
                        cf$undone, cf$cf_final, f$chopped_final, whatif_verdict(f$chopped_team, cf$low_team, cf$low_final, f$chopped_team, TRUE)),
                if (nzchar(cf$cascade)) paste0(" ", cf$cascade, ".")))
}

# How much the tracker's view swung once the week's LAST games had kicked off (the final block of games, up to but not including the final score): the chopped team's
# lowest and highest chance of being chopped, how often the team with the highest chance changed, and who was involved. NULL unless it was a real nail-biter
# (a swing of 25 points of chance or more, or the top pick changing at least twice). The fact-sheet block is named neutrally so the writer varies the wording.
last_game_swing <- function(season, week) {
  t <- readr::read_csv(teams_path(season, week), show_col_types = FALSE)
  lastx <- min(t$x_min[t$block == max(t$block)]); endx <- max(t$x_min)
  d <- t[t$x_min >= lastx & t$x_min < endx, ]
  if (!nrow(d)) return(NULL)
  ch <- d[d$chopped, ]; ch <- ch[order(ch$x_min), ]
  # the result is "settled" once the chopped team's chance stays at 99% or more to the end; only the undecided steps count
  ok <- ch$elimination_prob >= 0.99
  n_open <- if (any(!ok)) max(which(!ok)) else 0
  if (n_open < 3) return(NULL)
  keep <- ch$x_min[seq_len(n_open)]; d <- d[d$x_min %in% keep, ]; ch <- ch[ch$x_min %in% keep, ]
  lo <- ch[which.min(ch$elimination_prob), ]; hi <- ch[which.max(ch$elimination_prob), ]
  top <- d |> dplyr::group_by(x_min) |> dplyr::summarise(team = team_name[which.max(elimination_prob)], .groups = "drop") |> dplyr::arrange(x_min)
  changes <- sum(top$team[-1] != top$team[-nrow(top)])
  dl <- d[d$x_min == lo$x_min, ]; tt <- dl[which.max(dl$elimination_prob), ]   # the tracker's top pick at the chopped team's low point
  big_range <- hi$elimination_prob - lo$elimination_prob >= 0.25
  if (!big_range && changes < 2) return(NULL)
  list(low = lo$elimination_prob, low_time = fmt_et(lo$wall_utc), high = hi$elimination_prob, high_time = fmt_et(hi$wall_utc), big_range = big_range, changes = changes, teams = unique(top$team), chopped = ch$team_name[1], low_top_team = tt$team_name[1], low_top_prob = tt$elimination_prob[1])
}

swing_lines <- function(sw) {
  if (is.null(sw)) return(NULL)
  sprintf("LATE SWINGS IN THE LAST GAMES (the tracker's updates after the week's last games kicked off, until the result was settled): %s%s Write one or two sentences about this in 'The team that got chopped'.",
          if (sw$big_range) sprintf("the chopped team's chance of being chopped fell as low as %s (%s)%s and climbed as high as %s (%s), and ", pct0(sw$low), sw$low_time,
                                                if (!identical(sw$low_top_team, sw$chopped)) sprintf(", when the tracker's top pick for the chop was %s at %s", sw$low_top_team, pct0(sw$low_top_prob)) else "", pct0(sw$high), sw$high_time) else "",
          sprintf("the team the tracker rated most likely to be chopped changed %d %s%s.", sw$changes, if (sw$changes == 1) "time" else "times",
                  if (length(sw$teams) > 1) sprintf(", passing between %s", join_and(sw$teams)) else ""))
}

# WHY THE CHOPPED TEAM FELL SHORT, as fact-sheet lines: a team-level pattern (combined, by position group, how many starters fell far short, playing time
# versus production, fumbles), so the text can explain the week without listing individual scores (the page has a table of the players).
# Players of the chopped team that went down during their game, from ESPN's timestamped injury reports (NULL when none). The minutes are real time after kickoff
# (halftime counts). Plain facts: no injury type, no return date.
left_game_lines <- function(ci) {
  d <- ci$left
  if (is.null(d) || !nrow(d)) return(NULL)
  c("LEFT A GAME WITH AN INJURY (players on the chopped team that ESPN's injury report flagged during their own game, so they went down during it. Say it in plain words in 'Why it fell short', one sentence at most, only for a player listed below: that he went down in the game, and when in the game, using the quarter wording given (for example 'late in the 3rd quarter'), never minutes. Do not name the injury or the body part, do not say how it happened, and do not give or guess a return date or how long he will be out):",
    sprintf("- %s, %s (%s): flagged %s by ESPN %s.", sub(", .*$", "", d$player), d$position, ifelse(d$starter, "a pregame starter", "a bench player in the pregame lineup"),
            ifelse(d$status == "Out", "out", ifelse(d$status == "IR", "for injured reserve", tolower(d$status))),
            ifelse(!is.na(d$when) & nzchar(d$when), d$when, sprintf("about %d minutes after kickoff", as.integer(d$minutes)))),   # the quarter when ESPN's play-by-play could place it
    "")
}

underperf_lines <- function(ci) {
  d <- ci$diag
  if (is.null(d) || !d$n_starters) return(NULL)
  pos_txt <- paste(sprintf("%s (%d) projected %.1f, scored %.1f", d$by_pos$group, d$by_pos$n, d$by_pos$projected, d$by_pos$scored), collapse = "; ")
  c("WHY THE CHOPPED TEAM FELL SHORT (a summary of its pregame starters, the best lineup on pregame projections; the page has a table of the individual players, so describe the pattern in two or three sentences and do not list individual players' scores):",
    sprintf("Its %d pregame starters were projected for %.1f combined and scored %.1f combined, %.1f points short. By position group: %s.", d$n_starters, d$projected, d$scored, d$projected - d$scored, pos_txt),
    sprintf("%d of the %d starters scored under half their projection and %d of them scored under 1 point. Of the starters who fell that far short, %d played under 30%% of their team's offensive snaps (a playing-time problem) and %d played most of their game and still did not produce.",
            d$n_under_half, d$n_starters, d$n_under_1, d$n_barely_played, d$n_played_most),
    if (nrow(ci$fumbles)) sprintf("Fumbles lost by players on the chopped team: %s.", paste(sprintf("%s lost a fumble (%s)", ci$fumbles$player, ifelse(ci$fumbles$starter, "a pregame starter", "a bench player in the pregame lineup")), collapse = "; ")))
}

# The three groups of teams that were not chopped, as fact-sheet lines (see saved_selection() for the definitions)
saved_fact_lines <- function(saved, f) {
  teams <- saved$teams; pl <- saved$players
  head_line <- function(v) sprintf("- %s (managed by %s).", v$team_name, manager_label(v$owner_name))
  fin_line <- function(v) sprintf("Projected final score before the week: %.1f. Finished with %.1f, which is %s points %s that projection and %s points above the chopped team's %.1f. That was %s.",
                                  v$pre_proj, v$final, fmt_gap(abs(v$beat)), if (v$beat >= 0) "above" else "below", fmt_gap(v$above_chop), f$chopped_final, v$place_text)
  moves <- function(v) {
    sw <- saved$swaps[saved$swaps$roster_id == v$roster_id, ]
    # only a swap that saved the team is written about: without the swap that raised its score it would have been chopped
    if (nrow(sw) && !is.na(v$n_undone) && v$n_undone > 0 && identical(v$low_team, v$team_name)) sprintf("  ROSTER SWAPS THAT SAVED THIS TEAM (the only roster moves to write about this week), each scored on its own: %s", paste(vapply(seq_len(nrow(sw)), function(i) swap_sentence(sw[i, ]), character(1)), collapse = " "))
  }
  what_if <- function(v) {
    if (is.na(v$n_moves) || v$n_moves == 0 || is.na(v$n_undone) || v$n_undone == 0 || !identical(v$low_team, v$team_name)) return(NULL)
    paste0(sprintf("  What if %s had skipped only the swap that helped it, the one on %s (and kept any other swaps): its final score would have been %.1f instead of its actual %.1f. %s",
                   v$team_name, v$undone, v$cf_final, v$final, whatif_verdict(v$team_name, v$low_team, v$low_final, f$chopped_team, FALSE)),
           if (nzchar(v$cascade)) paste0(" ", v$cascade, "."))
  }
  helped <- function(v) {
    ab <- pl[pl$team_name == v$team_name & pl$kind == "above", ]; sb <- pl[pl$team_name == v$team_name & pl$kind == "sub", ]
    c(if (nrow(ab)) sprintf("  Players in the scoring lineup who beat their projection by the most: %s.", paste(sprintf("%s (%s) projected %.1f, scored %.1f (%.1f above)", ab$player, ab$nfl_team, ab$projected, ab$scored, ab$above), collapse = "; ")),
      if (nrow(sb)) sprintf("  Players in the final scoring lineup who were not in the pregame lineup (best ball picks the best lineup after the games): %s.",
                            paste(sprintf("%s (%s) projected %.1f, scored %.1f (%s)", sb$player, sb$nfl_team, sb$projected, sb$scored, sb$note), collapse = "; ")),
      moves(v), what_if(v))
  }
  grp <- function(cat) teams[teams$category == cat, ]
  out <- c("",
    "TEAMS THAT WERE NOT CHOPPED, in three groups. Each team is in only one group. These are TEAMS, not players.",
    "",
    "GROUP 1, SAVED THEMSELVES (the survivor that began the week in danger: it had the highest chance of being chopped before any game, at least twice the even-split chance):")
  g <- grp("danger")
  if (!nrow(g)) out <- c(out, "none this week.") else for (i in seq_len(nrow(g))) {
    v <- g[i, ]
    out <- c(out, paste(head_line(v), sprintf("Chance of being chopped before any game: %s.", pct0(v$pre_prob)), fin_line(v)), helped(v))
  }
  out <- c(out, "", "GROUP 2, CLOSE CALLS (teams whose chance of being chopped climbed to at least 25% once games were on, and at least 5 points above their chance before any game, and that were not in group 1):")
  g <- grp("close")
  if (!nrow(g)) out <- c(out, "none this week.") else for (i in seq_len(nrow(g))) {
    v <- g[i, ]; ap <- pl[pl$team_name == v$team_name & pl$kind == "after_peak", ]
    out <- c(out, paste(head_line(v), sprintf("Chance of being chopped before any game: %s. Its chance peaked at %s (%s), when it had %.1f points scored.", pct0(v$pre_prob), pct0(v$peak), v$peak_time, v$score_at_peak), fin_line(v)),
             if (nrow(ap)) sprintf("  After that peak, the players in the final scoring lineup who scored the most points were: %s.",
                                   paste(sprintf("%s (%s) projected %.1f, scored %.1f points after the peak (%s)", ap$player, ap$nfl_team, ap$projected, ap$scored, ap$note), collapse = "; ")),
             moves(v), what_if(v))
  }
  out <- c(out, "", "GROUP 3, MASSIVELY OUTPERFORMED THEIR PROJECTIONS (teams in neither group above that finished above their pregame projected score by at least 20 points more than the typical team did that week; they were not in danger):")
  g <- grp("beat")
  if (!nrow(g)) out <- c(out, "none this week.") else for (i in seq_len(nrow(g))) {
    v <- g[i, ]
    out <- c(out, paste(head_line(v), sprintf("Chance of being chopped before any game: %s.", pct0(v$pre_prob)), fin_line(v),
                        sprintf("The typical team (the median of all teams) finished %.1f points above its pregame projection this week, so this team beat its projection by %s points more than the typical team.", v$league_median_beat, fmt_gap(v$beat_vs_league)),
                        if (i == which.max(as.numeric(g$final))) "HIGHEST SCORE IN THIS GROUP: this team had the highest final score of the teams in this group." else ""), helped(v))
  }
  out
}

# --- Season views ----------------------------------------------------------------------------------------------
ordinal_place <- function(p, n) sprintf("%s of %d", ordinal(p), n)

# One row per team across all the weeks built so far: weeks survived, total points, average place among the teams alive that week (1 = highest),
# the worst weekly place, the closest the team came to the chop (points above the chopped team in its tightest survived week) and how many weeks
# its chance of being chopped climbed to 25% or more ("in danger"). Chopped teams sit at the bottom with the week they went out.
season_scoreboard <- function(season, weeks) {
  rows <- dplyr::bind_rows(lapply(weeks, function(w) {
    t <- readr::read_csv(teams_path(season, w), show_col_types = FALSE)
    fin <- t |> dplyr::distinct(roster_id, team_name, owner_name, final_team_points, chopped)
    pk <- t |> dplyr::group_by(team_name) |> dplyr::summarise(peak = max(elimination_prob), .groups = "drop")
    fin |> dplyr::left_join(pk, by = "team_name") |>
      dplyr::mutate(week = w, n = dplyr::n(), place = rank(-final_team_points, ties.method = "min"), margin = final_team_points - final_team_points[chopped][1])
  }))
  out <- rows |> dplyr::group_by(team_name) |> dplyr::arrange(week, .by_group = TRUE) |> dplyr::summarise(
    owner_name = dplyr::last(owner_name), weeks = dplyr::n(), chopped_week = if (any(chopped)) week[chopped][1] else NA_integer_,
    total = sum(final_team_points), avg_pts = mean(final_team_points), avg_place = mean(place),
    # the worst week is the one closest to the bottom (fewest teams below it): 12th of 13 is worse than 13th of 16, which a plain highest place number got wrong
    worst = { k <- order(n - place, -place / n)[1]; ordinal_place(place[k], n[k]) }, worst_val = { k <- order(n - place, -place / n)[1]; 1 / (n[k] - place[k] + 1) },
    closest = { sv <- !chopped; if (any(sv)) { k <- which(sv)[which.min(margin[sv])]; sprintf("%s (week %d)", fmt_gap(margin[k]), week[k]) } else "" },
    closest_val = { sv <- !chopped; if (any(sv)) min(margin[sv]) else NA_real_ },
    danger = sum(peak >= 0.25 & !chopped), .groups = "drop")
  out[order(!is.na(out$chopped_week), -out$total), ]
}

season_scoreboard_html <- function(season, weeks, out_dir = file.path("recap", season)) {
  sb <- season_scoreboard(season, weeks)
  fb <- if (file.exists(file.path(out_dir, "faab.csv"))) readr::read_csv(file.path(out_dir, "faab.csv"), show_col_types = FALSE) else NULL
  df <- data.frame(Team = sb$team_name, Manager = vapply(sb$owner_name, manager_label, character(1)),
                   Status = ifelse(is.na(sb$chopped_week), "Alive", paste("Chopped, week", sb$chopped_week)),
                   `Total points` = sprintf("%.1f", sb$total), `Average points` = sprintf("%.1f", sb$avg_pts), `Average place` = sprintf("%.1f", sb$avg_place), `Worst week` = sb$worst,
                   `Closest to the chop` = ifelse(nzchar(sb$closest), sb$closest, "-"), `Weeks in danger` = sb$danger, check.names = FALSE, stringsAsFactors = FALSE)
  if (!is.null(fb)) { k <- match(sb$team_name, fb$team_name); df[["FAAB left"]] <- ifelse(is.na(k) | fb$eliminated[k] > 0, "-", sprintf("$%.0f", fb$remaining[k])) }
  # color coding: margins of 5 points or fewer to the chop are highlighted; any week in danger is colored
  heat <- ifelse(!is.na(sb$closest_val) & sb$closest_val <= 5, "heat-red", "")   # only margins of 5 points or fewer are highlighted
  dng <- ifelse(sb$danger >= 2, "heat-red", ifelse(sb$danger >= 1, "heat-orange", ""))
  # numeric sort keys for the clickable column headers (a team with no value, shown as "-", always sorts to the bottom)
  sort_vals <- list(Status = ifelse(is.na(sb$chopped_week), 0, sb$chopped_week), `Total points` = round(sb$total, 2), `Average points` = round(sb$avg_pts, 2), `Average place` = round(sb$avg_place, 2),
                    `Worst week` = round(sb$worst_val, 3), `Closest to the chop` = round(sb$closest_val, 2), `Weeks in danger` = sb$danger)
  if ("FAAB left" %in% names(df)) sort_vals[["FAAB left"]] <- ifelse(df[["FAAB left"]] == "-", NA_real_, suppressWarnings(as.numeric(gsub("[$,]", "", df[["FAAB left"]]))))
  paste0('<p class="fine">Click a column heading to sort. Average points is points per week played. Place is among the teams still alive that week, 1 being the highest score. Closest to the chop is how many points above the chopped team the team finished in its tightest week; red means 5 points or fewer. A week in danger is one where the chance of being chopped reached 25% or more (orange for one week, red for two or more). FAAB left is the waiver budget remaining out of $1,000 as of the latest build.</p>',
         html_table(df, num_cols = c("Total points", "Average points", "Average place", "Weeks in danger", "FAAB left"), red_cols = character(), cell_cls = list(`Closest to the chop` = heat, `Weeks in danger` = dng), sortable = TRUE, sort_vals = sort_vals))
}

# Net points each manager added or lost with roster moves made once the week's games had started (from swaps_wNN.csv, written by weekly_recap.R).
# Free-agent pickups and waiver claims are counted together: the league's early-morning waiver runs on Friday, Saturday and Sunday fall inside the window.
swap_ledger_html <- function(out_dir, weeks) {
  rd <- function(w) { f <- file.path(out_dir, sprintf("swaps_w%02d.csv", w)); if (file.exists(f)) { d <- readr::read_csv(f, show_col_types = FALSE, col_types = readr::cols(.default = readr::col_character())); if (nrow(d)) { d$week <- w; d } } }
  sw <- dplyr::bind_rows(lapply(weeks, rd))
  if (is.null(sw) || !nrow(sw)) return("")
  sw$effect <- as.numeric(sw$effect)
  led <- sw |> dplyr::group_by(team_name, owner_name) |> dplyr::summarise(
    swaps = dplyr::n(), net = sum(effect), faab = sum(suppressWarnings(as.numeric(if ("bid" %in% names(sw)) bid else NA_character_)), na.rm = TRUE),
    best = { k <- which.max(effect); if (effect[k] >= 1) sprintf("%+.1f (week %s)", effect[k], week[k]) else "-" },
    worst = { k <- which.min(effect); if (effect[k] <= -1) sprintf("%+.1f (week %s)", effect[k], week[k]) else "-" }, .groups = "drop")
  led <- led[order(-led$net), ]
  df <- data.frame(Team = led$team_name, Manager = vapply(led$owner_name, manager_label, character(1)), Swaps = led$swaps, `Net points from swaps` = sprintf("%+.1f", led$net),
                   `FAAB spent` = sprintf("$%.0f", led$faab), `Best swap` = led$best, `Worst swap` = led$worst, check.names = FALSE, stringsAsFactors = FALSE)
  paste0("<h2>Roster swaps</h2>",
         '<p class="note">A swap here is any roster move made after the first game of the week had started, from Thursday night to Monday, whether a free-agent pickup or a waiver claim. Moves made before the first game are not included. FAAB spent is the dollars bid on the waiver claims among them (a free-agent pickup costs nothing), so it can be set against the points. Each swap is scored on its own, as the final score with the swap minus the final score without it, using best-ball lineups.</p>',
         html_table(df, num_cols = c("Swaps", "Net points from swaps", "FAAB spent")))
}

# Waiver budgets as of now (Sleeper keeps only the running total used, not a history): one row per roster.
faab_table <- function(league_id = LEAGUE_ID) {
  lg <- get_league(league_id); budget <- as.numeric(lg$settings$waiver_budget %||% NA)
  users <- build_user_lookup(get_users(league_id))
  dplyr::bind_rows(lapply(get_rosters(league_id), function(r) tibble::tibble(owner_id = r$owner_id, used = as.numeric(r$settings$waiver_budget_used %||% 0),
                                                                             eliminated = as.numeric(r$settings$eliminated %||% 0)))) |>
    dplyr::left_join(users, by = "owner_id") |> dplyr::mutate(budget = budget, remaining = budget - used) |>
    dplyr::select(team_name, owner_name, used, remaining, eliminated)
}

# The week's "Biggest spender" tile: who spent the most FAAB on players released by LAST week's chop (waiver claims between the chop and this week's first
# kickoff). Returns a one-row highlight tibble, or NULL (week 1, or no money was spent). The chop frees all 15 of the chopped team's players.
chop_spender_tile <- function(week, first_kickoff_secs, prev_owner, perf = NULL, league_id = LEAGUE_ID) {
  if (week < 2) return(NULL)
  chop <- Filter(function(z) identical(z$type, "chopped"), get_transactions(league_id, week - 1))
  if (!length(chop)) return(NULL)
  z0 <- chop[[1]]; t0 <- as.numeric(z0$status_updated %||% z0$created) / 1000; freed <- names(z0$drops %||% list())
  db <- get_players_db(); pn <- function(id) db[[id]]$full_name %||% id
  plr <- function(id) { p <- db[[id]]; sprintf("%s, %s (%s)", p$full_name %||% id, p$position %||% "?", p$team %||% "FA") }
  users <- build_user_lookup(get_users(league_id))
  ro <- dplyr::bind_rows(lapply(get_rosters(league_id), function(r) tibble::tibble(roster_id = r$roster_id, owner_id = r$owner_id))) |> dplyr::left_join(users, by = "owner_id")
  lab <- function(rid) { w <- ro[match(rid, ro$roster_id), ]; sprintf("%s (%s)", w$team_name, manager_label(w$owner_name)) }
  tx <- unique(c(get_transactions(league_id, week - 1), get_transactions(league_id, week)))
  claims <- dplyr::bind_rows(lapply(tx, function(y) {
    if (!identical(y$status, "complete") || !identical(y$type, "waiver")) return(NULL)
    ts <- as.numeric(y$status_updated) / 1000; if (ts <= t0 || ts >= first_kickoff_secs) return(NULL)
    ad <- intersect(names(y$adds %||% list()), freed); if (!length(ad)) return(NULL)
    tibble::tibble(pid = ad, roster_id = vapply(ad, function(i) as.integer(y$adds[[i]]), integer(1)), bid = suppressWarnings(as.numeric(y$settings$waiver_bid %||% 0)))
  }))
  total_all <- sum(vapply(tx, function(y) { if (!identical(y$status, "complete") || !identical(y$type, "waiver")) return(0); ts <- as.numeric(y$status_updated) / 1000
                                           if (ts <= t0 || ts >= first_kickoff_secs) return(0); v <- suppressWarnings(as.numeric(y$settings$waiver_bid %||% 0)); if (is.na(v)) 0 else v }, numeric(1)))
  if (!nrow(claims) || sum(claims$bid, na.rm = TRUE) <= 0) return(NULL)
  # how each claimed player did this week against his projection (was the money worth it?)
  pf <- if (is.null(perf)) NULL else perf[match(claims$pid, perf$player_id), ]
  claims$scored <- if (is.null(pf)) NA_real_ else ifelse(is.na(pf$scored), 0, pf$scored)
  claims$proj <- if (is.null(pf)) NA_real_ else pf$pre_proj
  one <- function(i) sprintf("%s $%.0f (scored %.1f, projected %.1f)", pn(claims$pid[i]), claims$bid[i], claims$scored[i], ifelse(is.na(claims$proj[i]), 0, claims$proj[i]))
  agg <- claims |> dplyr::group_by(roster_id) |> dplyr::summarise(n = dplyr::n(), spent = sum(bid, na.rm = TRUE), .groups = "drop") |> dplyr::arrange(dplyr::desc(spent))
  top <- agg[1, ]; idx <- which(claims$roster_id == top$roster_id); idx <- idx[order(-claims$bid[idx])]
  tot_s <- sum(claims$scored[idx], na.rm = TRUE); tot_p <- sum(claims$proj[idx], na.rm = TRUE)
  # one claim: just the player line; several: add how the group did together
  det1 <- sprintf("On %d of %s's players: %s.", top$n, manager_label(prev_owner), paste(vapply(idx, one, character(1)), collapse = "; "))
  if (top$n > 1) det1 <- paste(det1, sprintf("Together they scored %.1f against a projection of %.1f this week.", tot_s, tot_p))
  t1 <- tibble::tibble(label = "Biggest Waiver Spender", headline = sprintf("%s, $%.0f", lab(top$roster_id), top$spent), detail = det1)
  j <- which.max(claims$bid)
  # the priciest single player gets its own tile only if he is not already named in the biggest spender's tile
  t2 <- if (claims$roster_id[j] == top$roster_id) NULL else
    tibble::tibble(label = "Most expensive player", headline = sprintf("%s, $%.0f", plr(claims$pid[j]), claims$bid[j]),
                   detail = sprintf("Claimed by %s from %s's team. Scored %.1f against a projection of %.1f this week.", lab(claims$roster_id[j]), manager_label(prev_owner),
                                    claims$scored[j], ifelse(is.na(claims$proj[j]), 0, claims$proj[j])))
  t3 <- tibble::tibble(label = "Waiver spending", headline = sprintf("$%s on %s's players", format(sum(claims$bid, na.rm = TRUE), big.mark = ","), manager_label(prev_owner)),
                       detail = sprintf("%d of the %d players freed by the chop were claimed, for an average of $%.0f. The league spent $%s on waivers in all that week.",
                                        nrow(claims), length(freed), mean(claims$bid, na.rm = TRUE), format(total_all, big.mark = ",")))
  dplyr::bind_rows(t1, t2, t3)
}

# Everything the writer may use. Derived numbers (drops, gaps) are computed here so the model never has to calculate.
recap_fact_sheet <- function(season, week, facts_all, players, last_games, n_alive = NULL, saved = NULL, chopped_info = NULL, lost = NULL, waivers = NULL, sidelined = NULL, payoff = NULL, out_dir = NULL, upforbid = NULL, bare_bench = NULL) {
  f <- facts_all[facts_all$week == week, ]
  earlier <- facts_all[facts_all$week < week, ]
  # every team named in this sheet, with its manager (a user name until collector/managers.csv gives real names)
  mg <- dplyr::bind_rows(
    tibble::tibble(team = c(f$chopped_team, f$next_team, f$pregame_fav), owner = c(f$chopped_owner, f$next_owner, f$pregame_fav_owner)),
    tibble::tibble(team = vapply(last_games, function(g) g$team, character(1)), owner = vapply(last_games, function(g) g$owner, character(1))),
    if (!is.null(saved) && nrow(saved$teams)) tibble::tibble(team = saved$teams$team_name, owner = saved$teams$owner_name),
    if (!is.null(waivers) && nrow(waivers)) { w2 <- rbind(overbids(waivers), close_misses(waivers)); tibble::tibble(team = c(w2$winner_team, w2$runner_up_team), owner = c(w2$winner_owner, w2$runner_up_owner)) },
    if (nrow(earlier)) tibble::tibble(team = c(earlier$chopped_team, earlier$next_team, earlier$pregame_fav),
                                      owner = c(earlier$chopped_owner, earlier$next_owner, earlier$pregame_fav_owner))) |>
    dplyr::filter(!is.na(team), !is.na(owner)) |> dplyr::distinct(team, .keep_all = TRUE)
  L <- c(
    sprintf("SEASON %s, WEEK %d", season, week),
    "Managers (use the manager's name where you would otherwise say they/their):",
    sprintf("- %s: managed by %s", mg$team, manager_label(mg$owner)),
    "",
    sprintf("Teams still alive: %d. With nothing known, each team's even-split chance of being chopped is %s (1 in %d).", f$n_teams, sprintf("%.1f%%", 100 / f$n_teams + 1e-9), f$n_teams),
    sprintf("Chopped team: %s, final score %.1f.", f$chopped_team, f$chopped_final),
    sprintf("Next lowest team: %s, final score %.1f (gap %s points).", f$next_team, f$next_final, fmt_gap(f$gap)),
    sprintf("Before any game, the tracker's favorite for the chop was %s at %s%s.", f$pregame_fav, pct0(f$pregame_fav_chance),
            if (isTRUE(f$pregame_fav_was_chopped)) " (that team was the one chopped)" else " (that team was not chopped)"),
    sprintf("The chopped team's chance before any game: %s.", pct0(f$chopped_pregame_chance)),
    sprintf("The chopped team's chance peaked at %s (%s) before the week's last games.", pct0(f$peak_chance_before_last), f$peak_time),
    sprintf("When the week's last games kicked off, the chopped team's chance was %s.", pct0(f$chance_at_last_kickoff)),
    sprintf("The chopped team was the tracker's top pick for %s of game time.", pct0(f$top_pick_share)),
    sprintf("The chopped team's projected final score: %.1f before the week, a low of %.1f at %s (a drop of %.1f), and it finished with %.1f.",
            f$chopped_pregame_proj, f$chopped_low_proj, f$low_time, f$chopped_pregame_proj - f$chopped_low_proj, f$chopped_final),
    "",
    if (!is.null(chopped_info)) underperf_lines(chopped_info),
    if (!is.null(chopped_info)) left_game_lines(chopped_info),
    chopped_sidelined_lines(sidelined, f$chopped_team, out_dir, week),
    bare_bench_fact_lines(bare_bench, f$chopped_team),
    if (!is.null(chopped_info)) chopped_whatif_lines(chopped_info, f),
    if (!is.null(lost)) lost_bid_lines(lost, f),
    swing_lines(last_game_swing(season, week)),
    "",
    "When the week's last games kicked off, the three most likely teams to be chopped were:"
  )
  for (g in last_games) {
    todo <- if (nrow(g$still_to_play)) paste(sprintf("%s projected %.1f, scored %.1f", g$still_to_play$player, g$still_to_play$projected, g$still_to_play$scored), collapse = "; ") else "none"
    L <- c(L, sprintf("- %s: %s chance, %.1f points scored so far, finished with %.1f%s. Players still to play: %s.",
                      g$team, pct0(g$chance), g$score_so_far, g$final, if (isTRUE(g$chopped)) " (this team was chopped)" else "", todo))
  }
  if (!is.null(saved)) L <- c(L, saved_fact_lines(saved, f))
  if (!is.null(waivers)) L <- c(L, "", waiver_fact_lines(waivers))
  if (!is.null(sidelined) && nrow(sidelined)) L <- c(L, "", sidelined_fact_lines(sidelined, week))
  if (!is.null(payoff) && nrow(payoff)) L <- c(L, "", payoff_fact_lines(payoff))
  if (!is.null(upforbid) && any(upforbid$show)) L <- c(L, "", upforbid_fact_lines(upforbid))
  paste(L, collapse = "\n")
}

# The voice rules for the recap and the preview prompts (collector/preview_lib.R uses it too). kind = "recap" or "preview" changes only who gets which treatment.
sass_voice <- function(kind = c("recap", "preview")) {
  kind <- match.arg(kind)
  paste(
    "- Voice and tone: sassy, theatrical and funny. Borrow the STYLE of the gleeful, sarcastic game-show AI narrator in the Dungeon Crawler Carl books, in your own words: deadpan mock sympathy, gloating over failures, mock-solemn rulings on who won and who lost, and real, loud excitement when someone does well. Do not use the books' own vocabulary or catchphrases: never write 'achievement' (or 'achievement unlocked'), 'the System', 'crawler', 'penalty' or 'level up', and do not quote the books. No swearing. Short sentences, punchy lines, a joke or two in every section.",
    if (kind == "recap")
      "  Who gets what: (1) roast the manager of the CHOPPED team properly, every week: at least one sharp joke in each of its sections (What happened, Why it fell short, the bids that got away), a mock-solemn ruling on the chop, and a punchline at the end of each paragraph, all tied to a fact and never a dry list of figures. (2) Poke fun at managers who overspent on waivers (the massive overbids) and at managers who just missed out on a claim or lost a bid. (3) Give a team that had a close call a rough time, and an especially rough time to a team whose chance of being chopped peaked late on Monday night: mock the sweating and the scramble. (4) Get properly excited, with real hype, for managers who did something good, such as a waiver claim that paid off or was a bargain, a team that beat its projection by a lot, or a team that saved itself: loud, specific, over-the-top cheering in the voice of an announcer, more than a polite compliment."
    else
      "  Who gets what: (1) tease the teams most likely to be chopped, properly: open the 'Teams most likely to be chopped' section with a mock-solemn ruling, give each of the three teams at least one sharp joke tied to a fact (a bye, a lost bid, a pickup that added almost nothing) and end each paragraph on a punchline. (2) Poke fun at managers who overspent on waivers (the massive overbids) and at managers who just missed out on a claim or lost a bid. (3) Give a hard time to teams whose odds got worse while others improved, and to teams that a bye or an injury has left thin. (4) Get properly excited, with real hype, for managers who did something good, such as a smart waiver claim, a bargain, or a team whose odds improved a lot: loud, specific, over-the-top cheering in the voice of an announcer, more than a polite compliment. (5) Keep every team in the story: a team that won nothing on the waivers while its chance of being chopped stayed small gets one playful line, and a big spender gets a joke about the size of the spend (the budget share the fact sheet gives).",
    "  Limits: every joke is about fantasy football only, never about a person's life, family, looks, health or job, and never so mean that the manager would not laugh. Jokes must not add facts or numbers: hype and mockery must be tied to something the fact sheet says. Vary the jokes and wording from week to week and do not repeat a stock line. If a LEAGUE FACTS section follows the fact sheet you may use it; if an EDITOR'S NOTES section follows, follow it for this week only (it can ask for a different or extra roast); every other rule here still applies.",
    sep = "\n")
}

RECAP_SYSTEM_PROMPT <- paste(
  "You write the weekly recap for a fantasy football 'guillotine' league (best ball; the lowest-scoring team each week is chopped).",
  "A live tracker shows each team's chance of being chopped while games are on. You are given a FACT SHEET for one week; write a short recap for the league members.",
  "",
  "Rules:",
  "- Use ONLY facts and numbers in the fact sheet. Do not calculate new numbers, estimate, or add players, events, reasons or numbers that are not in it. Copy team and player names exactly as written.",
  "- Plain language for people who play fantasy football and know nothing about statistics. No statistical or technical terms (no calibration, variance, bell curve, skew, probability distribution, model error, Poisson, simulation).",
  "  Say 'chance' and 'projected' and 'projection'. When you mention the even-split chance, call it 'the even split' and say what it is (1 in however many teams).",
  "- Do not mention or speculate about injuries, injury status or why a player produced little, EXCEPT where the fact sheet has a block headed PAID FOR, THEN SIDELINED or LEFT A GAME WITH AN INJURY. Only then, and only for the players that block lists, say in plain words what it says (a player was ruled out, missed the game, is ruled out for the coming week, or went down during his game, and about when). Even then never name the injury or the body part, never say how it happened, and never give or guess a return date or how long anyone will be out. Everywhere else avoid the words injury, injured, hurt, limp, knock, ankle, hamstring and concussion even figuratively (so never 'insult to injury' or 'hurt the team'; say 'lowered the score' or 'cost the team'); the words hurt, limp, knock, ankle, hamstring and concussion stay banned everywhere. You may state playing time as a plain fact when the fact sheet gives it.",
  "- Where the fact sheet has a PAID FOR, THEN SIDELINED block, add one or two sentences at the end of 'Waiver results' (after its first paragraph, which ends with the season-overview sentence) about the two biggest prices listed, or the one if only one is listed: the manager, the player, the price, and that the player was ruled out. Mention only the players listed in that block, which holds only claims picked up for this week or claimed for next week; never write up an older claim there, even one named elsewhere in the fact sheet. Include a listed player even when he is already covered in the chopped team's section, and do not give a price already given earlier in 'Waiver results' a second time. Where it has a LEFT A GAME WITH AN INJURY block, add one sentence under '### Why it fell short' saying which player went down during his game and about when.",
  "- Where the fact sheet has a SIDELINED PLAYER ON THE CHOPPED TEAM block, open '### Why it fell short' with one or two sentences about that player, as the block says: the manager, the player, that he was ruled out and did not play, and the price, and what Sleeper projected for him if the block gives it. Say plainly that this is a big part of why the team fell short. Never say what the team's score or finish would have been with him. Roast the manager with mock sympathy, tied to the price. If he is also listed in the PAID FOR, THEN SIDELINED block (a claim picked up for this week), he is ALSO written up in 'Waiver results' as that rule says, even though he is covered here, because some readers only read that section; if he is not listed there, leave him out of 'Waiver results'.",
  "- Where the fact sheet has a BARE BENCH block, write exactly one sentence per team listed in the second paragraph of '### What happened', after the ending and just before that paragraph's last sentence, as part of the roast of the chopped team (never in '### Why it fell short'). If the chopped team is not the bare-bench team, aim the joke at the chopped team, never at the team listed: the chopped team's manager had backups who could play and still got chopped, while the listed team had no usable bench and survived anyway. If the chopped team is the bare-bench team, give it as a reason the week went wrong. Name the manager and say the whole bench was on a bye or ruled out, using only the names and counts in the block. Do not name an injury or give a return date.",
  "- Where the fact sheet has a THIN BENCH block (most of the CHOPPED team's bench could not play; the block appears only for the chopped team, never for any other team), write one sentence about it in '### Why it fell short', after the sentences about a sidelined player and before the position-group sentences, saying 'most of the bench' and naming the one backup who could play. Never put it in '### What happened', never use it as a joke aimed at the chopped team's roast, and never mention a thin bench for any team other than the chopped team. Do not name an injury or give a return date.",
  "- Where the fact sheet has a FINALLY PAYING OFF block, do two things. First, list EVERY player in it in 'Waiver results', in one short paragraph placed after the first (auction) paragraph and before the sentences about sidelined players: one short, excited sentence for each, saying the waiver pickup had scored very little until now and finally paid off, with the manager, the price and week of the claim and the figures from the block. Second, where the player's team also has its own paragraph (outperformers, close calls or saved themselves), add one short sentence there too. Say only how many points the PLAYER scored (his average before this week and his score this week), and NEVER say how many points he added to the team's score or final score. Celebrate the manager for the patience. Use only the figures in the block, do not add others, and do not call a claim a bargain or a bust.",
  "- Where the fact sheet has an UP FOR BID THIS WEEK block, add a last section '## Up for bid this week' (after 'Waiver results') as the block says: two to four lively sentences, in the same sassy voice, about the three or four best players listed (his average points over his last three games played, not the whole season, his week-to-week spread, his best game, the number of top-12 weeks that shows his upside, and Sleeper's projection for next week), and say that a table under the text lists them all. A player on a bye next week is still listed and may be called a bye-week pickup, but never say he adds anything next week. Use only the figures in the block. Mention that the waiver run is early Wednesday morning only as the block says. A status such as out or on injured reserve may be stated as that plain word only; never name an injury, a body part or a return date. Never predict who will win a bid and never tell a manager how to bid.",
  sass_voice("recap"),
  "- Be factual and specific; every bit of hype or mockery must be tied to a fact in the fact sheet. If the chopped team had a low chance when the last games kicked off, say plainly that the result was a long shot that happened, and do not suggest the tracker was wrong. Do not add caveats or disclaimers. Do not explain results with luck or skill; celebrate or mock what happened.",
  "- Write short sentences. Avoid em-dashes, parenthetical asides, and constructions like 'not X but Y' or colon-then-reveal.",
  "- Never use the words 'they', 'their', 'them' or 'theirs' anywhere in the body text, even in phrases like 'beat their projection' (write 'its projection' or 'the projection' instead). Where you would use one, use the manager's name from the Managers list instead, for example 'team_three's projected score fell'. The first time you mention a team, write the team name followed by ', managed by' and the manager's name.",
  "- Whenever you give a chance, say what it is the chance of, for example 'a 17% chance of being chopped' or 'team_one's chance of being chopped'. Never write 'X\'s chance' or 'had 47%' without saying it is the chance of being chopped.",
  "- Write about roster moves ONLY where the fact sheet has a section headed ROSTER SWAPS THAT DECIDED THE CHOP or ROSTER SWAPS THAT SAVED THIS TEAM. Everywhere else, leave roster moves out completely. Keep it tight, two or three sentences under '### The swap that backfired' (or in the team's paragraph): name the manager as the decision-maker and the two players ('Jo dropped Case Keenum for Travis Homer'), say what the final score would have been without the swap that decided it against the actual final score, and say who would then have been chopped or that the team would have survived. Do not give the points of the added and dropped players, do not explain the lineup change, and do not give the size of the swap's effect on its own, because the two final scores already say it. Credit a swap that raised the score and blame one that lowered it, and a single joke line is enough. A swap marked as a waiver claim was made by the waiver run, which processes claims at its own set time in the early morning, so never describe that hour as late, early, odd or a middle-of-the-night decision and never say the manager made the move at that hour; you may say the claim went through in the waiver run at that time. A free-agent pickup was made by the manager at the time shown, so its hour may be mentioned.",
  "- Write about waiver bids ONLY where the fact sheet has a section headed WAIVER BIDS THE CHOPPED TEAM LOST. Then add one short paragraph to 'The team that got chopped'. Keep it brief and summarised, never a list of every claim. First one sentence on the bids lost, naming the manager, the winner and each player with the bid against the winning bid in brackets, for example 'Taylor lost three waiver bids to Kai: Tetairoa McMillan ($15 vs $92), Ollie Gordon ($32 vs $87) and George Pickens ($88 vs $118).' Open the paragraph with when the bids were made ('Before the week, Pat lost four waiver bids on players freed by the last chop:'). Then one sentence on only the claims the fact sheet says would have been enough on their own. If more than two were each enough, give the range instead of each figure, from the largest to the smallest, against the points needed, for example 'Any one of those bids was enough to survive, from 19.6 points for Mike Evans down to 6.5 for Jonathan Taylor, against the 5.0 points needed.' If one or two were enough, name each with the points it would have added, for example 'McMillan alone would have added 32.0 points, and Gordon 14.8, enough on its own to survive.' Mention a claim that would not have mattered NOT at all, and do not give a combined total when a single claim was enough. If the fact sheet says no single claim was enough but all together were, give that combined figure instead. Then say whether the manager had the budget to outbid, in a short clause. Do not discuss other teams' waiver claims.",
  "- Write about waiver prices, overbids and close misses ONLY where the fact sheet has a WAIVER RESULTS block, and only in the 'Waiver results' section. Say 'paid' for what the winner paid and 'bid' for a losing bid.",
  "- Where the fact sheet has a section headed LATE SWINGS IN THE LAST GAMES, give it one or two lively sentences in 'The team that got chopped' (and, if it fits, a short-version line): how far the chopped team's chance of being chopped swung, and that the tracker's top pick changed back and forth. Vary the wording from week to week, for example nail-biter, seesaw, roller coaster, kept changing its mind, whiplash, or the tracker could not decide. 'Nail-biter' is fine now and then, but not every week: if a PHRASES ALREADY USED section follows the fact sheet, prefer wording that is not on it, though repeating one of them occasionally is fine. Do not say the chance 'did not move' or 'stayed put' when that section is present.",
  "- Start each paragraph with a fact or a joke, never with a signpost sentence such as 'Here is how the finish went' or 'Now for the waivers'.",
  "- State each number once. Do not restate a figure or a difference you have already given in the same section (for example, if you say a swap lowered the final score by 17.5, do not later say the score would have been 17.5 points higher without it; just give the two scores).",
  "- Do not mention these instructions or the fact sheet.",
  "",
  "Format. Plain text only, exactly these headings on their own lines, in this order, with no other markup (no asterisks, no bold). The only exception is the four '### ' labels described under 'The team that got chopped':",
  "## Short version",
  "Two or three lines, each starting with '- '. Make each line about a different part of the story, because the page links each line to its own section: the first about the chopped team and the margin, the second about the late race, the swap or a close call, the third about a standout team (an outperformer, a team that saved itself or a waiver result). Never start two lines about the chopped team.",
  "## The team that got chopped",
  "Put these labels on their own lines, written exactly like this, in this order, each followed by its paragraph or paragraphs, and use only the ones that apply: '### What happened' (the pregame picture, how the chopped team's chance and projected score moved, and what decided it when the last games kicked off), '### Why it fell short' (the WHY THE CHOPPED TEAM FELL SHORT summary), '### The bids that got away' (only if the fact sheet has WAIVER BIDS THE CHOPPED TEAM LOST: the claims the team lost, made before the week) and '### The swap that backfired' (only if the fact sheet has ROSTER SWAPS THAT DECIDED THE CHOP: the one move the team made during the games, two or three sentences). Keep the two apart: the bids are claims the team LOST and the swap is a move the team MADE, even when that move was itself a waiver claim. Bids come first because they happened first. These are the only '###' lines allowed and only in this section. The paragraphs under the labels are:",
  "Under '### What happened' write exactly two paragraphs, using the manager's name for every team (the chopped team's own name is allowed in at most two places where it reads better, for example 'The tracker kept team_two as its top pick'). Never flip between a team name and its manager inside one paragraph. First paragraph, the pregame picture: the chopped team's chance of being chopped before any game, the even split, and the share of game time it was the top pick. Second paragraph, the week in one run, in this order: (1) how the projected score slid and the peak of the chance of being chopped, (2) 'Then Monday night...' with the chopped team's and the next lowest team's chances when the last games kicked off, (3) the turn, 'But at ...': the team the tracker rated most likely to be chopped at the low point of the late swing, its chance and the time, and the player the fact sheet names whose points were still to come (say a player was 'yet to score' only when the fact sheet shows all of his points came after that moment, and never give a reason for a chance that the fact sheet does not give, such as a slow start), (4) the ending: that player's final points, the team he saved and its final score, and that the chopped team could not catch up to the next lowest team's score, giving both final scores. The LATE SWINGS block names the tracker's top pick at the chopped team's low point; find that team's players in its GROUP entry for the player whose points came after that moment. If at the last kickoff the next lowest team already had the HIGHER chance of being chopped than the chopped team, step (2) must say so ('Lee led the danger list with ...'), and step (3) must not start with 'But' or present that team as a new danger: instead say, in plain order, where the chances stood at the swing's low point, then name the player who was still to play at that moment, and only then say that the chopped team's chance climbed (use the LATE SWINGS figures and times). Never leave a player dangling at the end of a sentence; say what he was doing ('was still to play'). If there is no LATE SWINGS block, or no such team and player can be found, leave out step (3) and write the ending only: the chopped team's final score and the next lowest team's, saying it could not catch up. Do not add a separate paragraph for the finish and do not restate the late swing elsewhere in this section. Keep sentences short, give no more than three figures in a sentence, and name at most one player here, because a table and the other sections cover the rest. Example of the shape (an example only: never copy its words, numbers or names): 'Pat's projected score fell from 95.3 before the week to a low of 61.3 on Sunday at 9:22pm, a drop of 34.0 points, and Pat's chance of being chopped peaked at 98% right then. Then Monday night turned into musical chairs. When the last games kicked off, Casey had a 43% chance of being chopped and Pat had 40%. But at 10:15pm Sam was the one in danger, with a 91% chance of being chopped against Pat's 9% and Josh Allen, QB yet to score. Unfortunately for Pat, Allen finished with 31.5 points to take Sam to 96.7, and team_two could not catch up to Casey's 80.0, finishing at 75.0.'",
  "Under '### Why it fell short', using the WHY THE CHOPPED TEAM FELL SHORT summary (after the sentences about a sidelined player, if the fact sheet has that block), explain in two or three sentences why the team underperformed: which position groups fell short and by how much (group totals are fine), how many starters scored under half their projection, and whether the problem was playing time or production, plus any fumble. Do not list individual players' scores or write a paragraph about individual players, because a table on the page covers them; you may name at most one or two players if they are the clearest example. If the fact sheet has ROSTER SWAPS THAT DECIDED THE CHOP, cover them as the roster rule says; otherwise do not mention roster moves.",
  "## Teams that saved themselves",
  "Only if GROUP 1 in the fact sheet lists a team (otherwise leave this heading out). Say that this team began the week with one of the highest chances of being chopped and was not chopped, give its chance of being chopped before any game, and explain how it got out: how far above its projection it finished, which players in its scoring lineup beat their projection by the most, and how many bench players moved into its final scoring lineup. Name players without giving their projected or scored points, because a table under the paragraph lists each player.",
  "## Close calls",
  "Only if GROUP 2 lists a team (otherwise leave this heading out). Do not define what a close call is; the page already shows the definition under the heading. Keep each team to two or three short sentences: when its chance of being chopped peaked and how high, with the points it had scored then, how it finished and how many points above the chopped team that was, and the ONE player in its final scoring lineup who scored the most after that point, with his points scored after it, for example 'Team Rocket, managed by Sam, peaked at a 91% chance of being chopped on Monday at 10:15pm with 63.5 points scored, and finished with 96.7, which is 21.7 points above the chopped team. Josh Allen, QB scored 31.5 after that peak and did the heavy lifting.' Name only that one player. Leave out the other players, their projections and whether they were in the scoring lineup at that point, because they are not needed. If no player scored after the peak, leave the player out. One joke line per team is enough.",
  "## Teams that massively outperformed their projections",
  "Only if GROUP 3 lists a team (otherwise leave this heading out). Do not define the group; the page already shows the definition under the heading. For each team, give the typical team's figure from the fact sheet and how many points more than the typical team the team beat its projection by, and give the projected and final scores. Then say what carried the team using only the fact sheet, but do NOT give any player's projected or scored points here, because a table under the paragraph lists each player: name the two or three players who beat their projection by the most and say in a few words how many bench players best ball moved into the final scoring lineup, naming them without figures. The team marked HIGHEST SCORE IN THIS GROUP in the fact sheet gets a compliment: end that team's paragraph with one short, warm line in the voice that praises what it did, tied to something the fact sheet says, and put one emoji in that line that fits the praise, for example \U0001F3C6 for a trophy handover ('Robin, here is your trophy \U0001F3C6.'), \U0001F525 for a team that was on fire, \U0001F451 for a team that ruled the week or \U0001F680 for one that blew past its projection. Vary the line and the emoji from week to week; it does not have to be a trophy. Use that one emoji only there and nowhere else in the text (team names keep any symbol they already have, such as the knife in 'Khalil them all'), and if no team is marked do not use an emoji.",
  "## Waiver results",
  "Only if the fact sheet has a WAIVER RESULTS block (otherwise leave this heading out). Put it after the team sections; only the 'Up for bid this week' section may follow it. One or two short paragraphs. Say how many of the players freed by the chop were claimed and the total spent. Then name the managers who massively overbid, giving the player, the price, the runner-up bid and how this player did this week against the projection (use the MASSIVE OVERBIDS list; leave the topic out if it says none). Then name the managers who just missed out, giving the player, the winning price and the missing manager's bid, and say plainly when a bid tied and lost the tie (use the JUST MISSED OUT list; leave it out if it says none). Keep it playful and about fantasy football only. If a manager already appeared in the paragraph about the chopped team's lost bids, mention that manager in one short clause and do not repeat the figures. The FIRST paragraph of the section, the one about the auction, must end with exactly this sentence: 'Will it pay off? Follow along in the season overview.' Later paragraphs (the players who finally paid off, the players who were ruled out) come after it.",
  "## Up for bid this week",
  "Only if the fact sheet has an UP FOR BID THIS WEEK block (otherwise leave this heading out). It is the last section. Write it as the rule above and the block say.",
  sep = "\n")

extract_numbers <- function(x) {
  m <- gregexpr("[0-9]+(?:\\.[0-9]+)?", x, perl = TRUE)
  as.numeric(unlist(regmatches(x, m)))
}

# Parse "## Heading" sections; returns a named list of character vectors (lines), or NULL
parse_narrative <- function(text) {
  lines <- strsplit(gsub("\r", "", text), "\n")[[1]]
  idx <- grep("^##\\s+", lines)
  if (!length(idx)) return(NULL)
  sections <- list()
  for (k in seq_along(idx)) {
    end <- if (k < length(idx)) idx[k + 1] - 1 else length(lines)
    sections[[trimws(sub("^##\\s+", "", lines[idx[k]]))]] <- if (idx[k] < end) lines[(idx[k] + 1):end] else character(0)
  }
  sections
}

# Is the text publishable? Returns list(ok, reasons, sections).
check_narrative <- function(text, fact_sheet, need_compare) {
  reasons <- character(0)
  sections <- parse_narrative(text)
  has_group <- function(marker) { # does the fact sheet list at least one team under this group heading?
    L <- strsplit(fact_sheet, "\n")[[1]]; i <- grep(marker, L, fixed = TRUE)
    length(i) > 0 && length(L) > i[1] && startsWith(L[i[1] + 1], "- ")
  }
  need <- c("Short version", "The team that got chopped",
            if (has_group("GROUP 1, SAVED THEMSELVES")) "Teams that saved themselves",
            if (has_group("GROUP 2, CLOSE CALLS")) "Close calls",
            if (has_group("GROUP 3, MASSIVELY OUTPERFORMED")) "Teams that massively outperformed their projections",
            if (grepl("WAIVER RESULTS (", fact_sheet, fixed = TRUE)) "Waiver results",
            if (grepl("UP FOR BID THIS WEEK (", fact_sheet, fixed = TRUE)) "Up for bid this week")
  if (!is.null(sections) && "Waiver results" %in% names(sections) && !grepl("season overview", paste(sections[["Waiver results"]], collapse = " "), fixed = TRUE)) reasons <- c(reasons, "the Waiver results section does not have the season overview line")
  if (is.null(sections) || !all(need %in% names(sections))) reasons <- c(reasons, sprintf("missing section(s): %s", paste(setdiff(need, names(sections)), collapse = ", ")))
  inj_facts <- grepl("PAID FOR, THEN SIDELINED|LEFT A GAME WITH AN INJURY|SIDELINED PLAYER ON THE CHOPPED TEAM|UP FOR BID THIS WEEK|BARE BENCH|THIN BENCH", fact_sheet)   # the fact sheet allows plain mentions of injuries (never the type, the body part or a return date)
  if (grepl(if (inj_facts) "hurt|limp|knock|ankle|hamstring|concussion" else "injur|hurt|limp|knock|ankle|hamstring|concussion", text, ignore.case = TRUE)) reasons <- c(reasons, if (inj_facts) "uses a banned injury word" else "mentions injuries")
  if (inj_facts && grepl("\\b(expected|set|due|scheduled) (back|to return)\\b|\\breturn date\\b|\\b(will|should|could|might) (return|be back)\\b|\\bout for (the )?(season|weeks|month)\\b|\\bhow long\\b|\\b(knee|shoulder|toe|thumb|calf|groin|wrist|elbow|achilles|stinger|illness)\\b", text, ignore.case = TRUE, perl = TRUE)) reasons <- c(reasons, "names an injury, a body part or a return date")
  lab_re <- "^### (What happened|Why it fell short|The bids that got away|The swap that backfired)[ \t]*$"
  tl <- strsplit(gsub("\r", "", text), "\n")[[1]]
  if (grepl("[*_`]{2}", text) || any(grepl("^#{3,}", tl) & !grepl(lab_re, tl))) reasons <- c(reasons, "contains markdown formatting")
  if (!is.null(sections)) for (nm in setdiff(names(sections), "The team that got chopped")) if (any(grepl(lab_re, sections[[nm]]))) reasons <- c(reasons, sprintf("has a ### sub-label outside the chopped team's section (in '%s')", nm))
  if (grepl("fact sheet", text, ignore.case = TRUE)) reasons <- c(reasons, "mentions the fact sheet")
  if (grepl("achievement|\\bthe system\\b|crawler", text, ignore.case = TRUE)) reasons <- c(reasons, "uses the books' own vocabulary (achievement / the System / crawler)")
  team_names <- sub(": managed by .*$", "", sub("^- ", "", grep("^- .+: managed by ", strsplit(fact_sheet, "\n")[[1]], value = TRUE)))
  masked <- gsub("(?m)^##[^\n]*$", " ", text, perl = TRUE)   # the headings (e.g. "...outperformed their projections") are fixed wording
  for (tn in team_names[order(-nchar(team_names))]) masked <- gsub(tn, " ", masked, fixed = TRUE)
  if (grepl("\\b(they|their|theirs|them)\\b", masked, ignore.case = TRUE, perl = TRUE)) reasons <- c(reasons, "uses they/their/them instead of the manager's name")
  strip_names <- function(x) { for (u in all_usernames()) x <- gsub(u, " ", x, fixed = TRUE); x }
  allowed <- extract_numbers(strip_names(fact_sheet))
  used <- extract_numbers(strip_names(text))
  bad <- used[!vapply(used, function(n) any(abs(allowed - n) <= if (n == round(n)) 0.5001 else 0.0501), logical(1))]
  if (length(bad)) reasons <- c(reasons, sprintf("numbers not in the fact sheet: %s", paste(unique(bad), collapse = ", ")))
  list(ok = length(reasons) == 0, reasons = reasons, sections = sections)
}

# The whole step. Always returns list(text, sections, kind, model):
#   kind "draft"    a person pasted the prompt into Claude and saved the reply as narrative_wNN.draft.txt (or --narrative-file)
#   kind "template" the automatic text, used when neither of the above exists or a draft fails its checks
# A model-written text is only used if it passes check_narrative(); otherwise the rejected text is saved and the template is used.
# Also writes the fact sheet (.facts.txt) and a ready-to-paste prompt (.prompt.txt) next to the page every time.
make_narrative <- function(fact_sheet, need_compare, out_prefix, template, narrative_file = NULL, file_label = "supplied text file",
                           log = function(...) message(sprintf(...))) {
  load_recap_env()
  # optional: recap/{season}/narrative_wNN.notes.txt = the editor's one-off requests for THIS week (e.g. "roast one manager harder"); it is part of the prompt
  notes_f <- paste0(out_prefix, ".notes.txt")
  notes <- if (file.exists(notes_f)) trimws(paste(readLines(notes_f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")) else ""
  notes_block <- if (nzchar(notes)) paste0("\n\nEDITOR'S NOTES FOR THIS WEEK (follow these; they apply to this week only):\n", notes) else ""
  # collector/league_facts.txt = standing facts about the league that never change week to week (e.g. who the commissioner is); they go into every prompt
  lf <- file.path("collector", "league_facts.txt")
  facts_txt <- if (file.exists(lf)) trimws(paste(readLines(lf, warn = FALSE, encoding = "UTF-8"), collapse = "\n")) else ""
  league_block <- if (nzchar(facts_txt)) paste0("\n\nLEAGUE FACTS (true; you may use them in the roast and in jokes, and they add no numbers):\n", facts_txt) else ""
  # stock phrases for the late-swing moment that earlier weeks' recaps already used, so the writer does not repeat them
  wk_now <- suppressWarnings(as.integer(sub(".*_w0*([0-9]+)$", "\\1", out_prefix)))
  prev <- if (is.finite(wk_now) && wk_now > 1) list.files(dirname(out_prefix), pattern = "^narrative_w[0-9]+\\.txt$", full.names = TRUE) else character(0)
  prev <- prev[as.integer(sub("^narrative_w0*([0-9]+)\\.txt$", "\\1", basename(prev))) < wk_now]
  prev_txt <- tolower(paste(unlist(lapply(prev, function(f) readLines(f, warn = FALSE, encoding = "UTF-8"))), collapse = " "))
  stock <- c("nail-biter", "nailbiter", "seesaw", "roller coaster", "rollercoaster", "whiplash", "wobble", "kept changing its mind", "could not decide", "could not make up its mind", "musical chairs")
  used_block <- if (length(prev) && any(vapply(stock, grepl, logical(1), x = prev_txt, fixed = TRUE)))
    paste0("\n\nPHRASES ALREADY USED IN EARLIER WEEKS THIS SEASON (prefer different wording for the late swing; repeating one occasionally is fine):\n", paste(stock[vapply(stock, grepl, logical(1), x = prev_txt, fixed = TRUE)], collapse = ", ")) else ""
  writeLines(enc2utf8(fact_sheet), paste0(out_prefix, ".facts.txt"), useBytes = TRUE)
  writeLines(enc2utf8(paste0(RECAP_SYSTEM_PROMPT, "\n\nFACT SHEET\n\n", fact_sheet, league_block, used_block, notes_block, "\n\nWrite the recap now.")), paste0(out_prefix, ".prompt.txt"), useBytes = TRUE)
  text <- NULL; kind <- NULL; model <- NULL
  if (!is.null(narrative_file)) {
    text <- paste(readLines(narrative_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n"); kind <- "draft"; model <- file_label
  }
  if (!is.null(text) && nzchar(trimws(text))) {
    chk <- check_narrative(text, fact_sheet, need_compare)
    if (chk$ok) {
      unlink(paste0(out_prefix, ".rejected.txt")) # a draft rejected earlier is stale now
      writeLines(enc2utf8(text), paste0(out_prefix, ".txt"), useBytes = TRUE)
      writeLines(c(kind, model), paste0(out_prefix, ".kind"), useBytes = TRUE)
      return(list(text = text, sections = chk$sections, kind = kind, model = model))
    }
    writeLines(enc2utf8(text), paste0(out_prefix, ".rejected.txt"), useBytes = TRUE)
    log("narrative REJECTED (%s); using the automatic text. Draft saved to %s.rejected.txt", paste(chk$reasons, collapse = " | "), out_prefix)
  }
  # the automatic text; checked the same way as a model's, as a self-test (a failure means a bug in the template, so say so)
  ttext <- sections_to_text(template)
  tchk <- check_narrative(ttext, fact_sheet, need_compare)
  if (!tchk$ok) log("narrative: WARNING the automatic text failed its own checks (%s)", paste(tchk$reasons, collapse = " | "))
  writeLines(enc2utf8(ttext), paste0(out_prefix, ".txt"), useBytes = TRUE)
  writeLines(c("template", "a script"), paste0(out_prefix, ".kind"), useBytes = TRUE)
  list(text = ttext, sections = template, kind = "template", model = "a script")
}

# --- Automatic (no AI) description -----------------------------------------------------------------
# The same four sections the model-written version has, put together from the week's numbers with fixed sentences. It always works,
# needs nothing configured, and can't invent anything: every number comes straight from the facts. It reads more formulaically than a
# written story, which is what the "paste the prompt into Claude" option (collector/README.md) is for.

say_time <- function(x) { # "Sun 6:22pm" -> "Sunday at 6:22pm"
  days <- c(Mon = "Monday", Tue = "Tuesday", Wed = "Wednesday", Thu = "Thursday", Fri = "Friday", Sat = "Saturday", Sun = "Sunday")
  d <- sub(" .*$", "", x); t <- sub("^\\S+ ", "", x)
  paste0(ifelse(d %in% names(days), days[d], d), " at ", t)
}

plural <- function(n, one, many = paste0(one, "s")) ifelse(n == 1, one, many)

join_and <- function(x) if (length(x) <= 1) x else paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)])

# "a 11% chance" -> "an 11% chance" (the numbers read aloud that start with a vowel sound: 8, 11, 18, 80-89)
fix_articles <- function(x) gsub("\\ba ((?:8|11|18)(?:\\.\\d+)?%|8[0-9](?:\\.\\d+)?%)", "an \\1", x, perl = TRUE)

template_narrative <- function(week, facts_all, players, last_games, saved = NULL, chopped_info = NULL, lost = NULL, waivers = NULL, sidelined = NULL, payoff = NULL, out_dir = NULL, upforbid = NULL, bare_bench = NULL) {
  f <- facts_all[facts_all$week == week, ]
  earlier <- facts_all[facts_all$week < week, ]
  even <- sprintf("%.1f%%", 100 / f$n_teams + 1e-9)
  c_last <- f$chance_at_last_kickoff
  top_other <- Find(function(g) !isTRUE(g$chopped), last_games)

  last_line <- if (c_last >= 0.75) {
    sprintf("- Going into the last games, %s's chance of being chopped was %s.", f$chopped_team, pct0(c_last))
  } else if (c_last >= 0.25) {
    sprintf("- It was still open when the last games kicked off. %s had a %s chance of being chopped%s.", f$chopped_team, pct0(c_last),
            if (!is.null(top_other)) sprintf(", and %s had %s", top_other$team, pct0(top_other$chance)) else "")
  } else {
    sprintf("- It was a long shot. %s had only a %s chance of being chopped when the last games kicked off%s.", f$chopped_team, pct0(c_last),
            if (!is.null(top_other)) sprintf(", while %s was the favorite at %s", top_other$team, pct0(top_other$chance)) else "")
  }
  mgr <- manager_label(f$chopped_owner)
  short <- c(
    sprintf("- %s, managed by %s, was chopped with %.1f points, %s behind %s (%.1f).", f$chopped_team, mgr, f$chopped_final, fmt_gap(f$gap), f$next_team, f$next_final),
    if (isTRUE(f$pregame_fav_was_chopped)) {
      sprintf("- The tracker saw it coming. %s had the highest chance of being chopped in the league before any game, at %s, and was its top pick for %s of game time.",
              f$chopped_team, pct0(f$pregame_fav_chance), pct0(f$top_pick_share))
    } else {
      sprintf("- Before any game the tracker's favorite for the chop was %s, with a %s chance of being chopped. %s started with a %s chance and was its top pick for %s of game time.",
              f$pregame_fav, pct0(f$pregame_fav_chance), f$chopped_team, pct0(f$chopped_pregame_chance), pct0(f$top_pick_share))
    },
    last_line)

  drop <- f$chopped_pregame_proj - f$chopped_low_proj
  p1 <- sprintf("Before any game the tracker gave %s a %s chance of being chopped. An even split among %d teams would give each team %s.", f$chopped_team, pct0(f$chopped_pregame_chance), f$n_teams, even)
  p2 <- if (drop >= 5) {
    sprintf("%s's projected score fell from %.1f before the week to a low of %.1f on %s, a drop of %.1f points. %s's chance of being chopped peaked at %s on %s, before the last games.",
            mgr, f$chopped_pregame_proj, f$chopped_low_proj, say_time(f$low_time), drop, mgr, pct0(f$peak_chance_before_last), say_time(f$peak_time))
  } else {
    sprintf("%s's projected score stayed close to its starting %.1f, with a low of %.1f on %s. %s's chance of being chopped peaked at %s on %s, before the last games.",
            mgr, f$chopped_pregame_proj, f$chopped_low_proj, say_time(f$low_time), mgr, pct0(f$peak_chance_before_last), say_time(f$peak_time))
  }
  keep <- Filter(function(g) isTRUE(g$chopped) || g$chance >= 0.01, last_games)
  sentences <- vapply(seq_along(keep), function(i) {
    g <- keep[[i]]; n <- nrow(g$still_to_play)
    lead <- if (i == 1) "When the last games kicked off, " else ""
    todo <- if (n == 0) "no players left to play" else sprintf("%d %s still to play: %s", n, plural(n, "player"),
                paste(sprintf("%s was projected for %.1f and scored %.1f", g$still_to_play$player, g$still_to_play$projected, g$still_to_play$scored), collapse = "; "))
    sprintf("%s%s had %.1f points and %s. The team finished with %.1f.", lead, g$team, g$score_so_far, todo, g$final)
  }, character(1))
  p3 <- paste(sentences, collapse = " ")

  snaps_txt <- function(s) ifelse(nzchar(s), sub("^(\\d+) \\((\\d+)%\\)$", ", on \\1 offensive snaps (\\2% of the team's)", s), "")
  shortfalls <- sprintf("%s (%s) was projected for %.1f and had scored %.1f by then%s%s.", players$player, players$team, players$projected, players$scored_by_then,
                        ifelse(players$final != players$scored_by_then, sprintf(", then finished the week with %.1f", players$final), ""), snaps_txt(players$snaps))
  cwi <- NULL; flip <- FALSE
  if (!is.null(chopped_info) && !is.null(chopped_info$swaps) && nrow(chopped_info$swaps) && chopped_info$cf$n_undone > 0 && !identical(chopped_info$cf$low_team, f$chopped_team)) {
    cf <- chopped_info$cf; sw <- chopped_info$swaps; mgr_c <- manager_label(f$chopped_owner)
    n_words <- c("a roster swap", "two roster swaps", "three roster swaps")
    cwi <- paste0(sprintf("%s made %s during the games. ", mgr_c, if (nrow(sw) <= 3) n_words[nrow(sw)] else "several roster swaps"),
                  paste(vapply(seq_len(nrow(sw)), function(i) swap_sentence(sw[i, ]), character(1)), collapse = " "))
    if (cf$n_undone == 0) {
      cwi <- paste(cwi, "None of the swaps lowered the score, so skipping swaps could not have saved the team.")
    } else {
      flip <- !identical(cf$low_team, f$chopped_team)
      cwi <- paste0(paste(cwi, sprintf("Had %s skipped only the swap that lowered the score, the one on %s, the final score would have been %.1f instead of %.1f. %s",
                                       mgr_c, cf$undone, cf$cf_final, f$chopped_final, whatif_verdict(f$chopped_team, cf$low_team, cf$low_final, f$chopped_team, TRUE))),
                    if (nzchar(cf$cascade)) paste0(" ", cf$cascade, "."))
    }
  }
  if (flip) short <- c(short, sprintf("- A roster swap decided it. Had %s skipped the swap that lowered the score, %s would have been chopped instead.", manager_label(f$chopped_owner), chopped_info$cf$low_team))
  cs_sd <- if (!is.null(sidelined) && nrow(sidelined) && !is.null(out_dir)) sidelined[sidelined$team_name == f$chopped_team & sidelined$missed_this, ] else NULL
  why_sd <- if (!is.null(cs_sd) && nrow(cs_sd)) c("### Why it fell short", sidelined_template_text(head(cs_sd, 1), chopped = TRUE)) else NULL
  bb_txt <- bare_bench_template_text(bare_bench, "bare")
  if (!is.null(bb_txt)) p3 <- paste(p3, bb_txt)
  thin_txt <- bare_bench_template_text(if (!is.null(bare_bench)) bare_bench[bare_bench$team_name %in% f$chopped_team, ], "thin")
  if (!is.null(thin_txt)) p3 <- paste(p3, thin_txt)
  sections <- list(`Short version` = short, `The team that got chopped` = c("### What happened", p1, p2, p3, why_sd, if (!is.null(lost)) c("### The bids that got away", lost_bid_text(lost, f, mgr)), if (!is.null(cwi)) c("### The swap that backfired", cwi)))
  if (!is.null(saved) && nrow(saved$teams)) {
    pl <- saved$players
    helped <- function(v) {
      ab <- pl[pl$team_name == v$team_name & pl$kind == "above", ]; sb <- pl[pl$team_name == v$team_name & pl$kind == "sub", ]; mv <- pl[pl$team_name == v$team_name & pl$kind == "move", ]
      paste0(if (nrow(ab)) sprintf(" The players in the scoring lineup who beat projection by the most were %s; the table below has their points.", join_and(ab$player)) else "",
             if (nrow(sb)) sprintf(" Best ball also moved %s into the final scoring lineup from the bench.", join_and(sb$player)) else "",
             what_if_txt(v, manager_label(v$owner_name)))
    }
    what_if_txt <- function(v, m) {
      if (is.na(v$n_moves) || v$n_moves == 0 || is.na(v$n_undone) || v$n_undone == 0 || !identical(v$low_team, v$team_name)) return("")
      sw <- saved$swaps[saved$swaps$roster_id == v$roster_id, ]
      txt <- paste0(" ", m, " made roster swaps during the games. ", paste(vapply(seq_len(nrow(sw)), function(i) swap_sentence(sw[i, ]), character(1)), collapse = " "))
      if (v$n_undone == 0) return(paste(txt, "None of the swaps raised the score."))
      paste0(txt, sprintf(" Had %s skipped only the swap that helped, the one on %s, the final score would have been %.1f instead of %.1f. %s",
                          m, v$undone, v$cf_final, v$final, whatif_verdict(v$team_name, v$low_team, v$low_final, f$chopped_team, FALSE)),
             if (nzchar(v$cascade)) paste0(" ", v$cascade, "."))
    }
    fin <- function(v, m) sprintf("%s's team was projected for %.1f and finished with %.1f, %s points %s that projection and %s points above the chopped team.",
                                  m, v$pre_proj, v$final, fmt_gap(abs(v$beat)), if (v$beat >= 0) "above" else "below", fmt_gap(v$above_chop))
    mk <- function(cat, one) {
      g <- saved$teams[saved$teams$category == cat, ]
      if (!nrow(g)) return(NULL)
      vapply(seq_len(nrow(g)), function(i) one(g[i, ], manager_label(g$owner_name[i])), character(1))
    }
    sec_saved <- mk("danger", function(v, m) paste0(sprintf("%s, managed by %s, began the week with a %s chance of being chopped, the highest of any team that survived. ", v$team_name, m, pct0(v$pre_prob)), fin(v, m), helped(v)))
    sec_close <- mk("close", function(v, m) {
      ap <- pl[pl$team_name == v$team_name & pl$kind == "after_peak", ]
      paste0(sprintf("%s, managed by %s, was a close call. The team had a %s chance of being chopped before any game and the chance reached %s on %s, when the team had %.1f points. ", v$team_name, m, pct0(v$pre_prob), pct0(v$peak), say_time(v$peak_time), v$score_at_peak),
             fin(v, m),
             if (nrow(ap)) sprintf(" After that point the players who scored the most were %s.", join_and(sprintf("%s, projected %.1f and scoring %.1f more points%s", ap$player, ap$projected, ap$scored, ifelse(grepl("^not", ap$note), " after starting out of the scoring lineup", "")))) else "")
    })
    sec_beat <- mk("beat", function(v, m) paste0(sprintf("%s, managed by %s, was not in danger, with a %s chance of being chopped before any game. ", v$team_name, m, pct0(v$pre_prob)), fin(v, m),
                                                 sprintf(" The typical team finished %.1f points above its projection this week, so this team beat its projection by %s points more.", v$league_median_beat, fmt_gap(v$beat_vs_league)), helped(v)))
    if (!is.null(sec_saved)) sections[["Teams that saved themselves"]] <- sec_saved
    if (!is.null(sec_close)) sections[["Close calls"]] <- sec_close
    if (!is.null(sec_beat)) sections[["Teams that massively outperformed their projections"]] <- sec_beat
  }
  if (!is.null(waivers) && nrow(waivers)) {
    wt <- waiver_template_text(waivers); st <- sidelined_template_text(sidelined, week = week); pt <- payoff_template_text(payoff)
    add <- c(st, pt)
    sections[["Waiver results"]] <- if (is.null(add)) wt else c(wt, add)
    ub <- upforbid_template_text(upforbid); if (!is.null(ub)) sections[["Up for bid this week"]] <- ub   # the season-overview sentence closes the first paragraph (wt); the rest follow it
  }
  lapply(sections, fix_articles)
}

sections_to_text <- function(sections) paste(unlist(lapply(names(sections), function(n) c(paste0("## ", n), sections[[n]]))), collapse = "\n")

# A person's step: dropping narrative_wNN.draft.txt next to the page. Pending = newer than any result already written from it.
recap_pending_draft <- function(prefix) {
  d <- paste0(prefix, ".draft.txt")
  if (!file.exists(d)) return(FALSE)
  newer_than <- function(a, b) !file.exists(b) || file.mtime(a) > file.mtime(b)
  newer_than(d, paste0(prefix, ".txt")) && newer_than(d, paste0(prefix, ".rejected.txt"))
}

notify_mac <- function(title, msg) {
  if (Sys.info()[["sysname"]] != "Darwin" || interactive()) return(invisible(FALSE))
  try(system2("osascript", c("-e", sprintf('display notification "%s" with title "%s"', gsub('"', "'", msg), gsub('"', "'", title))), stdout = FALSE, stderr = FALSE), silent = TRUE)
  invisible(TRUE)
}

# --- Publishing to GitHub Pages ----------------------------------------------------------------------
# Opt-in: only runs when RECAP_PAGES_DIR names a local clone of the repo that GitHub Pages serves (set it in ~/.guillotine_recap_env).
# Copies ONLY recap/{season}/index.html and img/ (not the fact sheets, prompts or drafts), writes a root index.html that redirects to the
# newest season, commits and pushes. Needs git able to push without prompting (see collector/README.md).
publish_recap <- function(season, week = NULL, pages_dir = Sys.getenv("RECAP_PAGES_DIR"), log = function(...) message(sprintf(...)), what = "Recap") {
  pages_dir <- normalizePath(path.expand(pages_dir), mustWork = FALSE)
  proj <- normalizePath(".")
  if (pages_dir == proj || startsWith(pages_dir, paste0(proj, "/"))) stop("refusing to publish into the app's own repository: ", pages_dir)
  if (!dir.exists(file.path(pages_dir, ".git"))) stop("RECAP_PAGES_DIR is not a git clone: ", pages_dir)
  git <- function(..., ok_fail = NULL) {
    out <- suppressWarnings(system2("git", c("-C", shQuote(pages_dir), ...), stdout = TRUE, stderr = TRUE))
    st <- attr(out, "status")
    if (!is.null(st) && st != 0 && !(!is.null(ok_fail) && any(grepl(ok_fail, out)))) stop(sprintf("git %s failed: %s", paste(c(...)[1]), paste(out, collapse = " ")))
    out
  }
  if (is.null(tryCatch(git("remote", "get-url", "origin"), error = function(e) NULL))) stop("the Pages repo has no 'origin' remote")
  git("pull", "--rebase", "--autostash", ok_fail = "couldn't find remote ref|no such ref|Cannot rebase|no tracking information|There is no tracking")
  dest <- file.path(pages_dir, season)
  dir.create(dest, showWarnings = FALSE, recursive = TRUE)
  src <- file.path("recap", season)
  file.copy(file.path(src, "index.html"), file.path(dest, "index.html"), overwrite = TRUE)
  unlink(file.path(dest, "img"), recursive = TRUE)
  file.copy(file.path(src, "img"), dest, recursive = TRUE)
  writeLines(sprintf('<!doctype html><meta charset="utf-8"><meta name="robots" content="noindex,nofollow"><meta http-equiv="refresh" content="0; url=%s/"><a href="%s/">%s Weekly</a>', season, season, h(APP_TITLE)),
             file.path(pages_dir, "index.html"))
  file.create(file.path(pages_dir, ".nojekyll"))
  git("add", "-A")
  if (!length(git("status", "--porcelain"))) { log("publish: nothing changed in %s", pages_dir); return(invisible(FALSE)) }
  git("commit", "-m", shQuote(sprintf("%s %s%s", what, season, if (!is.null(week)) sprintf(" week %s", week) else "")))
  git("push", "-u", "origin", "HEAD")
  log("publish: pushed season %s to %s", season, trimws(git("remote", "get-url", "origin")))
  invisible(TRUE)
}

# Publish only if RECAP_PAGES_DIR is set and the page or its images changed since the last successful publish.
publish_if_needed <- function(season, week, log = function(...) message(sprintf(...)), what = "Recap") {
  load_recap_env()
  dir <- Sys.getenv("RECAP_PAGES_DIR")
  if (!nzchar(dir)) { log("publish: RECAP_PAGES_DIR not set; the page was built locally only"); return(invisible(FALSE)) }
  files <- c(file.path("recap", season, "index.html"), list.files(file.path("recap", season, "img"), full.names = TRUE))
  stamp <- paste(unname(tools::md5sum(files)), collapse = "")
  marker <- file.path("recap", season, ".published")
  if (file.exists(marker) && identical(readLines(marker, warn = FALSE)[1], stamp)) { log("publish: already up to date"); return(invisible(FALSE)) }
  publish_recap(season, week, dir, log, what = what)
  writeLines(stamp, marker)
  invisible(TRUE)
}

# narrative sections -> HTML (headings become h3; '- ' lines become a list; the rest become paragraphs)
# Heading colors follow the categories on the slope chart (analysis/plot_team_elimination.R): red = chopped, teal = saved themselves,
# orange = close call, purple = massively outperformed
SECTION_CLASS <- c(`The team that got chopped` = "chop", `Teams that saved themselves` = "saved", `Close calls` = "close",
                   `Teams that massively outperformed their projections` = "beat",
                   # the preview's text sections (colors only; no skull, no jump-bar ids)
                   `Teams most likely to be chopped` = "pvteams", `Waiver winners and misses` = "pvwaiver", Injuries = "pvinj", `Bye weeks` = "pvbye")

# One-line definitions shown under the headings of the three team groups (the same definitions the fact sheet gives the writer)
SECTION_NOTES <- c(
  `Teams that saved themselves` = "A team that began the week in danger and was not chopped: the survivor with the highest chance of being chopped before any game, at least twice the even split (1 in the number of teams).",
  `Close calls` = "Teams with a lower chance before any game whose chance of being chopped climbed to at least 25% once games were on (and at least 5 points above where it started), and that were not chopped.",
  `Teams that massively outperformed their projections` = "Teams that were not in danger and beat the score projected before the week by at least 20 points more than the typical team did that week.")

# sections: named list of lines. week: when given, the group headings get an id (wk5-chop ...) for the jump bar. links: optional named list
# (by section) of data frames (id, text), one row per bullet, appended to the bullets as "See ..." links. Lines starting "### " are
# sub-labels inside a section (used in the chopped team's section).
narrative_html <- function(sections, week = NULL, links = NULL, ids = NULL) {
  out <- character(0)
  for (nm in names(sections)) {
    cls <- SECTION_CLASS[nm]
    id <- if (!is.na(cls) && !is.null(week)) sprintf(' id="wk%d-%s"', week, cls) else if (nm %in% names(ids)) sprintf(' id="%s"', ids[[nm]]) else ""
    skull <- if (!is.na(cls) && identical(unname(cls), "chop")) " \U0001F480" else ""   # the chopped team's section is marked with a skull
    out <- c(out, paste0("<h3", id, if (!is.na(cls)) sprintf(' class="h-%s"', cls) else "", ">", h(nm), skull, "</h3>"),
             if (nm %in% names(SECTION_NOTES)) paste0('<p class="fine">', h(SECTION_NOTES[[nm]]), "</p>"))
    body <- trimws(sections[[nm]]); body <- body[nzchar(body)]
    lk <- links[[nm]]; nb <- 0
    i <- 1
    while (i <= length(body)) {
      if (startsWith(body[i], "- ")) {
        j <- i; while (j <= length(body) && startsWith(body[j], "- ")) j <- j + 1
        k <- nb + seq_len(j - i); nb <- nb + (j - i)
        more <- if (is.null(lk)) rep("", length(k)) else vapply(k, function(q) if (q <= nrow(lk) && !is.na(lk$id[q])) paste0(' <a class="more" href="#', lk$id[q], '">', h(lk$text[q]), "</a>") else "", character(1))
        out <- c(out, paste0("<ul>", paste0("<li>", h(sub("^- ", "", body[i:(j - 1)])), more, "</li>", collapse = ""), "</ul>")); i <- j
      } else if (startsWith(body[i], "### ")) {
        lab <- sub("^###\\s+", "", body[i])
        lid <- if (!is.null(week) && identical(unname(cls), "chop")) sprintf(' id="wk%d-chop-%s"', week, switch(lab, `What happened` = "what", `Why it fell short` = "why", `The swap that backfired` = "swap", `The bids that got away` = "lost-bids", "x")) else ""
        out <- c(out, paste0("<h4", lid, ' class="sublabel">', h(lab), "</h4>")); i <- i + 1
      } else { out <- c(out, paste0("<p>", h(body[i]), "</p>")); i <- i + 1 }
    }
  }
  paste(out, collapse = "\n")
}

# --- the static page ---------------------------------------------------------------------------

h <- function(x) htmltools::htmlEscape(as.character(x))

# cell_cls: optional named list, one entry per column that needs extra per-cell classes (a character vector with one class, or "", per row)
html_table <- function(df, num_cols = character(), cls = NULL, red_cols = character(), cell_cls = list(), sortable = FALSE, sort_vals = list(), raw_cols = character()) {   # raw_cols: columns whose cells are already-escaped HTML
  head <- paste0("<th", ifelse(names(df) %in% num_cols, ' class="num"', ""), ">", h(names(df)), "</th>", collapse = "")
  rows <- vapply(seq_len(nrow(df)), function(i) {
    cells <- vapply(seq_along(df), function(j) {
      nm <- names(df)[j]
      extra <- cell_cls[[nm]]
      cl <- c(if (nm %in% num_cols) "num", if (nm %in% red_cols) "chop", if (!is.null(extra) && nzchar(extra[i])) extra[i])
      sv <- sort_vals[[nm]]
      paste0("<td", if (length(cl)) paste0(' class="', paste(cl, collapse = " "), '"') else "", if (!is.null(sv) && !is.na(sv[i])) sprintf(' data-v="%s"', sv[i]) else "", ">", if (nm %in% raw_cols) as.character(df[[j]][i]) else h(as.character(df[[j]][i])), "</td>")
    }, character(1))
    paste0("<tr>", paste(cells, collapse = ""), "</tr>")
  }, character(1))
  paste0('<div class="scroll"><table', if (!is.null(cls) || sortable) paste0(' class="', paste(c(cls, if (sortable) "sortable"), collapse = " "), '"'), "><thead><tr>", head, "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table></div>")
}

glance_table <- function(facts_all) {
  facts_all <- facts_all[order(facts_all$week), ]
  data.frame(
    Week = facts_all$week,
    `Projected chop before the week` = sprintf("%s, %s", facts_all$pregame_fav, pct0(facts_all$pregame_fav_chance)),
    Chopped = facts_all$chopped_team,
    Final = sprintf("%.1f", facts_all$chopped_final),
    `Next lowest` = sprintf("%s, %.1f", facts_all$next_team, facts_all$next_final),
    Gap = fmt_gap(facts_all$gap),
    `Chance before any game` = pct0(facts_all$chopped_pregame_chance),
    `Chance when the last games kicked off` = pct0(facts_all$chance_at_last_kickoff),
    `Top pick, share of game time` = pct0(facts_all$top_pick_share),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

auto_summary <- function(f) {
  sprintf(paste0(
    "%s was chopped with %.1f points, %s behind %s (%.1f). Before any game the tracker's favorite for the chop was %s (%s)%s; ",
    "the chopped team started at %s. When the last games kicked off it stood at %s. Its projected score fell from %.1f before the week to a low of %.1f (%s), ",
    "and its chance peaked at %s (%s) before the final games. The chopped team was the tracker's top pick for %s of game time."),
    f$chopped_team, f$chopped_final, fmt_gap(f$gap), f$next_team, f$next_final,
    f$pregame_fav, pct0(f$pregame_fav_chance), if (isTRUE(f$pregame_fav_was_chopped)) ", who was the team chopped" else "",
    pct0(f$chopped_pregame_chance), pct0(f$chance_at_last_kickoff),
    f$chopped_pregame_proj, f$chopped_low_proj, f$low_time, pct0(f$peak_chance_before_last), f$peak_time, pct0(f$top_pick_share))
}

# --- the simulation cross-check section ------------------------------------------------------------
# Reads the cached spot checks (analysis/simulation_spot_check.R) for every week that has one. Plain templated sentences, no model.
sim_section_html <- function(season, weeks_with_sim, weeks_in_figure) {
  if (!length(weeks_with_sim)) return("")
  sc <- read_sim_check(season, weeks_with_sim)
  s <- sim_check_summary(sc)
  gap <- sc$sim_prob - sc$approx_prob
  per <- sc |> dplyr::group_by(week, step) |>
    dplyr::summarise(a_team = team_name[which.max(approx_prob)], a_p = max(approx_prob), s_team = team_name[which.max(sim_prob)], s_p = max(sim_prob),
                     wall_utc = dplyr::first(wall_utc), .groups = "drop")
  differ <- per[per$a_team != per$s_team, ]
  big <- sc[which.max(abs(gap)), ]
  mid <- gap[sc$approx_prob >= 0.10 & sc$approx_prob <= 0.70]
  low <- gap[sc$approx_prob < 0.10]
  dir <- function(x, up = "higher", down = "lower") if (x >= 0) up else down

  p_agree <- sprintf("Across %d team-and-moment pairs the average gap is %.1f percentage points, and the two tabs name the same most likely chop at %d of the %d moments.",
                     s$n_points, s$mean_abs_gap_pp, s$top_pick_agree, s$n_moments)
  p_where <- sprintf("The largest gap was in week %d on %s, when %s stood at %s on the Approximate tab and %s on the Simulations tab.",
                     big$week, say_time(fmt_et(big$wall_utc)), big$team_name, pct0(big$approx_prob), pct0(big$sim_prob))
  p_band <- if (length(mid)) sprintf(
    "For teams the Approximate tab put between 10%% and 70%%, the Simulations tab averaged %.1f points %s%s.%s",
    abs(100 * mean(mid)), dir(mean(mid)),
    if (length(low)) sprintf(", and gave teams under 10%% %.1f points %s", abs(100 * mean(low)), dir(mean(low), "more", "less")) else "",
    if (length(mid) < 50) sprintf(" That comes from only %d team-moments in the middle band, so treat it as a lead and not a finding.", length(mid)) else "") else ""
  p_differ <- if (nrow(differ)) paste0(
    sprintf("At %d of the %d moments the two tabs had a different top pick. ", nrow(differ), s$n_moments),
    paste(sprintf("Week %d, %s: the Approximate tab's top pick was %s at %s and the Simulations tab's was %s at %s.", differ$week, say_time(fmt_et(differ$wall_utc)),
                  differ$a_team, pct0(differ$a_p), differ$s_team, pct0(differ$s_p)), collapse = " ")
  ) else "At every sampled moment the two tabs named the same top pick."

  paste0(
    "<h2>A cross-check: does the Simulations tab agree?</h2>",
    '<p class="note">Everything above uses the Approximate tab, because it can be recomputed every five minutes of a week. The Simulations tab works differently. ',
    "It plays out 5,000 whole weeks, draws each player's score from real, skewed outcomes, and re-picks every team's best lineup in each one, so a bench player who booms can step in. ",
    "That is too slow to run at every step, so the two tabs are compared at seven moments per week: before the first game, five evenly spaced moments, and the kickoff of the week's last games.</p>",
    '<dl class="facts">',
    sprintf("<div><dt>Average gap</dt><dd>%.1f points</dd></div>", s$mean_abs_gap_pp),
    sprintf("<div><dt>Biggest gap</dt><dd>%.1f points</dd></div>", s$max_abs_gap_pp),
    sprintf("<div><dt>Same top pick</dt><dd>%d of %d moments</dd></div>", s$top_pick_agree, s$n_moments),
    sprintf("<div><dt>Correlation</dt><dd>%.2f</dd></div>", s$cor),
    "</dl>",
    '<div class="story"><p>', h(p_agree), "</p><p>", h(p_where), "</p>", if (nzchar(p_band)) paste0("<p>", h(p_band), "</p>") else "", "<p>", h(p_differ), "</p></div>",
    '<figure><div class="plate fit" style="max-width:820px"><picture><source media="(min-width:760px)" srcset="img/sim_check_wide.png"><img src="img/sim_check.png" alt="One row per week. Left: the Simulations tab\'s chance of being chopped against the Approximate tab\'s for every team at every sampled moment. Right: the chopped team\'s chance under each tab at those moments."></picture></div>',
    "<figcaption><b>Approximate tab against Simulations tab", if (length(weeks_in_figure) > 1) sprintf(", weeks %d to %d", min(weeks_in_figure), max(weeks_in_figure)) else sprintf(", week %d", weeks_in_figure), ".</b> ",
    "Left: each dot is one team at one sampled moment (red: the chopped team); the diagonal is perfect agreement. ",
    "Right: the chopped team's chance under each tab at those moments. ",
    "Each Simulations value comes from 5,000 simulated weeks, good to about 0.7 points.</figcaption></figure>")
}

# Rebuild index.html from the files already on disk (facts.csv, narratives, csvs, figures, the cached Simulations cross-check), with no replay:
# used by collector/weekly_preview.R so a new preview shows up without redoing a week.
rebuild_recap_page <- function(season, out_dir = file.path("recap", season), window = 999L) {
  facts_all <- readr::read_csv(file.path(out_dir, "facts.csv"), show_col_types = FALSE) |> dplyr::arrange(week)
  weeks_all <- sort(unique(facts_all$week)); weeks_window <- tail(weeks_all, window)
  sim_weeks <- sort(weeks_all[file.exists(vapply(weeks_all, function(x) sim_check_path(season, x), character(1)))])
  sim_html <- if (length(sim_weeks)) sim_section_html(season, sim_weeks, tail(sim_weeks, window)) else ""
  write_recap_page(season, max(weeks_all), facts_all, NULL, weeks_window, weeks_all, file.path(out_dir, "index.html"), sim_html = sim_html)
  invisible(file.path(out_dir, "index.html"))
}

# --- the page ------------------------------------------------------------------------------------------
# Dark page, Sleeper colors, matching the figures. Images are referenced relatively (img/...), so the whole recap/{season}
# folder can be hosted anywhere static. A drop-down at the top switches between the weeks (each week is its own section; the
# others are hidden by a few lines of inline JavaScript, and #week-N in the address opens a given week). Two views, switched
# by the tabs under the title: "Weekly recap" (the picker + one week) and "Season overview" (#season: the table of chops, the
# multi-week figures, the Approximate-vs-Simulations check). Without JavaScript everything just shows.

# The label above a week's write-up, from what make_narrative() saved for that week
narrative_label_html <- function(kind, model) {
  if (is.null(kind) || identical(kind, "template")) {
    '<p class="ai"><b>Automatic summary.</b> Put together from the week\'s numbers by a script and not reviewed by a person.</p>'
  } else if (identical(kind, "draft")) {
    '<p class="ai"><b>Written by AI.</b> Drafted with Claude from the week\'s numbers. A script checked every number in it against the week\'s numbers. It can still contain mistakes.</p>'
  } else {
    paste0('<p class="ai"><b>Written by AI.</b> This recap was written automatically by ', h(model), ' from the week\'s numbers. A script checked every number in it against the week\'s numbers, and it has not been reviewed by a person.</p>')
  }
}

race_caption <- function(w) sprintf(paste0(
  "<figcaption><b>Week %d.</b> Top: each team's chance of being chopped, which adds up to 100%% at every moment. ",
  "Bottom: points scored so far, as the best lineup you could build from the points already scored. ",
  "Red is the chopped team; other colors reached at least a 20%% chance at some point; all other teams are faint white lines. ",
  "Time is the real clock, US Eastern, with the shaded stretches that had no games cut out. ",
  "Teams do not start at an even 1-in-n split: each starts from its pregame projection, so a team projected low begins higher.</figcaption>"), w)

# One chart per team per week (analysis/plot_team_elimination.R::plot_team_single), for the drop-down at the bottom of
# each week's recap. Also writes teamcharts_wNN.csv, the list the drop-down is built from (the chopped team first, then
# the rest from lowest actual final to highest). A chart is only redrawn when it is missing or older than the week's
# timeline file (or `redo`), so rebuilding the page is cheap and a finished week is drawn once.
build_team_week_charts <- function(season, weeks, out_dir, img_dir, redo_weeks = integer(0), log = message) {
  dir.create(img_dir, showWarnings = FALSE, recursive = TRUE)
  for (w in weeks) {
    tf <- teams_path(season, w)
    if (!file.exists(tf)) next
    tm <- readr::read_csv(tf, show_col_types = FALSE, col_select = c("roster_id", "team_name", "final_team_points", "chopped")) |>
      dplyr::distinct(roster_id, .keep_all = TRUE) |> dplyr::arrange(dplyr::desc(chopped), final_team_points)
    drawn <- 0L
    for (r in tm$roster_id) {
      f <- file.path(img_dir, sprintf("team_w%d_r%d.png", w, r))
      if (w %in% redo_weeks || !file.exists(f) || file.mtime(f) < file.mtime(tf)) {
        ok <- tryCatch({ plot_mobile_team(season, w, r, f); TRUE }, error = function(e) { log("team chart skipped (week %d, roster %s): %s", w, r, conditionMessage(e)); FALSE })
        if (ok) drawn <- drawn + 1L
      }
    }
    readr::write_csv(tm, file.path(out_dir, sprintf("teamcharts_w%02d.csv", w)))
    if (drawn) log("week %d: drew %d team charts", w, drawn)
  }
  invisible(NULL)
}

week_section_html <- function(w, out_dir, facts_all, hidden, team_names = character(0)) {
  prefix <- file.path(out_dir, sprintf("narrative_w%02d", w))
  kind_f <- paste0(prefix, ".kind")
  kind <- if (file.exists(kind_f)) readLines(kind_f, warn = FALSE, encoding = "UTF-8") else character(0)
  sec <- if (file.exists(paste0(prefix, ".txt"))) parse_narrative(apply_manager_key(paste(readLines(paste0(prefix, ".txt"), warn = FALSE, encoding = "UTF-8"), collapse = "\n"), team_names)) else NULL
  pf <- file.path(out_dir, sprintf("players_w%02d.csv", w))
  players <- if (file.exists(pf)) readr::read_csv(pf, show_col_types = FALSE, col_types = readr::cols(.default = readr::col_character())) else NULL
  table_html <- if (!is.null(players) && nrow(players)) paste0(
    '<p class="fine">The chopped team\'s biggest shortfalls against projection when its projected score bottomed out. Snaps are offensive snaps over the whole game, with the share of the team\'s snaps in brackets.</p>',
    html_table(stats::setNames(players, c("Player", "Team", "Projected", "Scored by then", "Final", "Offensive snaps")),
               num_cols = c("Projected", "Scored by then", "Final", "Offensive snaps"), cls = "small")) else ""
  figure_html <- sprintf('<h3>Live tracking the odds</h3><figure><div class="plate fit" style="max-width:700px"><img src="img/race_w%d.png" loading="lazy" alt="Week %d: top panel shows each team\'s chance of being chopped, with the chopped team in red; bottom panel shows points scored so far."></div>%s</figure>', w, w, race_caption(w))
  story <- function(x, links = NULL) if (length(x)) paste0('<div class="story">', narrative_html(x, week = w, links = links), "</div>") else ""

  # tables for the three groups of teams that were not chopped (saved themselves / close calls / massively outperformed)
  rd <- function(f) if (file.exists(f)) readr::read_csv(f, show_col_types = FALSE, col_types = readr::cols(.default = readr::col_character())) else NULL
  sv <- rd(file.path(out_dir, sprintf("saved_w%02d.csv", w))); sp <- rd(file.path(out_dir, sprintf("saved_players_w%02d.csv", w)))
  if (!is.null(sv) && !"category" %in% names(sv)) sv <- NULL     # written by an older version of this script
  if (!is.null(sp) && !"kind" %in% names(sp)) sp <- NULL
  num <- function(x, d = 1) sprintf(paste0("%.", d, "f"), as.numeric(x))
  mgr <- function(x) vapply(x, manager_label, character(1))
  group_html <- function(cat) {
    if (is.null(sv)) return("")
    g <- sv[sv$category == cat, ]
    if (!nrow(g)) return("")
    d <- function(...) data.frame(..., check.names = FALSE, stringsAsFactors = FALSE)
    above <- sprintf("%+.1f", as.numeric(g$beat)); chop <- fmt_gap(as.numeric(g$above_chop)); pre <- pct0(as.numeric(g$pre_prob))
    tt <- switch(cat,
      danger = d(Team = g$team_name, Manager = mgr(g$owner_name), `Chance of being chopped before the week` = pre, Projected = num(g$pre_proj), Finished = num(g$final), `Above projection` = above, `Above the chopped team` = chop),
      close = d(Team = g$team_name, Manager = mgr(g$owner_name), `Chance of being chopped before the week` = pre, `Highest chance` = sprintf("%s (%s)", pct0(as.numeric(g$peak)), g$peak_time),
                `Points scored at that point` = num(g$score_at_peak), Projected = num(g$pre_proj), Finished = num(g$final), `Above the chopped team` = chop),
      beat = d(Team = g$team_name, Manager = mgr(g$owner_name), `Chance of being chopped before the week` = pre, Projected = num(g$pre_proj), Finished = num(g$final), `Above projection` = above,
               `More than the typical team` = if (is.null(g$beat_vs_league)) "" else sprintf("%+.1f", as.numeric(g$beat_vs_league)), `Above the chopped team` = chop))
    if (cat %in% c("danger", "beat")) {   # saved themselves and massively outperformed: no team table (the written recap carries the team's results), one table of the players behind them
      x <- if (is.null(sp)) NULL else sp[sp$kind %in% c("above", "sub") & sp$team_name %in% g$team_name, ]
      if (is.null(x) || !nrow(x)) return("")
      x <- x[order(x$team_name, x$kind != "above"), ]
      return(paste0('<p class="fine">The players behind the score: those in the final scoring lineup who beat their projection by the most, and bench players that best ball moved into it. Points are against the score projected before the week.</p>',
                    html_table(d(Team = x$team_name, Player = x$player, `NFL team` = x$nfl_team, Projected = num(x$projected), Scored = num(x$scored),
                                 `Above projection` = sprintf("%+.1f", as.numeric(x$scored) - as.numeric(x$projected)),   # bench players too: scored minus projected
                                 Note = ifelse(x$kind == "sub", ifelse(is.na(x$note), "on the bench in the pregame lineup", x$note), "")),
                               num_cols = c("Projected", "Scored", "Above projection"), cls = "small")))
    }
    out <- paste0('<p class="fine">Points are against the score projected for the team before any game. Teams are not chopped.</p>',
                  html_table(tt, num_cols = intersect(c("Projected", "Finished", "Above projection", "More than the typical team", "Above the chopped team", "Points scored at that point"), names(tt))))
    out
  }
  # the figure that sums the week up: every team, projected against finished
  sl_img <- file.path(out_dir, "img", sprintf("proj_slope_w%d.png", w))
  fig <- function(src, alt, cap) sprintf('<figure><div class="plate" style="max-width:820px"><img src="%s" loading="lazy" alt="%s"></div><figcaption>%s</figcaption></figure>', src, alt, cap)
  slope_html <- if (file.exists(sl_img)) paste0("<h3>Every team: projected against finished</h3>", fig(sprintf("img/proj_slope_w%d.png", w),
      sprintf("Week %d: one line per team from its pregame projected score to its final score, with the team's name beside its final score.", w),
      sprintf(paste0("<b>Week %d at a glance.</b> Each line is one team, from its pregame projected score (left) to its final score (right); a climbing line beat its projection. ",
                     "Red: the chopped team. Teal: saved itself (began the week in danger and was not chopped). Orange: a close call (chance of being chopped reached 25%% or more during the week). ",
                     "Purple: massively outperformed its projection without being in danger. Blue: every other team. Definitions are under the headings below."), w))) else ""

  # weekly highlights: a row of small tiles (top score, top player, boom, bust, best and worst swap)
  hf <- file.path(out_dir, sprintf("highlights_w%02d.csv", w))
  highlights_html <- if (file.exists(hf)) { hd <- readr::read_csv(hf, show_col_types = FALSE, col_types = readr::cols(.default = readr::col_character()))
    if (nrow(hd)) paste0('<div class="hl">', paste0('<div class="tile"><div class="lab">', h(hd$label), '</div><div class="big">', h(hd$headline), '</div>',
                          ifelse(is.na(hd$detail) | !nzchar(hd$detail), "", paste0('<div class="sub">', h(hd$detail), "</div>")), "</div>", collapse = ""), "</div>") else "" } else ""

  # every team's week, at the very bottom: a drop-down that swaps the chart (built by build_team_week_charts())
  tcf <- file.path(out_dir, sprintf("teamcharts_w%02d.csv", w))
  tc_tbl <- if (file.exists(tcf)) readr::read_csv(tcf, show_col_types = FALSE, col_types = readr::cols(.default = readr::col_character())) else NULL
  if (!is.null(tc_tbl)) tc_tbl <- tc_tbl[file.exists(file.path(out_dir, "img", sprintf("team_w%d_r%s.png", w, tc_tbl$roster_id))), ]
  teamcharts_html <- if (is.null(tc_tbl) || !nrow(tc_tbl)) "" else {
    src <- sprintf("img/team_w%d_r%s.png", w, tc_tbl$roster_id)
    lab <- paste0(tc_tbl$team_name, ifelse(tc_tbl$chopped %in% c("TRUE", "true", "T"), " (chopped)", ""))
    paste0('<h3 id="wk', w, '-teams" class="wvh h-teams">Every team\'s projection through the week</h3>',
           '<p class="fine">Pick a team to see how its projected score moved through the week. White line: points scored so far. Colored line: projected final score with 50% and 90% ranges, green if the team finished above its pre-game projection and red if below. Dashed line: the actual final.</p>',
           '<figure class="tcpick"><label for="tc-w', w, '">Team</label> <select id="tc-w', w, '" onchange="var i=document.getElementById(\'tc-img-w', w, '\');i.src=this.value;i.alt=this.options[this.selectedIndex].text+\': projected and actual score through the week\'">',
           paste0('<option value="', src, '">', h(lab), "</option>", collapse = ""), "</select>",
           '<div class="plate fit" style="max-width:700px"><img id="tc-img-w', w, '" src="', src[1], '" alt="', h(lab[1]), ': projected and actual score through the week" loading="lazy"></div></figure>')
  }

  # order: label, the short version, the week at a glance (slope figure), the race figure, the chopped team and its table, then the three
  # groups of teams that were not chopped, each with its tables, then anything else (the comparison with earlier weeks)
  nms <- if (is.null(sec)) character(0) else names(sec)
  groups <- c(`Teams that saved themselves` = "danger", `Close calls` = "close", `Teams that massively outperformed their projections` = "beat")
  take <- function(n) if (n %in% nms) sec[n] else NULL
  # the "Waiver results" text sits above the waiver table at the end of the page; its closing line links to the waivers part of the Season overview
  wv_text <- if ("Waiver results" %in% nms) sub("season overview", '<a href="#season-waivers">season overview</a>', paste0('<div class="story">', paste(paste0("<p>", h(trimws(sec[["Waiver results"]][nzchar(trimws(sec[["Waiver results"]]))])), "</p>"), collapse = ""), "</div>"), fixed = TRUE) else ""
  wv_tail <- waiver_results_html(out_dir, w, wv_text)
  # "Up for bid this week": the chopped team's freed players, its text above its table, right after the waiver results
  ub_text <- if ("Up for bid this week" %in% nms) paste0('<div class="story">', paste(paste0("<p>", h(trimws(sec[["Up for bid this week"]][nzchar(trimws(sec[["Up for bid this week"]]))])), "</p>"), collapse = ""), "</div>") else ""
  ub_tail <- upforbid_html(out_dir, w, ub_text)
  # each Short version bullet links to the part of the page it summarises: the chopped team's section if the bullet names the chopped team,
  # else the section of the first group team it names, else the waivers if it talks about them, else the chopped team's section
  sec_of <- c(chop = "The team that got chopped", saved = "Teams that saved themselves", close = "Close calls", beat = "Teams that massively outperformed their projections")
  have <- c(sapply(sec_of, function(n) n %in% nms), waivers = nzchar(wv_tail), upforbid = nzchar(ub_tail), teams = nzchar(teamcharts_html),
            swap = "The team that got chopped" %in% nms && any(trimws(sec[["The team that got chopped"]]) == "### The swap that backfired"))
  sv_links <- NULL
  if ("Short version" %in% nms) {
    bl <- trimws(sec[["Short version"]]); bl <- bl[startsWith(bl, "- ")]
    ft <- facts_all[facts_all$week == w, ]; ct <- if (nrow(ft)) as.character(ft$chopped_team[1]) else NA_character_
    cat_of <- c(danger = "saved", close = "close", beat = "beat")
    link_text <- c(chop = "Read the full story", saved = "See the teams that saved themselves", close = "See the close calls", beat = "See who outperformed", waivers = "See the waiver results", swap = "See the swap")
    # the sections a bullet could link to, best first: the chopped team's section if it names the chopped team, then the section of each group
    # team it names (earliest mention first), then the waivers if it talks about them. Bullets are served in order and no two bullets get the
    # same section: a bullet takes its best section that no earlier bullet has taken, and gets no link if they are all taken.
    cands <- function(b) {
      out <- character(0)
      if (!is.na(ct) && grepl(ct, b, fixed = TRUE)) out <- "chop"
      if (grepl("swap", b, ignore.case = TRUE)) out <- c(out, "swap")
      if (!is.null(sv) && nrow(sv)) {
        pos <- vapply(sv$team_name, function(tn) { q <- regexpr(tn, b, fixed = TRUE)[1]; if (q > 0) as.numeric(q) else Inf }, numeric(1))
        tg_all <- unname(cat_of[sv$category])
        o <- order(pos); o <- o[is.finite(pos[o]) & !is.na(tg_all[o])]
        out <- c(out, tg_all[o])
      }
      if (grepl("waiver", b, ignore.case = TRUE)) out <- c(out, "waivers")
      out <- unique(out[vapply(out, function(x) isTRUE(have[[x]]), logical(1))])
      if (!length(out) && isTRUE(have[["chop"]])) out <- "chop"
      out
    }
    taken <- character(0)
    tg <- vapply(bl, function(b) { pick <- setdiff(cands(b), taken)[1]; if (!is.na(pick)) taken <<- c(taken, pick); pick }, character(1), USE.NAMES = FALSE)
    tg_ok <- !is.na(tg)
    if (length(tg)) sv_links <- list(`Short version` = data.frame(id = ifelse(tg_ok, sprintf("wk%d-%s", w, ifelse(tg %in% "swap", "chop-swap", tg)), NA_character_), text = ifelse(tg_ok, unname(link_text[tg]), NA_character_), stringsAsFactors = FALSE))
  }
  chip_text <- c(chop = "The chop", saved = "Dodged it", close = "Close calls", beat = "Smashed it", waivers = "Waiver recap", upforbid = "Up for bid", teams = "Team projections")
  jump_html <- if (is.null(sec) || !any(have)) "" else paste0('<nav class="jump" aria-label="Jump to a part of this week">',
    paste(sprintf('<a class="j-%s" href="#wk%d-%s">%s</a>', names(chip_text)[have[names(chip_text)]], w, names(chip_text)[have[names(chip_text)]], chip_text[have[names(chip_text)]]), collapse = ""), "</nav>")
  body <- if (is.null(sec)) {
    paste0('<p class="note">', h(auto_summary(facts_all[facts_all$week == w, ])), "</p>", highlights_html, slope_html, figure_html, table_html, group_html("danger"), group_html("close"), group_html("beat"), wv_tail, ub_tail, teamcharts_html)
  } else {
    used <- c("Short version", "The team that got chopped", names(groups))
    paste0(jump_html, story(take("Short version"), links = sv_links), highlights_html, slope_html, figure_html, story(take("The team that got chopped")), table_html,
           paste(vapply(names(groups), function(n) paste0(story(take(n)), if (n %in% nms) group_html(groups[[n]]) else ""), character(1)), collapse = ""),
           story(sec[setdiff(nms, c(used, "Waiver results", "Up for bid this week", "Compared with earlier weeks"))]), wv_tail, ub_tail, teamcharts_html)
  }
  sprintf('<section class="week" id="week-%d" data-week="%d"%s><h2>Week %d</h2>%s</section>', w, w, if (hidden) " hidden" else "", w, body)
}

# --- the page ------------------------------------------------------------------------------------------
write_recap_page <- function(season, latest_week, facts_all, players_latest = NULL, weeks_window, weeks_all, path, narrative = NULL, sim_html = "") {
  latest_week <- max(weeks_all)   # the page always opens on the newest week, whichever week this run happened to rebuild
  # the week preview (collector/weekly_preview.R), if there is one: a tab of its own, opened by default while it is newer than the latest recap
  pv_files <- list.files(dirname(path), pattern = "^preview_w[0-9]+\\.csv$")
  pv_week <- if (length(pv_files)) max(as.integer(sub("^preview_w0*([0-9]+)\\.csv$", "\\1", pv_files))) else NA_integer_
  has_preview <- is.finite(pv_week)
  default_view <- if (has_preview && pv_week > latest_week) "preview" else "week"
  css <- "
:root{--bg:#05091D;--card:#131B38;--strip:#1B2547;--rule:#25345D;--text:#F4F6FB;--muted:#C9D6EE;--dim:#98B3D6;--accent:#00FFF9;--chop:#FF6482;--saved:#00FFF9;--close:#FFA84C;--beat:#B58CFF;--blue:#5B9DFF;--green:#4CD787;--amber:#FFC857}
*{box-sizing:border-box}
body{margin:0;color:var(--text);font:16px/1.55 -apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;
background:radial-gradient(900px 420px at 8% -80px,rgba(0,255,249,.13),transparent 70%),radial-gradient(800px 420px at 95% -60px,rgba(91,157,255,.14),transparent 70%),var(--bg);background-attachment:fixed}
body::before{content:'';display:block;height:4px;background:linear-gradient(90deg,var(--accent),var(--blue) 55%,var(--chop))}
main{max-width:1100px;margin:0 auto;padding:28px 18px 64px}
h1{font-size:clamp(1.8rem,5vw,2.6rem);margin:.2em 0 .1em;color:var(--accent);background:linear-gradient(90deg,var(--accent),var(--blue));-webkit-background-clip:text;background-clip:text;-webkit-text-fill-color:transparent;width:fit-content;max-width:100%}
h2{margin:2em 0 .5em;font-size:1.35rem;padding-left:12px;border-left:4px solid var(--accent)}h3{font-size:1.05rem;margin:1.4em 0 .3em}
section.week>h2{border-left-color:var(--chop)}
.kicker{color:var(--accent);font-size:.8rem;letter-spacing:.08em;text-transform:uppercase;font-weight:600}
.note{color:var(--muted);max-width:70ch}.fine{color:var(--dim);font-size:.85rem;max-width:80ch}
.picker{display:flex;align-items:center;gap:10px;margin:18px 0 0;padding:12px 14px;border:1px solid var(--rule);border-left:4px solid var(--accent);border-radius:6px;background:var(--card);width:fit-content;max-width:100%}
.picker select:focus{outline:2px solid var(--accent);outline-offset:1px}
.picker label{color:var(--accent);font-size:.75rem;letter-spacing:.07em;text-transform:uppercase}
.picker select{background:var(--bg);color:var(--text);border:1px solid var(--blue);border-radius:4px;padding:6px 10px;font:inherit;font-weight:600;color-scheme:dark}
.scroll{overflow-x:auto;border:1px solid var(--rule);border-radius:6px;background:var(--card);margin:12px 0}
table{border-collapse:collapse;width:100%;min-width:720px;font-size:.9rem}th,td{padding:9px 12px;text-align:left;border-bottom:1px solid var(--rule)}
thead tr{background:var(--strip)}th{color:var(--accent);font-size:.72rem;letter-spacing:.05em;text-transform:uppercase;font-weight:650}tr:last-child td{border-bottom:0}
tbody tr:hover{background:rgba(91,157,255,.08)}td.chop{color:var(--chop);font-weight:650}
table.sortable th{cursor:pointer;user-select:none}table.sortable th::after{content:' \\2195';color:var(--dim);font-size:.8em}table.sortable th[aria-sort=ascending]::after{content:' \\25B2';color:var(--accent)}table.sortable th[aria-sort=descending]::after{content:' \\25BC';color:var(--accent)}
td.heat-red{color:var(--chop);font-weight:700;background:rgba(255,100,130,.16)}td.heat-orange{color:var(--close);font-weight:650;background:rgba(255,168,76,.14)}td.heat-amber{color:var(--amber);background:rgba(255,200,87,.10)}
.num{text-align:right;font-variant-numeric:tabular-nums}table.small{min-width:520px}
img{display:block;width:100%;height:auto;max-width:100%}.plate{border:1px solid var(--rule);border-radius:8px;overflow-x:auto;margin:12px 0;box-shadow:0 0 28px rgba(0,255,249,.07)}
picture{display:block}
.plate img{min-width:480px}
#view-week .plate img,#view-preview .plate img,.plate.fit img{min-width:0}
figure{margin:18px 0}figcaption{color:var(--dim);font-size:.85rem;line-height:1.5;max-width:86ch;margin-top:8px}figcaption b{color:var(--accent)}
.facts{display:grid;grid-template-columns:repeat(auto-fit,minmax(150px,1fr));gap:12px 22px;margin:12px 0;padding-top:12px;border-top:1px solid var(--rule);max-width:70ch}
.facts div{min-width:0}.facts dt{color:var(--dim);font-size:.7rem;letter-spacing:.07em;text-transform:uppercase;margin-bottom:2px}.facts dd{margin:0;font-weight:700;font-size:1.15rem;color:var(--accent)}.facts div:nth-child(2n) dd{color:var(--blue)}
.ai{border:1px solid var(--rule);border-left:4px solid var(--amber);border-radius:6px;background:var(--card);padding:8px 14px;margin:12px 0;color:var(--muted);font-size:.9rem;max-width:80ch}
.ai b{color:var(--text)}.story{max-width:72ch}.story h3{margin:1.4em 0 .3em;font-size:1.05rem;color:var(--text)}
.story h3[class]{font-size:1.15rem;padding-left:10px;border-left:4px solid currentColor}
.story h3.h-chop{color:var(--chop)}.story h3.h-saved{color:var(--saved)}.story h3.h-close{color:var(--close)}.story h3.h-beat{color:var(--beat)}.story p,.story li{color:var(--muted)}
.hl{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:10px;margin:16px 0;max-width:820px}   /* no wider than the figures */
@media (max-width:700px){.hl{grid-template-columns:1fr}}
.hl .tile{background:var(--card);border:1px solid var(--rule);border-top:3px solid var(--accent);border-radius:6px;padding:10px 12px}
.hl .tile:nth-child(2n){border-top-color:var(--blue)}.hl .tile:nth-child(3n){border-top-color:var(--close)}.hl .tile:nth-child(4n){border-top-color:var(--chop)}
.hl .lab{color:var(--dim);font-size:.7rem;letter-spacing:.07em;text-transform:uppercase}.hl .big{font-weight:650;margin:2px 0}.hl .sub{color:var(--muted);font-size:.85rem}
nav.tabs{display:flex;gap:4px;margin:22px 0 0;border-bottom:1px solid var(--rule)}
nav.tabs a{padding:11px 20px;color:var(--dim);text-decoration:none;font-weight:650;border-bottom:3px solid transparent;margin-bottom:-1px}
nav.tabs a:hover{color:var(--accent)}nav.tabs a[aria-current=page]{color:var(--accent);border-bottom-color:var(--accent);background:linear-gradient(180deg,transparent,rgba(0,255,249,.08))}
.view>h2:first-child{margin-top:1.4em;font-size:1.6rem}
.tag{display:inline-block;margin-right:9px;padding:1px 9px;border-radius:999px;font-size:.68rem;font-weight:700;letter-spacing:.07em;text-transform:uppercase;vertical-align:2px;border:1px solid}
.tag-players{color:var(--chop);border-color:var(--chop);background:rgba(255,100,130,.10)}.tag-teams{color:var(--accent);border-color:var(--accent);background:rgba(0,255,249,.09)}
.story h3 .tag+*{color:inherit}.story p.fine{margin:-.2em 0 .5em}
/* tables scroll sideways with the first column frozen (like the app's tables); on a phone the cells are tighter and do not wrap */
td:first-child,th:first-child{position:sticky;left:0;z-index:1;background:var(--card);box-shadow:2px 0 6px rgba(0,0,0,.45)}
thead th:first-child{background:var(--strip);z-index:2}
tbody tr:hover td:first-child{background:#1D2A55}
@media (max-width:700px){
main{padding:18px 12px 72px}
.scroll{margin:10px -4px}
table,table.small{min-width:0;font-size:.78rem}th,td{padding:5px 7px}th{font-size:.62rem;letter-spacing:.03em}
td:not(:first-child),th:not(:first-child){white-space:nowrap}
td:first-child,th:first-child{min-width:84px;max-width:128px}
.hl .big{font-size:.95rem}
}
/* back to the top: appears once the page has been scrolled */
#totop{position:fixed;right:14px;bottom:calc(16px + env(safe-area-inset-bottom));z-index:50;padding:9px 15px;border-radius:999px;border:1px solid var(--accent);background:var(--card);color:var(--accent);font:inherit;font-size:.85rem;font-weight:650;cursor:pointer;box-shadow:0 2px 10px rgba(0,0,0,.5);opacity:0;pointer-events:none;transition:opacity .2s}
#totop.show{opacity:1;pointer-events:auto}
section.preview>h2{border-left-color:var(--blue)}
.disclaimer{border-left-color:var(--chop)!important;max-width:78ch;font-size:.95rem}
nav#quicktabs{position:fixed;top:0;left:0;right:0;z-index:40;display:flex;gap:4px;justify-content:center;padding:calc(6px + env(safe-area-inset-top)) 10px 6px;background:var(--bg);border-bottom:1px solid var(--rule);box-shadow:0 2px 10px rgba(0,0,0,.5);transform:translateY(-110%);transition:transform .2s}
nav#quicktabs.show{transform:translateY(0)}
nav#quicktabs a{padding:5px 12px;border-radius:999px;border:1px solid transparent;color:var(--dim);text-decoration:none;font-size:.85rem;font-weight:650;white-space:nowrap}
nav#quicktabs a[aria-current=page]{color:var(--accent);border-color:var(--accent);background:rgba(0,255,249,.08)}
nav.pagenav{display:flex;flex-wrap:wrap;align-items:center;gap:8px 10px;margin:2.2em 0 .5em;padding-top:1em;border-top:1px solid var(--rule)}
nav.pagenav a{padding:7px 14px;border-radius:999px;border:1px solid var(--rule);background:var(--card);color:var(--accent);text-decoration:none;font-size:.9rem;font-weight:650}
nav.pagenav a.next{border-color:var(--accent);background:rgba(0,255,249,.08)}nav.pagenav span{color:var(--dim);font-size:.85rem}
.seeall{margin:.3em 0 1.6em!important}.seeall a{font-weight:650}
nav.jump{position:sticky;top:var(--stick,0px);transition:top .2s;z-index:20;display:flex;gap:6px;overflow-x:auto;padding:8px 0;margin:10px 0 4px;background:var(--bg);border-bottom:1px solid var(--rule);white-space:nowrap}
nav.jump a{flex:none;padding:5px 12px;border:1px solid var(--rule);border-radius:999px;background:var(--card);color:var(--accent);text-decoration:none;font-size:.85rem;font-weight:650}
nav.jump a::after{content:' \\2193';font-size:.9em;opacity:.85}
nav.jump a:hover{filter:brightness(1.25)}
nav.jump a.j-chop{color:var(--chop);border-color:var(--chop)}nav.jump a.j-saved{color:var(--saved);border-color:var(--saved)}nav.jump a.j-close{color:var(--close);border-color:var(--close)}
nav.jump a.j-beat{color:var(--beat);border-color:var(--beat)}nav.jump a.j-waivers{color:var(--blue);border-color:var(--blue)}nav.jump a.j-upforbid{color:var(--blue);border-color:var(--blue)}nav.jump a.j-teams{color:var(--green);border-color:var(--green)}
.tcpick label{color:var(--dim);font-size:.85rem}.tcpick select{font:inherit;font-size:.9rem;margin:4px 0 10px 4px;padding:5px 10px;border:1px solid var(--rule);border-radius:8px;background:var(--card);color:var(--text);max-width:100%}
.story h4.sublabel{scroll-margin-top:58px;margin:1.1em 0 .1em;font-size:.72rem;letter-spacing:.07em;text-transform:uppercase;color:var(--dim)}
#view-preview h4.sublabel{margin:1.6em 0 .2em;font-size:1.05rem;font-weight:700;letter-spacing:0;text-transform:none;color:var(--text)}
#view-season h4.sublabel{margin:1.1em 0 .1em;font-size:.72rem;letter-spacing:.07em;text-transform:uppercase;color:var(--dim)}
#view-season h4.sublabel.fighead{margin:1.6em 0 .2em;font-size:1.05rem;font-weight:700;letter-spacing:0;text-transform:none;color:var(--text)}
ul.wvlist{margin:.2em 0 .6em;max-width:72ch}ul.wvlist li{color:var(--muted);margin:.15em 0}
.story li a.more{display:inline-block;margin-left:.3em;font-weight:650;white-space:nowrap}
h2[id^='wk'],h3[id^='wk'],h2[id^='season-'],h3[id^='season-'],h3[id^='pv-'],h4[id^='pv-']{scroll-margin-top:58px}
h3.wvh{margin:1.6em 0 .4em;font-size:1.15rem;padding-left:10px;border-left:4px solid currentColor}
.story h3.h-pvteams{color:var(--chop)}.story h3.h-pvwaiver{color:var(--amber)}.story h3.h-pvinj{color:var(--close)}.story h3.h-pvbye{color:var(--beat)}h3.h-numbers{color:var(--blue)}
h3.h-teams{color:var(--green)}h3.h-waivers{color:var(--blue)}h3.h-money{color:var(--amber)}h3.h-budget{color:var(--blue)}h3.h-worth{color:var(--close)}h3.h-reading{color:var(--beat)}h3.h-habits{color:var(--saved)}h3.h-roi{color:var(--chop)}
nav.jump a.j-plain{color:var(--accent);border-color:var(--accent)}nav.jump a.j-money{color:var(--amber);border-color:var(--amber)}nav.jump a.j-budget{color:var(--blue);border-color:var(--blue)}
nav.jump a.j-worth{color:var(--close);border-color:var(--close)}nav.jump a.j-reading{color:var(--beat);border-color:var(--beat)}nav.jump a.j-habits{color:var(--saved);border-color:var(--saved)}nav.jump a.j-roi{color:var(--chop);border-color:var(--chop)}
.t2{display:block;color:#AEB6C8;font-size:.78rem;line-height:1.25;font-weight:400}
a{color:var(--accent)}[hidden]{display:none!important}"
  weeks_desc <- sort(weeks_all, decreasing = TRUE)
  options_html <- paste(sprintf('<option value="%d"%s>Week %d%s</option>', weeks_desc, ifelse(weeks_desc == latest_week, " selected", ""), weeks_desc,
                                ifelse(weeks_desc == max(weeks_all), " (latest)", "")), collapse = "")
  team_names <- unique(unlist(lapply(weeks_all, function(w) if (file.exists(teams_path(season, w))) readr::read_csv(teams_path(season, w), show_col_types = FALSE, col_select = "team_name")$team_name)))
  sections_html <- paste(vapply(weeks_desc, function(w) week_section_html(w, dirname(path), facts_all, hidden = (w != latest_week), team_names = team_names), character(1)), collapse = "")
  js <- "(function(){var sel=document.getElementById('weekpick');if(!sel)return;
var V={week:document.getElementById('view-week'),season:document.getElementById('view-season'),projections:document.getElementById('view-projections')},T={week:document.getElementById('tab-week'),season:document.getElementById('tab-season'),projections:document.getElementById('tab-projections')};
if(document.getElementById('view-preview')){V.preview=document.getElementById('view-preview');T.preview=document.getElementById('tab-preview');}
var Q={};for(var qk in V){Q[qk]=document.getElementById('qt-'+qk);}
var DEF=document.body.getAttribute('data-default')||'week';
function showWeek(w){var s=document.querySelectorAll('section.week');for(var i=0;i<s.length;i++){s[i].hidden=s[i].getAttribute('data-week')!==String(w);}}
function route(){var h=location.hash,v=(h.indexOf('#season')===0?'season':(h==='#projections'?'projections':((h==='#preview'||h.indexOf('#pv-')===0)&&V.preview?'preview':((h===''||h==='#')&&DEF==='preview'&&V.preview?'preview':'week'))));
for(var k in V){V[k].hidden=(k!==v);if(k===v)T[k].setAttribute('aria-current','page');else T[k].removeAttribute('aria-current');if(Q[k]){if(k===v)Q[k].setAttribute('aria-current','page');else Q[k].removeAttribute('aria-current');}}
if(h===''||h==='#'||h==='#preview'||h==='#season'||h==='#projections'||/^#week-\\d+$/.test(h))window.scrollTo(0,0);
if(v==='week'){var m=/^#week-(\\d+)$/.exec(h),m2=/^#wk(\\d+)-/.exec(h);if(m&&sel.querySelector('option[value=\"'+m[1]+'\"]'))sel.value=m[1];if(m2&&sel.querySelector('option[value=\"'+m2[1]+'\"]'))sel.value=m2[1];showWeek(sel.value);if(m2){var we=document.getElementById(h.slice(1));if(we)we.scrollIntoView();}}
if(v==='season'&&window.initWaiverPlot)window.initWaiverPlot();if(v==='season'&&window.initScorePlot)window.initScorePlot();if(v==='season'&&window.initBudgetPlot)window.initBudgetPlot();if(v==='season'&&window.initPoolPlot)window.initPoolPlot();if(v==='season'&&window.initSpendPlot)window.initSpendPlot();
if(h==='#season-waivers'){var wv=document.getElementById('waivers');if(wv)wv.scrollIntoView();}else if(h.indexOf('#season-')===0){var se=document.getElementById(h.slice(1));if(se)se.scrollIntoView();}
if(h.indexOf('#pv-')===0){var pe=document.getElementById(h.slice(1));if(pe)pe.scrollIntoView();}}
sel.addEventListener('change',function(){showWeek(sel.value);try{history.replaceState(null,'','#week-'+sel.value);}catch(e){}});
Array.prototype.forEach.call(document.querySelectorAll('#tab-week,a.tabweek'),function(a){a.addEventListener('click',function(e){e.preventDefault();location.hash='#week-'+sel.value;if(!/^#week-/.test(location.hash)||location.hash==='#week-'+sel.value)route();});});
window.addEventListener('hashchange',route);route();if(location.hash.indexOf('#season-')!==0&&!/^#wk\\d+-/.test(location.hash))window.scrollTo(0,0);
var qt=document.getElementById('quicktabs'),lastY=window.pageYOffset,hold=0;
function qshow(on){if(!qt)return;qt.classList.toggle('show',on);document.documentElement.style.setProperty('--stick',on?qt.offsetHeight+'px':'0px');}
if(qt){window.addEventListener('scroll',function(){var y=window.pageYOffset;if(Date.now()<hold){lastY=y;return;}if(y<260)qshow(false);else if(y<lastY-6)qshow(true);else if(y>lastY+6)qshow(false);lastY=y;},{passive:true});
document.addEventListener('click',function(e){var a=e.target.closest&&e.target.closest('nav.jump a');if(a){hold=Date.now()+900;qshow(false);}});}
var tt=document.getElementById('totop');if(tt){window.addEventListener('scroll',function(){tt.classList.toggle('show',window.pageYOffset>500);},{passive:true});tt.addEventListener('click',function(){window.scrollTo({top:0,behavior:'smooth'});});}
/* sortable tables: click a column header; cells carry data-v when the sort value differs from the text */
Array.prototype.forEach.call(document.querySelectorAll('table.sortable'),function(t){var ths=t.querySelectorAll('thead th'),body=t.tBodies[0],dir={};
function val(td){var v=td.getAttribute('data-v');if(v===null)v=td.textContent.trim();if(v==='-'||v==='')return null;var n=parseFloat(String(v).replace(/[$,]/g,''));return isNaN(n)?String(v).toLowerCase():n;}
Array.prototype.forEach.call(ths,function(th,i){th.addEventListener('click',function(){dir[i]=!dir[i];var rows=Array.prototype.slice.call(body.rows);
rows.sort(function(a,b){var x=val(a.cells[i]),y=val(b.cells[i]);if(x===null||y===null)return x===y?0:(x===null?1:-1);var c=(typeof x==='number'&&typeof y==='number')?x-y:String(x).localeCompare(String(y));return dir[i]?c:-c;});
rows.forEach(function(r){body.appendChild(r);});Array.prototype.forEach.call(ths,function(o){o.removeAttribute('aria-sort');});th.setAttribute('aria-sort',dir[i]?'ascending':'descending');});});});})();"
  wv_season <- season_waivers_html(dirname(path))
  sj <- list(list("j-plain", "The chops", "season-chops"), list("j-budget", "Scoreboard", "season-scoreboard"), list("j-money", "Waivers", "season-waivers"))
  sc_season <- season_scores_html(dirname(path))
  if (!nzchar(wv_season)) sj <- sj[1:2]
  if (nzchar(sc_season)) sj <- c(sj, list(list("j-roi", "Scores over time", "season-scores")))
  season_jump <- paste0('<nav class="jump" aria-label="Jump to a part of the season overview">', paste(vapply(sj, function(x) sprintf('<a class="%s" href="#%s">%s</a>', x[[1]], x[[3]], h(x[[2]])), character(1)), collapse = ""), "</nav>")
  # the pages, in order, for the quick tab bar and the "Next" links at the bottom of each page
  pg <- list(week = list(sprintf("#week-%d", max(weeks_all)), "Weekly recap", "Recap"), season = list("#season", "Season overview", "Season"),
             projections = list("#projections", "Projection performance", "Projections"))
  if (has_preview) pg <- c(list(preview = list("#preview", sprintf("Week %d preview", pv_week), "Preview")), pg)
  pg_cls <- function(k) if (k == "week") ' class="tabweek"' else ""
  quick_tabs <- paste0('<nav id="quicktabs" aria-label="Switch page">', paste(vapply(names(pg), function(k) sprintf('<a id="qt-%s" href="%s"%s>%s</a>', k, pg[[k]][[1]], pg_cls(k), h(pg[[k]][[3]])), character(1)), collapse = ""), "</nav>")
  page_nav <- function(cur) {
    ord <- names(pg); i <- match(cur, ord); nxt <- ord[if (i == length(ord)) 1 else i + 1]; others <- setdiff(ord, c(cur, nxt))
    lk <- function(k, txt) sprintf('<a href="%s"%s>%s</a>', pg[[k]][[1]], pg_cls(k), h(txt))
    paste0('<nav class="pagenav" aria-label="Other pages">', sub('<a ', '<a class="next" ', lk(nxt, paste0("Next: ", pg[[nxt]][[2]], " \u2192")), fixed = TRUE),
           if (length(others)) paste0("<span>or go to</span>", paste(vapply(others, function(k) lk(k, pg[[k]][[2]]), character(1)), collapse = "")), "</nav>")
  }
  k <- length(weeks_window)
  wk_label <- if (k > 1) (if (k == length(weeks_all)) sprintf("All %d weeks", k) else sprintf("The last %d weeks", k)) else sprintf("Week %d", weeks_window)
  html <- paste0(
    '<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
    '<meta name="robots" content="noindex,nofollow"><title>', h(APP_TITLE), ' Weekly</title><style>', css, "</style></head><body data-default=\"", default_view, "\"><main>",
    '<p class="kicker">', h(APP_TITLE), ' &middot; ', h(season), " season &middot; previews, recaps and season stats &middot; generated automatically</p>",
    "<h1>", h(APP_TITLE), " Weekly</h1>",
    '<p class="note">A preview of the coming week goes up on Wednesday morning, once the waivers have run: each team\'s projection and chance of being chopped before and after them, the players picked up and the byes. ',
    "After the games, the recap replays the week on its real kickoff times to show what the tracker's chance of being chopped said at every moment against who actually got chopped. ",
    "The Season overview has the scoreboard and the waiver analysis, and Projection performance checks how well the projections held up. ",
    "This page is generated by a script and has not been reviewed by a person. ",
    "The write-ups are written by AI (Claude) from each week's numbers, or assembled by a script from those numbers when no AI draft has been added yet. ",
    "A script checks every number in them against the week's numbers, but they can still contain mistakes.</p>",
    '<nav class="tabs" aria-label="Page sections">', if (has_preview) sprintf('<a id="tab-preview" href="#preview">Week %d preview</a>', pv_week) else "", '<a id="tab-week" href="#week-', max(weeks_all), '">Weekly recap</a><a id="tab-season" href="#season">Season overview</a><a id="tab-projections" href="#projections">Projection performance</a></nav>',
    '<div id="view-week" class="view">',
    '<p class="note" style="margin-top:1.2em">One week at a time: who got chopped, what the tracker said along the way, and the figure for that week. The season-wide figures and the cross-check against the Simulations tab are under Season overview.</p>',
    '<div class="picker"><label for="weekpick">Choose a week</label><select id="weekpick">', options_html, "</select></div>",
    sections_html,
    page_nav("week"),
    "</div>",
    if (has_preview) paste0('<div id="view-preview" class="view">', preview_section_html(dirname(path), pv_week), page_nav("preview"), "</div>") else "",
    '<div id="view-season" class="view">',
    "<h2>Season overview</h2>",
    '<p class="note">The whole season so far, across every week: each chop, where every team stands and what the waivers bought. How the projections held up is under the Projection performance tab.</p>',
    season_jump,
    '<h2 id="season-chops">The chops so far</h2>', html_table(glance_table(facts_all), num_cols = c("Week"), red_cols = "Chopped"),
    '<p class="fine">Projected chop before the week is the team with the highest chance of being chopped before any game. Gap is the points between the chopped team and the next lowest. Top pick is the share of game time during which the eventual chop had the highest chance of any team.</p>',
    '<h2 id="season-scoreboard">Season scoreboard</h2>', season_scoreboard_html(season, sort(weeks_all), dirname(path)),
    wv_season,
    sc_season,
    page_nav("season"),
    "</div>",
    '<div id="view-projections" class="view">',
    "<h2>Projection performance</h2>",
    '<p class="note">How the tracker\'s pregame projections and its chance of being chopped held up, week by week, and how the Approximate tab compares with the Simulations tab. These are about the tracker itself; the teams and managers are under Season overview.</p>',
    if (file.exists(file.path(dirname(path), "img", "chop_rank.png"))) paste0('<h2 id="proj-rank">Did the tracker see it coming?</h2>',
      '<p class="note">One panel per week: where the tracker ranked the team that was actually chopped, hour by hour, with 1 being its top pick for the chop. A line along the top means it saw the chop coming; a line that drops means the chop was a surprise. The label under each title is the share of game time the chopped team was its top pick.</p>',
      '<figure><div class="plate fit" style="max-width:700px"><img src="img/chop_rank.png" alt="One panel per week showing the tracker\'s rank for the team that was actually chopped over the week, 1 being its top pick."></div>',
      "<figcaption><b>Did the tracker see it coming?</b> Rank 1 is the tracker's top pick for the chop.</figcaption></figure>") else "",
    "<h2>The two lowest teams each week</h2>",
    '<p class="note">For each week, the chopped team and the one that came closest to it. The ranges narrow as games finish and the projection settles on the final score.</p>',
    '<figure><div class="plate fit" style="max-width:820px"><picture><source media="(min-width:760px)" srcset="img/lowest_two_wide.png"><img src="img/lowest_two.png" alt="The two lowest-scoring teams each week: points scored so far, projected final score with 50 and 90 percent ranges, and the actual final score."></picture></div>',
    "<figcaption><b>The two lowest finishers, ", tolower(wk_label), ".</b> White line: points so far. Colored line: projected final with 50% and 90% ranges (red: the chopped team). Dashed line: the actual final.</figcaption></figure>",
    sim_html,
    '<h2>How this was made</h2><p class="fine">Each week is rebuilt from nflverse play-by-play, whose time stamps give every game its real clock, together with Sleeper\'s archived pregame projections and the league\'s scoring rules. ',
    "At every five minutes while games were on, the tracker's own math is run on the state of the league at that moment. The model shown is the Approximate tab. ",
    "A few players each week score differently in the play-by-play than in Sleeper's feed. The live tracker uses Sleeper's numbers directly, so this only touches the replays.</p>",
    page_nav("projections"),
    "</div>",
    '<p class="fine" style="margin-top:2em">Updated ', format(Sys.time(), "%B %d, %Y"), ". Data: nflverse play-by-play and Sleeper projections.</p>",
    quick_tabs,
    '<button id="totop" type="button" aria-label="Back to the top">&#8593; Top</button>',
    "<script>", js, "</script></main></body></html>")
  writeLines(enc2utf8(html), path, useBytes = TRUE)
}
