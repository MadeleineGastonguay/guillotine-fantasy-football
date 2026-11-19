# --- Sleeper's own projections feed ---------------------------------------
# Public, undocumented endpoint that backs the projected-points numbers shown
# in the Sleeper app itself (sourced from RotoWire, per the `company` field).
# Returns a raw stat line per player using the exact same field names as
# this league's `scoring_settings` (rush_yd, rec_td, fum_lost, ...), so it
# can be re-scored directly with no id crosswalk or stat-id mapping needed --
# unlike the ESPN-based approach this replaces, this will line up with what
# you see in the Sleeper app (modulo this league's custom scoring weights,
# which Sleeper's own generic pts_half_ppr/pts_ppr/pts_std columns don't
# reflect but our re-scoring does).

# Note: unlike the rest of the Sleeper API this app uses, this endpoint
# lives at the API root, not under /v1 -- so it's called directly here
# rather than through sleeper_get()/SLEEPER_BASE.
#
# It has also been observed to occasionally return a malformed payload
# (far fewer entries than normal, all with empty stat blocks) -- transient
# on Sleeper's end. We retry a couple of times if the parsed result looks
# implausibly small rather than silently proceeding with empty projections
# for everyone.
library(httr2)

get_sleeper_projections <- function(season, week, max_attempts = 3) {
  url <- sprintf("https://api.sleeper.app/projections/nfl/%s/%s?season_type=regular", season, week)
  for (attempt in seq_len(max_attempts)) {
    resp <- request(url) |> req_user_agent("guillotine-tracker/0.1") |> req_perform()
    raw <- resp_body_json(resp, simplifyVector = FALSE)

    out <- list()
    for (e in raw) {
      pid <- e$player_id
      if (is.null(pid) || is.null(e$stats) || length(e$stats) == 0) next
      out[[pid]] <- setNames(as.numeric(unlist(e$stats)), names(e$stats))
    }

    if (length(out) > 1000) return(out)
    message(sprintf(
      "Sleeper projections payload looked truncated (%d usable of %d entries), retrying (%d/%d)...",
      length(out), length(raw), attempt, max_attempts
    ))
    Sys.sleep(1)
  }
  out
}
