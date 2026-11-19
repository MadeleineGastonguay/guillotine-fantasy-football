# Guillotine Fantasy Football: how it works

Shiny app that estimates, in real time, each team's probability of finishing
last this week (and therefore being eliminated) in a Sleeper best-ball
guillotine league of any size.

## How it works

- **Actual points so far**: pulled directly from Sleeper's matchups endpoint.
  Sleeper already computes each player's fantasy points live during games
  using this league's own `scoring_settings` — so there's no need to
  recompute live stats ourselves, and no risk of the "current score" being
  wrong.
- **Rest-of-week projections**: from Sleeper's own (undocumented) projections
  feed (`R/sleeper_projections.R`), sourced from RotoWire — the same feed
  that backs the projected-points numbers you see in the Sleeper app. It
  returns a raw stat line per player (rush/rec yards, TDs, receptions,
  fumbles lost, 2-point conversions, etc.) using the exact same field names
  as this league's `scoring_settings`, so we re-score it directly under this
  league's actual rules (`R/stat_scoring.R`) with no id crosswalk needed.
  This replaced an earlier ESPN-based version whose projections didn't match
  what the app shows, because ESPN's projection model is a different source
  entirely.
- **Live in-game updates**: a player's remaining-points estimate isn't just
  their frozen pregame projection decaying on a clock — it's a blend of the
  pregame projection and the pace they've actually shown so far this game,
  with more weight going to the observed pace as the game progresses
  (`R/pipeline.R`, see `blended_rate`). So a player running hot relative to
  their projection gets a bumped-up rest-of-game estimate, and one having a
  quiet game gets scaled down, rather than the estimate just ignoring how
  the game is actually going.
- **Players listed Out**: every time the app pulls, any rostered player listed Out -- by Sleeper's
  `injury_status` or by ESPN's injury list for games that are live right now -- gets a rest-of-week
  projection and spread of 0 (`zero_out_players()` in `R/pipeline.R`); points already scored stay.
  Sleeper's player file (where its status lives) is only refreshed daily, so mid-game news comes
  from ESPN's per-game summary (`espn_live_injuries()` in `R/espn_api.R`, live games only, matched
  to Sleeper players by name + team). For a player ESPN lists in a live game, ESPN's current status is the one shown (better or worse
  than Sleeper's); players ESPN doesn't list keep Sleeper's daily status, because ESPN's list is not
  complete. Only Out changes the projection; every other status is display-only.
- **Game clock / status**: the one thing still pulled from ESPN
  (`R/espn_api.R`) is which NFL games are upcoming, live, or final, and,
  while live, how much game time is left — Sleeper doesn't expose this at
  that granularity. This is a team-level lookup only; no per-player ESPN id
  matching is needed anymore.
- **Best ball lineup**: Sleeper best-ball leagues auto-select the
  highest-scoring valid lineup from the *entire* roster each week (not just
  whatever is nominally listed as "starters" before the week is over). So
  the simulation optimizes across all rostered skill players, not a fixed
  starting 9, using an exact ILP solver (`R/bestball.R`, via `lpSolve`) for
  the QB/RB/RB/WR/WR/TE/FLEX/FLEX/SUPER_FLEX slot structure. The same
  solver also drives the Team Detail tab's starters/bench ordering (see
  below).
- **Monte Carlo simulation**: for each of `N_SIMULATIONS` draws, every
  player's still-to-be-determined points are sampled as
  `locked_actual + Normal(remaining_mean, remaining_sd)`, the optimal lineup
  is solved per team, and whichever team has the lowest total in
  that draw is tallied as "eliminated." Elimination probability = fraction
  of draws where a team came in last. Run four times per refresh, once per
  variance model (see below), so the app can show all four side by side.
- **Live games panel**: every rostered player league-wide whose NFL game is
  currently in progress, with points so far, projected remainder, and
  projected final — a cross-team view for watching live swings play out,
  separate from the per-team breakdown.
- **Team detail, starters-then-bench**: the per-team tab lists players in
  Sleeper's own display order — the optimal 9 (QB, RB, RB, WR, WR, TE, FLEX,
  FLEX, SUPER_FLEX, in that slot order) based on each player's current
  expected total (points so far + projected remaining), then the bench below.
  This uses `optimal_lineup_assignment()` in `R/bestball.R`, which is the
  same exact-ILP solver as the simulation, just run once on the current
  numbers instead of thousands of times on simulated ones.
- **Refresh only does real work when a game is live**: the UI still ticks
  every `REFRESH_SECONDS`, but a lightweight ESPN scoreboard check
  (`any_game_live()` in `R/espn_api.R`) gates whether it actually re-pulls
  data and re-runs the (four) Monte Carlo passes, or just updates a "no
  games live" timestamp. Nothing meaningful can change between refreshes
  when no NFL game is in progress, so there's no reason to pay for the
  compute.

## Four variance models, run side by side

The app computes elimination probability four different ways every refresh
— `remaining_sd_backtest`, `remaining_sd_underdog`, `remaining_sd_playerprofiler`,
and `remaining_sd_player` in `R/pipeline.R` — shown as separate columns in
the Standings tab. All four share the same `remaining_mean` (Sleeper's own
pregame projection, live-blended); only the spread differs. This section
explains where each comes from, and why they disagree.

> **Which are switched on.** Only Model 2 (Underdog's CV) is currently active in the app (see `VARIANCE_MODEL_NAMES` in `R/config.R`); the other three are kept in the code so they can be compared. The two models built from a backtest, Models 1 and 4, are **experimental**: we have no good way of judging how realistic their spreads are, so treat them as work in progress.

### Model 1: backtested against this league's own history (experimental)

*Experimental: not switched on, and we have no reliable way to evaluate how realistic these spreads are.*

`remaining_sd` comes from a real backtest
(`analysis/backtest_projection_error.R` + `analysis/backtest_topn.R`), not a
guess:

1. Pulled 3 real NFL seasons (2023-2025) of actual-vs-pregame-projected stat
   lines from Sleeper's own public API (confirmed genuine historical data,
   not synthetic — cross-checked real players/teams/scores against known
   2025 results).
2. Re-scored both under this league's own `scoring_settings` and computed
   the residual (`actual - projected`) per player-week.
3. Restricted to each season's top-N players by position (top-12 QB, top-24
   RB, top-36 WR, top-12 TE — i.e. clear starters), matching the methodology
   published fantasy-analytics sources use, so committee/streamer-caliber
   players don't inflate the estimate for a locked-in starter.
4. Fit `SD(residual) = a + b * projected_pts` per position (an intercept,
   not just a proportional coefficient — real variance doesn't shrink to 0
   as the projection shrinks to 0; results in `R/config.R`).

This was cross-checked against two independent published estimates —
[Underdog Network](https://underdognetwork.com/football/best-ball-research/weekly-variance-by-position-a-key-to-best-ball)
(CV: QB 0.36, RB 0.54, WR 0.58, TE 0.63) and
[PlayerProfiler](https://www.playerprofiler.com/article/the-player-variance-manifesto/)
(CV roughly QB 0.40, RB 0.5-0.6, WR 0.6-0.7, TE 0.7-0.8) — and landed close
to both (implied CV at the mean: QB 0.43, RB 0.60, WR 0.68, TE 0.74).

### Model 2: Underdog Network's published CV, taken as-is

`remaining_sd_underdog` skips the backtest entirely and just applies the
Underdog Network figures above directly — a flat coefficient of variation
per position (`SD = CV × projected_pts`, no intercept). It's the simpler,
less-grounded-in-this-league's-specifics option: it doesn't know this
league's scoring rules shift variance at all (it's the same CV regardless of
scoring format), and without an intercept it wrongly implies a player
projected near 0 has ~0 variance. It's included mainly as a second, fully
independent reference point next to the backtest — where they agree, that's
reassuring; where they diverge (mostly at the low end, where the missing
intercept matters most), that's informative too.

### Model 3: PlayerProfiler's published CV, taken as-is

`remaining_sd_playerprofiler` is the same flat-CV structure as Underdog
(no intercept), but with
[PlayerProfiler's](https://www.playerprofiler.com/article/the-player-variance-manifesto/)
numbers instead: QB 0.40, RB 0.55, WR 0.65, TE 0.75. These are *less
precise* than Underdog's — PlayerProfiler states them as where a position's
curve visually "peaks" on a density chart ("RBs peak at about 0.5-0.6", "TEs
at about 0.7-0.8"), not a single computed average, so the values here are
just the midpoint of each stated range. Included because it's a second
independent published source with a meaningfully different (higher, at
every position) estimate than Underdog's — useful as a sensitivity check on
how much the choice of "which article's numbers" actually matters.

### Model 4: player-specific, shrunk toward the backtest (experimental)

*Experimental: not switched on, and we have no reliable way to evaluate how realistic these spreads are.*

`remaining_sd_player` starts from the backtest model (Model 1) and adjusts
it per player based on that player's own history, via
`analysis/backtest_player_variance.R` — no new data pull, it reuses the
same 3-season residual cache Model 1 was fit from. For each player: compute
their own residual SD across every game in the cache, compare it to what
the position-level model would have predicted for them (their historical
average projection level), and shrink that ratio toward 1.0 (i.e. "behaves
like an average position player") based on how many games of history they
have. Someone with no history in the cache (rookies, mainly) gets a ratio
of exactly 1.0 — a clean fallback to the plain position-level model, so
per-player granularity only shows up where it's genuinely earned by real
history; otherwise you get the same number as everyone else at that
position.

The shrinkage *strength* differs by position, and that difference is itself
backed by data, not assumed: before trusting any player's own variance as
predictive, we checked whether it's even a stable trait — correlate each
player's SD in one season against their SD the next (2023-2025, players
with ≥6 games in both seasons). Result: RB 0.31, WR 0.36, TE 0.42, but
**QB 0.04** — indistinguishable from a shuffled/null baseline (~0 ± 0.05).
A QB's own variance last year barely predicts their variance this year, so
QBs get heavy shrinkage (in practice, they end up very close to the
position-level model regardless of how many career games they have) while
RB/WR/TE get more moderate shrinkage. This fell out of the data rather than
being a rule written in by hand.

### Live responsiveness: SD is evaluated at the current pace, not the pregame number

All four `sd_full_game_*` functions (`R/pipeline.R`) take `blended_rate` as
their input, not the frozen pregame `full_week_projection`. `blended_rate`
is the same live-updated "if this pace continued for a full game" estimate
that already drives `remaining_mean` (see Live in-game updates, above) — so
a player who's run hot or cold relative to their pregame number gets a
correspondingly wider or narrower band, not just a smaller one as the clock
runs down. Earlier versions of this app evaluated SD at the frozen pregame
projection regardless of what was actually happening in the game, which
was a real gap: the mean would react to a hot start but the SD wouldn't,
understating uncertainty for anyone running well above (or below) their
projection.

One thing this deliberately does *not* do: mix `locked_points` (already
realized, zero variance by definition) into the quantity fed to the SD
formula. An earlier draft of this fix tried `locked_points + remaining_mean`
as the input, which is wrong — the variance of a constant plus a random
variable is just the variance of the random variable, so folding in the
realized part added nothing but a category error. `blended_rate` avoids
this because it's a *rate* (comparable in kind to the pregame projection,
not to a running point total), so it can be substituted for
`full_week_projection` in the exact same formulas without changing what
those formulas mean.

### Why `√remaining_fraction` for the in-game decay, specifically

`remaining_sd_X = sd_full_game_X(blended_rate) × sqrt(remaining_fraction)`
— SD decays more slowly than `remaining_mean` does (which decays linearly
with `remaining_fraction`), so a game that's half over still carries most
of its full-game uncertainty, not half of it.

This has a real (if imperfect) justification: if scoring arises from a
roughly time-homogeneous process — production accumulating at a fairly
constant rate across the game, whether pictured as many small continuous
gains or as a Poisson-like process of discrete scoring events (closer to
how NFL football, which is TD-driven and lumpy, actually works) — then
`Var(remaining) ∝ remaining_fraction`, and taking a square root gives
`SD(remaining) ∝ √remaining_fraction`. That conclusion holds under both the
"smooth" and "lumpy" pictures of how points accrue, which is a more robust
argument than it might first look.

Its weak point is the time-homogeneity assumption itself: real NFL scoring
isn't uniform across a game — game script (trailing teams pass more,
leading teams run the clock), garbage time, and red-zone clustering all
violate it. There's also no intra-game backtest data validating this
specific shape for this app (that would need quarter-by-quarter historical
box scores, which is a meaningfully bigger data-gathering lift than the
full-game backtest already done). Weighed against the simpler alternative
(`SD ∝ remaining_fraction`, i.e. holding CV constant through the game,
which was seriously considered before landing here) — neither is
empirically validated, but `√remaining_fraction` has a real theoretical
basis, and was the deliberate choice, made with that trade-off in mind
rather than left as an unexamined default.

**A note for whoever picks this up next:** a real coverage check against
2026 weeks 1-3 (`analysis/plot_accuracy_summary.R`) found this decay and
the mean's own pregame/live blend (`blend_weight = elapsed_fraction^3`,
below) drift out of sync — `blend_weight` stays near 0 for most of a game
(the mean barely moves off its pregame number even for a player clearly
running hot or cold), while `√remaining_fraction` shrinks from kickoff
regardless. The predicted 50% interval covered only ~25-35% of actual
outcomes through most of the game, then swung to ~80-95% (too wide) in the
last ~10-15 minutes. A patch tying the SD decay to `blend_weight` instead
of `remaining_fraction` measurably fixed this on that data, but was
reverted rather than kept: the real fix is a first-principles model (a
Bayesian rate estimate updated from the observed pace, combined with the
`√remaining_fraction` compounding-uncertainty argument via the law of total
variance) that derives *both* the mean-blend weight and the SD decay
together, rather than patching one to match the other's ad hoc curve. That
derivation is the next thing planned for this section.

### What about Sleeper's own in-app probabilities?

**Deliberately not used as a fifth model, and not what any of the models
above were tuned to match.** Working a real example from this league (RJ Harvey, 8.16-point
pregame projection, only unresolved player on an otherwise-final roster,
Sleeper showing 52%/1% win-probability splits against a fixed 72.56-point
opponent) and solving for the implied SD under a Normal model gives
`σ ≈ 2.39`, i.e. an implied CV of ~0.29 — tighter than *every* source above,
including the "elite starters only" published figures. That's a real,
interesting data point, but with only one anchor point and no way
to inspect Sleeper's actual method, the more defensible choice was to trust
the backtest (13,200 real player-weeks, corroborated by two independent
outside sources) over a single anecdote from an opaque system. If you
collect more such anchors over time and they consistently point the same
direction, that'd be a reason to revisit this.

### Why not `ffanalytics`?

The [`ffanalytics`](https://github.com/FantasyFootballAnalytics/ffanalytics)
R package has real uncertainty functions (`points_sd()`,
`confidence_interval()`), but they measure something different: *spread
across analyst projection sources* (ESPN, CBS, NFL.com, FFToday, etc. all
projecting the same player-week) — i.e. how much experts currently disagree
— not *historical outcome variance* (how much the actual result tends to
differ from a projection). Those are genuinely different questions, and
inter-analyst disagreement isn't a good stand-in for the outcome variance
this app's simulation needs. It also isn't on CRAN and depends on live-
scraping several third-party sites, which is a fragile dependency to add to
something meant to run unattended during live games. Not integrated.

One caveat on the backtest itself: ranking "top-N" by that season's actual
output has a built-in selection bias (residual means run positive — the
top performers *by definition* tended to beat their preseason expectations
that year). That's fine for the variance estimate this section is used for,
but it's why `remaining_mean` still comes from Sleeper's own unscaled
pregame projection rather than from this backtest — the full, unrestricted
backtest population (not shown here) had close-to-zero mean bias, so there's
no evidence pregame projections are systematically off; only the top-N slice
used for variance is affected, and only because of how "top-N" is selected.

To reproduce or extend the backtest:
```bash
Rscript analysis/backtest_projection_error.R  # pulls 3 seasons, fits full-population model
Rscript analysis/backtest_topn.R              # restricts to top-N starters, fits final model
Rscript analysis/backtest_player_variance.R   # derives per-player shrunk ratios (Model 4)
```
Each week/season pull is cached under `cache/backtest/`, so re-running after
the first time is fast. The third script needs `residuals_all.rds` from the
first to already exist, and writes `cache/player_variance_lookup.rds`,
which is what the running app actually reads (it does not recompute this
live).

## Other known simplifications (v1)

- **Live blending** (the mean, `blended_rate`) uses a simple linear
  shrinkage between the pregame rate and the observed in-game rate,
  weighted by elapsed game fraction. It's a reasonable heuristic, not a
  fitted model — a player who scores a long TD on the first play will look
  like they're on an outsized full-game pace until more of the game has
  played out and the blend weight shifts toward that signal. (The variance
  side has its own, separately-documented set of assumptions — see "Live
  responsiveness" and "Why `√remaining_fraction`" above.)
- No DEF/K handling — this league's `roster_positions` has no DEF or K slot,
  so it was intentionally left out.
- **`USE_PARALLEL_SIMULATIONS`** (`R/config.R`) defaults to `FALSE`. It was
  briefly `TRUE` (running the four models via `parallel::mclapply`, ~4s
  instead of ~15s locally), but forking multiplies memory pressure and
  caused OOM crashes when deployed to shinyapps.io, whose instances are far
  more memory-constrained than a local dev machine. Sequential is the safe
  default; flip it on only for local use if you want the speed and have RAM
  to spare. Related: `build_live_snapshot()` explicitly `rm()`s and
  `gc()`s Sleeper's ~100MB full player dictionary as soon as this league's
  ~270 relevant players have been extracted from it, rather than leaving it
  to a lazy GC trigger -- worth doing regardless of the parallelism
  setting, since a container can get OOM-killed before R's automatic GC
  threshold fires.
- The UI recomputes synchronously on each refresh (running *four* full
  Monte Carlo passes now, one per variance model), so the page will appear
  to pause briefly during a refresh rather than updating incrementally. This
  only happens when a game is actually live, though — see the refresh-gating
  bullet above.
- Sleeper's projections endpoint has occasionally been observed to return a
  malformed/truncated payload; `get_sleeper_projections()` retries a couple
  of times if the parsed result looks implausibly small, but a persistent
  outage there would fall back to empty projections (pregame estimate = 0)
  rather than crashing the app.
- The player-specific model (Model 4) reflects last year's role: a player
  who changed teams, got a new play-caller, or is coming off injury may not
  behave like their historical variance suggests. It's grounded in real
  year-over-year evidence (the correlations above), but that evidence is
  still just moderate, not proof of stability for any individual player.

## Possible future analyses

Ideas worth running when there is time. None of these is built.

- **How much does the bench matter in the Approximate tab?** The Approximate
  tab (`run_elimination_instant()`) picks each team's lineup once, from the
  mean projections, and adds up only the starters' variances. In best
  ball a bench player who has a big game moves into the lineup, which adds
  upside the Approximate tab ignores. The Simulations tab re-picks the best
  lineup in every draw, so it counts the bench. The 2026 weeks 1-3
  cross-check (`analysis/simulation_spot_check.R`, `analysis/plot_sim_check.R`)
  found the two tabs agree to within about 0.9 percentage points on average
  (13.2 at most), but it cannot isolate the bench effect, because the tabs
  also differ in outcome shapes (normal curves vs. real skewed residuals).
  To separate them, run the simulation twice (normal shapes with a flexible
  bench, and skewed shapes with a fixed lineup) and compare each with the
  Approximate tab on every replayed week.
- **Add skew to the Approximate tab.** The Approximate tab treats every player,
  and so every team total, as a bell curve. Real weekly scoring has a long
  upper tail (per-player skew is about 1.2-1.4 for RB/WR/TE, per the note in
  `R/simulation.R`), and low-projection players are where it matters most: a
  TE projected for 4.4 who scores 14.4 (2026 week 2, one team) is about
  3.7 SDs out on a bell curve. The Simulations tab already uses the real skewed
  shapes (`get_position_shape_pool()`). Two ways to get most of that without
  the full simulation:
  1. Keep the closed form but skew the curves: measure each position's skew
     from the shape pool, add the starters' means, variances and third
     cumulants (they add for independent players) to get a skewed curve for the
     team total (e.g. skew-normal or Cornish-Fisher), and run the same
     "who is lowest" integral on those.
  2. A light simulation with each team's lineup held fixed. Much faster than
     the full one, which is slow because it re-picks the lineup in every draw.

  Not built or tested. Check either against the Simulations tab at the 21
  moments in the 2026 weeks 1-3 cross-check
  (`analysis/simulation_spot_check.R`); keep it only if it agrees better than
  the current Approximate tab does. It keeps the fixed-lineup assumption.

## Running it

```r
shiny::runApp("guillotine_app")
```

or, from this repo's working directory:

```bash
Rscript -e "shiny::runApp('guillotine_app', host='127.0.0.1', port=7654)"
```

Required packages: `shiny`, `DT`, `httr2`, `jsonlite`, `dplyr`, `purrr`,
`tidyr`, `tibble`, `scales`, `lpSolve`, `parallel` (base R, no install needed).

### Two-stage refresh, parallel simulation

A refresh happens in two stages, each its own reactive flush, so the UI
doesn't sit blank while waiting for the slow part:

1. **Pipeline** (`fetch_pipeline()`, ~1-2s): pulls Sleeper + ESPN data and
   updates `snapshot_data()`. Team Detail and Live Games only depend on
   this, so they render as soon as this stage finishes.
2. **Simulation** (`run_simulations()`, ~4s): triggered automatically by an
   observer watching `snapshot_data()`, runs all four variance models'
   Monte Carlo passes and updates `sim_results()`, which Standings depends
   on.

The four models are independent, so stage 2 runs them via
`parallel::mclapply()` instead of one after another -- about a 3-4x
speedup locally (14.8s sequential -> 4.2s parallel at `N_SIMULATIONS = 500`
across 4 models on an 11-core machine).

**Does this parallelism survive deployment?** `mclapply()` forks the R
process, which only works on Unix-like systems (Linux, macOS) -- on Windows
it silently falls back to running sequentially (`mc.cores` is ignored, no
error, just no speedup). Most Shiny hosting (shinyapps.io, Posit Connect,
a self-hosted Shiny Server) runs on Linux, so forking itself isn't the
issue. The real question is **how many CPU cores your hosting plan actually
gives the app** — forking across cores that don't exist doesn't help.
shinyapps.io's free/starter tiers are commonly limited to a single core per
app instance, in which case this would run at roughly sequential speed
despite the code being "parallel"; higher tiers (and most self-hosted or
Connect deployments) allocate more, where you'd see a real speedup. If you
tell me where you're planning to deploy, I can check that platform's
specific CPU allocation.

The app has three tabs: **Standings** (elimination probability under all
four variance models), **Team detail** (starters-then-bench breakdown for
one team), and **Live games** (every rostered player league-wide whose NFL
game is currently in progress). Team detail and Live games both show each
unresolved player's projected remaining points with a `± SD` column per
variance model, so you can see not just the point estimate but how much
each model thinks that specific player's rest-of-game could swing.

## Config

See `R/config.R`:
- `LEAGUE_ID` — your Sleeper league id.
- `REFRESH_SECONDS` — how often the auto-refresh timer ticks (default 60s);
  it only does the expensive recompute when a game is actually live (see
  `any_game_live()` in `R/espn_api.R`).
- `N_SIMULATIONS` — Monte Carlo draws per refresh, per model (more is slower
  but less noisy — each solve of the full league is ~7ms, so N sims × 18
  teams × 7ms × 4 models is roughly the wall-clock cost).
- `PROJECTION_ERROR_MODEL` — the backtested variance model (Model 1), as
  `{a, b}` per position for `SD = a + b * projected_pts`.
- `UNDERDOG_CV` / `PLAYERPROFILER_CV` — the two published flat-CV variance
  models (Models 2 and 3), as per-position coefficients.
- `PLAYER_VARIANCE_LOOKUP_FILE` — where Model 4's precomputed per-player
  ratios live (`cache/player_variance_lookup.rds`); regenerate with
  `analysis/backtest_player_variance.R`.

## Cache

- `cache/sleeper_players.rds` holds Sleeper's full player dictionary
  (~15MB), refreshed at most once every 24h since Sleeper asks that endpoint
  not be hit often. Delete the file to force a refresh sooner.
- `cache/backtest/` holds the raw season/week pulls and fitted results from
  the backtest scripts in `analysis/` (residuals, top-N fits). Only read by
  the `analysis/` scripts themselves, not needed at runtime.
- `cache/player_variance_lookup.rds` is Model 4's precomputed per-player
  ratios (written by `analysis/backtest_player_variance.R`, consumed by
  `R/pipeline.R` at runtime).

## Running the app for a second Sleeper league
One codebase, two deploys. The per-league values live in `league_config.R` (league ID, `APP_TITLE`, `SHOW_RECAP_LINK`, `N_CHOPS_PER_WEEK`, optional `N_SIMULATIONS`); `R/config.R` loads it and holds this league's values as defaults, so `collector/` and `analysis/` are unaffected. The starting lineup is read from the league's `roster_positions` at app start (`init_lineup()` / `derive_lineup()` in `R/config.R`; FLEX, SUPER_FLEX, REC_FLEX and WRRB_FLEX are supported, K / DEF / IDP stop with a clear message), so a different lineup or team count needs no code change. Shiny auto-sources everything in `R/` into its own environment before `app.R` runs (turning that off from `global.R` does not work in current Shiny), so `init_lineup()` writes the lineup into every environment that holds it (`assign_in_scope()` in `R/config.R`).

To publish another league: copy `leagues/my_league.R`, fill in its `LEAGUE_ID` and title (`SHOW_RECAP_LINK <- FALSE`), then run `Rscript deploy_league.R leagues/<file>.R --name <app-name>`. It reads the league from Sleeper and prints what the app will see (lineup, team count, best ball), stages a clean folder `../guillotine_app_<name>_deploy/` with only the runtime files (no `rsconnect/`, `logs/`, `data/`, `recap/`, `collector/`, `analysis/`), and prints the `rsconnect::deployApp()` command; `--deploy` runs it. The original app's link and app ID live in this folder's `rsconnect/` and are never touched. Calibration constants (`UNDERDOG_CV`, the residual-shape file) were fitted under the original league's scoring and lineup and are approximate for a very different league.

**Kickoff times.** In Team detail the Status of a game not yet started is its kickoff time (e.g. "Sun 1:00 PM ET") from ESPN's scoreboard (`kickoff_utc` in `espn_get_game_status()`), shown with `kickoff_label()` in `app.R`; "Upcoming" appears only if ESPN gave no time.

**More than one chop a week.** `N_CHOPS_PER_WEEK` (default 1) is the number of teams chopped each week; a team's elimination chance is then its chance of being among the N lowest scores. The Simulations tab counts, in each draw, the teams with the N lowest totals (`chopped_share()` in `R/simulation.R`), and the Approximate tab uses `elimination_prob_instant_k()`: an integral over the team's own score of the chance that at most N-1 other teams are below it (a Poisson-binomial count, with finished teams as fixed scores). The chances add up to N, and with N = 1 the original code path runs unchanged. The number of chops is capped at the teams still alive minus one (`effective_chops()`), so a league that chops 2 a week until 2 teams are left and then lets the top score win (My League: weeks 1-15, then week 16) is handled with no week-specific code. `deploy_league.R` reads how many teams were eliminated per week from Sleeper and stops if it disagrees with the config.
