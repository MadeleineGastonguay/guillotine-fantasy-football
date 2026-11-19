# Projection-accuracy tooling

Two independent pieces, both outside the Shiny app (and excluded from the shinyapps.io
bundle by `.rscignore`). All output lives under `accuracy_data/`.

## 1. Replay past weeks through the app's model (`run_replay.R`)

Rebuilds every skill player's points at each minute of past games from nflverse
play-by-play, pairs them with Sleeper's archived pre-game projection, and runs them
through the app's own live-projection formula (`add_live_projection_columns()` in
`R/pipeline.R`). One row per player per game minute, plus a final row.

```bash
Rscript collector/run_replay.R 2025 1 1          # season 2025, week 1, every game minute
Rscript collector/run_replay.R 2025 1:18 5       # whole season, every 5 game minutes (~70 s)
Rscript collector/run_replay.R 2026 1 1          # a 2026 week (nflverse refreshes nightly, so a game
                                                 #   appears the morning after it is played)
Rscript analysis/replay_calibration.R            # first-look coverage diagnostics
```

Output: `accuracy_data/replay/{season}_w{WW}_players.csv.gz`. Columns include the matchup
(`matchup`, `matchup_label`, `opponent`, `home_away`), game time (`game_clock`, `period`,
`grid_seconds`, `elapsed_fraction`, scores), `pre_proj` / `pre_sd` / `pre_cv`,
`current_points`, `projected_final` / `projected_sd` / `projected_cv`, pre-game and current
quantiles (`pre_q05..q95`, `cur_q05..q95`), role fields (`role_final` = starter /
backup_entered / inactive_or_dnp, `entered_yet`, `first_involved_seconds`, `gs`, `off_snp`,
`snap_share`, `depth_chart_order`), cumulative stats (`cum_*`), game context (spread, total,
roof, temperature, wind) and `final_actual_points` (Sleeper's official final = ground truth).

By default the residual-shape pool used for the quantile columns leaves out the replayed season
(`shape_pool = excluding_<season>`), so coverage checks are out-of-sample for the shape. Pass
`--in-sample` to include it.

## 2. Pre-kickoff snapshots (`snapshot_pregame.R`)

Freezes Sleeper's projections (raw stat lines + rescored points), depth chart, injury status
and ESPN odds/weather for every skill player in this week's games into a new timestamped
file (`accuracy_data/pregame/{season}_w{WW}_{utc}.csv.gz`; never overwritten). Rows carry
`taken_after_kickoff` when a game had already started. On a network failure it retries, then
logs to `accuracy_data/pregame/snapshot.log` and writes nothing.

```bash
Rscript collector/snapshot_pregame.R            # skips when no game is still upcoming
Rscript collector/snapshot_pregame.R --force    # write regardless
```

### Scheduling it (macOS launchd)

`com.guillotine.pregame.plist` runs it before the usual kickoff windows (Thu 7:30pm,
Sat 12:00/7:30pm, Sun 8:45am/12:30/3:45/7:45pm, Mon 7:00pm, Mac local time). launchd runs a
job missed during sleep once at wake. Install / remove:

```bash
cp collector/com.guillotine.pregame.plist ~/Library/LaunchAgents/
launchctl bootstrap gui/$(id -u) ~/Library/LaunchAgents/com.guillotine.pregame.plist
launchctl print gui/$(id -u)/com.guillotine.pregame | head        # confirm it is loaded

launchctl bootout gui/$(id -u)/com.guillotine.pregame             # remove
```

launchd cannot wake a sleeping Mac. To have it awake on Sundays before the 12:30 run:
`sudo pmset repeat wakeorpoweron U 12:25:00` (one repeating schedule at a time), or keep the
Mac awake during game windows.

## 3. Weekly recap (`weekly_recap.R`)

After a week finishes, replays it on its real kickoff times (every game on its own clock, from nflverse
play-by-play time stamps), builds the team-level elimination timeline, the figures and a static page, so the
league can see how the tracker's chances of being chopped moved against who actually got chopped.

```bash
Rscript collector/weekly_recap.R                  # the latest week in which every game has a final score
Rscript collector/weekly_recap.R 2026 4           # a specific season and week
Rscript collector/weekly_recap.R --force          # redo a week that is already done
Rscript collector/weekly_recap.R --window=4       # weeks covered by the scorecard / lowest-two figures (default 3)
```

Output goes to `recap/{season}/` (a plain static folder; host it anywhere, it is excluded from the
shinyapps.io bundle): `index.html`, `img/` (one race chart per week, plus `chop_rank.png` ("did the tracker see it coming?") and `lowest_two.png`
over the last few weeks), `facts.csv` (one row of headline numbers per week), `scorecard.png` (the detailed three-row figure for every week, kept in the repo and not published) and `week_NN.done` markers.
Needs the network (Sleeper, nflverse). Takes about 1 minute plus about 4-5 minutes for the Simulations-tab cross-check described below.

It refuses to publish if the replay is missing any game on the week's schedule (nflverse can lag a night
behind the last game); it logs `RECAP FAILED: ...`, exits 1, and a later scheduled run picks the week up.
The page's summary sentences are generated from the numbers and are not reviewed by a person. The "why" behind
a week's story (which players underperformed and so on) is only the mechanical table of the chopped team's
biggest shortfalls; the interpretation is left to whoever publishes.

### Scheduling it (macOS launchd)

`com.guillotine.recap.plist` runs it Tuesday and Wednesday at 9:00am Mac local time. Tuesday is the real run (nflverse
refreshes overnight, so Monday night's game is in the data by then); Wednesday is the retry, and does nothing if
Tuesday's run finished. Install / remove:

```bash
cp collector/com.guillotine.recap.plist ~/Library/LaunchAgents/
launchctl bootstrap gui/$(id -u) ~/Library/LaunchAgents/com.guillotine.recap.plist
launchctl print gui/$(id -u)/com.guillotine.recap | head          # confirm it is loaded
launchctl kickstart gui/$(id -u)/com.guillotine.recap             # run it now (a no-op if the week is done)

launchctl bootout gui/$(id -u)/com.guillotine.recap               # remove
```

Logs: `logs/recap.out.log` and `logs/recap.err.log` (the script's messages are in the err log). It is NOT deployed
or uploaded anywhere automatically: publishing `recap/{season}/` (GitHub Pages, a link from the app, ...) is a
separate step.

### The written description on the page

Every recap page carries a short write-up (a short version, the week's story, which players fell short, the waiver
results). It comes from one **fact sheet** of numbers the script computes (`recap/{season}/narrative_wNN.facts.txt`), in
one of two ways, tried in this order:

1. **Your draft (free).** The script also saves `narrative_wNN.prompt.txt`: the writing instructions plus the fact sheet, ready to
   paste into Claude (the app, on your plan). Save Claude's reply as `recap/{season}/narrative_wNN.draft.txt`. The next run (the
   Wednesday retry, or `Rscript collector/weekly_recap.R` right away) rebuilds the page with it, labelled "Written by AI".
2. **Automatic text (free, the default).** `template_narrative()` in `recap_lib.R` puts the same four sections together from fixed
   sentences. It can't invent anything and always works, but reads more formulaically; the page labels it "Automatic summary".

Anything from tier 1 must pass `check_narrative()` before it is shown: every number has to appear in the fact sheet (no
arithmetic or invented figures), no injury talk, no markdown, and the required sections present. A draft that fails is saved as
`narrative_wNN.rejected.txt`, never shown, the reason is logged, and the page uses tier 2. The writing rules you or your AI assistant are
given (plain language, no jargon, no injury speculation, no hype, no new numbers) are in `RECAP_SYSTEM_PROMPT`.

When a week's page is first built with the automatic text, a macOS notification says so and names the prompt file. (If no
notification appears, allow notifications for "Script Editor"/"osascript" in System Settings.)

Testing: `--narrative-file=path.txt` runs your own text through the same checks and page.

### Publishing to GitHub Pages

Opt-in: nothing is published unless `RECAP_PAGES_DIR` is set (in `~/.guillotine_recap_env`). It must
be a local clone of a **separate** repo that serves GitHub Pages (the script refuses to publish into this app's repository).
On each run the script copies only `recap/{season}/index.html` and `img/` into `{repo}/{season}/`, writes a root `index.html` that
redirects to the newest season, commits and pushes. Fact sheets, prompts and drafts are never copied. A repeat run does nothing if
nothing changed, and a push that failed is retried on the next scheduled run. `--no-publish` builds locally only.

One-time setup:

1. On GitHub create an empty repo (e.g. `guillotine-recap`). GitHub Pages on a free account needs the repo to be **public**, so
   the page (team names, player names, scores) is readable by anyone with the link; the pages carry `noindex` so search engines
   are asked to skip them, which is not a guarantee. A private repo works only on a paid GitHub plan.
2. Clone it somewhere outside this project: `git clone https://github.com/<you>/guillotine-recap.git ~/Proj/guillotine-recap`
3. Repo Settings > Pages: Source "Deploy from a branch", branch `main`, folder `/ (root)`.
4. Let git push without prompting (needed for the scheduled run): `git config --global credential.helper osxkeychain`, then make one
   manual push from the clone (it asks once for your GitHub username and a personal access token, e.g. a fine-grained token with
   Contents read/write on that one repo) and macOS remembers it.
5. Add `RECAP_PAGES_DIR=~/Proj/guillotine-recap` to `~/.guillotine_recap_env`.
6. Run `Rscript collector/weekly_recap.R 2026 3 --force`; the site appears at `https://<you>.github.io/guillotine-recap/`.

### The team groups, and the projected-vs-finished figure

Each week's page has these sections about the teams that were NOT chopped (`saved_selection()` in `collector/recap_lib.R`). Each team is in
at most one group; a team that fits more than one takes the first below.

1. **Teams that saved themselves** (teal): the survivor with the highest chance of being chopped before any game, if that chance was at
   least twice the even split (1 in the number of teams). At most one team a week.
2. **Close calls** (orange): survivors whose chance of being chopped climbed to at least 25% once games were on, and at least 5 points
   above their pregame chance; up to two, highest peaks first. The text says when the chance peaked, how many points the team had then, who
   scored after that point (and whether they were already in its scoring lineup), and how it finished against the chopped team.
3. **Teams that massively outperformed their projections** (purple): survivors not in 1 or 2 whose finish above their pregame projected score
   is at least 20 points MORE than the typical team's that week (the median over all teams), so a week in which the whole league beats its
   projections does not make everyone a "massive" outperformer; up to two, biggest first (`beat_over_median` in `saved_selection()`). The text
   gives the typical team's figure, lists the players who beat projection, and any players who were not in the pregame lineup but ended in the
   scoring lineup (bench players, or players picked up that week).

Groups with no team are left out of the page. The thresholds are arguments of `saved_selection()`. Files per week: `saved_wNN.csv`,
`saved_players_wNN.csv` and `img/proj_slope_wN.png` (every team, pregame projection on the left, final score on the right, one line per
team; shown first on the page as the week at a glance). The fact sheet has the same blocks, so `check_narrative()` verifies the numbers, and
an AI draft needs these headings: `Short version`, `The team that got chopped`, then `Teams that saved themselves`, `Close calls` and
`Teams that massively outperformed their projections` for each group that is not empty, and `Waiver results` when there is a waiver block. (There is no longer a comparison with earlier weeks; the Season overview has that.)

**Navigating a week.** The page adds three things to every week's write-up, none of them written into the text by the model:
- a sticky jump bar under the week's title (The chop, Dodged it, Close calls, Smashed it, Waiver recap; only the sections that exist), each chip in the colour of its section and of the figure's key (red, teal, orange, purple, blue). The
  anchors are `#wkN-chop`, `#wkN-saved`, `#wkN-close`, `#wkN-beat` and `#wkN-waivers`, and the address bar opens that week and scrolls to the section;
- a "Read the full story" / "See the close calls" / ... link at the end of each Short version bullet. The bullet goes to the chopped team's section if it
  names the chopped team, otherwise to the section of the first saved / close-call / outperformer team it names, otherwise to the waivers if it
  mentions them. A bullet that mentions a roster swap can go to the swap sub-label instead. No two bullets link to the same section (a
  bullet takes its best section that no earlier bullet took, and has no link if they are all taken; rules in `cands()` in `week_section_html()`);
- the four sub-labels inside "The team that got chopped" (`### What happened`, `### Why it fell short`, `### The bids that got away` (waiver bids the team lost, before the week), `### The swap that backfired` (the move it made that lowered its score), in that order). Unlike the
  other two, these are lines in the text itself: the prompt asks for them, `check_narrative()` accepts only those four and only in that section,
  and the automatic text writes them too. Weeks 1-5 were labelled by hand; the unlabelled texts are kept as `narrative_wNN.before-sublabels.txt`.

**Switching pages.** A slim quick-tab bar (Preview, Recap, Season, Projections) slides in at the top when you scroll up and hides when you scroll down or are near the top, so you can change page from anywhere; the jump chips move down to sit under it, and tapping a chip hides it again. Each page ends with a "Next: ..." link and links to the other pages. The Recap links return to the week you last picked. All of it is in `write_recap_page()` (`quick_tabs`, `page_nav()`, the `quicktabs` bits of the inline script); without JavaScript the page still works, the quick bar just stays hidden.

**The Season overview** has the same sticky jump bar (The chops, Scoreboard, Waivers), and its waiver subsections use the weekly recap's heading style with one color each (`h3.wvh` in the page CSS; ids `season-money` (the "League wallet": spending per period and budget left), `season-worth`, `season-reading`, `season-habits`, `season-roi`).

### Roster moves, and "what if they had not made them"

Sleeper's matchup endpoint returns each roster as it stood at the END of the week (players added that week are on it, players dropped
that week are not). `roster_intervals()` in `R/replay.R` undoes the week's completed transactions (add / drop / trade, newest first, only
those between the first and last step of the replay) so every step uses the roster the team actually had at that moment; a player picked
up on Sunday is not credited to the team before then. Sleeper locks a player once his game starts, which the data bears out: in
weeks 1-3, 28 of the 29 drops and 32 of the 33 adds made during the games happened before that player's kickoff (the exception: one
tight end dropped and re-added around a Thursday night game). Limitations: a player dropped and later re-added by the same team gets one
interval, and waiver claims that go through before the first game (the main Wednesday run) fall outside the window and change nothing. The league's early-morning waiver runs on Friday, Saturday and Sunday fall inside it and are applied like any other move. The season ledger counts free-agent pickups and waiver claims together, as all roster moves made after the first game of the week started.

For every team with moves during the games, `recap_players()` also asks: what if none of those moves had been made (kept the players it
dropped, never added the players it picked up)? Undoing a drop also undoes any later pickup of that player by another team (a team cannot
have a player that was never available), and so on down the chain; every changed roster is re-scored from the same replay points and the
difference applied to the official final score, then the lowest score of the league is found again. This gives "would have been chopped"
or "would still have survived" in the fact sheet and text (`moves_wNN.csv` lists every team with moves and its score without them). It
assumes nothing else changes, e.g. that nobody else would have made a different move.

### The Simulations-tab cross-check on the page

Each run also compares the Approximate tab's chance of being chopped with the Simulations tab's (5,000 simulated weeks) at seven
moments of the new week (`analysis/simulation_spot_check.R`, cached per week in `accuracy_data/replay/{season}_wNN_sim_check.csv.gz`, so
only the new week is computed). The page gets a "does the Simulations tab agree?" section, written from templated sentences (no model),
with the statistics over every week checked so far and a figure for the last few weeks. `--no-sim` skips computing the new week's check (saves the 4-5 minutes) but the section is still built from the cached weeks, so a
rebuild never drops it; `--redo-sim` recomputes the new week's check.

The page has two views under the title: **Weekly recap** (week picker, that week's story, figure and shortfall table) and **Season
overview** (`#season`: the table of every chop, the multi-week figures, and this cross-check).

### Highlights, scoreboard and tabs

- **Weekly highlight tiles** (`highlights_wNN.csv`, three across, no wider than the figures): top team score, top player score, biggest boom and bust against
  projection, biggest waiver spender, most expensive player (only when he is not already in the spender tile), total waiver spending on the freed players, and the best and worst swap after waivers (in-game moves, made once the week's first game has started and so after the waivers on the chopped team's players were processed; with the FAAB cost of a waiver claim). *Biggest Waiver Spender*,
  *Most expensive player* and *Waiver spending* come from the waiver claims on the players released by LAST week's chop (the chop frees all 15 of that team's players), between the chop
  and this week's first kickoff; each shows how the claimed players did this week against their projection. Week 1 has neither.
- **Season scoreboard** (Season overview): sortable by clicking a column heading. Average points is points per week played (a chopped team's average covers only the weeks it played). Closest to the chop is highlighted red at 5 points or fewer, weeks in danger
  orange (one) or red (two or more), and FAAB left is each team's waiver budget out of $1,000 as of the latest build (`faab.csv`; Sleeper keeps only the running
  total used, not a history).
- The roster-swaps ledger was removed from the Season overview (`swap_ledger_html()` is still in `recap_lib.R`, unused); `swaps_wNN.csv` is still written.
- **Waivers** (`collector/waivers_lib.R`; Sleeper keeps lost claims too, with status `failed`, the bid and the reason):
  - *Weekly text:* if the chopped team lost waiver claims to another owner between the previous week's end and this week's end, the fact sheet gets a
    `WAIVER BIDS THE CHOPPED TEAM LOST` block (its bid, the winning bid and winner, its budget left, the points winning would have added to its final score against the gap it needed,
    and the combined effect), and the prompt tells the writer to add a short paragraph to "The team that got chopped". Week 1 has none.
  - *Weekly text* "Waiver results" section (needs the WAIVER RESULTS block in the fact sheet, which `waiver_fact_lines()` builds; both the block and the instructions are in `narrative_wNN.prompt.txt`,
    so pasting that file into Claude and saving the reply as `narrative_wNN.draft.txt` recreates it): how many freed players were claimed and the total paid, the *massive overbids* (the winner paid at least $50
    and 25% of the price above the runner-up bid) and the managers who *just missed out* (runner-up bid within $5 or 5% of the price, or tied it). It must end with "Will it pay off? Follow along in the season overview.";
    the page turns "season overview" into a link to the Waivers part of the Season overview (`#season-waivers`) and shows the section above the waiver table. The check rejects a draft without it.
  - *Weekly table* "Waiver results" (end of each weekly page, `waivers_wNN.csv`): the claims on last week's chopped players, with price, number of bids in that waiver run,
    runner-up bid, overpay (price minus runner-up) and the player's score against projection this week.
  - *Season overview* "Waivers" (`waiver_claims.csv`, `faab_history.csv`, `img/waiver_spending.png`, `img/waiver_budgets.png`, `img/waiver_value.png`): dollars won per surviving team between one chop and the next, split into the
    chopped team's players and everyone else; a line plot of every team's budget over time (a chopped team's line ends with a red cross); one dot per claim of dollars paid against points added
    (the team's best-ball score with the player minus without him, summed over the weeks he was held, so it is rebuilt and moves every week; colored by points per $100, circle = freed by the chop, triangle = other; guide lines at 5 and 20; hollow grey = not played yet; labels give the team and the week
    picked up), with a text reading of who paid off built from the same data; and a sortable return-on-investment table by manager. The "money left against results" figure is
    deliberately not built yet (needs about five weeks of data).
- **Tabs:** Weekly recap, Season overview (`#season`) and Projection performance (`#projections`: the scorecard, the lowest-two figure and the cross-check against the
  Simulations tab, which are about the tracker itself).


### Tables on a phone
Every table on the page scrolls sideways with its first column frozen (as in the app), and below 700px wide the cells are tighter and do not wrap. A round "Top" button appears after scrolling and returns to the top.

## Week preview (`collector/weekly_preview.R`)

Published Wednesday morning, after the waivers have run (about 3am Eastern) and before any game of the week. For every team still alive it shows the projected score and
the chance of being chopped **before the waivers** and **after** them, which players the waivers added, which players are on a bye, and the claims each team lost.
Both versions use today's projections, so a change comes from the waiver moves alone; the chance of being chopped is the app's Approximate model
(`run_elimination_instant()`, the same code the app runs, via `build_live_snapshot()`).

```bash
Rscript collector/weekly_preview.R                 # the current NFL week; waits (exits without publishing) until this morning's waiver claims are on Sleeper
Rscript collector/weekly_preview.R 2026 5 --force --no-publish    # a specific week, rebuilt locally
```

- **Before-waivers rosters** are rebuilt by undoing the completed waiver claims made since the last chop, newest first (a claim's added player leaves, the dropped player returns). Sleeper does keep each completed week's final rosters (the matchup rosters the recaps use) and a timestamped transaction log, but not a snapshot of the rosters at an arbitrary moment such as just before the waivers, and the upcoming week's matchup is simply the current roster. Checked for week 5: the rebuilt rosters equal week 4's final matchup rosters, apart from two free-agent drops made Tuesday after that week locked.
- **Bye cost** is the projected best-ball score if the players on a bye were projected at their usual level (their mean pregame projection in the weeks played this season) minus the score now.
- **Lost claims** are the claims a team lost to another owner, with what winning would have added to its projected score.
- **Text**, as for the recaps: the script writes `preview_wNN.facts.txt` and `preview_wNN.prompt.txt` (instructions, facts, league facts, and any `preview_wNN.notes.txt`); paste the prompt into Claude and save the reply as
  `preview_wNN.draft.txt`, then run the script again (or let the Thursday run do it) to rebuild with it. A draft is checked like a recap (required sections, no they/their/them, no injuries, every number in the fact sheet); until one exists,
  or if it fails, the page shows an automatic summary and a notification says so.
- **Disclaimer:** the top of the preview carries a box saying it was generated from Sleeper's projections (and injury statuses) as of the morning each was pulled and may not reflect Sleeper's updates up to kickoff. The times come from `preview_meta_wNN.csv`, written on each build from when the data was fetched (the day, and morning/afternoon/evening), so a preview whose injury news was added on a later day says so.
- **Links to the tables:** the page ends each section of the preview text (after the short version) with a "See all ..." link that opens the matching table further down: every team, waiver pickups, injury news, players on a bye. They are added by the page, not written into the text, so they appear for every week (the anchors are `#pv-teams`, `#pv-pickups`, `#pv-injuries`, `#pv-byes`). All four tables sit under one colored heading, "The numbers" (`#pv-numbers`), as sub-headings; the text sections above it have colored headings like the recap's. A sticky jump bar at the top (At risk, Waivers, Injuries, Byes, The numbers; only the sections that exist) uses the same chip style and colors; the text headings carry the ids `pv-sec-teams`, `pv-sec-waivers`, `pv-sec-injuries`, `pv-sec-byes`.
- **Saved data:** the first build saves everything it fetched in `preview_wNN.data.rds`, and later runs of the same week (adding a draft, a rebuild) reuse it, so a draft written from `preview_wNN.prompt.txt` still matches the numbers it was written from (projections drift through the day, and a changed number would fail the draft's number check). `--refresh` fetches new data.
- **Injuries:** only players who would be in their fantasy team's lineup if healthy (a healthy projection is his projection if above 0, else his usual one), are not on a bye, and played their last game, so a long-term absence is not repeated each week.
  For each: his Sleeper status (Out/IR, Doubtful, Questionable, as of the morning the preview is built) and the team's chance of being chopped now, if he plays and if he sits (the rest of the league unchanged); `assumed` says what Sleeper's own projection expects
  (Sleeper gives many injured players a 0.0 projection, which means it expects them to sit, and the tracker follows Sleeper's projections; the text and table say this in those words). Written to `preview_injuries_wNN.csv`, shown as "Injury news" on the page, and written about in the "Injuries" section of the text.
- **Lost claims** are described by what they would have done to the chance of being chopped: the league is recomputed as if that claim had gone through instead of the winner's (the winner does not get the player and keeps the one it dropped).
- **Waiver pickups table:** columns are Player, Claimed by, Price, Second-highest bid, Overpay, Dropped, Projected this week, Adds to the team. Overpay is the price minus the second-highest bid (a dash when nobody else bid), the same definition as the Overpay column in the recap's waiver results table; every week's preview builds it this way.
- **Players on injured reserve (IR):** a player Sleeper lists on IR cannot play, so he stays in the Injury news table with the status IR but has no "If he plays" figure (a dash); the table, the fact sheet and the text give only the chance of being chopped with him out. The pipeline (`zero_out_players()` in `R/pipeline.R`) now zeroes an IR player's projection like an Out player's, in the app as well. His pre-game projection is also set to 0 (before the live columns are built), so the app's "pre-game X" line, the pre-game spread and the team's pre-game total start from 0.
- **Files** (`recap/{season}/`): `preview_wNN.csv` (one row per team), `preview_pickups_wNN.csv`, `preview_byes_wNN.csv`, `preview_lost_wNN.csv`, `preview_wNN.{txt,kind,facts.txt,prompt.txt}`, `img/preview_wN.png`.
- **On the page** a "Week N preview" tab is added as the first tab (the page is titled "Guillotine Weekly"), and it is the tab the page opens on while the preview is newer than the latest recap (it falls back to the recap once that week's recap exists). The page is rebuilt from
  the files on disk (`rebuild_recap_page()` in `recap_lib.R`), with no replay, and published like a recap (`--no-publish` builds locally).
- **Schedule:** `com.guillotine.preview.plist` runs it Wednesday 9:30am and 12:00 (retry if the waiver run was late) and Thursday 9:00am (picks up a draft saved after the first run). Install it like the recap job:

```bash
cp collector/com.guillotine.preview.plist ~/Library/LaunchAgents/
launchctl bootstrap gui/$(id -u) ~/Library/LaunchAgents/com.guillotine.preview.plist
launchctl print gui/$(id -u)/com.guillotine.preview | head
launchctl kickstart gui/$(id -u)/com.guillotine.preview          # run it now
```

Logs: `logs/preview.out.log` and `logs/preview.err.log`.

### League facts
`collector/league_facts.txt` holds standing facts that go into every prompt as a LEAGUE FACTS block (for example, that Pat is the commissioner, to be used in the roast if he is chopped). Add a line there for anything that stays true all season.

### Interactive "Was it worth it?"
`waiver_value_interactive_html()` (`collector/waivers_lib.R`) draws the chart with plotly.js (loaded from cdn.plot.ly) on the Season overview: click or tap a dot for the player, team, week picked up,
cost, points per $100, total points added and average per game, and use the menu to show one team's claims (all teams by default). The page shows `img/waiver_value.png` until plotly has loaded and the
tab is open, so it still works if the library cannot load. The chart is rebuilt every week with the rest of the page.
The vertical axis is the AVERAGE POINTS PER GAME a claim has added (points added divided by the weeks the player was held; 0 until he has played), and points per $100 is that average divided by the price, times 100 (colour scale tops out at 30, guide lines at 3 and 10). In the manager table, *Total points* is the summed points added, *Average per game* adds up each claim's per-game average (the points per game the manager's claims add together), and *Points per $100* is that average per game divided by the money paid on claims that have played.

### Voice of the written text
Both prompts (the recap's `RECAP_SYSTEM_PROMPT` and the preview's `PREVIEW_SYSTEM_PROMPT`) take their voice rules from `sass_voice()` in `recap_lib.R`: a sassy, theatrical game-show-AI narrator borrowing the style of Dungeon Crawler Carl in the writer's own words (no swearing, no quotes, and none of the books' vocabulary such as "achievement", "the System" or "crawler"; the checks reject those words).
The recap roasts the chopped manager every week, pokes fun at overspending on waivers and near-miss bids, gives teams that sweated out Monday night a rough time, and gets properly excited about good waiver claims and big beats of the projection.
The preview teases the teams most likely to be chopped, the overspenders and the near misses, and cheers the smart claims. Every joke has to be tied to a fact in the fact sheet and stay about fantasy football. Only the preview may mention injuries, and only as the status the fact sheet gives.


### Injuries in the recaps
The written recap used to ban injury talk entirely. It now allows PLAIN FACTS in two places, and only when the fact sheet carries the matching block (`collector/waivers_lib.R`, `collector/recap_lib.R`):
- **PAID FOR, THEN SIDELINED** (weekly recap): waiver claims that cost $1 or more where the player missed THAT week's game with an injury (listed Out/Doubtful on the NFL's weekly report, from nflverse, and did not play) or is ruled out for the coming week (Sleeper's status, plus any ESPN report newer than Sleeper's daily copy). A $0 claim is not "paid for". Only claims picked up for that week (or claimed for the coming week and already ruled out) are listed: an older claim that sits out a game is left out of the waiver paragraph (`sidelined_fresh()`), though a chopped team's own older claim can still appear in its "Why it fell short". One or two sentences at the end of "Waiver results". Each week only mentions that week's cases. The Season overview does not show them (no list, no chart markers): the weekly recaps cover them, week by week. `waiver_roi()` still writes the `missed_inj` / `missed_weeks` / `status_now` / `on_roster_now` columns to `waiver_claims.csv` for that.
- **LEFT A GAME WITH AN INJURY**: players on the chopped team that ESPN's injury report timestamped during their own game, placed in the game by ESPN's play-by-play (the quarter, with early/late, "at halftime" or "in overtime"; minutes after kickoff only if the plays are missing) (`espn_game_exit_reports()`, cached per week). One sentence under "Why it fell short". ESPN only keeps these reports for the most recent week, so each week has to be built right after its games.
- **SIDELINED PLAYER ON THE CHOPPED TEAM**: a paid-for claim of the chopped team that missed the week's game (same source as the first block), written up first under "Why it fell short" with Sleeper's projection for him from that week's preview data (`preview_injuries_wNN.csv`, only players who would have started if healthy). It never says what the team's score would have been with him. A player covered here is ALSO listed in "Waiver results" (some readers only read that section), under the usual two-biggest-prices limit.
- **FINALLY PAYING OFF** (not an injury block; `paying_off_for_week()`): an earlier claim (held at least two games before the week) that added under 4 points a game to the team's score before the week and at least 6 points in it (waiver_roi() now also writes `added_last`, `scored_last`, `scored_total`, `last_week_held`; those add-to-team figures only decide who qualifies). The player must also have jumped himself: at least 8 points this week and 5 or more above his own earlier average (otherwise a weak replacement can make a so-so game look like a payoff). The text shows only what the PLAYER scored: his average points a game before the week and his score this week, never points added to the team. Every such player gets a line in "Waiver results" (some readers only read that section), and one more in the team's own paragraph where it has one.
- **Layout of the saved-themselves and outperformed sections (Week 6 shape):** the written paragraph carries the team's results (chance before the week, projected, finished, above projection, above the chopped team, and for outperformers the typical team's figure) and names the standout players and bench movers without their figures; under it there is NO team table, only ONE player table (Team, Player, NFL team, Projected, Scored, Above projection, Note), with the players who beat their projection first and the bench players moved into the final lineup after, marked in Note. Close calls keep their one team table. The prompt tells the writer the same.
- **Up for bid this week** (last section of the recap; `waiver_watch()` in `collector/waivers_lib.R`, file `upforbid_wNN.csv`): the chopped team's freed QB/RB/WR/TE, which go up on the waivers early Wednesday, so the Tuesday recap can tell managers who is available. The table has the position in its own column in the app's colors (QB red, RB green, WR blue, TE orange) so readers can sort or scan by position. Per player: **last-3 average** (his average points over his last three games with at least one offensive snap, league scoring), **spread** (week-to-week spread: the standard deviation of his points over all games he played), games played, best game, **top-12 weeks** (the upside measure: games in which he finished among the 12 highest scorers at his position that week, from every player's league-scored Sleeper stat line), Sleeper's projection for next week (0 on IR), points this week, and a Status column with Sleeper's current designation (for example Out) plus his NFL team's bye: "On a bye next week (week N)" or "Bye in week N" for a bye still to come (each team's bye week is read once from ESPN's weekly scoreboards and cached in `cache/nfl_byes_SEASON.rds`). Shown: last-3 average of 8.0 or more (players on a bye included, flagged in Status), best average first, at most 10 and at least 3 (constants `WATCH_*` above the function). The text names the top three or four, says a table lists all of them, may state a status only as a plain word (out, on injured reserve), never a body part or return date, and never predicts a bid winner or tells anyone how to bid.
- **BARE BENCH** (`bare_bench_teams()` in `collector/waivers_lib.R`): a team whose WHOLE bench could not play that week: after the best-ball lineup is picked from the actual points, every player left on the bench took no offensive snap AND his team was on a bye (`nfl_bye_weeks()`) or he was ruled out (Out/Doubtful on the NFL's weekly injury report, or Sleeper lists him Out / IR / PUP). A healthy backup who just did not play does not count. The prompt asks for one sentence per such team in the second "What happened" paragraph, just before its closing line, as part of the chopped team's roast: if another team was chopped, the joke is on the chopped team (it had usable backups and still got chopped, while the bare-bench team survived without any); if it was this team, it is given as a reason the week went wrong. (Checked live for week 8: only Team Name, Riley, had one: 4 players on a bye, 2 Out, exactly 9 healthy players for 9 lineup spots.)
Rules that stay: never name the injury or the body part (sources disagree on them), never say how it happened, never give or guess a return date or how long anyone will be out (ESPN's return dates are mostly placeholders and are not used). The validator (`check_narrative()`) rejects a draft that does any of that, and rejects any injury wording at all when the fact sheet has neither block.
