# Guillotine Fantasy Football

A live tracker for **guillotine fantasy football leagues** on [Sleeper](https://sleeper.com), where the lowest-scoring team each week is eliminated ("chopped") and its players go up for bid. It shows, while the games are being played, each team's chance of being chopped. An optional add-on writes weekly recap and preview web pages.

The recap pages are entirely optional: by default none are built, scheduled or published, and the app does not link to any of them.

**Jump to:** [Why this exists](#why-this-exists) · [How the probabilities are calculated](#how-the-probabilities-are-calculated) · [Limits](#limits) · [Adapting it to your league](#adapting-it-to-your-league) · [Recap and preview pages (optional)](#recap-and-preview-pages-optional) · [License](#license)

---

## Why this exists

In a guillotine league, what matters during the week is whether your score ends up above the lowest one. Sleeper's numbers don't answer that well while games are still being played:

- **Best-ball projected scores go wrong mid-week.** Part-way through the week, some players have played and others haven't. Sleeper's projected score appears to favor the players who have already played over the ones who haven't yet, even when an unplayed player is projected to score more, so a team can look more at risk than it is.
- **A projected score is not a chance of being chopped.** Finishing last depends on every other team's remaining upside too. Sleeper's "safe" percentage appears to depend only on a team's own projection.
- **The final lineup is uncertain.** In best ball, a bench player with a big game takes a starter's slot. A single projected score uses today's lineup and can't reflect that.

Sleeper doesn't publish projections that update during games, so this app has its own function that updates each player's projection as his game goes on. For each team it adds a projection for every player still to play to the points already scored, re-solves the best-ball lineup, models the uncertainty in what is left, and reports the chance of finishing lowest (or among the lowest few, in leagues that chop several teams a week). The main view uses a fast approximation that treats each team's projected lineup as fixed. The **Simulations tab** simulates every player's remaining points hundreds of times and re-picks each team's best lineup in every draw, so a bench player who outscores a starter moves into the lineup.

## How the probabilities are calculated

1. **Points already scored** come from Sleeper's matchups, which already use your league's scoring.
2. **Live projection.** Sleeper's pregame projection, re-scored under your league's rules, is blended with the player's pace so far: `rate = (1 - w) x pregame + w x observed pace`, where the weight `w` is the fraction of the game played, cubed. The pregame number stays in charge until late in the game. Remaining points are the rate times the time left. Players listed Out or on injured reserve are set to zero.
3. **Uncertainty.** Each player's remaining points get a standard deviation of `CV x rate x sqrt(fraction of the game left)`. The CV is set by position, from Underdog Network's published weekly variance: QB 0.36, RB 0.54, WR 0.58, TE 0.63 (`UNDERDOG_CV` in `R/config.R`).
4. **Historical data.** Three seasons (2023-2025) of Sleeper's actual-versus-projected stat lines, re-scored under your league's rules, cross-check those CVs (the backtest gave similar, slightly higher values: about QB 0.43, RB 0.60, WR 0.68, TE 0.74) and supply the *shape* of the noise: simulated scores resample the real, skewed historical residuals for each position instead of using a normal curve.
5. **From players to teams.** Each draw gives every player a final score, solves every team's best-ball lineup, and chops the lowest total. A team's chance is the share of draws in which it was lowest (or among the lowest few).

The code also contains other variance models: PlayerProfiler's published CVs, and two built from a backtest of historical data (a position-level fit and a per-player one). Only the position-CV model is switched on. The two backtest-based models are experimental, because we have no good way to judge how realistic their spreads are. They are described in [`docs/how-it-works.md`](docs/how-it-works.md).

## Limits

- **Players are assumed independent.** Each player's score is simulated on its own. In real games it isn't: a quarterback and his receivers rise and fall together, and game script moves a running back against a passing offense. This makes a stacked team's range of outcomes too narrow, so its chances of an unusually high or low total are understated. Ranking teams is less affected than the exact percentages.
- **Within-game uncertainty is assumed, not measured.** The square-root shrinkage over the game has no in-game historical data behind it. In early checks on a few replayed weeks, a predicted 50% range contained only about 25-35% of actual outcomes through most of a game, and was too wide in the last stretch.
- **The uncertainty values are published averages, not measured for your league.** How widely a score swings, as a fraction of the projection, depends partly on scoring rules. The backtest agreed with the published values for the scoring tested, but a league with very different scoring (large bonuses, a different points-per-reception setting) could differ. Change `UNDERDOG_CV` in `R/config.R` if you think yours does.
- **Projections are Sleeper's.** News the projection hasn't caught up with flows straight into the odds. Only Out and injured-reserve players are adjusted.
- **League format:** Sleeper best-ball guillotine leagues with QB, RB, WR and TE slots (including FLEX and SUPER_FLEX). K, DEF and IDP slots are not supported.
- **Unofficial data:** the Sleeper projections feed and the ESPN endpoints are unofficial and can change without notice. Check each source's terms before using the output beyond personal use.
- **macOS:** the scheduling and notification parts of the collector use macOS tools. The R code itself should run anywhere.

## Adapting it to your league

### What's in this repo

| Path | What it is |
|---|---|
| `app.R`, `R/` | **The live app** (R Shiny): chance of being chopped for every team, updated during games. This is the core. |
| `collector/` | **Optional** weekly recap and preview page generator, and the tools that replay past weeks. |
| `analysis/` | Backtests and model-checking scripts used to calibrate the probabilities. |
| `deploy_league.R`, `leagues/` | Deploy the app to shinyapps.io for a league. |
| `league_config.example.R` | The one setting you need: your Sleeper league ID. |
| `docs/how-it-works.md` | The long technical write-up of the model. |

### Set it up and run the app

You need R and the packages `shiny`, `dplyr`, `readr`, `tidyr`, `purrr`, `tibble`, `httr2`, `ggplot2`, `scales`, `patchwork`, `ggrepel`, `lpSolve` and `DT`.

1. Copy `league_config.example.R` to `league_config.R` and set `LEAGUE_ID` to your Sleeper league's ID (the long number in its URL) and `APP_TITLE` to your league's name. The starting lineup, scoring and number of teams are read from Sleeper. If your league chops more than one team a week, also set `N_CHOPS_PER_WEEK`.
2. Run `shiny::runApp()` from the project folder.

### Deploy the app

`Rscript deploy_league.R leagues/my_league.R --name my-app --deploy` stages a clean copy for your league and deploys it to shinyapps.io. Copy `leagues/example_league.R` for each league. Set `SHINYAPPS_ACCOUNT` or pass `--account`.

### Recap and preview pages (optional)

A static page you rebuild weekly: a recap (who was chopped and why, the close calls, a replay of the chop odds over the week), a preview (odds before and after the waivers, injuries, byes, who is up for bid) and a season overview (scoreboard and waiver analysis). Nothing here runs unless you run it.

**What you get just by running the scripts** (after setting your league ID, no other steps):

- `Rscript collector/weekly_recap.R 2026 3` builds week 3 of 2026 into `recap/2026/index.html`; open it in a browser. Run each finished week in order so the Season overview fills in. Past weeks can be built after the fact, from nflverse play-by-play and Sleeper's archived projections.
- `Rscript collector/weekly_preview.R 2026 4` builds the preview for the coming week.
- Every figure, table and number on the pages: the scoreboard, the odds replay, the waiver analysis, the interactive charts.
- The written text, as **automatic text**: fixed template sentences filled in from the same numbers. It is plain and formulaic but complete, and the page labels it "Automatic summary".
- Two files per week saved next to the page, in `recap/2026/`: the **fact sheet** of numbers (`narrative_wNN.facts.txt`) and a **ready-to-paste prompt** for an AI assistant (`narrative_wNN.prompt.txt`). The preview saves the same with `preview_wNN`.
- Managers appear under their Sleeper usernames.

**What needs extra steps** (all optional):

1. **AI-written text.** Paste the week's prompt file into an AI assistant, save its reply as `narrative_wNN.draft.txt` in the same folder (`preview_wNN.draft.txt` for a preview), and run the same build command again. The script checks the draft: every number must appear in the fact sheet, the required sections must be there, and there is no injury talk or markdown. If it passes, the page uses it and labels it "Written by AI". If it fails, the draft is kept as `narrative_wNN.rejected.txt`, the reason is logged, and the automatic text is used instead. Nothing is sent anywhere, and no API key is involved.
2. **Display names for managers.** The first build creates `collector/managers.csv`, listing every Sleeper username with an empty `real_name` column. Fill in a name for each, and the text uses them from the next build (`collector/managers.example.csv` shows the format).
3. **League facts for the AI prompt** (for example, who the commissioner is). Copy `collector/league_facts.example.txt` to `collector/league_facts.txt`. They go into the prompt, so they only matter if you use step 1.
4. **Publishing.** Off by default. Create a separate empty public repo for your recap pages, turn on GitHub Pages for it, clone it outside this project, and put `RECAP_PAGES_DIR=/path/to/that/clone` in `~/.guillotine_recap_env`. The build then copies only the finished page and images into it and pushes; it refuses to publish into this project's repo. Free GitHub Pages needs a public repo, so the page is readable by anyone with the link. `--no-publish` builds locally only.
5. **A weekly schedule.** Nothing is scheduled by default. The `collector/` folder has macOS `launchd` files you can install to run the builds automatically; see [`collector/README.md`](collector/README.md).
6. **A link from the app to your published recap site.** Once you have published your recap pages to GitHub Pages (step 4), you can add a "Weekly recap" button to the top of the app that opens that site. In `league_config.R`, set `SHOW_RECAP_LINK <- TRUE` and set `RECAP_URL` to your GitHub Pages address, for example `https://YOUR_USER.github.io/YOUR_RECAP_REPO/`. It is off by default, and it only makes sense after step 4, since it links to the page you published.

Details of each script, the checks and scheduling are in [`collector/README.md`](collector/README.md), and the full model write-up is in [`docs/how-it-works.md`](docs/how-it-works.md).

## License

MIT. See [LICENSE](LICENSE). This is an independent hobby project and is not affiliated with Sleeper or ESPN.
