# --- Per-league settings -----------------------------------------------------------------------
# Copy this file to league_config.R (which git ignores) and fill it in. app.R and every script under
# collector/ and analysis/ read it. Anything you leave out falls back to the defaults at the top of R/config.R.

# Your Sleeper league ID: the long number in the league's URL (sleeper.com/leagues/<this number>/...)
LEAGUE_ID <- "YOUR_LEAGUE_ID"
APP_TITLE <- "Guillotine"            # your league's name: the app heading, the iOS home-screen name and the recap pages' title
APP_ICON <- "\U0001F52A"             # shown after the title in the page heading
SHOW_RECAP_LINK <- FALSE             # TRUE adds a "Weekly recap" button in the app that links to your published recap site (RECAP_URL)
# RECAP_URL <- "https://YOUR_USER.github.io/YOUR_RECAP_REPO/"   # the GitHub Pages address of the recap repo you publish to
# N_CHOPS_PER_WEEK <- 1L             # a league that chops several teams a week sets it (see leagues/example_league.R)
# N_SIMULATIONS <- 500L              # optional: fewer draws make each refresh lighter (large leagues)
