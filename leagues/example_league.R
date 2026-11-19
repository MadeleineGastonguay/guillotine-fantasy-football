# Example settings for a SECOND league, for deploy_league.R. Copy it, fill it in and stage it for deployment:
#   Rscript deploy_league.R leagues/my_league.R --name my-league-app
# Anything not set here keeps the defaults from R/config.R.

LEAGUE_ID <- "YOUR_LEAGUE_ID"
APP_TITLE <- "My League"                  # shown in the page heading and as the iOS home-screen name
APP_ICON <- "\U0001F52A"
SHOW_RECAP_LINK <- FALSE                  # no weekly recap site for this league
N_CHOPS_PER_WEEK <- 2L                    # a league that chops two teams a week. The app never chops more teams than are alive minus one,
                                          # so when two are left this becomes one chop automatically.
# N_SIMULATIONS <- 300L                   # optional: fewer draws per refresh if a refresh turns out too heavy
