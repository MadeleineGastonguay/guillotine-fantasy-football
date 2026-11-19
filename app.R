library(shiny)
library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(scales)
library(DT)
library(parallel)

source("R/config.R")   # also loads league_config.R (league ID, title, recap link) when present
source("R/sleeper_api.R")
source("R/sleeper_projections.R")
source("R/espn_api.R")
source("R/stat_scoring.R")
source("R/bestball.R")
source("R/simulation.R")
source("R/distributions.R")
# source("R/bayesian.R")
source("R/history.R")
source("R/fallback.R")
source("R/pipeline.R")
source("R/logging.R")
init_lineup()   # this league's starting lineup from Sleeper (falls back to the built-in one)

# Plain-fantasy-football-language explanation of each variance model, aimed
# at league members who know the game (boom/bust, projections) but not
# statistics -- so "how much that score could swing" rather than naming
# "standard deviation" as a concept (the tables no longer show a ± SD column;
# the spread appears as the Likely Score Range column and distribution charts). See MODEL_DESCRIPTIONS in
# R/config.R for the per-model wording and sources.
#
# A plain function, not a renderUI output: this needs to appear on both
# Standings tabs, and Shiny does not support binding the same uiOutput() id
# twice -- confirmed directly, not assumed: doing so throws an uncaught
# client-side "Duplicate binding for ID" error that leaves the app stuck in
# a reload loop. Since this content depends only on static config
# (ALL_STANDINGS_MODEL_NAMES, MODEL_DESCRIPTIONS), not on any reactive
# session data, a plain function called directly from the UI is both correct
# and simpler than working around the binding restriction with two
# separately-named reactive outputs. Defined here, before `ui <-`, because
# the UI calls it inline at construction time (file source order matters --
# it can't be defined later in the file, e.g. alongside server()).
variance_blurb_ui <- function() {
  model_ids <- names(ALL_STANDINGS_MODEL_NAMES)
  n <- length(model_ids)

  if (n == 1) {
    mid <- model_ids[1]
    label <- ALL_STANDINGS_MODEL_NAMES[[mid]]
    info <- MODEL_DESCRIPTIONS[[mid]]
    return(p(
      style = "max-width:760px; margin:0 0 10px 0;",
      sprintf(paste0(
        "Elimination probability comes from each team's projected score and how much that score could ",
        "realistically swing up or down (shown as the Likely Score Range column and chart below), using %s's estimate of ",
        "that swing: %s"
      ), label, info$text),
      if (!is.null(info$url)) {
        tagList(" ", tags$a(
          href = info$url, target = "_blank", rel = "noopener noreferrer",
          style = "color:var(--sl-teal);", "(source)"
        ))
      }
    ))
  }

  items <- lapply(model_ids, function(mid) {
    label <- ALL_STANDINGS_MODEL_NAMES[[mid]]
    info <- MODEL_DESCRIPTIONS[[mid]]
    tags$li(
      style = "margin-bottom:4px;",
      tags$strong(style = "color:var(--sl-text);", label), ": ", info$text,
      if (!is.null(info$url)) {
        tagList(" ", tags$a(
          href = info$url, target = "_blank", rel = "noopener noreferrer",
          style = "color:var(--sl-teal);", "(source)"
        ))
      }
    )
  })
  tagList(
    p(
      style = "max-width:760px; margin:0 0 4px 0;",
      sprintf(paste0(
        "Elimination probabilities come from each team's projected score and how much that score could ",
        "realistically swing up or down (shown as the Likely Score Range column and chart below). There are %d elimination ",
        "probabilities reported, each using a different source for how much swing to expect:"
      ), n)
    ),
    tags$ul(
      style = "max-width:760px; padding-left:18px; margin:0 0 10px 0; color:var(--sl-muted); font-size:12.5px; line-height:1.5;",
      items
    )
  )
}

# Heading + explanation above each team ridgeline. HTML rather than plot
# title/subtitle text so it wraps to the page width instead of being clipped.
ridge_heading_ui <- function(what) {
  tagList(
    h5(style = "margin:14px 0 2px 0; font-weight:600;", "Where each team's final score could land"),
    p(style = "max-width:760px; font-size:12px; margin:0 0 4px 0;",
      what, " Ordered from lowest projected score (top) to highest; redder = higher elimination chance; wider = more uncertain; white tick = projected score.")
  )
}

# Colors sampled directly from sleeper.com's own site (computed styles, not
# guessed): near-black navy page background, a lighter navy for cards/rows,
# the signature bright teal accent, a muted blue-gray for secondary text,
# and the pink-red they use for live/danger indicators (mapped here to high
# elimination-probability highlighting). Font is their Inter.
ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    # Home-screen / bookmark icon and label (iOS "Add to Home Screen"). The PNG
    # lives in www/, which Shiny serves from the site root. iOS rounds the
    # corners itself, so the image is a full-bleed square.
    tags$link(rel = "apple-touch-icon", href = "apple-touch-icon.png"),
    tags$link(rel = "icon", type = "image/png", href = "apple-touch-icon.png"),
    tags$meta(name = "apple-mobile-web-app-title", content = APP_TITLE),
    tags$meta(name = "theme-color", content = "#05091D"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"),
    tags$style(HTML("
      :root {
        --sl-bg:        #05091D;
        --sl-card:      #131B38;
        --sl-card-2:    #1B2547;
        --sl-border:    #25345D;
        --sl-teal:      #00FFF9;
        --sl-teal-dim:  rgba(0, 255, 249, 0.12);
        --sl-text:      #F4F6FB;
        --sl-muted:     #98B3D6;
        --sl-danger:    #FF6482;
      }
      body {
        font-family: 'Inter', -apple-system, sans-serif;
        font-size: 13px;
        background-color: var(--sl-bg);
        color: var(--sl-text);
      }
      h1, .titlePanel, title { font-size: 1.3rem; }
      h1, h2, h3, h4 { color: var(--sl-text); }
      p, .help-block, label { color: var(--sl-muted); }

      /* Buttons */
      .btn-default {
        background-color: var(--sl-card-2);
        border-color: var(--sl-border);
        color: var(--sl-text);
      }
      .btn-default:hover, .btn-default:focus {
        background-color: var(--sl-teal-dim);
        border-color: var(--sl-teal);
        color: var(--sl-teal);
      }

      /* Weekly recap button (links to the hosted recap page) */
      .recap-btn {
        display: inline-block;
        padding: 12px 22px;
        background-color: var(--sl-teal);
        color: var(--sl-bg) !important;
        font-size: 16px;
        font-weight: 700;
        border-radius: 8px;
        text-decoration: none !important;
        box-shadow: 0 0 14px rgba(0, 255, 249, 0.35);
      }
      .recap-btn:hover, .recap-btn:focus {
        background-color: var(--sl-text);
        box-shadow: 0 0 20px rgba(0, 255, 249, 0.6);
      }

      /* Tabs */
      .nav-tabs { border-bottom-color: var(--sl-border); }
      .nav-tabs > li > a {
        color: var(--sl-muted);
        background-color: transparent;
        border: none;
        margin-right: 4px;
        text-align: center;
        line-height: 1.3;
      }
      .nav-tabs > li > a:hover {
        background-color: var(--sl-card);
        border-color: transparent;
        color: var(--sl-text);
      }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
        background-color: transparent;
        color: var(--sl-teal);
        border: none;
        border-bottom: 2px solid var(--sl-teal);
      }

      /* Select input (selectize) */
      .selectize-input, .selectize-control.single .selectize-input {
        background-color: var(--sl-card);
        border-color: var(--sl-border);
        color: var(--sl-text);
        box-shadow: none;
      }
      .selectize-input.focus { border-color: var(--sl-teal); }
      .selectize-dropdown, .selectize-dropdown-content {
        background-color: var(--sl-card);
        color: var(--sl-text);
        border-color: var(--sl-border);
      }
      .selectize-dropdown .option.active { background-color: var(--sl-teal-dim); color: var(--sl-teal); }
      .selectize-input input { color: var(--sl-text); }

      /* DataTables */
      .dataTables_wrapper { color: var(--sl-muted); }
      .dataTables_filter input {
        background-color: var(--sl-card);
        border-color: var(--sl-border);
        color: var(--sl-text);
      }
      table.dataTable { font-size: 12px; color: var(--sl-text); border-collapse: collapse !important; }
      table.dataTable.compact td, table.dataTable.compact th { padding: 4px 6px; }
      table.dataTable thead th, table.dataTable thead td {
        background-color: var(--sl-card-2);
        color: var(--sl-teal);
        border-bottom: 2px solid var(--sl-border) !important;
      }
      table.dataTable tbody tr { background-color: var(--sl-bg); }
      table.dataTable.stripe tbody tr.odd, table.dataTable.display tbody tr.odd {
        background-color: var(--sl-card);
      }
      table.dataTable tbody tr:hover, table.dataTable.hover tbody tr:hover {
        background-color: var(--sl-teal-dim) !important;
      }
      table.dataTable tbody td { border-top: 1px solid var(--sl-border); }
      table.dataTable thead .sorting:after,
      table.dataTable thead .sorting_asc:after,
      table.dataTable thead .sorting_desc:after { color: var(--sl-teal); opacity: 0.7; }

      /* FixedColumns clone (frozen leading columns while scrolling). This
         DT/DataTables version uses .dtfc-fixed-left (not the older
         .DTFC_Left*Wrapper class names) and forces a plain white background
         on those cells itself, so overrides need !important. Verified via
         direct DOM inspection, not guessed. */
      .dtfc-fixed-left { box-shadow: 2px 0 6px rgba(0,0,0,0.5); }
      th.dtfc-fixed-left { background-color: var(--sl-card-2) !important; color: var(--sl-teal) !important; }
      td.dtfc-fixed-left { background-color: var(--sl-bg) !important; }
      tr.odd td.dtfc-fixed-left { background-color: var(--sl-card) !important; }

      /* Overlay shown on top of the Standings table while the Monte Carlo
         models are (re)computing -- makes an in-progress refresh obvious
         instead of the table just silently updating row by row. */
      .sim-overlay {
        position: absolute;
        inset: 0;
        background: rgba(5, 9, 29, 0.78);
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        gap: 14px;
        border-radius: 8px;
        z-index: 5;
      }
      .sim-spinner {
        width: 36px; height: 36px;
        border: 3px solid var(--sl-border);
        border-top-color: var(--sl-teal);
        border-radius: 50%;
        animation: sim-spin 0.8s linear infinite;
      }
      @keyframes sim-spin { to { transform: rotate(360deg); } }
      .sim-progress-track {
        position: relative;
        width: 220px;
        height: 6px;
        background: var(--sl-card-2);
        border-radius: 3px;
        overflow: hidden;
      }
      .sim-progress-fill {
        position: absolute;
        top: 0; left: 0;
        height: 100%;
        background: var(--sl-teal);
        transition: width 0.3s ease;
      }
      /* Shimmering segment for whatever is currently in flight. A plain
         determinate bar reads as 'nothing happening' when only one model is
         active (it sits at 0% until the single model finishes), so the
         in-progress portion always animates -- also reused, full width, as
         the indeterminate bar while the first data fetch is running. */
      .sim-progress-active {
        position: absolute;
        top: 0;
        height: 100%;
        background: linear-gradient(90deg, rgba(0,255,249,0.15), rgba(0,255,249,0.8), rgba(0,255,249,0.15));
        background-size: 200% 100%;
        animation: sim-shimmer 1.1s linear infinite;
      }
      @keyframes sim-shimmer { from { background-position: 200% 0; } to { background-position: -200% 0; } }
      .sim-overlay-detail { color: var(--sl-muted); font-size: 12px; text-align: center; padding: 0 16px; }
      /* Non-covering 'refreshing' bar: sits on the top edge of a table wrapper
         (position: relative) while the table underneath stays fully visible. */
      .refresh-bar {
        position: absolute;
        top: 0; left: 0; right: 0;
        height: 3px;
        background: var(--sl-card-2);
        overflow: hidden;
        z-index: 6;
      }

      /* Click-a-row selection in the player tables + chart placeholder text. */
      table.dataTable tbody tr { cursor: pointer; }
      /* DT paints a selected row with an inset box-shadow + its own blue; override both. */
      table.dataTable > tbody > tr.selected > * {
        box-shadow: inset 0 0 0 9999px rgba(0, 255, 249, 0.16) !important;
        color: var(--sl-text) !important;
      }
      table.dataTable > tbody > tr.selected td.dtfc-fixed-left { background-color: var(--sl-card-2) !important; }
      .shiny-output-error-validation { color: var(--sl-muted); font-size: 12.5px; padding: 8px 2px; }
      .shiny-plot-output { margin-top: 10px; }

      /* Clickable team cells in the standings tables -> Team detail tab. */
      .team-link { cursor: pointer; }
      .team-link:hover > span:last-child > span:first-child {
        color: var(--sl-teal);
        text-decoration: underline;
      }
    ")),
    # Delegated on document (not bound per table) so it also covers the cloned
    # FixedColumns copy of the Team column and rows re-rendered by DT. Switches
    # the tab in the browser right away and tells R which team was clicked.
    tags$script(HTML("
      $(document).on('click', '.team-link', function() {
        var rid = $(this).data('roster-id');
        Shiny.setInputValue('team_click', {id: rid, nonce: Math.random()}, {priority: 'event'});
        $('#main_tabs a[data-value=\"Team detail\"]').tab('show');
      });
    "))
  ),
  titlePanel(paste0(APP_TITLE, " ", APP_ICON, " - Live Elimination Probabilities")),
  if (isTRUE(SHOW_RECAP_LINK)) div(
    style = "margin-bottom:14px;",
    tags$a(
      class = "recap-btn", href = RECAP_URL, target = "_blank", rel = "noopener",
      "\U0001F4CA Weekly recap: who got chopped, and how the odds moved \u2192"
    )
  ),
  fluidRow(
    column(
      12,
      div(
        style = "display:flex; align-items:center; gap:16px; margin-bottom:10px;",
        actionButton("refresh_now", "Refresh now"),
        textOutput("status_line"),
      )
    )
  ),
  div(style = "margin-bottom:8px;", uiOutput("week_badge", inline = TRUE)),
  p(
    style = "max-width:760px; color:var(--sl-muted); font-size:12px; margin:0 0 10px 0;",
    "Standings update automatically every minute while a game is live. ",
    "Outside of live games, use \"Refresh now\" above to pull the latest data. ",
    "Scroll to bottom for details of projection and probability calculations."
  ),
  tabsetPanel(
    id = "main_tabs",
    tabPanel(
      HTML("Standings<br/>(Approximate)"),
      br(),
      div(
        style = "position:relative; min-height:200px;",
        DTOutput("instant_table"),
        uiOutput("instant_overlay"),
        uiOutput("instant_refresh_bar")
      ),
      ridge_heading_ui("Each curve is the range of final scores a team could land on (normal curves from the approximate model)."),
      plotOutput("ridge_instant", height = "auto"),
      uiOutput("graveyard_section")
    ),
#     tabPanel(
#       HTML("Standings<br/>(Bayesian)"),
#       br(),
#       p(
#         style = "max-width:760px; color:var(--sl-muted); font-size:12.5px; margin:0 0 8px 0;",
#         "An alternative to the Approximate tab's model, for comparison. Both blend a player's pregame ",
#         "projection with their in-game pace and shrink uncertainty as the game plays out, but this one ",
#         "derives the blend and the shrinkage from one consistent statistical model (a Bayesian estimate ",
#         "of the player's true rate, updated against a compound-Poisson model of in-game scoring) instead ",
#         "of one hand-picked curve patched to roughly match another. It's new and its own constant is fit ",
#         "from only a few weeks of replay data so far -- treat it as experimental, not as a replacement ",
#         "for the Approximate tab yet."
#       ),
#       div(
#         style = "position:relative; min-height:200px;",
#         DTOutput("bayesian_table"),
#         uiOutput("bayesian_overlay"),
#         uiOutput("bayesian_refresh_bar")
#       ),
#       ridge_heading_ui("Each curve is the range of final scores a team could land on (normal curves from the Bayesian model)."),
#       plotOutput("ridge_bayesian", height = "auto")
#     ),
    tabPanel(
      HTML("Standings<br/>(Simulations)"),
      br(),
      div(
        style = "position:relative; min-height:160px;",
        DTOutput("standings_table"),
        uiOutput("sim_overlay"),
        uiOutput("sim_refresh_bar")
      ),
      ridge_heading_ui(paste0("Each curve is that team's ", N_SIMULATIONS, " simulated final scores. The simulation lets bench players swap in, ",
                              "so curves can sit right of the projected-score tick.")),
      plotOutput("ridge_sim", height = "auto")
    ),
    tabPanel(
      "Team detail",
      br(),
      selectInput("team_pick", "Team", choices = NULL, width = "300px"),
      uiOutput("team_detail_header"),
      p(
        style = "max-width:760px; color:var(--sl-muted); font-size:12.5px; margin:0 0 8px 0;",
        "Distribution: dashed = the range of outcomes before games started, filled = the range now. ",
        "Click a player for a larger chart."
      ),
      DTOutput("team_detail_table"),
      plotOutput("player_dist_team", height = "300px")
    ),
    tabPanel(
      "Live games",
      br(),
      p("Every rostered player whose game is currently in progress, league-wide."),
      DTOutput("live_games_table"),
      plotOutput("player_dist_live", height = "300px")
    )
  ),
  p(
    style = "max-width:760px; color:var(--sl-muted); font-size:12px; margin:20px 0 10px 0;",
    "Projected points match Sleeper's exactly before kickoff. Once a game is live, Sleeper doesn't publish ",
    "updated projections, so the projected totals (\"Proj. Total\"/\"Proj. Final\") become our own approximation -- blending ",
    "pregame projection with pace so far -- and can differ from Sleeper's own app. ",
    "When calcualting the best ball roster, Sleeper ignores players that have not yet played but are projected ",
    "to outperform players that have already finished. This app corrects for that so the projected number of points ",
    "reflects the most likely best ball roster. ",
    "\"(est.)\" in a player's pre-game projection means Sleeper has no projection for them (for example a backup promoted to ",
    "starter after an injury), so the app estimates one from their recent games and a typical starter at their position."
  ),
  variance_blurb_ui(),
  p(
    style = "max-width:760px; color:var(--sl-muted); font-size:12px; margin:0 0 10px 0;",
    "Best ball doesn't lock in one roster -- each week, whichever bench player actually outscores a ",
    "starter automatically swaps in. Capturing that requires simulating many possible weeks, since which ",
    "lineup ends up best depends on how each player's uncertain projection actually plays out. The ",
    "Standings (Approximate) tab skips that: it assumes the roster stays fixed to whichever ", length(ROSTER_SLOTS), " players are ",
    "best-projected right now, ignoring how uncertainty in scoring could make a different lineup the real ",
    "best one. That's what makes it computable without simulation -- but only approximately, and it ",
    "understates risk for rosters with strong bench depth. See the Standings (Simulations) tab for the probabilities ",
    "that fully account for this variability."
  )

)

fmt_time <- function(x) paste(format(x, "%H:%M:%S", tz = APP_TZ), "ET")
# ESPN's scheduled kickoff ("2026-10-30T00:15Z") as e.g. "Thu 8:15 PM ET" (NA stays NA)
kickoff_label <- function(x) {
  t <- as.POSIXct(as.character(x), format = "%Y-%m-%dT%H:%MZ", tz = "UTC")
  lab <- sub(" 0", " ", format(t, "%a %I:%M %p", tz = APP_TZ))
  ifelse(is.na(t), NA_character_, paste(lab, "ET"))
}

# Sleeper's own position color coding: QB red, RB green, WR blue, TE orange,
# bench gray. Multi-eligibility slots (FLEX, SUPER_FLEX) are spelled out as
# their eligible positions' initials (e.g. FLEX -> "WRT"), each letter in
# its own position's color, matching how Sleeper labels those slots.
POSITION_COLORS <- c(QB = "#FF5757", RB = "#4CD787", WR = "#5B9DFF", TE = "#FFA84C", BN = "#8896B0")

pos_badge <- function(pos) {
  vapply(pos, function(p) {
    color <- POSITION_COLORS[[p]] %||% "#98B3D6"
    sprintf('<span style="color:%s; font-weight:600;">%s</span>', color, p)
  }, character(1))
}

# Injury designation as a small colored "(OUT)" tag shown after a player's name; blank when healthy.
injury_badge <- function(status) {
  short <- c(Questionable = "Q", Doubtful = "D", Out = "OUT", IR = "IR", PUP = "PUP", Sus = "SUSP", DNR = "DNR")
  color <- c(Q = "#FFC857", D = "#FF9F43", OUT = "#FF6482", IR = "#FF6482", PUP = "#FF6482", SUSP = "#FF6482", DNR = "#FF6482")
  vapply(as.character(status), function(s) {
    if (is.na(s) || !nzchar(s)) return("")
    lab <- unname(short[s]); if (is.na(lab)) lab <- s
    col <- unname(color[lab]); if (is.na(col)) col <- "#98B3D6"
    sprintf('<span title="%s" style="color:%s; font-weight:600;">(%s)</span>', s, col, lab)
  }, character(1), USE.NAMES = FALSE)
}

# Player name (HTML-escaped) followed by their injury tag, e.g. "DeVonta Smith (OUT)".
player_name_cell <- function(name, status) {
  tag <- injury_badge(status)
  paste0(html_escape(name), ifelse(nzchar(tag), paste0(" ", tag), ""))
}

slot_badge <- function(slot) {
  vapply(slot, function(s) {
    # a flex slot is spelled with the initials of the positions that may fill it (FLEX -> WRT, SUPER_FLEX -> WRTQ, REC_FLEX -> WT), each in its
    # position's color; the eligible positions come from SLOT_ELIGIBILITY, so any flex slot the league uses is handled
    if (s %in% names(SLOT_ELIGIBILITY)) return(pos_badge(s))
    accepts <- names(SLOT_ELIGIBILITY)[vapply(SLOT_ELIGIBILITY, function(e) s %in% e, logical(1))]
    accepts <- intersect(c("WR", "RB", "TE", "QB"), accepts)
    if (!length(accepts)) return(pos_badge(s))
    letters_for_slot <- c(WR = "W", RB = "R", TE = "T", QB = "Q")[accepts]
    colors <- POSITION_COLORS[accepts]
    spans <- paste0(sprintf('<span style="color:%s;">%s</span>', colors, letters_for_slot), collapse = "")
    paste0('<span style="font-weight:600;">', spans, '</span>')
  }, character(1))
}

# Team owner avatar, rendered as a small circular thumbnail. `url` comes from
# Sleeper's own API (either a custom-uploaded avatar's full CDN URL, or one
# built from Sleeper's default avatar hash -- see build_roster_table()), not
# from arbitrary user text, but it's still quote-escaped before being
# embedded in an HTML attribute since a malicious league member could in
# principle set a crafted display name... avatar isn't free text, so this is
# just cheap insurance. Missing avatar -> a blank placeholder circle so the
# column stays aligned.
avatar_html <- function(url, size = 32) {
  vapply(url, function(u) {
    if (is.na(u) || !nzchar(u)) {
      return(sprintf('<div style="width:%dpx;height:%dpx;border-radius:50%%;background:var(--sl-card-2);display:inline-block;vertical-align:middle;"></div>', size, size))
    }
    safe_url <- gsub('"', "&quot;", u, fixed = TRUE)
    sprintf(
      '<img src="%s" style="width:%dpx;height:%dpx;border-radius:50%%;object-fit:cover;vertical-align:middle;">',
      safe_url, size, size
    )
  }, character(1))
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

# Avatar + team name + "@owner" combined into a single cell (replaces a
# separate Owner column everywhere a team is listed). team_name/owner_name
# are league-member-controlled free text (Sleeper lets any member set their
# own team name and display name), so unlike the badges above this needs
# real HTML escaping, not just quote-escaping for one attribute.
team_cell <- function(roster_id, url, team_name, owner_name) {
  mapply(function(rid, u, name, owner) {
    sprintf(
      paste0(
        '<span class="team-link" data-roster-id="%d" title="View team detail" style="display:inline-flex; align-items:center; gap:8px;">',
        '%s',
        '<span style="display:flex; flex-direction:column; line-height:1.3;">',
        '<span>%s</span>',
        '<span style="font-size:11px; color:var(--sl-muted); font-weight:400;">@%s</span>',
        '</span></span>'
      ),
      as.integer(rid), avatar_html(u, size = 28), html_escape(name), html_escape(owner)
    )
  }, roster_id, url, team_name, owner_name, USE.NAMES = FALSE)
}
# "Current Score" cell for the standings tables: points scored so far, with
# how many of the roster's players have started playing underneath ("12/15
# played") -- one combined column instead of two, to save width. The tables
# sort this column by a hidden numeric copy of the score (see hidden_idx).
score_players_cell <- function(score, started, total) {
  sprintf(
    paste0(
      '<span style="display:flex; flex-direction:column; line-height:1.3;">',
      '<span>%s</span>',
      '<span style="font-size:11px; color:var(--sl-muted); font-weight:400; white-space:nowrap;">%d/%d played</span>',
      '</span>'
    ),
    round(score, 1), as.integer(started), as.integer(total)
  )
}

# "Projected Final Score" cell: the current projection on top, with the team's
# pre-game projection (before any of this week's games started) underneath in
# muted text. The top number is colored red/green by the table's hidden trend
# column -- this line says what "above/below where it started" is measured
# against. Sorts by a hidden numeric proj_score column.
projected_score_cell <- function(projected, pregame, estimated = FALSE) {
  sprintf(
    paste0(
      '<span style="display:flex; flex-direction:column; line-height:1.3;">',
      '<span>%s</span>',
      '<span style="font-size:11px; color:var(--sl-muted); font-weight:400; white-space:nowrap;">pre-game %s%s</span>',
      '</span>'
    ),
    round(projected, 1), round(pregame, 1),
    ifelse(estimated,
           paste0(' <span title="Sleeper has no projection for this player, so this pre-game number is an estimate: ',
                  'his recent average blended with a typical starter at his position.">(est.)</span>'),
           "")
  )
}

server <- function(input, output, session) {
  snapshot_data <- reactiveVal(NULL)
  sim_results <- reactiveVal(list())   # named list, filled in one model at a time
  last_sim_input_hash <- reactiveVal(NULL) # digest of roster_data last actually simulated
  elim_history <- reactiveVal(NULL) # this week's stored elimination-probability snapshots (R/history.R)
  status_msg <- reactiveVal("Loading...")
  # Drive the slim "updating" bar shown over each table while a refresh is in
  # flight (data fetch, then Monte Carlo) -- as opposed to the full-cover
  # overlay, which is only for first boot when there's nothing to show yet.
  refreshing <- reactiveVal(FALSE)
  sim_busy <- reactiveVal(FALSE)

  # IMPORTANT: chaining reactiveVal updates through observeEvent() (as an
  # earlier version of this app did) does NOT actually yield control back to
  # the browser mid-chain -- Shiny processes a whole cascade of
  # reactiveVal-triggered observers within one flush cycle and only pushes
  # to the browser once that cascade goes quiet. So a chain like
  # "snapshot_data() changes -> observer runs simulations -> sim_results()
  # changes" all happens before the browser ever sees the snapshot_data
  # update, even though they're "separate" observers. The only way to
  # actually get an intermediate render is to defer the next step onto a
  # genuinely separate turn of R's event loop, via later::later() -- that's
  # what run_next_model() below does. Reads of reactive values from inside
  # a later() callback need isolate() since it's not a normal reactive
  # context.

  # Every later::later() callback below can fire AFTER the browser tab was
  # closed or reloaded (the simulation alone now runs as a ~15s chain of
  # small steps). Touching a reactiveVal then throws "Can't access reactive
  # `sim_busy`; its module session has been destroyed", so each callback
  # checks first and simply stops if its session is gone.
  fetch_pipeline <- function() {
    if (session$isClosed()) return(invisible())
    on.exit(refreshing(FALSE), add = TRUE)
    status_msg("Refreshing from Sleeper + ESPN...")
    tryCatch({
      snap <- build_live_snapshot()
      snapshot_data(snap)

      # Skip the (~6s) simulation pass entirely if nothing that actually
      # feeds it has changed since last time -- hashing roster_data (the
      # exact position/locked_points/remaining_mean/remaining_sd_* inputs
      # simulate_team_totals() uses) rather than the whole snapshot, so
      # irrelevant differences (fetched_at, unrelated metadata) don't force
      # a recompute. Re-running the Monte Carlo on identical inputs would
      # only reshuffle sampling noise, not reflect anything new.
      new_hash <- digest::digest(snap$roster_data)
      unchanged <- identical(new_hash, isolate(last_sim_input_hash())) &&
        length(isolate(sim_results())) == length(VARIANCE_MODEL_NAMES)

      # One row per rostered player, appended to logs/player_projection_log.csv,
      # for retrospective analysis (projection vs. actual, SD/CV over time).
      # Logging failures shouldn't surface as a "refresh failed" message --
      # the actual data fetch above already succeeded.
      tryCatch(log_player_snapshot(snap), error = function(e) {
        message("Projection logging failed: ", conditionMessage(e))
      })

      # Record this snapshot's Approximate elimination probabilities to
      # data/elim_history/ (skipped if the inputs match the last stored
      # snapshot) -- no chart currently reads this (see R/history.R), kept
      # for a possible future one. Like the projection log above, failing
      # here must never break the refresh.
      tryCatch({
        if (append_elim_history(snap, isolate(instant_result()), new_hash)) {
          elim_history(read_elim_history(snap$season, snap$week))
        }
      }, error = function(e) message("Elimination history logging failed: ", conditionMessage(e)))

      choices <- setNames(snap$team_meta$roster_id, snap$team_meta$team_name)
      # A fresh selectInput(choices = NULL) initializes its client-side value
      # to "" (empty string), not NULL/undefined -- that "" gets synced back
      # to the server as input$team_pick before fetch_pipeline() ever runs.
      # `%||%` only substitutes on NULL, so "" was passing straight through
      # as the `selected` value, which isn't one of the real choices; the
      # client-side select widget then silently failed to apply the whole
      # update (choices included), leaving the dropdown permanently empty.
      prev_pick <- isolate(input$team_pick)
      selected_team <- if (is.null(prev_pick) || !nzchar(prev_pick)) {
        snap$team_meta$roster_id[1]
      } else {
        prev_pick
      }
      updateSelectInput(session, "team_pick", choices = choices, selected = selected_team)

      if (unchanged) {
        status_msg(sprintf("Week %s | Data as of %s | No changes since last simulation",
                            snap$week, fmt_time(snap$fetched_at)))
      } else {
        # Deliberately NOT resetting sim_results() to list() here anymore: on a
        # refresh the Simulations table keeps showing its last numbers (with
        # the slim bar over it) and each model's entry is swapped in place as
        # it finishes. The only time results are empty is first boot, which
        # is what the full-cover overlay is for.
        sim_busy(TRUE)
        last_sim_input_hash(new_hash)
        status_msg(sprintf("Week %s | Data as of %s | Computing elimination probabilities...",
                            snap$week, fmt_time(snap$fetched_at)))
        # Defer to a fresh event-loop turn (not a direct call) so Shiny
        # actually flushes the snapshot_data() update -- and Team Detail /
        # Live Games render -- to the browser before any simulation work
        # starts, rather than bundling it all into one flush. Small positive
        # delay (rather than the default 0) as a defensive margin so the
        # callback is genuinely timer-scheduled instead of "as soon as
        # possible", which should interleave more reliably with httpuv
        # servicing the socket -- kept here as cheap insurance, though the
        # actual bug that made Team Detail/Live Games stay blank turned out
        # to be unrelated to this timing (see selected_team above: a fresh
        # selectInput(choices=NULL) starts as "" not NULL, so the old
        # `isolate(input$team_pick) %||% default` check never caught it and
        # silently passed an invalid `selected` value to updateSelectInput,
        # which made the whole update -- choices included -- fail to apply).
        sim_generation <<- sim_generation + 1L
        gen <- sim_generation
        later::later(function() run_next_model(names(ALL_STANDINGS_MODEL_NAMES), gen), delay = 0.3)
      }
    }, error = function(e) {
      status_msg(paste("Refresh failed:", conditionMessage(e)))
    })
  }

  # Runs ONE model's simulation, then schedules itself again for the next
  # model via later::later() -- each scheduled step is its own event-loop
  # turn, so the browser sees Standings fill in one model's columns at a
  # time as each ~1-4s model finishes, instead of the whole tab staying
  # blank for the full ~6s+ it takes all four to complete. (Sequential, not
  # parallel -- see USE_PARALLEL_SIMULATIONS in R/config.R for why: forking
  # multiple variance models at once multiplies memory pressure and caused
  # OOM crashes on shinyapps.io's more constrained instances. This
  # progressive rendering gets back most of the *perceived* speed of
  # parallelism without any of that risk.)
  #
  # Within a model, the work is ALSO cut into small pieces (one team's block
  # of SIM_CHUNK_SIZE draws each), each scheduled as its own event-loop turn.
  # R is single-threaded, so a model run as one ~5s call meant nothing the
  # user did in the browser -- clicking a team, switching tabs, picking from
  # the dropdown -- was even read until it finished. Between pieces the
  # server now handles any waiting input and flushes its result, so those
  # respond within a fraction of a second while the simulation keeps going.
  SIM_CHUNK_SIZE <- 250L
  sim_generation <- 0L # bumped whenever a new run starts, so a superseded run stops itself

  simulate_model_chunked <- function(snap, mid, gen, on_done, on_error) {
    sd_col <- paste0("remaining_sd_", mid)
    roster_ids <- names(snap$roster_data)
    shape_pool <- get_position_shape_pool()
    n_blocks <- ceiling(N_SIMULATIONS / SIM_CHUNK_SIZE)
    block_sizes <- rep(SIM_CHUNK_SIZE, n_blocks)
    block_sizes[n_blocks] <- N_SIMULATIONS - SIM_CHUNK_SIZE * (n_blocks - 1)
    block_starts <- cumsum(c(0, block_sizes))[seq_len(n_blocks)]
    tasks <- expand.grid(team = seq_along(roster_ids), block = seq_len(n_blocks))
    sim_matrix <- matrix(NA_real_, nrow = N_SIMULATIONS, ncol = length(roster_ids))

    step <- function(k) {
      if (session$isClosed()) return(invisible()) # tab closed mid-simulation
      if (gen != sim_generation) return(invisible()) # a newer run has taken over
      tryCatch({
        if (k > nrow(tasks)) {
          on_done(finalize_elimination_simulation(sim_matrix, roster_ids, snap$team_meta))
        } else {
          ti <- tasks$team[k]
          bi <- tasks$block[k]
          rows <- block_starts[bi] + seq_len(block_sizes[bi])
          sim_matrix[rows, ti] <<- simulate_team_totals(
            snap$roster_data[[roster_ids[ti]]], block_sizes[bi],
            sd_col = sd_col, shape_pool = shape_pool
          )
          later::later(function() step(k + 1), delay = 0.01)
        }
      }, error = on_error)
    }
    step(1)
  }

  run_next_model <- function(todo, gen = sim_generation) {
    if (session$isClosed()) return(invisible())
    snap <- isolate(snapshot_data())
    if (is.null(snap) || length(todo) == 0) return(invisible())
    if (gen != sim_generation) return(invisible())
    mid <- todo[1]

    on_error <- function(e) {
      if (session$isClosed()) return(invisible())
      sim_busy(FALSE)
      status_msg(paste("Simulation failed:", conditionMessage(e)))
    }
    on_done <- function(res) {
      current <- isolate(sim_results())
      current[[mid]] <- res
      sim_results(current)
      remaining <- todo[-1]
      n_total <- length(names(ALL_STANDINGS_MODEL_NAMES))
      if (length(remaining) > 0) {
        status_msg(sprintf("Week %s | Computed %d of %d models...",
                            snap$week, n_total - length(remaining), n_total))
        later::later(function() run_next_model(remaining, gen), delay = 0.3)
      } else {
        sim_busy(FALSE)
        status_msg(sprintf("Week %s | Updated %s | %d simulations per model",
                            snap$week, fmt_time(Sys.time()), N_SIMULATIONS))
      }
    }

    tryCatch({
      # "sleeper_reverse" isn't a per-player variance model like the other
      # three (see SLEEPER_IMPLIED_TEAM_SD in R/config.R) -- it needs its own
      # flat-SD simulation path, not the standard per-player one. (Cheap --
      # no per-draw lineup solves -- so it isn't chunked.)
      if (mid == "sleeper_reverse") {
        on_done(run_elimination_simulation_flat_sd(snap$team_meta, n_sims = N_SIMULATIONS))
      } else {
        simulate_model_chunked(snap, mid, gen, on_done, on_error)
      }
    }, error = on_error)
  }

  # Both manual and timer-driven refreshes go through here rather than calling
  # fetch_pipeline() inline: it blocks R for seconds, and the browser can't
  # see refreshing(TRUE) / the "Refreshing..." status until R returns to its
  # event loop, so run inline the loading bar would only ever appear after the
  # refresh was already over (same reason the boot fetch is deferred below).
  request_refresh <- function() {
    refreshing(TRUE)
    status_msg("Refreshing from Sleeper + ESPN...")
    later::later(function() fetch_pipeline(), delay = 0.3)
  }

  observeEvent(input$refresh_now, request_refresh()) # manual button always does a full refresh

  # Auto-refresh still ticks every REFRESH_SECONDS, but skips the expensive
  # pipeline + Monte Carlo work unless a game is actually live -- a single
  # lightweight scoreboard call decides. Always does the full refresh on
  # first load, regardless.
  #
  # Also refreshes one extra time right after a game ends: `any_game_live()`
  # flips to FALSE the instant the last live game goes final, which would
  # otherwise permanently gate off refreshing before we ever pull the true
  # final locked_points/status for those players -- both for the UI and for
  # the projection log, which wants that final snapshot for comparison
  # against pregame/in-game projections. Checking whether the *previous*
  # snapshot's `state` still shows anything "in" (not the narrower `status`
  # name -- STATUS_HALFTIME is also "in" and would otherwise read as not
  # live, which could stop autorefresh outright if every live game happened
  # to be at halftime on the same tick) catches exactly that transition, and
  # stops triggering once the next fetch confirms FINAL.
  autorefresh <- reactiveTimer(REFRESH_SECONDS * 1000)
  observeEvent(autorefresh(), {
    was_live_last_snapshot <- !is.null(snapshot_data()) &&
      any(snapshot_data()$roster_table$state == "in", na.rm = TRUE)
    # snapshot_data() is guaranteed non-NULL past the first `||` operand
    # (short-circuited), so it's safe to read week/season off it here.
    is_live_now <- !is.null(snapshot_data()) && isTRUE(any_game_live(
      snapshot_data()$season, snapshot_data()$week
    ))
    if (is.null(snapshot_data()) || is_live_now || was_live_last_snapshot) {
      request_refresh()
    } else {
      status_msg(sprintf("No games live | Last checked %s (data as of %s)",
                          fmt_time(Sys.time()),
                          if (!is.null(snapshot_data())) fmt_time(snapshot_data()$fetched_at) else "-"))
    }
    # ignoreInit: observeEvent() otherwise runs once at session start (the
    # timer returns a non-NULL value immediately), and with no snapshot yet
    # that first run called fetch_pipeline() synchronously *before the first
    # flush was sent* -- so the page sat blank for the whole fetch and no
    # boot-time loading indicator could ever be shown. It also duplicated the
    # boot fetch below. The first fetch belongs to the deferred call below;
    # this observer is for the recurring ticks (its is.null(snapshot_data())
    # branch still retries if that first fetch failed).
  }, ignoreInit = TRUE)

  # The first fetch is deferred with later::later() rather than run directly
  # inside onFlushed(): fetch_pipeline() is synchronous and blocks R for
  # seconds, and httpuv can only write the first flush to the websocket when
  # R returns to its event loop -- so run inline, the boot-time "Loading"
  # status and overlays were queued but not delivered until the fetch had
  # already finished, i.e. the user just saw a blank page and never the
  # loading state. (Confirmed by polling the DOM: status line empty for the
  # whole fetch, then everything arriving at once.) The short delay gives
  # that first flush time to actually go out. Same technique as
  # run_next_model() below uses to yield between models.
  observe({
    session$onFlushed(function() {
      later::later(function() fetch_pipeline(), delay = 0.3)
    }, once = TRUE)
  })

  output$status_line <- renderText(status_msg())

  # Bold, colored chip so the current week is unmistakable at a glance,
  # rather than buried in a sentence of body text.
  output$week_badge <- renderUI({
    req(snapshot_data())
    tags$span(
      style = paste(
        "display:inline-block; background:var(--sl-teal-dim); color:var(--sl-teal);",
        "font-weight:700; font-size:13px; padding:4px 12px; border-radius:6px;",
        "letter-spacing:0.03em; white-space:nowrap;"
      ),
      sprintf("WEEK %s", snapshot_data()$week)
    )
  })

  # (variance_blurb_ui(), defined above server(), replaces what used to be
  # an output$standings_blurb renderUI here -- see its own comment for why.)

  # Full-cover spinner + progress bar over the Standings table, for the first
  # boot only: sim_results() starts empty and is filled in one model at a time
  # by run_next_model(), so its length vs. the total model count is an exact
  # progress signal. On later refreshes results are kept (not reset), so this
  # never reappears -- the slim refresh bar below covers that case instead.
  #
  # Before the very first data fetch lands (app boot), there's nothing to
  # simulate yet, so it says "Loading league data" instead of claiming
  # simulations are running. Once data exists, the completed portion is
  # solid and the model currently being computed shimmers.
  loading_overlay_ui <- function(message, detail = NULL, done = 0, total = 1) {
    pct <- round(100 * done / total)
    div(
      class = "sim-overlay",
      div(class = "sim-spinner"),
      div(style = "color:var(--sl-text); font-weight:600; font-size:13px;", message),
      if (!is.null(detail)) div(class = "sim-overlay-detail", detail),
      div(
        class = "sim-progress-track",
        div(class = "sim-progress-fill", style = sprintf("width:%d%%;", pct)),
        div(class = "sim-progress-active",
            style = sprintf("left:%d%%; width:%d%%;", pct, 100 - pct))
      )
    )
  }

  # True only while the first fetch has failed and there's still no data --
  # otherwise a boot-time overlay would spin forever over an error.
  first_fetch_failed <- function() {
    is.null(snapshot_data()) && startsWith(status_msg(), "Refresh failed")
  }

  boot_error_ui <- function() {
    div(
      class = "sim-overlay",
      div(style = "color:var(--sl-danger); font-weight:600; font-size:13px;", "Couldn't load league data"),
      div(class = "sim-overlay-detail", "See the status line above, then use \"Refresh now\" to retry.")
    )
  }

  output$sim_overlay <- renderUI({
    if (first_fetch_failed()) return(boot_error_ui())
    if (is.null(snapshot_data())) {
      return(loading_overlay_ui("Loading league data…", "Pulling rosters, matchups and projections from Sleeper and ESPN"))
    }
    total <- length(ALL_STANDINGS_MODEL_NAMES)
    done <- length(sim_results())
    if (done >= total) return(NULL)
    loading_overlay_ui(sprintf("Running simulations… (%d of %d models)", done, total), done = done, total = total)
  })

  # Same idea for the Approximate tab, whose table needs only the data fetch
  # (no simulation), so its overlay is just the boot-time load.
  output$instant_overlay <- renderUI({
    if (first_fetch_failed()) return(boot_error_ui())
    if (!is.null(snapshot_data())) return(NULL)
    loading_overlay_ui("Loading league data…", "Pulling rosters, matchups and projections from Sleeper and ESPN")
  })

  # Slim bar along the top edge of a table while a refresh is in flight. Unlike
  # the overlays above, it doesn't cover anything: the table keeps showing its
  # current numbers and the bar just says "an update is on its way".
  refresh_bar_ui <- function() {
    div(class = "refresh-bar", div(class = "sim-progress-active", style = "left:0; width:100%;"))
  }

  # Approximate tab: only the data fetch matters (its numbers need no simulation).
  output$instant_refresh_bar <- renderUI({
    if (is.null(snapshot_data()) || !refreshing()) return(NULL)
    refresh_bar_ui()
  })

#   # Bayesian tab: identical readiness to the Approximate tab (data fetch only, no
#   # simulation) -- can't reuse instant_overlay/instant_refresh_bar directly, since Shiny
#   # doesn't allow binding the same uiOutput id twice on one page.
#   output$bayesian_overlay <- renderUI({
#     if (first_fetch_failed()) return(boot_error_ui())
#     if (!is.null(snapshot_data())) return(NULL)
#     loading_overlay_ui("Loading league data…", "Pulling rosters, matchups and projections from Sleeper and ESPN")
#   })
#   output$bayesian_refresh_bar <- renderUI({
#     if (is.null(snapshot_data()) || !refreshing()) return(NULL)
#     refresh_bar_ui()
#   })

  # Simulations tab: from the start of the fetch until the last model lands.
  # Only once results exist -- before that the full overlay is showing instead.
  output$sim_refresh_bar <- renderUI({
    if (is.null(snapshot_data())) return(NULL)
    if (length(sim_results()) < length(ALL_STANDINGS_MODEL_NAMES)) return(NULL)
    if (!(refreshing() || sim_busy())) return(NULL)
    refresh_bar_ui()
  })

  # Render these even while their tab is hidden. By default Shiny suspends
  # outputs in a hidden tab and only renders them once the browser reports
  # the tab as visible -- but R is blocked for seconds at a time by the fetch
  # and the simulation, so a user who opens the Simulations tab mid-computation
  # got a blank area (their "tab is visible" message just sat in the queue)
  # and never saw the loading bar, which only appeared if they'd clicked the
  # tab before R got busy. Rendered eagerly, the overlay is already in the
  # page when they switch over.
  for (id in c("sim_overlay", "sim_refresh_bar", "instant_overlay", "instant_refresh_bar")) {
  # (Bayesian tab disabled: add "bayesian_overlay", "bayesian_refresh_bar" back with it.)
    outputOptions(output, id, suspendWhenHidden = FALSE)
  }

  # Selected team for the Team detail tab. input$team_pick only exists after a
  # browser round trip (updateSelectInput -> client -> back), which also waits
  # behind the simulation, so until the user picks something fall back to the
  # same default (first roster) that updateSelectInput selects.
  # Two ways to pick a team -- the dropdown, or clicking a team name in a
  # standings table -- both funnel into picked_team(), so a click takes effect
  # without waiting for updateSelectInput to round-trip through the browser.
  picked_team <- reactiveVal(NULL)
  observeEvent(input$team_pick, {
    if (nzchar(input$team_pick)) picked_team(as.integer(input$team_pick))
  })
  observeEvent(input$team_click, {
    rid <- as.integer(input$team_click$id)
    picked_team(rid)
    updateSelectInput(session, "team_pick", selected = rid)
  })
  team_pick_id <- reactive({
    snap <- req(snapshot_data())
    picked_team() %||% snap$team_meta$roster_id[1]
  })

  # Teams already chopped from the guillotine (see get_eliminated_teams() in
  # R/sleeper_api.R) -- shown separately from the live Standings table since
  # they have no roster left to simulate. Hidden entirely once nobody's been
  # eliminated yet.
  output$graveyard_section <- renderUI({
    req(snapshot_data())
    eliminated <- snapshot_data()$eliminated_teams
    if (is.null(eliminated) || nrow(eliminated) == 0) return(NULL)

    tagList(
      h4(style = "margin-top:28px; color:var(--sl-muted);", "\U0001F480 Graveyard"),
      div(
        style = "display:flex; flex-direction:column; gap:8px; max-width:700px;",
        lapply(seq_len(nrow(eliminated)), function(i) {
          row <- eliminated[i, ]
          div(
            style = paste(
              "display:flex; align-items:center; gap:12px; padding:8px 14px;",
              "background:var(--sl-card); border-radius:8px; opacity:0.7;"
            ),
            HTML(avatar_html(row$avatar_url, size = 32)),
            div(
              style = "display:flex; flex-direction:column;",
              div(style = "font-weight:600;", row$team_name),
              div(style = "font-size:12px; color:var(--sl-muted);", paste0("@", row$owner_name))
            ),
            div(
              style = "margin-left:auto; text-align:right; color:var(--sl-danger); font-weight:600;",
              if (is.na(row$chopped_week)) {
                sprintf("Chopped • %.1f pts", row$final_points)
              } else {
                sprintf("Chopped Week %d • %.1f pts", row$chopped_week, row$final_points)
              }
            )
          )
        })
      )
    )
  })

  output$standings_table <- renderDT({
    results <- sim_results()
    # req(list()) does NOT block -- isTruthy() treats any non-NULL,
    # non-atomic object (lists included) as truthy regardless of length, so
    # an empty list() sails past req() and results[[model_ids[1]]] is NULL,
    # which crashes on select(). Check length explicitly instead. This also
    # makes partial progress real: build the table from whichever models
    # have actually finished so far, not all four unconditionally.
    req(length(results) > 0)
    model_ids <- names(ALL_STANDINGS_MODEL_NAMES)
    completed_ids <- model_ids[model_ids %in% names(results)]
    req(length(completed_ids) > 0)

    base <- results[[completed_ids[1]]] |>
      select(roster_id, team_name, owner_name, avatar_url, current_score, players_started, players_total, mean_score = naive_projected_total, pregame_projected_total)

    elim_cols <- character(0)
    for (mid in completed_ids) {
      label <- ALL_STANDINGS_MODEL_NAMES[[mid]]
      elim_name <- paste0("Elim % (", label, ")")
      elim_cols <- c(elim_cols, elim_name)
      # Rounded to the 0.1% the table displays: sorting on the unrounded value would treat "0.0%" rows as
      # ordered by probabilities like 1e-8, so the projected-points tie-break below would never apply.
      base[[elim_name]] <- round(results[[mid]]$elimination_prob[match(base$roster_id, results[[mid]]$roster_id)], 3)
    }

    sim_draws <- attr(results[[completed_ids[1]]], "draws") # raw simulated team totals (see finalize_elimination_simulation())

    combined <- base |>
      arrange(desc(.data[[elim_cols[1]]]), mean_score) |>   # highest elimination probability first; ties -> lowest projected points first
      mutate(
        Team = team_cell(roster_id, avatar_url, team_name, owner_name),
        # Hidden helper driving "Projected Final Score"'s red/green highlight:
        # is the team's current projected total (already reflecting locked
        # points and live-blended remaining projections) above or below
        # where it started the week (pregame_projected_total, R/pipeline.R)?
        trend = case_when(
          mean_score > pregame_projected_total ~ "up",
          mean_score < pregame_projected_total ~ "down",
          TRUE ~ "even"
        ),
        proj_score = mean_score # hidden numeric sort key for the HTML "Projected Final Score" cell
      ) |>
      rename(`Projected Final Score` = mean_score) |>
      mutate(`Projected Final Score` = projected_score_cell(proj_score, pregame_projected_total),
             `Current Score` = score_players_cell(current_score, players_started, players_total),
             `Likely Score Range` = if (is.null(sim_draws)) "" else range_cells_draws(roster_id, proj_score, sim_draws)) |>
      select(Team, `Current Score`, `Projected Final Score`, everything(), -players_total, -players_started,
             -roster_id, -avatar_url, -team_name, -owner_name, -pregame_projected_total)

    first_elim_idx <- which(names(combined) == elim_cols[1]) - 1 # DT columns are 0-indexed
    # "Current Score" is an HTML cell (score + "12/15 players"), so it sorts
    # by the hidden numeric current_score column instead of its text.
    hidden_idx <- match(c("trend", "current_score", "proj_score"), names(combined)) - 1

    datatable(
      combined,
      rownames = FALSE,
      class = "compact stripe hover",
      extensions = "FixedColumns",
      escape = -match(c("Team", "Current Score", "Projected Final Score", "Likely Score Range"), names(combined)), # all carry generated HTML
      # Initial sort: elimination probability (high to low), then projected final score (low to high --
      # the team closest to the bottom first). Column order idx for "Projected Final Score" sorts by its
      # hidden numeric proj_score (see the orderData entry below).
      options = list(paging = FALSE, searching = FALSE,
                      order = list(list(first_elim_idx, "desc"), list(match("Projected Final Score", names(combined)) - 1, "asc")),
                      scrollX = TRUE, fixedColumns = list(leftColumns = 1),
                      columnDefs = list(list(visible = FALSE, targets = hidden_idx),
                                        list(orderable = FALSE, targets = match("Likely Score Range", names(combined)) - 1),
                                        list(orderData = match("current_score", names(combined)) - 1,
                                             targets = match("Current Score", names(combined)) - 1),
                                        list(orderData = match("proj_score", names(combined)) - 1,
                                             targets = match("Projected Final Score", names(combined)) - 1))),
    ) |>
      formatPercentage(elim_cols, 1) |>
      formatStyle(elim_cols,
                  color = "#F4F6FB",
                  background = styleColorBar(c(0, 1), "rgba(255, 100, 130, 0.55)"), # Sleeper's own danger red-pink
                  backgroundSize = "98% 88%", backgroundRepeat = "no-repeat", backgroundPosition = "center") |>
      formatStyle("Projected Final Score", valueColumns = "trend",
                  color = styleEqual(c("up", "down", "even"), c("#4CD787", "var(--sl-danger)", "var(--sl-text)")))
  }, server = FALSE)

  # Closed-form/no-simulation elimination estimate (see run_elimination_instant()
  # in R/simulation.R) -- depends only on snapshot_data(), not sim_results(),
  # so unlike the real simulated table above this renders the instant a
  # refresh lands, without waiting on any Monte Carlo pass to finish.
  #
  # All four tables use renderDT(..., server = FALSE): by default DT keeps the
  # rows on the server and has the browser fetch them with a separate AJAX
  # request that only R can answer -- and R is blocked for ~5s at a time by
  # the simulation. That request only got served in time because it happened
  # to land in the 0.3s gap before the simulation starts (fine on localhost,
  # not on a slow connection, where the table would sit empty until the
  # simulation finished). With server = FALSE the rows travel inside the
  # normal update, so a table shows the moment its data does. Sorting and
  # search then run in the browser, which is trivial at ~17-80 rows.
  instant_result <- reactive({
    req(snapshot_data())
    snap <- snapshot_data()
    run_elimination_instant(snap$roster_table, snap$team_meta, sd_col = "remaining_sd_underdog")
  })

  output$instant_table <- renderDT({
    res <- instant_result()

    elim_name <- "Elim % (Approx.)"

    combined <- res |>
      mutate(elimination_prob = round(elimination_prob, 3)) |>   # the 0.1% shown -- so the projected-points tie-break can apply (see Simulations table)
      arrange(desc(elimination_prob), naive_projected_total) |>
      mutate(Team = team_cell(roster_id, avatar_url, team_name, owner_name),
             `Current Score` = score_players_cell(current_score, players_started, players_total),
             `Projected Final Score` = projected_score_cell(naive_projected_total, pregame_projected_total),
             proj_score = naive_projected_total, # hidden numeric sort key
             trend = case_when(
               naive_projected_total > pregame_projected_total ~ "up",
               naive_projected_total < pregame_projected_total ~ "down",
               TRUE ~ "even"
             ))
    combined[[elim_name]] <- combined$elimination_prob
    combined[["Likely Score Range"]] <- range_cells_normal(combined$naive_projected_total, combined$projected_sd)
    combined <- combined |> select(Team, `Current Score`, `Projected Final Score`, all_of(elim_name), `Likely Score Range`, trend, current_score, proj_score)
    hidden_idx <- match(c("trend", "current_score", "proj_score"), names(combined)) - 1 # current_score/proj_score: numeric sort keys for the HTML score cells

    datatable(
      combined,
      rownames = FALSE,
      class = "compact stripe hover",
      extensions = "FixedColumns",
      escape = -match(c("Team", "Current Score", "Projected Final Score", "Likely Score Range"), names(combined)),
      # Elimination probability (high to low), then projected final score (low to high)
      options = list(paging = FALSE, searching = FALSE,
                      order = list(list(match(elim_name, names(combined)) - 1, "desc"),
                                   list(match("Projected Final Score", names(combined)) - 1, "asc")),
                      scrollX = TRUE, fixedColumns = list(leftColumns = 1),
                      columnDefs = list(list(visible = FALSE, targets = hidden_idx),
                                        list(orderable = FALSE, targets = match("Likely Score Range", names(combined)) - 1),
                                        list(orderData = match("current_score", names(combined)) - 1,
                                             targets = match("Current Score", names(combined)) - 1),
                                        list(orderData = match("proj_score", names(combined)) - 1,
                                             targets = match("Projected Final Score", names(combined)) - 1)))
    ) |>
      formatStyle("Projected Final Score", valueColumns = "trend",
                  color = styleEqual(c("up", "down", "even"), c("#4CD787", "var(--sl-danger)", "var(--sl-text)"))) |>
      formatPercentage(elim_name, 1) |>
      formatStyle(elim_name,
                  color = "#F4F6FB",
                  background = styleColorBar(c(0, 1), "rgba(255, 100, 130, 0.55)"),
                  backgroundSize = "98% 88%", backgroundRepeat = "no-repeat", backgroundPosition = "center")
  }, server = FALSE)

#   # Comparison model (R/bayesian.R) -- same closed-form machinery as instant_result() above,
#   # just pointed at the Bayesian mean/SD/team-total columns instead of the default ones.
#   bayesian_result <- reactive({
#     req(snapshot_data())
#     snap <- snapshot_data()
#     run_elimination_instant(snap$roster_table, snap$team_meta, sd_col = "remaining_sd_bayesian",
#                             mean_col = "bayes_remaining_mean", projected_col = "bayesian_projected_total")
#   })

#   output$bayesian_table <- renderDT({
#     res <- bayesian_result()

#     elim_name <- "Elim % (Bayesian)"

#     combined <- res |>
#       mutate(elimination_prob = round(elimination_prob, 3)) |>
#       arrange(desc(elimination_prob), bayesian_projected_total) |>
#       mutate(Team = team_cell(roster_id, avatar_url, team_name, owner_name),
#              `Current Score` = score_players_cell(current_score, players_started, players_total),
#              `Projected Final Score` = projected_score_cell(bayesian_projected_total, pregame_projected_total),
#              proj_score = bayesian_projected_total, # hidden numeric sort key
#              trend = case_when(
#                bayesian_projected_total > pregame_projected_total ~ "up",
#                bayesian_projected_total < pregame_projected_total ~ "down",
#                TRUE ~ "even"
#              ))
#     combined[[elim_name]] <- combined$elimination_prob
#     combined[["Likely Score Range"]] <- range_cells_normal(combined$bayesian_projected_total, combined$projected_sd)
#     combined <- combined |> select(Team, `Current Score`, `Projected Final Score`, all_of(elim_name), `Likely Score Range`, trend, current_score, proj_score)
#     hidden_idx <- match(c("trend", "current_score", "proj_score"), names(combined)) - 1

#     datatable(
#       combined,
#       rownames = FALSE,
#       class = "compact stripe hover",
#       extensions = "FixedColumns",
#       escape = -match(c("Team", "Current Score", "Projected Final Score", "Likely Score Range"), names(combined)),
#       options = list(paging = FALSE, searching = FALSE,
#                       order = list(list(match(elim_name, names(combined)) - 1, "desc"),
#                                    list(match("Projected Final Score", names(combined)) - 1, "asc")),
#                       scrollX = TRUE, fixedColumns = list(leftColumns = 1),
#                       columnDefs = list(list(visible = FALSE, targets = hidden_idx),
#                                         list(orderable = FALSE, targets = match("Likely Score Range", names(combined)) - 1),
#                                         list(orderData = match("current_score", names(combined)) - 1,
#                                              targets = match("Current Score", names(combined)) - 1),
#                                         list(orderData = match("proj_score", names(combined)) - 1,
#                                              targets = match("Projected Final Score", names(combined)) - 1)))
#     ) |>
#       formatStyle("Projected Final Score", valueColumns = "trend",
#                   color = styleEqual(c("up", "down", "even"), c("#4CD787", "var(--sl-danger)", "var(--sl-text)"))) |>
#       formatPercentage(elim_name, 1) |>
#       formatStyle(elim_name,
#                   color = "#F4F6FB",
#                   background = styleColorBar(c(0, 1), "rgba(255, 100, 130, 0.55)"),
#                   backgroundSize = "98% 88%", backgroundRepeat = "no-repeat", backgroundPosition = "center")
#   }, server = FALSE)

  # Avatar + name + current/projected score, above the Team detail table.
  # "Current" and "Projected" both restrict to the optimal (projected)
  # starting lineup, not the full 15-man roster -- consistent with the
  # Standings tab's "Projected Final Score" (see naive_projected_total in
  # R/pipeline.R, and the comment there on why this deterministic number,
  # not the Monte Carlo mean, is the one that matches Sleeper's own figure).
  output$team_detail_header <- renderUI({
    req(snapshot_data())
    rid <- team_pick_id()
    team_df <- snapshot_data()$roster_table |> filter(roster_id == rid)
    meta <- snapshot_data()$team_meta |> filter(roster_id == rid)

    current_total <- meta$current_score # see naive_projected in R/pipeline.R

    # Same red/green trend logic as the Standings tables' "Projected Final
    # Score": is the team's current projected total above or below where it
    # started the week (meta$pregame_projected_total, R/pipeline.R)?
    projected_color <- if (meta$naive_projected_total > meta$pregame_projected_total) {
      "#4CD787"
    } else if (meta$naive_projected_total < meta$pregame_projected_total) {
      "var(--sl-danger)"
    } else {
      "var(--sl-teal)"
    }

    est_names <- team_df$full_name[team_df$projection_source == "estimate"]
    est_note <- if (length(est_names) > 0) {
      p(style = "max-width:760px; font-size:12px; color:var(--sl-muted); margin:-8px 0 12px 0;",
        paste0("(est.) next to a pre-game projection: Sleeper has no projection for ", paste(est_names, collapse = " or "),
               ", so the app estimates one from their recent games blended with a typical starter at their position."))
    }

    tagList(div(
      style = "display:flex; align-items:center; gap:14px; margin:4px 0 16px 0;",
      HTML(avatar_html(meta$avatar_url, size = 44)),
      div(
        style = "display:flex; flex-direction:column;",
        div(style = "font-weight:600; font-size:16px;", meta$team_name),
        div(style = "color:var(--sl-muted); font-size:12px;", paste0("@", meta$owner_name))
      ),
      div(
        style = "margin-left:auto; text-align:right;",
        div(style = "font-size:11px; color:var(--sl-muted); text-transform:uppercase; letter-spacing:0.04em;", "Current"),
        div(style = "font-size:20px; font-weight:700;", round(current_total, 1)),
        div(style = "font-size:11px; color:var(--sl-muted); white-space:nowrap;",
            sprintf("%d/%d played", meta$players_started, meta$players_total))
      ),
      div(
        style = "text-align:right;",
        div(style = "font-size:11px; color:var(--sl-muted); text-transform:uppercase; letter-spacing:0.04em;", "Projected"),
        div(style = paste0("font-size:20px; font-weight:700; color:", projected_color, ";"), round(meta$naive_projected_total, 1)),
        div(style = "font-size:11px; color:var(--sl-muted); white-space:nowrap;",
            paste("pre-game", round(meta$pregame_projected_total, 1)))
      )
    ), est_note)
  })

  # The Team detail rows (lineup-ordered), shared by the table and the
  # click-a-row distribution chart so a selected row index maps to the right player.
  team_detail_df <- reactive({
    req(snapshot_data())
    rid <- team_pick_id()
    team_df <- snapshot_data()$roster_table |> filter(roster_id == rid)

    # Best-ball lineup based on each player's current expected total (points
    # so far + projected remaining) -- the same "optimal 9" the Monte Carlo
    # simulation would pick if every remaining player hit their mean exactly.
    total <- team_df$locked_points + team_df$remaining_mean
    slot <- optimal_lineup_assignment(team_df$position, total)
    slot_rank <- match(slot, ROSTER_SLOTS)
    slot_rank[slot == "BN"] <- length(ROSTER_SLOTS) + 1

    if (!"kickoff_utc" %in% names(team_df)) team_df$kickoff_utc <- NA_character_
    team_df <- team_df |>
      mutate(
        kickoff = kickoff_label(kickoff_utc),
        status_label = case_when(
          is.na(status) ~ "Bye",
          status == "STATUS_FINAL" ~ "Final",
          status == "STATUS_HALFTIME" ~ "Halftime",
          status == "STATUS_SCHEDULED" ~ dplyr::coalesce(kickoff, "Upcoming"),   # when the game starts; "Upcoming" only if ESPN gave no time
          state == "in" ~ "Live", # catches STATUS_IN_PROGRESS and any other in-game status not called out above
          TRUE ~ status
        ),
        total = total,
        Slot = slot,
        slot_rank = slot_rank
      ) |>
      arrange(slot_rank, desc(total))
    team_df
  })

  output$team_detail_table <- renderDT({
    team_df <- team_detail_df()

    tbl <- team_df |>
      transmute(
        Slot = slot_badge(Slot),
        Player = player_name_cell(full_name, injury_status),
        Pos = pos_badge(position),
        Matchup = paste(nfl_team, matchup_label),
        Status = status_label,
        `Points So Far` = round(locked_points, 1)
      )
    tbl$`Proj. Total` <- projected_score_cell(team_df$total, team_df$full_week_projection, team_df$projection_source == "estimate") # + "pre-game X" underneath

    # Hidden helper column driving "Proj. Total"'s red/green highlight: is this
    # player's current full-game projected total (locked + live-blended
    # remaining) now ahead of or behind their plain pregame projection?
    # Exactly equal -- not just close -- whenever a player's game hasn't
    # started yet (blended_rate is defined to equal full_week_projection
    # with no live signal), so pregame players correctly land on "even"
    # rather than a spurious red/green from float noise.
    tbl$trend <- case_when(
      team_df$total > team_df$full_week_projection ~ "up",
      team_df$total < team_df$full_week_projection ~ "down",
      TRUE ~ "even"
    )
    # Slot holds an HTML badge ("QB", "WR", "BN"...) that would sort
    # alphabetically; sort it by lineup order (starters first, bench last)
    # via this hidden numeric column instead.
    tbl$Distribution <- player_sparkline_cells(team_df, team_df$total, tbl$trend) # dashed = pre-game, filled = now
    tbl$slot_rank <- team_df$slot_rank
    tbl$proj_total_num <- team_df$total # numeric sort key for the HTML "Proj. Total" cell
    hidden_idx <- match(c("trend", "slot_rank", "proj_total_num"), names(tbl)) - 1 # DT columnDefs targets are 0-indexed

    datatable(
      tbl, rownames = FALSE,
      class = "compact stripe hover",
      extensions = "FixedColumns",
      selection = "single", # click a player -> pre-game vs current distribution chart below
      escape = -match(c("Slot", "Player", "Pos", "Proj. Total", "Distribution"), names(tbl)), # these columns carry generated HTML (color badges; projection + pre-game line; sparkline)
      # order = list(): start in the lineup order the rows are already in;
      # clicking a column header re-sorts in the browser.
      options = list(paging = FALSE, searching = FALSE, order = list(),
                      scrollX = TRUE, fixedColumns = list(leftColumns = 2),
                      columnDefs = list(list(visible = FALSE, targets = hidden_idx),
                                        list(orderable = FALSE, targets = match("Distribution", names(tbl)) - 1),
                                        list(orderData = match("slot_rank", names(tbl)) - 1, targets = 0),
                                        list(orderData = match("proj_total_num", names(tbl)) - 1,
                                             targets = match("Proj. Total", names(tbl)) - 1)))
    ) |>
      formatStyle("Proj. Total", valueColumns = "trend",
                  color = styleEqual(c("up", "down", "even"), c("#4CD787", "var(--sl-danger)", "var(--sl-text)")))
  }, server = FALSE)

  # "Game Elapsed" cell: a whole percent, rounded DOWN so a game that is not final never reads 100% (it used to,
  # which looked finished); overtime games show "OT" and the overtime clock instead, because the share of the
  # game left no longer follows a percent of 60 minutes there.
  game_elapsed_label <- function(elapsed, period, clock) {
    ifelse(!is.na(period) & period > 4, paste("OT", clock), paste0(floor(pmin(elapsed, 0.999) * 100), "%"))
  }

  live_games_df <- reactive({
    req(snapshot_data())
    snapshot_data()$roster_table |>
      filter(state == "in") |> # not status == "STATUS_IN_PROGRESS": that misses STATUS_HALFTIME (also "in"), which silently emptied this table during any halftime
      mutate(total = locked_points + remaining_mean, pct_elapsed = elapsed_fraction) |>
      arrange(desc(total))
  })

  output$live_games_table <- renderDT({
    live_df <- live_games_df()

    tbl <- live_df |>
      transmute(
        Player = player_name_cell(full_name, injury_status),
        Pos = pos_badge(position),
        Matchup = paste(nfl_team, matchup_label),
        `Fantasy Team` = team_cell(roster_id, avatar_url, team_name, owner_name), # clickable -> Team detail (see .team-link handler in the UI head)
        `Game Elapsed` = game_elapsed_label(pct_elapsed, period, clock),
        `Points So Far` = round(locked_points, 1)
      )
    tbl$`Proj. Final` <- projected_score_cell(live_df$total, live_df$full_week_projection, live_df$projection_source == "estimate") # + "pre-game X" underneath

    # Same hidden trend helper as Team Detail's "Proj. Total": this player's current
    # full-game projected total (locked + live-blended remaining) vs. their
    # pure pregame projection.
    tbl$trend <- case_when(
      live_df$total > live_df$full_week_projection ~ "up",
      live_df$total < live_df$full_week_projection ~ "down",
      TRUE ~ "even"
    )
    tbl$Distribution <- player_sparkline_cells(live_df, live_df$total, tbl$trend) # dashed = pre-game, filled = now
    tbl$proj_final_num <- live_df$total # numeric sort key for the HTML "Proj. Final" cell
    tbl$elapsed_num <- live_df$pct_elapsed # numeric sort key for the text "Game Elapsed" cell
    hidden_idx <- match(c("trend", "proj_final_num", "elapsed_num"), names(tbl)) - 1

    datatable(
      tbl, rownames = FALSE,
      class = "compact stripe hover",
      extensions = "FixedColumns",
      selection = "single", # click a player -> pre-game vs current distribution chart below
      escape = -match(c("Player", "Pos", "Fantasy Team", "Proj. Final", "Distribution"), names(tbl)), # generated HTML (color badge; avatar + name; projection + pre-game line; sparkline)
      options = list(paging = FALSE, searching = TRUE,
                      scrollX = TRUE, fixedColumns = list(leftColumns = 2),
                      columnDefs = list(list(visible = FALSE, targets = hidden_idx),
                                        list(orderable = FALSE, targets = match("Distribution", names(tbl)) - 1),
                                        list(orderData = match("proj_final_num", names(tbl)) - 1,
                                             targets = match("Proj. Final", names(tbl)) - 1),
                                        list(orderData = match("elapsed_num", names(tbl)) - 1,
                                             targets = match("Game Elapsed", names(tbl)) - 1)))
    ) |>
      formatStyle("Proj. Final", valueColumns = "trend",
                  color = styleEqual(c("up", "down", "even"), c("#4CD787", "var(--sl-danger)", "var(--sl-text)")))
  }, server = FALSE)

  # Elimination-probability history: no chart currently uses this (the
  # earlier plotly-based "Odds over time" tab was removed -- shinyapps.io
  # didn't have plotly installed, which crashed the whole app at startup),
  # but data/elim_history/*.csv keeps accumulating via append_elim_history()
  # in fetch_pipeline() below, in case a different chart is built later.
  # Load whatever history is already on disk as soon as we know the week.
  observeEvent(snapshot_data(), {
    snap <- snapshot_data()
    elim_history(read_elim_history(snap$season, snap$week))
  })

  # --- Distribution charts ---------------------------------------------------
  # Team ridgelines (one per Standings tab). Approximate = normal curves (its
  # own closed-form assumption); Simulations = the raw simulated draws.
  ridge_height <- function() {
    n <- tryCatch(nrow(snapshot_data()$team_meta), error = function(e) N_TEAMS_FALLBACK)
    if (is.null(n) || n == 0) n <- N_TEAMS_FALLBACK
    as.integer(120 + 36 * n) # taller rows: team names wrap to two lines
  }

  # A chart drawn while its container is 0px wide (hidden tab, mid-resize) makes
  # the graphics device fail with "figure margins too large" and leaves that
  # error on screen; wait until the browser reports a real width.
  has_width <- function(id) req((session$clientData[[paste0("output_", id, "_width")]] %||% 0) > 250)

  output$ridge_instant <- renderPlot({
    has_width("ridge_instant")
    res <- instant_result()
    teams <- data.frame(roster_id = res$roster_id, team_name = res$team_name,
                        mean = res$naive_projected_total, sd = res$projected_sd,
                        elim = res$elimination_prob)
    ridgeline_plot(teams, draws = NULL)
  }, height = ridge_height, res = 110, bg = "#05091D", execOnResize = TRUE)

#   output$ridge_bayesian <- renderPlot({
#     has_width("ridge_bayesian")
#     res <- bayesian_result()
#     teams <- data.frame(roster_id = res$roster_id, team_name = res$team_name,
#                         mean = res$bayesian_projected_total, sd = res$projected_sd,
#                         elim = res$elimination_prob)
#     ridgeline_plot(teams, draws = NULL)
#   }, height = ridge_height, res = 110, bg = "#05091D", execOnResize = TRUE)

  output$ridge_sim <- renderPlot({
    has_width("ridge_sim")
    results <- sim_results()
    req(length(results) > 0)
    model_ids <- names(ALL_STANDINGS_MODEL_NAMES)
    r <- results[[model_ids[model_ids %in% names(results)][1]]]
    draws <- attr(r, "draws")
    req(!is.null(draws))
    r <- r |> arrange(desc(elimination_prob))
    teams <- data.frame(roster_id = r$roster_id, team_name = r$team_name,
                        mean = r$naive_projected_total, sd = NA_real_, elim = r$elimination_prob)
    ridgeline_plot(teams, draws = draws)
  }, height = ridge_height, res = 110, bg = "#05091D", execOnResize = TRUE)

  # Player charts: click a row in Team detail / Live games (single selection;
  # with server = FALSE the selected index is into the ORIGINAL row order, so
  # it stays correct after the user re-sorts a column).
  player_chart <- function(df, sel) {
    validate(need(length(sel) == 1, "Click a player above to compare their pre-game and current distribution."))
    row <- df[sel, ]
    validate(need(!is.na(row$status), "No game for this player this week (bye)."))
    player_dist_plot(row)
  }
  output$player_dist_team <- renderPlot({
    has_width("player_dist_team")
    player_chart(team_detail_df(), input$team_detail_table_rows_selected)
  }, res = 110, bg = "#05091D", execOnResize = TRUE)
  output$player_dist_live <- renderPlot({
    has_width("player_dist_live")
    player_chart(live_games_df(), input$live_games_table_rows_selected)
  }, res = 110, bg = "#05091D", execOnResize = TRUE)

  # Render Team detail / Live games as soon as the snapshot lands. They depend
  # only on the snapshot, not the simulation, but while suspended (the default
  # for a hidden tab) they wait for the browser to report the tab as visible --
  # a message that sits behind the simulation in R's queue, so they only
  # appeared once it finished. (Must come after the outputs are defined.)
  for (id in c("team_detail_header", "team_detail_table", "live_games_table")) {
    outputOptions(output, id, suspendWhenHidden = FALSE)
  }
}

`%||%` <- function(a, b) if (is.null(a)) b else a

shinyApp(ui, server)
