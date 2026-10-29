# -------------------------------------------------------------------------
# Shared configuration
# -------------------------------------------------------------------------
# Keep app-wide constants here so modules do not repeat the same choices.

# Monash brand palette (Monash brandbook, brand elements > colours). Monash blue
# leads; the tertiary blues are accents; orange is a data-visualisation utility.
MONASH_COLOURS <- c(
  monash_blue = "#006DAE",
  blueberry = "#121256",
  electric_blue = "#285AFF",
  heritage_blue = "#ABF5F9",
  grey_1 = "#5A5A5A",
  grey_2 = "#969696",
  grey_3 = "#E6E6E6",
  orange = "#F86700"
)

# Chart roles used by R/plots.R and R/forecast_plots.R. Change a role here to
# restyle every chart consistently. Page colours live in www/styles.css (:root).
CHART_COLOURS <- c(
  primary = MONASH_COLOURS[["monash_blue"]],
  secondary = MONASH_COLOURS[["orange"]],
  tertiary = MONASH_COLOURS[["blueberry"]],
  highlight = MONASH_COLOURS[["electric_blue"]],
  text_strong = "#333333",
  text = "#474747",
  muted = MONASH_COLOURS[["grey_1"]],
  subtle = MONASH_COLOURS[["grey_2"]],
  light = "#C8C8C8",
  grid = MONASH_COLOURS[["grey_3"]]
)

# Brand categorical palette for charts with several series (sites,
# facilities, metrics). Order matters: the first colours are the most
# distinct; yellow is last because thin yellow lines are hard to read.
CHART_CATEGORICAL <- c(
  MONASH_COLOURS[["monash_blue"]],
  MONASH_COLOURS[["orange"]],
  MONASH_COLOURS[["blueberry"]],
  "#0B6554", # Forest (brand utility)
  "#7463D7", # Purple (brand utility)
  "#CD2FB0", # Ruby (brand utility)
  MONASH_COLOURS[["grey_1"]],
  "#9D6A43", # Brown (brand utility)
  "#FFBA00"  # Yellow (brand utility)
)

chart_categorical_colours <- function(n) {
  rep_len(unname(CHART_CATEGORICAL), n)
}

# Finance metrics keep the same colour in every chart.
chart_metric_colours <- function(metrics) {
  fixed <- c(
    "Income" = CHART_COLOURS[["primary"]],
    "Expenses" = CHART_COLOURS[["secondary"]],
    "Net Result" = CHART_COLOURS[["tertiary"]]
  )
  colours <- unname(fixed[metrics])
  missing <- is.na(colours)
  colours[missing] <- chart_categorical_colours(length(metrics) + 3L)[-(1:3)][seq_len(sum(missing))]
  colours
}

# Month abbreviations (%b) follow the system locale; on a Chinese or other
# non-English Windows locale they become e.g. "1月", which chart fonts cannot
# draw. Charts and labels are in English, so dates are formatted in "C".
invisible(Sys.setlocale("LC_TIME", "C"))

MONASH_FONT <- bslib::font_collection("Helvetica Neue", "Helvetica", "Arial", "sans-serif")

app_theme <- bslib::bs_theme(
  version = 5,
  bg = "#F4F7FA",
  fg = "#222222",
  primary = MONASH_COLOURS[["monash_blue"]],
  secondary = MONASH_COLOURS[["grey_1"]],
  info = MONASH_COLOURS[["electric_blue"]],
  base_font = MONASH_FONT,
  heading_font = MONASH_FONT,
  "navbar-bg" = MONASH_COLOURS[["blueberry"]],
  # Near-square controls to match the 4px panels in www/styles.css.
  "border-radius" = "4px",
  "border-radius-sm" = "3px",
  "border-radius-lg" = "4px"
)

APP_YEARS <- 2021:2025
LATEST_APP_YEAR <- max(APP_YEARS)

SITE_NAMES <- c(
  "H&F Clayton",
  "Venues Clayton",
  "Aquatics Clayton",
  "Caulfield",
  "Peninsula",
  "Participation Programs"
)

SITE_DETAIL_YEARS <- 2025

VENUES_TRACKING_YEARS <- 2022:2025

FINANCE_METRICS <- c(
  "Income",
  "Expenses",
  "Net Result"
)

OVERVIEW_METRICS <- c(
  FINANCE_METRICS,
  "Budget Variance"
)

FORECAST_HORIZONS <- c(
  "3 months",
  "6 months",
  "12 months"
)


# -------------------------------------------------------------------------
# Prototype availability rules
# -------------------------------------------------------------------------
# These rules are temporary. Once Excel import is connected, replace them
# with logic based on the sheets actually detected in uploaded workbooks.

prototype_site_detail_areas <- function(site) {

  switch(
    site,

    "H&F Clayton" = c(
      "Finance",
      "Direct Visits",
      "Indirect Visits",
      "Monthly Highlights"
    ),

    "Aquatics Clayton" = c(
      "Finance",
      "Direct Visits",
      "Indirect Visits",
      "Enrolments",
      "Monthly Highlights"
    ),

    "Participation Programs" = c(
      "Finance",
      "Direct Visits",
      "Indirect Visits",
      "Enrolments",
      "Monthly Highlights"
    ),

    c(
      "Finance",
      "Direct Visits",
      "Indirect Visits",
      "Memberships",
      "Monthly Highlights"
    )
  )
}


prototype_venues_areas <- function(year) {

  year <- as.integer(year)

  choices <- c(
    "Facility Usage & Attendance",
    "Facility Hire",
    "Membership",
    "Online Sales",
    "Workforce",
    "Finance",
    "Discounts"
  )

  if (year >= 2023) {
    choices <- c(
      choices,
      "Customer Operations"
    )
  }

  if (year >= 2024) {
    choices <- c(
      choices,
      "Offers & Targets"
    )
  }

  choices
}
