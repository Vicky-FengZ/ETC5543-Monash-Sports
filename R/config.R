# -------------------------------------------------------------------------
# Shared configuration
# -------------------------------------------------------------------------
# Keep app-wide constants here so modules do not repeat the same choices.

app_theme <- bslib::bs_theme(
  version = 5,
  bg = "#F6F8FB",
  fg = "#1F2937",
  primary = "#243B53",
  secondary = "#64748B"
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
