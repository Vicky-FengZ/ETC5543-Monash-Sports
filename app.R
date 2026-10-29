library(shiny)
library(bslib)
library(bsicons)

if (!requireNamespace("writexl", quietly = TRUE)) {
  stop(
    "Package `writexl` is required to download cleaned data as Excel.",
    call. = FALSE
  )
}

options(shiny.port = 7198)

options(shiny.maxRequestSize = 100 * 1024^2)

# -------------------------------------------------------------------------
# Source project files
# -------------------------------------------------------------------------

source("R/config.R")
source("R/ui_helpers.R")

source("R/data/load.R")
source("R/calculations.R")
source("R/plots.R")
source("R/executive_visual_registry.R")
source("R/forecasting.R")
source("R/forecast_plots.R")

source("R/mod_executive_dashboard.R")
source("R/mod_overview.R")
source("R/mod_site_comparison.R")
source("R/mod_site_detail.R")
source("R/mod_venues_operations.R")
source("R/mod_forecasting.R")
source("R/mod_data_upload.R")


# -------------------------------------------------------------------------
# UI
# -------------------------------------------------------------------------

ui <- page_navbar(
  # Official Monash reversed (white) logo for dark backgrounds, followed by
  # a plain-text unit label. Use the supplied artwork only; do not redraw it.
  title = div(
    class = "brand-title",
    tags$img(
      src = "brand/monash-logo-reversed-white.png",
      alt = "Monash University",
      class = "brand-logo"
    ),
    span(class = "brand-unit", "Sport")
  ),

  theme = app_theme,
  window_title = "Monash Sport Performance Dashboard",

  header = tagList(
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "styles.css"
      )
    ),
    # Spinners on recalculating outputs, in brand colours.
    useBusyIndicators(),
    busyIndicatorOptions(
      spinner_color = MONASH_COLOURS[["monash_blue"]],
      pulse_background = paste0(
        "linear-gradient(45deg, ",
        MONASH_COLOURS[["monash_blue"]], ", ",
        MONASH_COLOURS[["heritage_blue"]], ")"
      )
    )
  ),

  mod_executive_dashboard_ui("executive_dashboard"),

  mod_overview_ui("overview"),

  mod_site_comparison_ui("site_comparison"),

  nav_menu(
    "Detailed Analysis",
    icon = bs_icon("clipboard-data"),
    mod_site_detail_ui("site_detail"),
    mod_venues_operations_ui("venues_operations")
  ),

  mod_forecasting_ui("forecasting"),

  mod_data_upload_ui("data_upload"),

  # Loaded report years, shown on the right of the navbar.
  nav_item(
    div(
      class = "navbar-data-status-slot",
      uiOutput("data_status", inline = TRUE)
    )
  )
)


# -------------------------------------------------------------------------
# Server
# -------------------------------------------------------------------------

server <- function(input, output, session) {

  # Uploaded workbooks are parsed once, then shared with the dashboard and
  # the cleaned-data preview through this session-level reactive source.
  dashboard_data <- mod_data_upload_server("data_upload")

  output$data_status <- renderUI({
    navbar_data_status(dashboard_data()$uploads)
  })

  mod_executive_dashboard_server(
    "executive_dashboard",
    data = dashboard_data
  )

  mod_overview_server(
    "overview",
    data = dashboard_data
  )

  mod_site_comparison_server(
    "site_comparison",
    data = dashboard_data
  )

  mod_site_detail_server(
    "site_detail",
    data = dashboard_data
  )

  mod_venues_operations_server(
    "venues_operations",
    data = dashboard_data
  )

  mod_forecasting_server(
    "forecasting",
    data = dashboard_data
  )
}


shinyApp(ui, server)
