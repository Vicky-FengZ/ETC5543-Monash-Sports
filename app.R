library(shiny)
library(bslib)
library(bsicons)

# -------------------------------------------------------------------------
# Source project files
# -------------------------------------------------------------------------

source("R/config.R")
source("R/ui_helpers.R")

source("R/data_import.R")
source("R/data_validation.R")
source("R/data_cleaning.R")
source("R/calculations.R")
source("R/plots.R")

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
  title = div(
    class = "brand-title",
    "Monash Sport",
    span("Performance Dashboard")
  ),

  theme = app_theme,
  window_title = "Monash Sport Performance Dashboard",

  header = tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"
    )
  ),

  mod_overview_ui("overview"),

  mod_site_comparison_ui("site_comparison"),

  nav_menu(
    "Detailed Analysis",
    mod_site_detail_ui("site_detail"),
    mod_venues_operations_ui("venues_operations")
  ),

  mod_forecasting_ui("forecasting"),

  mod_data_upload_ui("data_upload")
)


# -------------------------------------------------------------------------
# Server
# -------------------------------------------------------------------------

server <- function(input, output, session) {

  # Data upload module will eventually become the central data source.
  # At the prototype stage it simply returns uploaded-file metadata.
  dashboard_data <- mod_data_upload_server("data_upload")

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
