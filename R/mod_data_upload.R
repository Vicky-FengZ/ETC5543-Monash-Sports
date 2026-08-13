# -------------------------------------------------------------------------
# Data Upload module
# -------------------------------------------------------------------------
# At this stage the module is intentionally light. It owns the Data page and
# returns file metadata so the import/validation pipeline can be connected later.

# UI ----------------------------------------------------------------------
# Central entry point for user-supplied Excel files. Reports are separated by
# purpose so each downstream module can later receive the correct data source.

mod_data_upload_ui <- function(id) {

  ns <- NS(id)

  nav_panel(
    "Data",
    icon = bs_icon("database"),

    div(
      class = "p-4",

      div(
        class = "page-heading",
        h2("Data Management"),
        p(
          "Upload and manage the reports used throughout the dashboard."
        )
      ),

      # Core reports: intended to feed Overview and Site Comparison.
      layout_columns(

        card(
          card_header(
            "Monthly Site Reports"
          ),

          card_body(
            div(
              class = "upload-zone",

              bs_icon(
                "cloud-arrow-up",
                size = "42px"
              ),

              h5(
                "Upload Monthly Site Reports"
              ),

              p(
                "Upload one or multiple Excel files."
              ),

              fileInput(
                ns("monthly_reports"),
                label = NULL,
                multiple = TRUE,
                accept = c(
                  ".xlsx",
                  ".xls"
                ),
                buttonLabel = "Browse Files",
                placeholder = "No files selected"
              ),

              tags$small(
                class = "text-secondary",
                "Supported formats: .xlsx and .xls"
              )
            )
          )
        ),

        card(
          card_header(
            "Data Status"
          ),

          card_body(
            uiOutput(
              ns("status")
            )
          )
        ),

        col_widths = c(7, 5)
      ),

      br(),

      # Detailed reports: feed the two Detailed Analysis pages.
      layout_columns(

        card(
          card_header(
            "Detailed Site Reports"
          ),

          card_body(
            fileInput(
              ns("site_reports"),
              "Upload Site Reports",
              multiple = TRUE,
              accept = c(
                ".xlsx",
                ".xls"
              )
            ),

            p(
              class = "text-secondary",
              paste(
                "These files will feed Detailed Analysis → Site Detail",
                "once the import pipeline is connected."
              )
            )
          )
        ),

        card(
          card_header(
            "Venues Tracking Reports"
          ),

          card_body(
            fileInput(
              ns("venues_reports"),
              "Upload Venues Tracking Reports",
              multiple = TRUE,
              accept = c(
                ".xlsx",
                ".xls"
              )
            ),

            p(
              class = "text-secondary",
              paste(
                "These files will feed Detailed Analysis → Venues Operations",
                "once the import pipeline is connected."
              )
            )
          )
        ),

        col_widths = c(6, 6)
      )
    )
  )
}


# Server ------------------------------------------------------------------
# At the prototype stage this module returns file metadata only. Import,
# validation and cleaning will be connected here in the next data stage.

mod_data_upload_server <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {

      # One reactive object keeps the three uploaded report groups together.
      uploaded_files <- reactive({

        list(
          monthly_reports = input$monthly_reports,
          site_reports = input$site_reports,
          venues_reports = input$venues_reports
        )
      })

      # Simple upload status summary; this will later include validation
      # messages such as detected years, sheets and formatting issues.
      output$status <- renderUI({

        files <- uploaded_files()

        count_rows <- function(x) {
          if (is.null(x)) {
            0L
          } else {
            nrow(x)
          }
        }

        monthly_n <- count_rows(
          files$monthly_reports
        )

        site_n <- count_rows(
          files$site_reports
        )

        venues_n <- count_rows(
          files$venues_reports
        )

        tagList(
          h6(
            "Current upload status"
          ),

          div(
            class = "mt-3",

            p(
              paste0(
                if (monthly_n > 0) "✓ " else "○ ",
                "Monthly Site Reports: ",
                monthly_n,
                " file(s)"
              )
            ),

            p(
              paste0(
                if (site_n > 0) "✓ " else "○ ",
                "Detailed Site Reports: ",
                site_n,
                " file(s)"
              )
            ),

            p(
              paste0(
                if (venues_n > 0) "✓ " else "○ ",
                "Venues Tracking Reports: ",
                venues_n,
                " file(s)"
              )
            )
          ),

          hr(),

          p(
            class = "text-secondary",
            paste(
              "Files are not imported into the analysis yet.",
              "The next step is to connect data_import.R,",
              "data_validation.R and data_cleaning.R."
            )
          )
        )
      })

      uploaded_files
    }
  )
}
