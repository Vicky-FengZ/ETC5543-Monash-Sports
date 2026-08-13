# -------------------------------------------------------------------------
# Venues Operations module
# -------------------------------------------------------------------------

# UI ----------------------------------------------------------------------
# Detailed Analysis -> Venues Operations.
# This page is separated from Site Detail because Tracking Reports contain
# much more operational and category-specific information.

mod_venues_operations_ui <- function(id) {

  ns <- NS(id)

  initial_year <- max(
    VENUES_TRACKING_YEARS
  )

  nav_panel(
    "Venues Operations",

    div(
      class = "p-4",

      div(
        class = "page-heading",
        h2("Venues Operations"),
        p(
          "Explore detailed operational patterns from the Venues Tracking Reports."
        )
      ),

      # Choose the Tracking Report year and the operational topic to inspect.
      div(
        class = "filter-bar",

        div(
          class = "filter-section-title",
          "Operational detail"
        ),

        layout_columns(

          selectInput(
            ns("year"),
            "Year",
            choices = VENUES_TRACKING_YEARS,
            selected = initial_year
          ),

          selectInput(
            ns("area"),
            "Operational area",
            choices = prototype_venues_areas(
              initial_year
            ),
            selected = prototype_venues_areas(
              initial_year
            )[[1]]
          ),

          col_widths = c(3, 5)
        ),

        tags$small(
          class = "text-secondary",
          paste(
            "Operational areas will eventually update according to",
            "the sheets available in the selected year's Tracking Report."
          )
        )
      ),

      # Current operational focus and context-specific KPI summary.
      uiOutput(
        ns("focus_heading")
      ),

      uiOutput(
        ns("kpis")
      ),

      br(),

      # Main view: month-by-month trend for the selected operational area.
      card(
        full_screen = TRUE,

        card_header(
          div(
            div(
              class = "chart-header-title",
              "Monthly Operational Trend"
            ),

            uiOutput(
              ns("main_subtitle")
            )
          )
        ),

        placeholder_chart(
          "Month-by-month operational performance"
        )
      ),

      # Supporting views: one category breakdown plus one secondary analysis.
      layout_columns(

        card(
          full_screen = TRUE,

          card_header(
            div(
              div(
                class = "chart-header-title",
                "Operational Breakdown"
              ),

              uiOutput(
                ns("breakdown_subtitle")
              )
            )
          ),

          placeholder_chart(
            "Break down the selected operational area into its main categories"
          )
        ),

        card(
          full_screen = TRUE,

          card_header(
            div(
              div(
                class = "chart-header-title",
                "Secondary Analysis"
              ),

              chart_subtitle(
                "Context-specific analysis will appear here"
              )
            )
          ),

          placeholder_chart(
            "Additional operational detail"
          )
        ),

        col_widths = c(7, 5)
      )
    )
  )
}


# Server ------------------------------------------------------------------
# Changes available operational areas by year and prepares the labels/KPIs
# for the selected Venues Tracking Report topic.

mod_venues_operations_server <- function(
  id,
  data = NULL
) {

  moduleServer(
    id,
    function(input, output, session) {

      # Prototype availability rule. Later replace this with actual sheet
      # detection from the uploaded Tracking Report.
      observeEvent(
        input$year,
        {
          req(input$year)

          choices <- prototype_venues_areas(
            input$year
          )

          updateSelectInput(
            session,
            "area",
            choices = choices,
            selected = choices[[1]]
          )
        },
        ignoreInit = TRUE
      )

      output$focus_heading <- renderUI({

        req(
          input$year,
          input$area
        )

        div(
          class = "snapshot-heading",

          h4(
            paste0(
              input$year,
              " · ",
              input$area
            )
          ),

          p(
            "Operational detail from the selected Venues Tracking Report."
          ),

          prototype_note()
        )
      })

      # KPI definitions differ by operational area because each topic has
      # different business measures.
      kpi_specs <- reactive({

        req(input$area)

        switch(
          input$area,

          "Facility Usage & Attendance" = list(
            list(
              title = "Total Attendance",
              value = "—",
              subtitle = "Placeholder",
              icon = "graph-up-arrow"
            ),
            list(
              title = "Usage Hours",
              value = "—",
              subtitle = "Placeholder",
              icon = "calendar3"
            ),
            list(
              title = "Peak Activity",
              value = "—",
              subtitle = "Placeholder",
              icon = "bar-chart-line"
            )
          ),

          "Facility Hire" = list(
            list(
              title = "Hire Revenue",
              value = "—",
              subtitle = "Placeholder",
              icon = "cash-stack"
            ),
            list(
              title = "Bookings",
              value = "—",
              subtitle = "Placeholder",
              icon = "calendar3"
            ),
            list(
              title = "Avg Revenue / Booking",
              value = "—",
              subtitle = "Placeholder",
              icon = "receipt"
            )
          ),

          "Membership" = list(
            list(
              title = "Active Members",
              value = "—",
              subtitle = "Placeholder",
              icon = "graph-up-arrow"
            ),
            list(
              title = "New Members",
              value = "—",
              subtitle = "Placeholder",
              icon = "clipboard-data"
            ),
            list(
              title = "Expiring",
              value = "—",
              subtitle = "Placeholder",
              icon = "calendar3"
            )
          ),

          "Online Sales" = list(
            list(
              title = "Online Sales",
              value = "—",
              subtitle = "Placeholder",
              icon = "cash-stack"
            ),
            list(
              title = "Transactions",
              value = "—",
              subtitle = "Placeholder",
              icon = "receipt"
            ),
            list(
              title = "Avg Transaction",
              value = "—",
              subtitle = "Placeholder",
              icon = "cash-stack"
            )
          ),

          "Workforce" = list(
            list(
              title = "Casual Hours",
              value = "—",
              subtitle = "Placeholder",
              icon = "calendar3"
            ),
            list(
              title = "Casual Wages",
              value = "—",
              subtitle = "Placeholder",
              icon = "cash-stack"
            ),
            list(
              title = "Avg Cost / Hour",
              value = "—",
              subtitle = "Placeholder",
              icon = "receipt"
            )
          ),

          "Finance" = list(
            list(
              title = "YTD Actual",
              value = "—",
              subtitle = "Placeholder",
              icon = "graph-up-arrow"
            ),
            list(
              title = "YTD Budget",
              value = "—",
              subtitle = "Placeholder",
              icon = "clipboard-data"
            ),
            list(
              title = "Variance",
              value = "—",
              subtitle = "Placeholder",
              icon = "arrow-up-right"
            )
          ),

          "Customer Operations" = list(
            list(
              title = "Service Desk Calls",
              value = "—",
              subtitle = "Placeholder",
              icon = "receipt"
            ),
            list(
              title = "Cancellations",
              value = "—",
              subtitle = "Placeholder",
              icon = "arrow-up-right"
            ),
            list(
              title = "Resolution Rate",
              value = "—",
              subtitle = "Placeholder",
              icon = "bullseye"
            )
          ),

          "Offers & Targets" = list(
            list(
              title = "Active Offers",
              value = "—",
              subtitle = "Placeholder",
              icon = "bullseye"
            ),
            list(
              title = "Target Achievement",
              value = "—",
              subtitle = "Placeholder",
              icon = "graph-up-arrow"
            ),
            list(
              title = "Variance",
              value = "—",
              subtitle = "Placeholder",
              icon = "arrow-up-right"
            )
          ),

          "Discounts" = list(
            list(
              title = "Discount Value",
              value = "—",
              subtitle = "Placeholder",
              icon = "bullseye"
            ),
            list(
              title = "Transactions",
              value = "—",
              subtitle = "Placeholder",
              icon = "receipt"
            ),
            list(
              title = "Avg Discount",
              value = "—",
              subtitle = "Placeholder",
              icon = "cash-stack"
            )
          )
        )
      })

      output$kpis <- renderUI({
        make_value_box_row(
          kpi_specs(),
          width = 1 / 3
        )
      })

      output$main_subtitle <- renderUI({

        req(
          input$year,
          input$area
        )

        chart_subtitle(
          paste0(
            input$area,
            " · January–December ",
            input$year
          )
        )
      })

      output$breakdown_subtitle <- renderUI({

        req(input$area)

        chart_subtitle(
          paste0(
            "Category-level view · ",
            input$area
          )
        )
      })

      invisible(
        list(
          year = reactive(input$year),
          area = reactive(input$area)
        )
      )
    }
  )
}
