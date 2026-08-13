# -------------------------------------------------------------------------
# Forecasting module
# -------------------------------------------------------------------------

# UI ----------------------------------------------------------------------
# Forward-looking page. It is kept separate from historical analysis because
# forecasts answer "what may happen next" rather than "what happened".

mod_forecasting_ui <- function(id) {

  ns <- NS(id)

  nav_panel(
    "Forecasting",
    icon = bs_icon("graph-up-arrow"),

    div(
      class = "p-4",

      div(
        class = "page-heading",
        h2("Forecasting"),
        p(
          "Explore forward-looking performance based on validated historical trends."
        )
      ),

      # Forecast controls: scope, metric and horizon. Site is only required
      # when the user selects a site-level forecast.
      div(
        class = "filter-bar",

        div(
          class = "filter-section-title",
          "Forecast settings"
        ),

        layout_columns(

          selectInput(
            ns("level"),
            "Forecast level",
            choices = c(
              "Overall",
              "Site"
            ),
            selected = "Overall"
          ),

          selectInput(
            ns("metric"),
            "Metric",
            choices = FINANCE_METRICS,
            selected = "Income"
          ),

          selectInput(
            ns("horizon"),
            "Forecast horizon",
            choices = FORECAST_HORIZONS,
            selected = "12 months"
          ),

          conditionalPanel(
            condition = "input.level == 'Site'",
            ns = ns,

            selectInput(
              ns("site"),
              "Site",
              choices = SITE_NAMES,
              selected = SITE_NAMES[[1]]
            )
          ),

          col_widths = c(3, 3, 3, 3)
        )
      ),

      # Availability note: prevents placeholder forecasts being mistaken
      # for validated model output.
      div(
        class = "snapshot-heading",

        h4("Forecast Prototype"),

        p(
          paste(
            "Forecasts will only be enabled for metrics with",
            "sufficient, continuous and consistently defined historical data."
          )
        ),

        prototype_note(
          paste(
            "Forecast values below are intentionally left as placeholders",
            "until the modelling stage"
          )
        )
      ),

      # Forecast summary: expected level, change and uncertainty range.
      uiOutput(
        ns("kpis")
      ),

      br(),

      # Main forecast chart: historical actuals + forecast + intervals.
      card(
        full_screen = TRUE,

        card_header(
          div(
            div(
              class = "chart-header-title",
              "Historical Performance & Forecast"
            ),

            uiOutput(
              ns("main_subtitle")
            )
          )
        ),

        placeholder_chart(
          "Historical actuals, forecast path and prediction intervals"
        )
      ),

      # Supporting views: seasonality and comparison against future budget.
      layout_columns(

        card(
          full_screen = TRUE,

          card_header(
            div(
              div(
                class = "chart-header-title",
                "Seasonal Pattern"
              ),

              chart_subtitle(
                "Identify recurring high and low months"
              )
            )
          ),

          placeholder_chart(
            "Historical monthly seasonality"
          )
        ),

        card(
          full_screen = TRUE,

          card_header(
            div(
              div(
                class = "chart-header-title",
                "Forecast vs Budget"
              ),

              chart_subtitle(
                "Planning comparison when future budget data is available"
              )
            )
          ),

          placeholder_chart(
            "Compare forecast expectations with planned budget"
          )
        ),

        col_widths = c(6, 6)
      )
    )
  )
}


# Server ------------------------------------------------------------------
# Currently controls prototype labels/KPIs only. Model fitting and forecast
# calculations should be added outside this module and passed in as data.

mod_forecasting_server <- function(
  id,
  data = NULL
) {

  moduleServer(
    id,
    function(input, output, session) {

      # Placeholder KPI structure. These values should stay blank until a
      # validated forecasting pipeline is connected.
      output$kpis <- renderUI({

        req(input$horizon)

        boxes <- list(
          list(
            title = paste0(
              "Next ",
              input$horizon,
              " Forecast"
            ),
            value = "—",
            subtitle = "Model not connected",
            icon = "graph-up-arrow"
          ),
          list(
            title = "Expected Change",
            value = "—",
            subtitle = "Compared with the latest equivalent period",
            icon = "arrow-up-right"
          ),
          list(
            title = "Forecast Range",
            value = "—",
            subtitle = "Prediction interval will be shown here",
            icon = "bullseye"
          )
        )

        make_value_box_row(
          boxes,
          width = 1 / 3
        )
      })

      # Builds a readable description of the active forecast request.
      output$main_subtitle <- renderUI({

        req(
          input$level,
          input$metric,
          input$horizon
        )

        location <- if (
          identical(
            input$level,
            "Site"
          ) &&
            !is.null(input$site)
        ) {
          input$site
        } else {
          "Overall"
        }

        chart_subtitle(
          paste0(
            location,
            " · ",
            input$metric,
            " · ",
            input$horizon,
            " horizon"
          )
        )
      })

      invisible(
        list(
          level = reactive(input$level),
          metric = reactive(input$metric),
          horizon = reactive(input$horizon),
          site = reactive(input$site)
        )
      )
    }
  )
}
