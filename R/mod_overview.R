# -------------------------------------------------------------------------
# Overview module
# -------------------------------------------------------------------------

# UI ----------------------------------------------------------------------
# Builds the Overview page. This module owns only presentation and controls;
# calculations and plots will later come from calculations.R and plots.R.

mod_overview_ui <- function(id) {

  ns <- NS(id)

  nav_panel(
    "Overview",
    icon = bs_icon("graph-up"),

    div(
      class = "p-4",

      # Page heading: explains the purpose of the Overview page.
      div(
        class = "page-heading",
        h2("Overall Performance"),
        p(
          "A high-level view of current performance and long-term trends."
        )
      ),

      # Global period filter: controls both the snapshot year and trend range.
      div(
        class = "filter-bar",

        div(
          class = "filter-section-title",
          "Analysis period"
        ),

        layout_columns(

          selectInput(
            ns("from_year"),
            "From Year",
            choices = APP_YEARS,
            selected = min(APP_YEARS)
          ),

          selectInput(
            ns("to_year"),
            "To Year",
            choices = APP_YEARS,
            selected = max(APP_YEARS)
          ),

          col_widths = c(3, 3)
        )
      ),

      # Latest-year snapshot: the selected To Year is treated as the
      # current reporting year for the KPI cards below.
      uiOutput(
        ns("snapshot_heading")
      ),

      # KPI summary: high-level financial measures for the snapshot year.
      layout_column_wrap(
        width = 1 / 4,

        value_box(
          title = "Total Income",
          value = "$12.4M",
          uiOutput(
            ns("income_note")
          ),
          showcase = bs_icon("cash-stack")
        ),

        value_box(
          title = "Total Expenses",
          value = "$9.8M",
          uiOutput(
            ns("expense_note")
          ),
          showcase = bs_icon("receipt")
        ),

        value_box(
          title = "Net Result",
          value = "$2.6M",
          uiOutput(
            ns("net_note")
          ),
          showcase = bs_icon("wallet2")
        ),

        value_box(
          title = "Budget Variance",
          value = "+4.2%",
          uiOutput(
            ns("variance_note")
          ),
          showcase = bs_icon("bullseye")
        )
      ),

      br(),

      # Main analysis row:
      #   left  = year-by-year trend for a selected metric
      #   right = Budget vs Actual context across the same period
      layout_columns(

        card(
          full_screen = TRUE,

          card_header(
            div(
              class = "chart-header-layout",

              div(
                div(
                  class = "chart-header-title",
                  "Annual Performance Trend"
                ),
                uiOutput(
                  ns("trend_period")
                )
              ),

              div(
                class = "chart-header-control",

                selectInput(
                  ns("metric"),
                  "Metric",
                  choices = OVERVIEW_METRICS,
                  selected = "Net Result"
                )
              )
            )
          ),

          placeholder_chart(
            "Year-by-year performance"
          )
        ),

        card(
          full_screen = TRUE,

          card_header(
            div(
              div(
                class = "chart-header-title",
                "Budget vs Actual by Year"
              ),
              uiOutput(
                ns("budget_period")
              )
            )
          ),

          placeholder_chart(
            "Budget performance"
          )
        ),

        col_widths = c(7, 5)
      )
    )
  )
}


# Server ------------------------------------------------------------------
# Handles period selection and dynamic labels. The `data` argument is kept
# ready for the next stage, when real uploaded data replaces placeholders.

mod_overview_server <- function(
  id,
  data = NULL
) {

  moduleServer(
    id,
    function(input, output, session) {

      # Keep From Year / To Year logically consistent.
      observeEvent(
        input$from_year,
        {
          req(input$from_year)

          from_year <- as.integer(
            input$from_year
          )

          current_to <- if (
            is.null(input$to_year)
          ) {
            max(APP_YEARS)
          } else {
            as.integer(input$to_year)
          }

          updateSelectInput(
            session,
            "to_year",
            choices = APP_YEARS[
              APP_YEARS >= from_year
            ],
            selected = max(
              from_year,
              current_to
            )
          )
        },
        ignoreInit = TRUE
      )

      observeEvent(
        input$to_year,
        {
          req(input$to_year)

          to_year <- as.integer(
            input$to_year
          )

          current_from <- if (
            is.null(input$from_year)
          ) {
            min(APP_YEARS)
          } else {
            as.integer(input$from_year)
          }

          updateSelectInput(
            session,
            "from_year",
            choices = APP_YEARS[
              APP_YEARS <= to_year
            ],
            selected = min(
              to_year,
              current_from
            )
          )
        },
        ignoreInit = TRUE
      )

      # Single source of truth for the selected period. Other outputs use
      # this reactive instead of repeating the same year calculations.
      period <- reactive({

        req(
          input$from_year,
          input$to_year
        )

        from_year <- as.integer(
          input$from_year
        )

        to_year <- as.integer(
          input$to_year
        )

        list(
          from = from_year,
          to = to_year,
          previous = to_year - 1,
          label = paste0(
            from_year,
            "–",
            to_year
          )
        )
      })

      # Snapshot heading + KPI notes communicate exactly which year the
      # summary values refer to.
      output$snapshot_heading <- renderUI({

        x <- period()

        div(
          class = "snapshot-heading",

          h4(
            paste0(
              x$to,
              " Performance Snapshot"
            )
          ),

          p(
            paste0(
              "Latest reporting year within the selected period (",
              x$label,
              ")."
            )
          ),

          prototype_note()
        )
      })

      output$income_note <- renderUI({
        x <- period()

        span(
          paste0(
            x$to,
            " · ↑ 6.8% vs ",
            x$previous
          )
        )
      })

      output$expense_note <- renderUI({
        x <- period()

        span(
          paste0(
            x$to,
            " · ↑ 3.1% vs ",
            x$previous
          )
        )
      })

      output$net_note <- renderUI({
        x <- period()

        span(
          paste0(
            x$to,
            " reporting year"
          )
        )
      })

      output$variance_note <- renderUI({
        x <- period()

        span(
          paste0(
            x$to,
            " · placeholder variance"
          )
        )
      })

      # Chart subtitles keep the scope of each chart visible to the user.
      output$trend_period <- renderUI({
        x <- period()

        chart_subtitle(
          paste0(
            "Performance over time · ",
            x$label
          )
        )
      })

      output$budget_period <- renderUI({
        x <- period()

        chart_subtitle(
          paste0(
            "Comparison across the selected period · ",
            x$label
          )
        )
      })

      invisible(
        list(
          period = period,
          metric = reactive(input$metric)
        )
      )
    }
  )
}
