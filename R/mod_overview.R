# -------------------------------------------------------------------------
# Overview module
# -------------------------------------------------------------------------

# UI ----------------------------------------------------------------------
# Builds the Overview page. Calculations and plotting functions live in the
# shared calculations.R and plots.R files.

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

          plotOutput(
            ns("annual_performance_trend"),
            height = "330px"
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

          plotOutput(
            ns("budget_vs_actual"),
            height = "330px"
          )
        ),

        col_widths = c(7, 5)
      ),

      # Revenue opportunity keeps the Overview at an organisation-wide level.
      # Detailed site, facility and discount-type analysis belongs on the
      # Site Comparison page.
      div(
        class = "section-heading-custom mt-4",

        h4(
          class = "mb-1",
          "Revenue Opportunity Overview"
        ),

        p(
          class = "text-secondary mb-0",
          paste(
            "Track the organisation-wide relationship between actual income,",
            "discount opportunity and potential revenue."
          )
        ),

        prototype_note(
          paste(
            "Opportunity values will activate after the DISCOUNTS",
            "import is connected"
          )
        )
      ),

      # Latest-year opportunity snapshot. Actual Income is already shown in
      # the main KPI row, so these cards focus on the additional insight.
      layout_column_wrap(
        width = 1 / 3,

        value_box(
          title = "Discount Opportunity",
          value = "—",
          uiOutput(
            ns("discount_opportunity_note")
          ),
          showcase = bs_icon("percent")
        ),

        value_box(
          title = "Potential Revenue",
          value = "—",
          uiOutput(
            ns("potential_revenue_note")
          ),
          showcase = bs_icon("cash-coin")
        ),

        value_box(
          title = "Revenue Capture Rate",
          value = "—",
          uiOutput(
            ns("capture_rate_note")
          ),
          showcase = bs_icon("speedometer2")
        )
      ),

      br(),

      card(
        full_screen = TRUE,

        card_header(
          div(
            div(
              class = "chart-header-title",
              "Revenue Capture Over Time"
            ),

            uiOutput(
              ns("revenue_capture_subtitle")
            )
          )
        ),

        placeholder_chart(
          "Actual income, discount opportunity and potential revenue",
          paste(
            "Annual stacked values with revenue capture rate",
            "once Discounts are imported"
          )
        )
      )
    )
  )
}


# Server ------------------------------------------------------------------
# Handles period selection, Monthly Site Report calculations and plots.

mod_overview_server <- function(
  id,
  data = NULL
) {

  moduleServer(
    id,
    function(input, output, session) {

      monthly_reports <- reactive({
        if (is.null(data)) {
          return(NULL)
        }

        dashboard_data <- if (shiny::is.reactive(data)) {
          data()
        } else {
          data
        }

        if (inherits(dashboard_data, "monthly_site_report_data")) {
          return(dashboard_data)
        }

        if (!is.list(dashboard_data)) {
          return(NULL)
        }

        if (!is.null(dashboard_data$monthly_reports)) {
          return(dashboard_data$monthly_reports)
        }

        dashboard_data$monthly
      })

      available_years <- reactive({
        reports <- monthly_reports()

        if (
          is.null(reports) ||
            is.null(reports$finance) ||
            nrow(reports$finance) == 0L
        ) {
          return(integer())
        }

        years <- sort(unique(as.integer(reports$finance$report_year)))
        years[!is.na(years)]
      })

      annual_finance <- reactive({
        calculate_overview_annual_finance(monthly_reports())
      })

      duplicate_years <- reactive({
        overview_duplicate_report_years(monthly_reports())
      })

      previous_available_years <- reactiveVal(integer())

      observeEvent(
        available_years(),
        {
          years <- available_years()

          if (length(years) == 0L) {
            return()
          }

          current_from <- suppressWarnings(as.integer(isolate(input$from_year)))
          current_to <- suppressWarnings(as.integer(isolate(input$to_year)))
          previous_years <- previous_available_years()

          was_at_first_year <- length(previous_years) == 0L ||
            (length(current_from) == 1L && current_from == min(previous_years))
          was_at_last_year <- length(previous_years) == 0L ||
            (length(current_to) == 1L && current_to == max(previous_years))

          selected_from <- if (
            !was_at_first_year &&
              length(current_from) == 1L &&
              current_from %in% years
          ) {
            current_from
          } else {
            min(years)
          }

          selected_to <- if (
            !was_at_last_year &&
              length(current_to) == 1L &&
              current_to %in% years
          ) {
            current_to
          } else {
            max(years)
          }

          if (selected_from > selected_to) {
            selected_from <- min(years)
            selected_to <- max(years)
          }

          updateSelectInput(
            session,
            "from_year",
            choices = years,
            selected = selected_from
          )
          updateSelectInput(
            session,
            "to_year",
            choices = years,
            selected = selected_to
          )

          previous_available_years(years)
        },
        ignoreInit = FALSE
      )

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

          years <- available_years()

          if (length(years) == 0L) {
            years <- APP_YEARS
          }

          updateSelectInput(
            session,
            "to_year",
            choices = years,
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

          years <- available_years()

          if (length(years) == 0L) {
            years <- APP_YEARS
          }

          updateSelectInput(
            session,
            "from_year",
            choices = years,
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
            "Net Result comparison · ",
            x$label
          )
        )
      })

      filtered_annual_finance <- reactive({
        x <- annual_finance()
        selected_period <- period()

        x[
          x$report_year >= selected_period$from &
            x$report_year <= selected_period$to,
          ,
          drop = FALSE
        ]
      })

      validate_monthly_chart_data <- function(annual_data) {
        reports <- monthly_reports()

        validate(
          need(
            !is.null(reports),
            "Upload at least one Monthly Site Report on the Data page."
          ),
          need(
            length(duplicate_years()) == 0L,
            paste0(
              "More than one Monthly Site Report is active for: ",
              paste(duplicate_years(), collapse = ", "),
              ". Delete the superseded file on the Data page."
            )
          ),
          need(
            nrow(annual_data) > 0L,
            "No Monthly Site Report data is available for this period."
          )
        )
      }

      output$annual_performance_trend <- renderPlot(
        {
          x <- filtered_annual_finance()
          validate_monthly_chart_data(x)

          metric <- input$metric
          values <- overview_metric_values(x, metric)

          validate(
            need(
              any(is.finite(values)),
              paste("No", metric, "values are available for this period.")
            )
          )

          plot_annual_performance_trend(x, metric)
        },
        res = 96,
        bg = "white"
      )

      output$budget_vs_actual <- renderPlot(
        {
          x <- filtered_annual_finance()
          validate_monthly_chart_data(x)

          complete <- is.finite(x$net_actual) & is.finite(x$net_budget)
          validate(
            need(
              any(complete),
              paste(
                "Complete Budget and Actual values are not available",
                "for this period."
              )
            )
          )

          plot_budget_vs_actual_net_result(x)
        },
        res = 96,
        bg = "white"
      )

      # Revenue opportunity labels use the same reporting period but remain
      # separate from the standard finance metric selector.
      output$discount_opportunity_note <- renderUI({
        x <- period()

        span(
          paste0(
            x$to,
            " · Student, staff and other discounts"
          )
        )
      })

      output$potential_revenue_note <- renderUI({
        x <- period()

        span(
          paste0(
            x$to,
            " · Actual income + opportunity"
          )
        )
      })

      output$capture_rate_note <- renderUI({
        x <- period()

        span(
          paste0(
            x$to,
            " · Actual income ÷ potential revenue"
          )
        )
      })

      output$revenue_capture_subtitle <- renderUI({
        x <- period()

        chart_subtitle(
          paste0(
            "Organisation-wide revenue capture · ",
            x$label
          )
        )
      })

      invisible(
        list(
          period = period,
          metric = reactive(input$metric),
          monthly_reports = monthly_reports,
          available_years = available_years,
          annual_finance = annual_finance,
          filtered_annual_finance = filtered_annual_finance,
          duplicate_years = duplicate_years
        )
      )
    }
  )
}
