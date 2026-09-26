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
          value = textOutput(
            ns("income_value"),
            inline = TRUE
          ),
          uiOutput(
            ns("income_note")
          ),
          showcase = bs_icon("cash-stack")
        ),

        value_box(
          title = "Total Expenses",
          value = textOutput(
            ns("expense_value"),
            inline = TRUE
          ),
          uiOutput(
            ns("expense_note")
          ),
          showcase = bs_icon("receipt")
        ),

        value_box(
          title = "Net Result",
          value = textOutput(
            ns("net_value"),
            inline = TRUE
          ),
          uiOutput(
            ns("net_note")
          ),
          showcase = bs_icon("wallet2")
        ),

        value_box(
          title = "Budget Variance",
          value = textOutput(
            ns("variance_value"),
            inline = TRUE
          ),
          uiOutput(
            ns("variance_note")
          ),
          showcase = bs_icon("bullseye")
        )
      ),

           br(),

      card(
        full_screen = TRUE,

        card_header(
          div(
            div(
              class = "chart-header-title",
              "Income & Expenses Over Time"
            ),

            uiOutput(
              ns("monthly_finance_period")
            )
          )
        ),

        plotOutput(
          ns("monthly_income_expenses"),
          height = "460px"
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
            height = "480px"
          )
        ),

        card(
          full_screen = TRUE,

          card_header(
            div(
              div(
                class = "chart-header-title",
                "Budget vs Actual Net Result Over Time"
              ),
              uiOutput(
                ns("budget_period")
              )
            )
          ),

          plotOutput(
            ns("budget_vs_actual"),
            height = "480px"
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

        uiOutput(
          ns("revenue_opportunity_status")
        )
      ),

      # Latest-year opportunity snapshot. Actual Income is already shown in
      # the main KPI row, so these cards focus on the additional insight.
      layout_column_wrap(
        width = 1 / 3,

        value_box(
          title = "Discount Opportunity",
          value = textOutput(
            ns("discount_opportunity_value"),
            inline = TRUE
          ),
          uiOutput(
            ns("discount_opportunity_note")
          ),
          showcase = bs_icon("percent")
        ),

        value_box(
          title = "Potential Revenue",
          value = textOutput(
            ns("potential_revenue_value"),
            inline = TRUE
          ),
          uiOutput(
            ns("potential_revenue_note")
          ),
          showcase = bs_icon("cash-coin")
        ),

        value_box(
          title = "Revenue Capture Rate",
          value = textOutput(
            ns("capture_rate_value"),
            inline = TRUE
          ),
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

        plotOutput(
          ns("revenue_capture_plot"),
          height = "520px"
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

      annual_revenue <- reactive({
        calculate_overview_annual_revenue(monthly_reports())
      })

      monthly_finance <- reactive({
        calculate_overview_monthly_finance(monthly_reports())
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

      snapshot_finance <- reactive({
        selected_year <- period()$to
        x <- annual_finance()

        x[x$report_year == selected_year, , drop = FALSE]
      })

      snapshot_revenue <- reactive({
        selected_year <- period()$to
        x <- annual_revenue()

        x[x$report_year == selected_year, , drop = FALSE]
      })

      render_snapshot_currency <- function(column) {
        renderText({
          x <- snapshot_finance()

          if (nrow(x) != 1L || !is.finite(x[[column]][[1L]])) {
            return("—")
          }

          format_compact_currency(x[[column]][[1L]])
        })
      }

      output$income_value <- render_snapshot_currency("income_actual")
      output$expense_value <- render_snapshot_currency("expenses_actual")
      output$net_value <- render_snapshot_currency("net_actual")
      output$variance_value <- render_snapshot_currency("budget_variance")

      # Snapshot heading + KPI notes communicate exactly which year the
      # summary values refer to.
      output$snapshot_heading <- renderUI({

        selected_period <- period()
        snapshot <- snapshot_finance()
        has_data <- nrow(snapshot) == 1L

        div(
          class = "snapshot-heading",

          h4(
            if (has_data) {
              paste0(selected_period$to, " Performance Snapshot")
            } else {
              "Performance Snapshot"
            }
          ),

          p(
            if (has_data) {
              paste0(
                "Latest reporting year within the selected period (",
                selected_period$label,
                ")."
              )
            } else {
              "Upload Monthly Site Reports to display performance values."
            }
          ),

          span(
            class = paste(
              "data-source-note",
              if (has_data) "ready" else "empty"
            ),
            if (has_data) {
              "Source: Monthly Site Reports"
            } else {
              "No Monthly Site Report data loaded"
            }
          )
        )
      })

      finance_change_note <- function(column) {
        selected_period <- period()
        current <- snapshot_finance()

        if (
          nrow(current) != 1L ||
            !is.finite(current[[column]][[1L]])
        ) {
          return(
            span(paste0("No data for ", selected_period$to))
          )
        }

        history <- annual_finance()
        history <- history[
          history$report_year < selected_period$to &
            is.finite(history[[column]]),
          ,
          drop = FALSE
        ]

        if (nrow(history) == 0L) {
          return(
            span(
              paste0(
                selected_period$to,
                " · No earlier uploaded year"
              )
            )
          )
        }

        previous <- history[which.max(history$report_year), , drop = FALSE]
        previous_value <- previous[[column]][[1L]]
        change <- if (previous_value == 0) {
          NA_real_
        } else {
          current[[column]][[1L]] / previous_value - 1
        }

        span(
          paste0(
            selected_period$to,
            " · ",
            if (is.finite(change)) {
              paste0(
                format_percentage_change(change),
                " vs ",
                previous$report_year[[1L]]
              )
            } else {
              paste0(
                "Comparison unavailable vs ",
                previous$report_year[[1L]]
              )
            }
          )
        )
      }

      output$income_note <- renderUI({
        finance_change_note("income_actual")
      })

      output$expense_note <- renderUI({
        finance_change_note("expenses_actual")
      })

      output$net_note <- renderUI({
        selected_period <- period()
        snapshot <- snapshot_finance()

        span(
          if (
            nrow(snapshot) == 1L &&
              is.finite(snapshot$net_actual[[1L]])
          ) {
            paste0(
              selected_period$to,
              " · Actual Income − Actual Expenses"
            )
          } else {
            paste0("No data for ", selected_period$to)
          }
        )
      })

      output$variance_note <- renderUI({
        selected_period <- period()
        snapshot <- snapshot_finance()

        span(
          if (
            nrow(snapshot) == 1L &&
              is.finite(snapshot$budget_variance[[1L]])
          ) {
            paste0(
              selected_period$to,
              " · ",
              if (snapshot$budget_variance[[1L]] >= 0) {
                "Favourable vs budget"
              } else {
                "Unfavourable vs budget"
              }
            )
          } else {
            paste0("No data for ", selected_period$to)
          }
        )
      })

      # Chart subtitles keep the scope of each chart visible to the user.

            output$monthly_finance_period <- renderUI({
        x <- period()

        chart_subtitle(
          paste0(
            "Monthly Actual Income and Expenses · ",
            x$label
          )
        )
      })

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
            "Monthly Budget vs Actual Net Result · ",
            x$label
          )
        )
      })

            filtered_monthly_finance <- reactive({
        x <- monthly_finance()
        selected_period <- period()

        x[
          x$report_year >= selected_period$from &
            x$report_year <= selected_period$to,
          ,
          drop = FALSE
        ]
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

      filtered_annual_revenue <- reactive({
        x <- annual_revenue()
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

            output$monthly_income_expenses <- renderPlot(
        {
          x <- filtered_monthly_finance()

          validate_monthly_chart_data(x)

          available_values <- is.finite(x$income_actual) |
            is.finite(x$expenses_actual)

          validate(
            need(
              any(available_values),
              paste(
                "No monthly Income or Expenses values are available",
                "for this period."
              )
            )
          )

          plot_monthly_income_expenses(x)
        },
        res = 96,
        bg = "white"
      )

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
    x <- filtered_monthly_finance()

    validate_monthly_chart_data(x)

    complete <- is.finite(x$net_actual) &
      is.finite(x$net_budget)

    validate(
      need(
        any(complete),
        paste(
          "Complete monthly Budget and Actual Net Result values",
          "are not available for this period."
        )
      )
    )

    plot_monthly_budget_vs_actual_net_result(x)
  },
  res = 96,
  bg = "white"
)

      render_revenue_currency <- function(column) {
        renderText({
          x <- snapshot_revenue()

          if (nrow(x) != 1L || !is.finite(x[[column]][[1L]])) {
            return("—")
          }

          format_compact_currency(x[[column]][[1L]])
        })
      }

      output$discount_opportunity_value <- render_revenue_currency(
        "discount_opportunity"
      )
      output$potential_revenue_value <- render_revenue_currency(
        "potential_revenue"
      )
      output$capture_rate_value <- renderText({
        x <- snapshot_revenue()

        if (nrow(x) != 1L || !is.finite(x$capture_rate[[1L]])) {
          return("—")
        }

        format_percentage(x$capture_rate[[1L]])
      })

      output$revenue_opportunity_status <- renderUI({
        x <- filtered_annual_revenue()

        if (nrow(x) == 0L) {
          return(
            span(
              class = "data-source-note empty",
              "Upload Monthly Site Reports to activate this section"
            )
          )
        }

        failed_years <- x$report_year[x$validation_status != "pass"]

        if (length(failed_years) == 0L) {
          span(
            class = "data-source-note ready",
            paste0(
              "Total Revenue validation passed · ",
              nrow(x),
              if (nrow(x) == 1L) " reporting year" else " reporting years"
            )
          )
        } else {
          span(
            class = "data-source-note warning",
            paste0(
              "Calculated totals used · review source validation for ",
              paste(failed_years, collapse = ", ")
            )
          )
        }
      })

      revenue_snapshot_note <- function(text) {
        selected_period <- period()
        x <- snapshot_revenue()

        span(
          if (nrow(x) == 1L) {
            paste0(selected_period$to, " · ", text)
          } else {
            paste0("No data for ", selected_period$to)
          }
        )
      }

      output$discount_opportunity_note <- renderUI({
        revenue_snapshot_note(
          "Student, staff and other discounts"
        )
      })

      output$potential_revenue_note <- renderUI({
        revenue_snapshot_note(
          "Calculated Actual Income + opportunity"
        )
      })

      output$capture_rate_note <- renderUI({
        revenue_snapshot_note(
          "Actual Income ÷ Potential Revenue"
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

      output$revenue_capture_plot <- renderPlot(
        {
          x <- filtered_annual_revenue()
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
              nrow(x) > 0L,
              paste(
                "Revenue opportunity data is not available for",
                "the selected period."
              )
            ),
            need(
              any(
                is.finite(x$actual_income) &
                  is.finite(x$discount_opportunity) &
                  is.finite(x$capture_rate)
              ),
              "Complete revenue opportunity values are not available."
            )
          )

          plot_revenue_capture_over_time(x)
        },
        res = 96,
        bg = "white"
      )

      invisible(
        list(
          period = period,
          metric = reactive(input$metric),
          monthly_reports = monthly_reports,
          available_years = available_years,
          annual_finance = annual_finance,
          annual_revenue = annual_revenue,
          filtered_annual_finance = filtered_annual_finance,
          filtered_annual_revenue = filtered_annual_revenue,
          snapshot_finance = snapshot_finance,
          snapshot_revenue = snapshot_revenue,
          duplicate_years = duplicate_years
        )
      )
    }
  )
}
