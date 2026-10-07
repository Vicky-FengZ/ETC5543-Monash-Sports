# Monthly finance forecasts and operational planning scenarios.

mod_forecasting_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Forecasting", icon = bs_icon("graph-up-arrow"),
    div(
      class = "p-4 forecasting-page",
      div(class = "page-heading", h2("Forecasting & Planning"),
          p("Plan Income and Expenses, explore wage and price changes, and find the income needed to cover costs.")),
      div(
        class = "filter-bar",
        div(class = "filter-section-title", "1. Choose your planning period"),
        layout_columns(
          selectInput(ns("site"), "Scope", choices = "Overall", selected = "Overall"),
          selectInput(ns("from_year"), "Use history from", choices = character()),
          selectInput(ns("cutoff"), "Last actual month", choices = character()),
          selectInput(ns("horizon"), "Forecast horizon", setNames(c(3, 6, 12), FORECAST_HORIZONS), selected = 12),
          col_widths = breakpoints(xs = c(6, 6, 6, 6), lg = c(3, 3, 3, 3))
        ),
        div(class = "forecast-secondary-settings",
          selectInput(ns("method"), "Baseline model", c("Automatic (compare backtests)" = "auto", setNames(names(FORECAST_MODEL_NAMES), FORECAST_MODEL_NAMES)), selected = "auto"),
          checkboxInput(ns("include_trailing_zeros"), "Include final zero months as confirmed actuals", FALSE)
        )
      ),
      uiOutput(ns("data_health")),
      div(class = "section-heading-custom", h4("2. Set your business assumptions"),
          p(class = "text-secondary", "Changes apply once from the effective month. Wage amounts are annual employer costs; scenario percentages do not compound every month.")),
      layout_columns(
        card(
          class = "forecast-assumption-card",
          card_header(div(class = "forecast-card-heading", bs_icon("people"), "Expenses & wages")),
          selectInput(ns("wage_mode"), "How will wages change?", c(
            "Total annual payroll increase ($)" = "annual_amount",
            "Annual increase per employee / FTE ($)" = "per_employee",
            "Payroll increase (%)" = "percentage"
          )),
          conditionalPanel("input.wage_mode != 'percentage'", ns = ns,
            numericInput(ns("wage_amount"), "Annual wage increase ($)", value = 0, min = 0, step = 1000)
          ),
          conditionalPanel("input.wage_mode == 'per_employee'", ns = ns,
            numericInput(ns("employee_count"), "Employees / FTE affected", value = 1, min = 0, step = 1)
          ),
          conditionalPanel("input.wage_mode == 'percentage'", ns = ns,
            numericInput(ns("wage_pct"), "Payroll increase (%)", value = 0, min = 0, max = 100, step = 0.5)
          ),
          layout_columns(
            numericInput(ns("wage_share"), "Payroll share of Expenses (%)", value = NA_real_, min = 0, max = 100, step = 1),
            numericInput(ns("other_cost_pct"), "Non-payroll cost change (%)", value = 0, min = -100, max = 200, step = 0.5),
            col_widths = breakpoints(xs = c(12, 12), md = c(6, 6))
          ),
          p(class = "forecast-help", "Payroll is not separately identified in Monthly Site Reports. Enter your own payroll share when using a wage percentage or a non-payroll cost change. Fixed annual amounts are spread across 12 months.")
        ),
        card(
          class = "forecast-assumption-card",
          card_header(div(class = "forecast-card-heading", bs_icon("tags"), "Income & pricing")),
          layout_columns(
            numericInput(ns("price_pct"), "Price increase (%)", value = 0, min = 0, max = 500, step = 0.5),
            numericInput(ns("price_share"), "Income that can be repriced (%)", value = 100, min = 0, max = 100, step = 5),
            col_widths = breakpoints(xs = c(12, 12), md = c(6, 6))
          ),
          numericInput(ns("volume_pct"), "Demand / volume change (%)", value = 0, min = -99, max = 200, step = 1),
          selectInput(ns("effective_month"), "Changes take effect from", choices = c("First forecast month" = 1), selected = 1),
          p(class = "forecast-help", "The initial 100% repriceable share is an editable assumption. Lower it for fixed contracts or other income that cannot change. Demand is your assumption, not a measured response to price. Volume changes affect income only; enter any associated cost change separately.")
        ),
        col_widths = breakpoints(xs = c(12, 12), lg = c(6, 6))
      ),
      uiOutput(ns("scenario_notice")),
      div(class = "section-heading-custom", h4("3. Compare the plan with the baseline"),
          uiOutput(ns("period_label"))),
      uiOutput(ns("kpis")),
      layout_columns(
        card(full_screen = TRUE,
             card_header(div(div(class = "chart-header-title", "Income: actuals, baseline & scenario"),
                             uiOutput(ns("income_model")))),
             plotOutput(ns("income_plot"), height = "400px")),
        card(full_screen = TRUE,
             card_header(div(div(class = "chart-header-title", "Expenses: actuals, baseline & scenario"),
                             uiOutput(ns("expenses_model")))),
             plotOutput(ns("expenses_plot"), height = "400px")),
        col_widths = breakpoints(xs = c(12, 12), lg = c(6, 6))
      ),
      p(class = "forecast-help forecast-interval-note", "Shading shows monthly 80% / 95% baseline prediction intervals when estimable. Scenario results are conditional on your inputs. The shading does not include uncertainty about wage, price or demand assumptions."),
      card(
        full_screen = TRUE,
        card_header(div(class = "forecast-card-heading", bs_icon("bullseye"), "Income needed to cover costs")),
        layout_columns(
          uiOutput(ns("break_even_explanation")),
          plotOutput(ns("price_plot"), height = "350px"),
          col_widths = breakpoints(xs = c(12, 12), lg = c(4, 8))
        )
      ),
      card(
        class = "forecast-details-card",
        card_header("Monthly plan & model checks"),
        navset_card_tab(
          nav_panel("Monthly plan",
            div(class = "forecast-table-actions", downloadButton(ns("download_plan"), "Download monthly plan (CSV)"),
                downloadButton(ns("download_assumptions"), "Download assumptions (CSV)")),
            p(class = "forecast-help", "Income needed is equal to that month's scenario Expenses. The minimum price KPI covers the total selected period, not necessarily every individual month."),
            div(class = "forecast-table-scroll", tableOutput(ns("monthly_table")))),
          nav_panel("Model checks", uiOutput(ns("validation_note")),
            div(class = "forecast-table-scroll", tableOutput(ns("model_table")))),
          nav_panel("How it works",
            p("Baseline models use monthly Actual Income and Expenses from Monthly Site Reports. Organisation totals are preferred over site totals to avoid counting the same activity twice."),
            p("Automatic selection compares Seasonal naive, Naive and, with enough history, additive Holt-Winters on the same rolling-origin folds. The lowest out-of-sample RMSE wins separately for Income and Expenses. A missing month splits the training period."),
            p("Scenario income = baseline income x demand factor x (1 + repriceable share x price increase). Scenario expenses = baseline expenses + extra payroll costs + non-payroll cost changes."),
            p("Changes are additional to the baseline, which already reflects historical costs and prices. Do not enter a previously observed increase a second time. Financial baselines and interval lower bounds are constrained to zero."),
            p("Minimum price increase uses the total forecast period, includes any existing income shortfall, and assumes demand remains at your chosen level. It is a conditional planning threshold, not a prediction of what customers will pay."),
            tags$a(href = "https://otexts.com/fpp3/", target = "_blank", rel = "noopener", "Method reference: Forecasting: Principles and Practice (3rd edition)"))
        )
      )
    )
  )
}

mod_forecasting_server <- function(id, data = NULL) {
  moduleServer(id, function(input, output, session) {
    default <- function(value, fallback) if (is.null(value) || !length(value)) fallback else value
    reports <- reactive({
      forecast_monthly_reports(if (shiny::is.reactive(data)) data() else data)
    })
    previous_cutoff <- reactiveVal(NULL)
    observeEvent(reports(), {
      finance <- reports()$finance
      sites <- c("Overall", forecast_available_sites(reports()))
      current_site <- isolate(input$site)
      updateSelectInput(session, "site", choices = sites,
                        selected = if (length(current_site) == 1L && current_site %in% sites) current_site else "Overall")
      if (!is.data.frame(finance) || !nrow(finance)) {
        updateSelectInput(session, "from_year", choices = character())
        updateSelectInput(session, "cutoff", choices = character())
        previous_cutoff(NULL)
        return()
      }
      dates <- sort(unique(as.Date(finance$date[finance$is_summary %in% TRUE &
                               finance$value_type %in% "Actual" & !is.na(finance$date)])))
      if (!length(dates)) return()
      years <- sort(unique(as.integer(format(dates, "%Y"))))
      current_year <- suppressWarnings(as.integer(isolate(input$from_year)))
      selected_year <- if (length(current_year) == 1L && current_year %in% years) current_year else min(years)
      updateSelectInput(session, "from_year", choices = years, selected = selected_year)
      current <- isolate(input$cutoff)
      selected <- if (length(current) == 1L && current %in% as.character(dates) &&
                      !identical(current, previous_cutoff())) current else as.character(max(dates))
      updateSelectInput(session, "cutoff", choices = setNames(as.character(dates), forecast_month_label(dates)), selected = selected)
      previous_cutoff(as.character(max(dates)))
    }, ignoreNULL = FALSE)

    prepared <- reactive({
      cutoff <- input$cutoff
      if (is.null(cutoff) || !nzchar(cutoff)) cutoff <- NULL
      year <- suppressWarnings(as.integer(input$from_year))
      if (!length(year)) year <- NULL
      prepare_forecast_history(reports(), default(input$site, "Overall"), cutoff, year,
                               isTRUE(input$include_trailing_zeros))
    })
    # Business input edits do not refit models or reparse uploaded Excel.
    baseline <- reactive({
      fit_finance_forecasts(prepared(), as.integer(default(input$horizon, 12)), default(input$method, "auto"))
    })
    observeEvent(baseline(), {
      x <- baseline()$forecast
      if (!is.null(baseline()$error) || !nrow(x)) return()
      dates <- x$date[x$metric == "income"]
      selected <- suppressWarnings(as.integer(isolate(input$effective_month)))
      if (length(selected) != 1L || !selected %in% seq_along(dates)) selected <- 1L
      updateSelectInput(session, "effective_month", choices = setNames(seq_along(dates), forecast_month_label(dates)), selected = selected)
    })
    scenario <- reactive({
      tryCatch(calculate_finance_scenario(
        baseline(), wage_mode = default(input$wage_mode, "annual_amount"),
        wage_amount = default(input$wage_amount, 0), wage_pct = default(input$wage_pct, 0),
        employee_count = default(input$employee_count, 1), wage_share = default(input$wage_share, NA_real_),
        other_cost_pct = default(input$other_cost_pct, 0), price_pct = default(input$price_pct, 0),
        price_share = default(input$price_share, 100), volume_pct = default(input$volume_pct, 0),
        effective_month = as.integer(default(input$effective_month, 1))
      ), error = function(e) list(error = conditionMessage(e)))
    })
    valid_scenario <- function() {
      result <- scenario()
      validate(need(is.null(result$error), result$error))
      result
    }
    observeEvent(input$use_minimum_price, {
      x <- scenario()
      if (!is.null(x$error) || !is.finite(x$min_price_increase)) return()
      rate <- max(0, ceiling(x$min_price_increase * 1000 - 1e-9) / 10)
      if (rate <= 500) updateNumericInput(session, "price_pct", value = rate)
    })
    output$data_health <- renderUI({
      p <- prepared()
      b <- baseline()
      if (!is.null(p$error)) {
        return(div(class = "forecast-status forecast-status-empty", bs_icon("info-circle"), p$error))
      }
      training <- p$training
      div(class = "forecast-status",
        div(class = "forecast-status-title", bs_icon("calendar-check"), "Planning basis"),
        p(paste(nrow(training), "continuous months:", forecast_month_label(min(training$date)),
                "to", forecast_month_label(max(training$date)), "| Source: Monthly Site Reports")),
        if (!is.null(b$error)) p(class = "forecast-warning", b$error),
        if (length(c(p$notes, b$notes))) tags$ul(lapply(unique(c(p$notes, b$notes)), tags$li))
      )
    })
    output$scenario_notice <- renderUI({
      result <- scenario()
      if (is.null(result$error) || !is.null(baseline()$error)) return(NULL)
      div(class = "forecast-status forecast-status-warning", bs_icon("exclamation-triangle"), result$error)
    })
    output$period_label <- renderUI({
      b <- baseline()
      if (!is.null(b$error)) return(p(class = "text-secondary", "Results appear after enough monthly data are uploaded."))
      dates <- b$forecast$date
      p(class = "text-secondary", paste(forecast_month_label(min(dates)), "to", forecast_month_label(max(dates)),
                                        "|", default(input$site, "Overall"), "| All values in AUD"))
    })
    output$kpis <- renderUI({
      result <- scenario()
      available <- is.null(result$error)
      value <- function(name, percentage = FALSE) {
        if (!available) return("—")
        v <- result[[name]]
        if (!is.finite(v)) return("Not achievable")
        if (percentage) forecast_required_rate(v) else format_compact_currency(v, digits = 2L)
      }
      boxes <- list(
        list(title = "Scenario Income", value = value("income"),
             subtitle = if (available) paste("Baseline:", format_compact_currency(result$baseline_income, digits = 2L)) else "Upload data to start", icon = "cash-stack"),
        list(title = "Scenario Expenses", value = value("expenses"),
             subtitle = if (available) paste("Extra wages:", format_compact_currency(result$extra_wage_cost)) else "Enter your cost assumptions", icon = "receipt"),
        list(title = "Minimum Income Growth", value = value("min_income_growth", TRUE),
             subtitle = "Compared with baseline income; covers total-period costs", icon = "graph-up-arrow"),
        list(title = "Minimum Price Increase", value = value("min_price_increase", TRUE),
             subtitle = "On repriceable income, at your assumed demand", icon = "tags")
      )
      do.call(layout_columns, c(
        list(col_widths = breakpoints(xs = c(12, 12, 12, 12),
                                     md = c(6, 6, 6, 6), xl = c(3, 3, 3, 3))),
        lapply(boxes, function(box) value_box(
          title = box$title, value = box$value, box$subtitle,
          showcase = bs_icon(box$icon)
        ))
      ))
    })
    for (metric in c("income", "expenses")) {
      local({
        current_metric <- metric
        output[[paste0(current_metric, "_model")]] <- renderUI({
          b <- baseline()
          chart_subtitle(if (is.null(b$error)) unname(b$models[current_metric]) else "Upload monthly actuals to start")
        })
        output[[paste0(current_metric, "_plot")]] <- renderPlot({
          plot_finance_forecast(prepared(), baseline(), valid_scenario(), current_metric)
        }, res = 110)
      })
    }
    output$price_plot <- renderPlot({ plot_forecast_price_sensitivity(valid_scenario()) }, res = 110)
    output$break_even_explanation <- renderUI({
      x <- scenario()
      if (!is.null(x$error)) return(p(class = "forecast-help", "Choose valid data and assumptions to calculate the requirement."))
      minimum <- if (is.finite(x$min_price_increase)) forecast_required_rate(x$min_price_increase) else "Not achievable with the current repriceable income"
      div(class = "forecast-break-even-copy",
        span(class = if (x$covers_costs) "forecast-result-positive" else "forecast-result-action",
             if (x$covers_costs) "Your scenario covers costs" else "Your scenario needs more income"),
        p("To cover total Expenses across the selected forecast period, Income needs to reach"),
        div(class = "forecast-required-income", paste0("$", formatC(x$expenses, format = "f", digits = 0, big.mark = ","))),
        p(tags$strong("Minimum price increase: "), minimum),
        p(class = "forecast-help", "Displayed minimum rates are rounded up to 0.1 percentage points."),
        if (is.finite(x$min_price_increase) && x$min_price_increase <= 5) {
          actionButton(session$ns("use_minimum_price"), "Use this minimum price", icon = bs_icon("check2"), class = "btn-outline-primary")
        },
        p(class = "forecast-help", "This includes any existing baseline shortfall. Later effective dates leave fewer months to recover the cost. No price increase is needed when income already covers the selected period's expenses."),
        p(class = "forecast-help", "This is a total-period threshold. Individual months may still need additional cash or income.")
      )
    })
    output$monthly_table <- renderTable({
      x <- valid_scenario()$table
      columns <- c("baseline_income", "scenario_income", "baseline_expenses", "scenario_expenses", "extra_wage_cost")
      result <- data.frame(Month = forecast_month_label(x$date), x[columns], check.names = FALSE)
      result$income_needed <- x$required_income_to_break_even
      names(result) <- c("Month", "Baseline Income", "Scenario Income", "Baseline Expenses", "Scenario Expenses", "Extra Wages", "Income Needed")
      for (column in names(result)[-1L]) result[[column]] <- paste0("$", formatC(result[[column]], format = "f", digits = 0, big.mark = ","))
      result
    }, striped = TRUE, bordered = FALSE, spacing = "m", rownames = FALSE)
    output$validation_note <- renderUI({
      b <- baseline()
      if (!is.null(b$error)) return(p(b$error))
      if (!nrow(b$scores)) return(p("Too few continuous observations for rolling-origin evaluation. The seasonal baseline repeats the latest observed month-of-year values; backtest accuracy is unavailable."))
      p("RMSE and MAE are out-of-sample monthly errors in AUD. Models are scored on the same held-out months; a shorter validation horizon is used when history is limited. Low historical error does not establish future accuracy.")
    })
    output$model_table <- renderTable({
      b <- baseline()
      if (!is.null(b$error) || !nrow(b$scores)) return(NULL)
      x <- b$scores
      data.frame(Metric = tools::toTitleCase(x$metric), Model = x$model,
                 RMSE = format_compact_currency(x$rmse), MAE = format_compact_currency(x$mae),
                 Origins = x$origins, `Test horizon (months)` = x$horizon,
                 Selected = ifelse(x$selected, "Yes", ""), check.names = FALSE)
    }, striped = TRUE, rownames = FALSE)
    output$download_plan <- downloadHandler(
      filename = function() "monthly_finance_scenario.csv",
      content = function(file) utils::write.csv(valid_scenario()$table, file, row.names = FALSE, na = "")
    )
    output$download_assumptions <- downloadHandler(
      filename = function() "forecast_scenario_assumptions.csv",
      content = function(file) {
        x <- valid_scenario()
        p <- prepared()
        extra <- data.frame(assumption = c("scope", "training_from", "training_to", "income_model", "expenses_model"),
                            value = c(default(input$site, "Overall"), format(min(p$training$date)),
                                      format(max(p$training$date)), unname(baseline()$models)))
        utils::write.csv(rbind(x$assumptions, extra), file, row.names = FALSE)
      }
    )
    invisible(list(reports = reports, prepared = prepared, baseline = baseline, scenario = scenario))
  })
}
