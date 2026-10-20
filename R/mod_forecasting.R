# A short planning workflow; users enter business assumptions, not model choices.

mod_forecasting_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Forecasting", icon = bs_icon("graph-up-arrow"),
    div(
      class = "p-4 forecasting-page",
      div(class = "page-heading", h2("Forecasting & Planning"),
          p("Plan pay and prices for Monash Sport overall, then see the income needed to cover your costs.")),
      card(
        class = "forecast-controls",
        card_header("Your plan"),
        layout_columns(
          selectInput(ns("horizon"), "Forecast horizon", setNames(c(3, 6, 12), FORECAST_HORIZONS), selected = 12),
          div(class = "forecast-payroll-controls",
            conditionalPanel("input.payroll_mode == 'quick'", ns = ns,
              numericInput(ns("casual_wage_pct"), "Casual pay increase (%)", value = 0, min = 0, max = 100, step = 0.5),
              p(class = "forecast-help", "Quick estimate using Venues payroll.")),
            conditionalPanel("input.payroll_mode == 'manual'", ns = ns,
              numericInput(ns("monthly_payroll_adjustment"), "Monthly payroll adjustment (AUD)",
                value = 0, step = 100),
              p(class = "forecast-help", "Positive adds costs; negative reduces them. Include only changes not already reflected in the forecast."))
          ),
          div(numericInput(ns("price_pct"), "Price increase (%)", value = 0, min = 0, max = 500, step = 0.5),
              p(class = "forecast-help", "Default: reprice all income, with demand unchanged.")),
          col_widths = breakpoints(xs = c(12, 12, 12), lg = c(4, 4, 4))
        ),
        div(class = "forecast-control-footer",
          p(class = "forecast-help", "Choose a payroll method and other assumptions in More options."),
          actionButton(ns("reset_plan"), "Reset plan", icon = bs_icon("arrow-counterclockwise"), class = "btn-outline-secondary"))
      ),
      tags$details(id = ns("more_options"), class = "forecast-advanced",
        tags$summary("More options",
          span(class = "forecast-options-description", "Payroll, history, pricing and timing"),
          uiOutput(ns("options_state"), inline = TRUE)),
        div(class = "forecast-options-section",
          h5("Payroll"),
          div(class = "forecast-mode-selector",
            radioButtons(ns("payroll_mode"), "Casual payroll planning",
              choices = c("Quick estimate" = "quick", "Manual adjustment" = "manual"),
              selected = "quick", inline = TRUE)),
          p(class = "forecast-help", "Enter the selected payroll adjustment in Your plan. Only one method applies.")
        ),
        div(class = "forecast-options-section",
          h5("History"),
          layout_columns(
            selectInput(ns("from_year"), "Use history from", choices = character()),
            selectInput(ns("cutoff"), "Last actual month", choices = character()),
            col_widths = breakpoints(xs = c(12, 12), lg = c(6, 6))),
          div(class = "forecast-zero-reviews",
            uiOutput(ns("finance_zero_review")), uiOutput(ns("wage_zero_review")))
        ),
        div(class = "forecast-options-section",
          h5("Pricing and timing"),
          layout_columns(
            numericInput(ns("price_share"), "Income that can be repriced (%)", value = 100, min = 0, max = 100, step = 5),
            numericInput(ns("volume_pct"), "Demand / volume change (%)", value = 0, min = -99, max = 200, step = 1),
            selectInput(ns("effective_month"), "Changes take effect from", choices = c("First forecast month" = 1), selected = 1),
            col_widths = breakpoints(xs = c(12, 12, 12), lg = c(4, 4, 4))),
          p(class = "forecast-help", "Lower the repriceable share for fixed-price income. Demand is your planning assumption.")
        )
      ),
      uiOutput(ns("data_health")),
      uiOutput(ns("scenario_notice")),
      div(class = "section-heading-custom", h4("Your results"),
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
      p(class = "forecast-help forecast-interval-note", "Shading shows financial forecast uncertainty; it excludes uncertainty in your payroll and pricing assumptions."),
      card(
        full_screen = TRUE,
        card_header(div(class = "forecast-card-heading", bs_icon("bullseye"), "Income needed to cover costs")),
        layout_columns(
          div(class = "forecast-break-even-panel", uiOutput(ns("break_even_explanation"))),
          div(class = "forecast-price-panel",
            h5("Price sensitivity"),
            p(class = "forecast-help", "Compare income at different prices using your current plan."),
            plotOutput(ns("price_plot"), height = "400px")),
          col_widths = breakpoints(xs = c(12, 12), lg = c(4, 8)))
      ),
      card(
        class = "forecast-monthly-plan", full_screen = TRUE,
        card_header(div(class = "forecast-monthly-plan-header",
          div(class = "forecast-card-heading", bs_icon("table"), "Monthly plan"),
          div(class = "forecast-table-actions",
            downloadButton(ns("download_plan"), "Download plan (CSV)", class = "btn-outline-primary"),
            downloadButton(ns("download_assumptions"), "Download assumptions (CSV)", class = "btn-outline-secondary")))),
        p(class = "forecast-help", "Income Needed shows each month's costs. The minimum price increase covers the whole plan."),
        div(class = "forecast-table-scroll", tableOutput(ns("monthly_table")))
      )
    )
  )
}

mod_forecasting_server <- function(id, data = NULL) {
  moduleServer(id, function(input, output, session) {
    default <- function(value, fallback) if (is.null(value) || !length(value)) fallback else value
    dashboard <- reactive(if (shiny::is.reactive(data)) data() else data)
    # Forecasting is organisation-wide. Ignore any stale or forged site input;
    # site selectors on the other dashboard pages remain independent.
    selected_scope <- reactive("Overall")
    payroll_mode <- reactive(default(input$payroll_mode, "quick"))
    reports <- reactive({
      forecast_monthly_reports(dashboard())
    })
    venues_reports <- reactive(forecast_venues_reports(dashboard()))
    previous_cutoff <- reactiveVal(NULL)
    observeEvent(reports(), {
      finance <- reports()$finance
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

    history_settings <- reactive({
      cutoff <- input$cutoff
      if (is.null(cutoff) || !length(cutoff) || !nzchar(cutoff)) cutoff <- NULL
      year <- suppressWarnings(as.integer(input$from_year))
      if (!length(year)) year <- NULL
      list(cutoff = cutoff, year = year)
    })
    # A context-specific token prevents an old browser selection from confirming
    # different files or months, including delayed input events.
    zero_review_state <- function(context) {
      state <- reactiveVal(list(version = 0L, context = NULL))
      observeEvent(context(), {
        state(list(version = state()$version + 1L, context = context()))
      }, ignoreNULL = FALSE, priority = 200)
      state
    }
    finance_zero_context <- reactive({
      settings <- history_settings()
      p <- prepare_forecast_history(reports(), selected_scope(), settings$cutoff, settings$year, TRUE)
      list(months = p$zero_months, scope = selected_scope(), settings = settings, data = reports())
    })
    finance_zero_state <- zero_review_state(finance_zero_context)
    finance_zeros_confirmed <- reactive({
      state <- finance_zero_state()
      length(state$context$months) > 0L &&
        identical(input$finance_zero_status, paste0(state$version, ":confirmed"))
    })
    prepared <- reactive({
      settings <- history_settings()
      prepare_forecast_history(reports(), selected_scope(), settings$cutoff, settings$year,
                               finance_zeros_confirmed())
    })
    wage_zero_context <- reactive({
      p <- prepared()
      if (payroll_mode() != "quick" || !is.null(p$error) || !nrow(p$training)) return(NULL)
      cutoff <- max(p$training$date)
      year <- history_settings()$year
      w <- prepare_casual_wage_history(venues_reports(), cutoff, year, TRUE)
      list(months = w$zero_months, scope = selected_scope(), cutoff = cutoff,
           year = year, finance = p$training, data = venues_reports())
    })
    wage_zero_state <- zero_review_state(wage_zero_context)
    wage_zeros_confirmed <- reactive({
      state <- wage_zero_state()
      length(state$context$months) > 0L &&
        identical(input$wage_zero_status, paste0(state$version, ":confirmed"))
    })
    zero_review_control <- function(state, id, title, description) {
      review <- state()
      months <- review$context$months
      if (!length(months)) return(NULL)
      # Group adjacent dates, but never label a missing month as a zero month.
      index <- as.integer(format(months, "%Y")) * 12L + as.integer(format(months, "%m"))
      ranges <- split(months, cumsum(c(TRUE, diff(index) != 1L)))
      labels <- vapply(ranges, function(dates) {
        paste(unique(forecast_month_label(range(dates))), collapse = " to ")
      }, character(1))
      div(class = "forecast-zero-review",
        tags$strong(title),
        p(class = "forecast-help", paste0(paste(labels, collapse = ", "), ": ", description)),
        radioButtons(session$ns(id), "Were these months fully reported?",
          choices = setNames(paste0(review$version, c(":unreported", ":confirmed")),
                             c("Not yet reported", "Confirmed zero actuals")),
          selected = paste0(review$version, ":unreported"), inline = TRUE, width = "100%"))
    }
    output$finance_zero_review <- renderUI({
      zero_review_control(finance_zero_state, "finance_zero_status", "Final zero months · Income & Expenses",
                          "both Income and Expenses are zero.")
    })
    output$wage_zero_review <- renderUI({
      zero_review_control(wage_zero_state, "wage_zero_status", "Final zero months · Casual payroll",
                          "reported casual wages are zero.")
    })
    # Empty conditional outputs must still wake up when new data need review,
    # even while More options is collapsed or the previous render was NULL.
    outputOptions(output, "finance_zero_review", suspendWhenHidden = FALSE)
    outputOptions(output, "wage_zero_review", suspendWhenHidden = FALSE)
    # Business input edits do not refit models or reparse uploaded Excel.
    baseline <- reactive({
      fit_finance_forecasts(prepared(), as.integer(default(input$horizon, 12)), "auto")
    })
    casual_baseline <- reactive({
      b <- baseline()
      if (!is.null(b$error)) return(list(error = "Upload Monthly Site Reports to choose the planning period."))
      p <- prepared()
      year <- history_settings()$year
      prepare_casual_wage_baseline(
        venues_reports(), b$forecast$date[b$forecast$metric == "income"],
        cutoff = max(p$training$date), from_year = year,
        include_trailing_zeros = wage_zeros_confirmed(),
        method = "auto"
      )
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
      tryCatch({
        selected_mode <- payroll_mode()
        if (!selected_mode %in% c("quick", "manual")) stop("Choose Quick estimate or Manual adjustment.", call. = FALSE)
        mode <- if (selected_mode == "manual") "monthly_amount" else "casual_baseline"
        rise <- if (selected_mode == "manual") 0 else forecast_check_number(
          default(input$casual_wage_pct, 0), "Casual pay increase (%)", 0, 100)
        casual_change <- mode == "casual_baseline" && rise != 0
        if (casual_change &&
            any(grepl("uses the sum of available site summaries", prepared()$notes, fixed = TRUE))) {
          stop("Overall is based on a partial set of site summaries. Upload the organisation summary before applying a Quick payroll change.", call. = FALSE)
        }
        calculate_finance_scenario(
        baseline(), wage_mode = mode,
        wage_pct = rise, casual_hours_pct = 0,
        wage_amount = if (mode == "monthly_amount") default(input$monthly_payroll_adjustment, NA_real_) else 0,
        price_pct = default(input$price_pct, 0),
        price_share = default(input$price_share, 100),
        volume_pct = default(input$volume_pct, 0),
        effective_month = as.integer(default(input$effective_month, 1)),
        casual_baseline = if (mode == "casual_baseline") casual_baseline() else NULL
        )
      }, error = function(e) list(error = conditionMessage(e)))
    })
    observeEvent(input$reset_plan, {
      # Changing tokens also invalidates confirmation before client updates arrive.
      for (state in list(finance_zero_state, wage_zero_state)) {
        state(list(version = state()$version + 1L, context = state()$context))
      }
      updateRadioButtons(session, "payroll_mode", selected = "quick")
      updateNumericInput(session, "monthly_payroll_adjustment", value = 0)
      updateSelectInput(session, "horizon", selected = 12)
      updateSelectInput(session, "effective_month", selected = 1)
      updateNumericInput(session, "casual_wage_pct", value = 0)
      updateNumericInput(session, "price_pct", value = 0)
      updateNumericInput(session, "price_share", value = 100)
      updateNumericInput(session, "volume_pct", value = 0)
      finance <- reports()$finance
      if (is.data.frame(finance) && nrow(finance)) {
        dates <- as.Date(finance$date[finance$is_summary %in% TRUE & finance$value_type %in% "Actual" & !is.na(finance$date)])
        if (length(dates)) {
          updateSelectInput(session, "from_year", selected = min(as.integer(format(dates, "%Y"))))
          updateSelectInput(session, "cutoff", selected = as.character(max(dates)))
        }
      }
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
    output$options_state <- renderUI({
      span(class = "forecast-mode-badge",
           if (payroll_mode() == "manual") "Manual adjustment" else "Quick payroll")
    })
    output$data_health <- renderUI({
      p <- prepared()
      b <- baseline()
      if (!is.null(p$error)) {
        return(div(class = "forecast-status forecast-status-empty", bs_icon("info-circle"), p$error))
      }
      training <- p$training
      w <- if (payroll_mode() == "quick") casual_baseline() else NULL
      div(class = "forecast-status",
        layout_columns(
          div(div(class = "forecast-status-title", "Forecast starting point"),
              p(paste(selected_scope(), "·", nrow(training), "months through", forecast_month_label(max(training$date)))),
              if (nrow(training) < 24L) p(class = "forecast-warning", "Limited history: use these results as planning estimates.")),
          div(div(class = "forecast-status-title",
                  if (payroll_mode() == "manual") "Manual payroll adjustment" else "Predicted casual payroll"),
              if (payroll_mode() == "manual") p(class = "forecast-help", "Your monthly amount adjusts Overall Expenses from the selected effective month.") else
              if (is.null(w)) p(class = "forecast-help", "Choose a payroll reference in More options.") else
              if (!is.null(w$error)) p(class = "forecast-help", w$error) else tagList(
                p(paste(format_compact_currency(sum(w$forecast$baseline), digits = 2L), "for",
                        forecast_month_label(min(w$forecast$date)), "to", forecast_month_label(max(w$forecast$date)))),
                p(class = "forecast-help", paste0("Venues casual wages only · Last actual: ",
                  forecast_month_label(w$last_actual), ". History-based reference, not a staff roster.")))),
          col_widths = breakpoints(xs = c(12, 12), lg = c(6, 6))),
        if (!is.null(b$error)) p(class = "forecast-warning", b$error)
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
                                        "|", selected_scope(), "| All values in AUD"))
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
             subtitle = if (available) paste("Baseline:", format_compact_currency(result$baseline_income, digits = 2L)) else
               if (is.null(baseline()$error)) "Complete your plan to see results" else "Upload data to start", icon = "cash-stack"),
        list(title = "Scenario Expenses", value = value("expenses"),
             subtitle = if (available) paste("Payroll change:", format_compact_currency(result$extra_wage_cost)) else "Enter your cost assumptions", icon = "receipt"),
        list(title = "Minimum Price Increase", value = value("min_price_increase", TRUE),
             subtitle = "Needed to cover costs over the whole plan", icon = "tags")
      )
      do.call(layout_columns, c(
        list(col_widths = breakpoints(xs = c(12, 12, 12), md = c(4, 4, 4))),
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
          chart_subtitle(if (is.null(b$error)) "Monthly planning estimate" else "Upload monthly actuals to start")
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
        p("Income needed to cover this plan's total Expenses:"),
        div(class = "forecast-required-income", paste0("$", formatC(x$expenses, format = "f", digits = 0, big.mark = ","))),
        p(tags$strong("Minimum price increase: "), minimum,
          " | Minimum Income growth: ", forecast_required_rate(x$min_income_growth)),
        if (is.finite(x$min_price_increase) && x$min_price_increase <= 5) {
          actionButton(session$ns("use_minimum_price"), "Use this minimum price", icon = bs_icon("check2"), class = "btn-outline-primary")
        },
        p(class = "forecast-help", "Includes any existing shortfall. Minimum rates round up to 0.1 percentage points. This covers the whole plan, not necessarily every month.")
      )
    })
    output$monthly_table <- renderTable({
      x <- valid_scenario()$table
      columns <- c("baseline_income", "scenario_income", "baseline_expenses", "scenario_expenses", "extra_wage_cost")
      result <- data.frame(Month = forecast_month_label(x$date), x[columns], check.names = FALSE)
      result$income_needed <- x$required_income_to_break_even
      names(result) <- c("Month", "Baseline Income", "Scenario Income", "Baseline Expenses", "Scenario Expenses", "Payroll Change", "Income Needed")
      if (all(is.finite(x$baseline_casual_wages))) {
        result[["Predicted Casual Payroll"]] <- x$baseline_casual_wages
        result[["Scenario Casual Payroll"]] <- x$scenario_casual_wages
      }
      for (column in names(result)[-1L]) {
        values <- result[[column]]
        result[[column]] <- paste0(ifelse(values < 0, "-$", "$"),
                                  formatC(abs(values), format = "f", digits = 0, big.mark = ","))
      }
      result
    }, striped = TRUE, bordered = FALSE, spacing = "m", rownames = FALSE)
    output$download_plan <- downloadHandler(
      filename = function() "monthly_finance_scenario.csv",
      content = function(file) utils::write.csv(valid_scenario()$table, file, row.names = FALSE, na = "")
    )
    output$download_assumptions <- downloadHandler(
      filename = function() "forecast_scenario_assumptions.csv",
      content = function(file) {
        x <- valid_scenario()
        p <- prepared()
        extra <- data.frame(assumption = c("scope", "training_from", "training_to", "income_model", "expenses_model", "model_selection", "payroll_mode",
                                          "finance_final_zeros_confirmed", "casual_wage_final_zeros_confirmed"),
                            value = c(selected_scope(), format(min(p$training$date)),
                                      format(max(p$training$date)), unname(baseline()$models), "automatic", payroll_mode(),
                                      finance_zeros_confirmed(), if (payroll_mode() == "quick") wage_zeros_confirmed() else FALSE))
        w <- if (payroll_mode() == "quick") casual_baseline() else NULL
        if (!is.null(w) && is.null(w$error)) {
          extra <- rbind(extra, data.frame(
            assumption = c("casual_wage_scope", "casual_wage_model", "casual_wage_training_from", "casual_wage_last_actual", "casual_wage_sources", "casual_hours_assumption"),
            value = c("Venues-reported payroll component only; applied to Overall", w$model,
                      format(min(w$training$date)), format(w$last_actual), paste(w$sources, collapse = "; "),
                      "Quick estimate applies pay increases only; no extra paid-hours change; staffing trends may be present in the historical reference")))
        }
        utils::write.csv(rbind(x$assumptions, extra), file, row.names = FALSE)
      }
    )
    invisible(list(reports = reports, prepared = prepared, baseline = baseline,
                   casual_baseline = casual_baseline, scenario = scenario))
  })
}
