# A short planning workflow; users enter business assumptions, not model choices.

mod_forecasting_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Forecasting", icon = bs_icon("graph-up-arrow"),
    div(
      class = "p-4 forecasting-page",
      div(class = "page-heading", h2("Forecasting & Planning"),
          p("Plan casual pay, paid hours and prices. See the cost impact and the price increase needed to cover expenses.")),
      card(
        class = "forecast-controls",
        card_header("Your plan"),
        div(class = "forecast-mode-selector",
          radioButtons(ns("payroll_mode"), "Casual payroll planning",
            choices = c("Quick estimate" = "quick", "Advanced pay & hours" = "advanced"),
            selected = "quick", inline = TRUE)),
        layout_columns(
          selectInput(ns("horizon"), "Forecast horizon", setNames(c(3, 6, 12), FORECAST_HORIZONS), selected = 12),
          div(class = "forecast-payroll-controls",
            conditionalPanel("input.payroll_mode == 'quick'", ns = ns,
              numericInput(ns("casual_wage_pct"), "Casual pay increase (%)", value = 0, min = 0, max = 100, step = 0.5),
              p(class = "forecast-help", "Uses a historical Venues payroll reference. Only pay rates are adjusted; no extra paid-hours change is assumed.")),
            conditionalPanel("input.payroll_mode == 'advanced'", ns = ns,
              div(class = "forecast-manual-mode",
                tags$strong("Your wages and roster are the reference"),
                p("Enter staff categories below. The quick pay rise and Venues wage forecast are not used in this mode.")))
          ),
          div(numericInput(ns("price_pct"), "Price increase (%)", value = 0, min = 0, max = 500, step = 0.5),
              p(class = "forecast-help", "Default: all income can be repriced and demand stays unchanged. Change this in More options.")),
          col_widths = breakpoints(xs = c(12, 12, 12), lg = c(4, 4, 4))
        ),
        conditionalPanel("input.payroll_mode == 'advanced'", ns = ns,
          div(class = "forecast-payroll-editor",
            div(class = "forecast-payroll-heading",
              div(h5("Casual staff categories"), uiOutput(ns("payroll_scope"))),
              actionButton(ns("add_payroll_group"), "Add category", icon = bs_icon("plus-lg"), class = "btn-outline-primary")),
            p(class = "forecast-help", "Enter each category's total paid hours per month, across all its staff. Do not multiply by headcount again. Use non-overlapping categories and count each paid hour once."),
            div(class = "forecast-payroll-scroll", uiOutput(ns("payroll_group_inputs"))),
            p(class = "forecast-help", "Future hours is optional and overrides the hours-change percentage when entered. Use it for new categories with zero current hours. One monthly roster is repeated across the plan; changes start on the selected effective date."),
            uiOutput(ns("payroll_budget_summary")),
            checkboxInput(ns("payroll_in_expenses"),
              "I confirm these current casual wage costs belong to the selected Expenses scope, and these changes are not already included in its forecast.",
              FALSE),
            p(class = "forecast-help", "Only the future-minus-current payroll difference is added. This is a user-entered scenario, not a replacement of the payroll embedded in the financial forecast. Fixed annual salaries and separate employer on-costs are not automatically calculated.")
          )),
        div(class = "forecast-control-footer",
          p(class = "forecast-help", "Finance forecast methods are automatic; payroll uses only your active mode. Changes are additional to the forecast, start in the first plan month and do not compound monthly."),
          actionButton(ns("reset_plan"), "Reset plan", icon = bs_icon("arrow-counterclockwise"), class = "btn-outline-secondary"))
      ),
      uiOutput(ns("data_health")),
      tags$details(class = "forecast-advanced",
        tags$summary("More options", span("Optional scope, history, pricing and timing")),
        checkboxInput(ns("custom_settings"), "Enable these optional business settings", FALSE),
        conditionalPanel("input.custom_settings", ns = ns,
          layout_columns(
            selectInput(ns("site"), "Scope", choices = "Overall", selected = "Overall"),
            selectInput(ns("from_year"), "Use history from", choices = character()),
            selectInput(ns("cutoff"), "Last actual month", choices = character()),
            col_widths = breakpoints(xs = c(12, 12, 12), lg = c(4, 4, 4))),
          checkboxInput(ns("include_trailing_zeros"), "Use final zero months as confirmed actuals", FALSE),
          p(class = "forecast-help", "The quick Venues pay adjustment is available for Overall only. For a specific site, use Advanced pay & hours and enter categories belonging to that site. Changing scope requires a new cost-inclusion confirmation."),
          h5("Pricing and timing"),
          layout_columns(
          numericInput(ns("price_share"), "Income that can be repriced (%)", value = 100, min = 0, max = 100, step = 5),
          numericInput(ns("volume_pct"), "Demand / volume change (%)", value = 0, min = -99, max = 200, step = 1),
          selectInput(ns("effective_month"), "Changes take effect from", choices = c("First forecast month" = 1), selected = 1),
            col_widths = breakpoints(xs = c(12, 12, 12), lg = c(4, 4, 4))),
          p(class = "forecast-help", "Lower the repriceable share for fixed contracts. Demand is an assumption, not an estimated response to prices, and changes income only.")
        )
      ),
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
      p(class = "forecast-help forecast-interval-note", "Shading shows monthly 80% / 95% finance baseline intervals when estimable. It does not include uncertainty in casual payroll forecasts or your price, pay, paid-hours and demand assumptions."),
      card(
        full_screen = TRUE,
        card_header(div(class = "forecast-card-heading", bs_icon("bullseye"), "Income needed to cover costs")),
        layout_columns(
          div(class = "forecast-break-even-panel", uiOutput(ns("break_even_explanation"))),
          div(class = "forecast-price-panel",
            h5("Price sensitivity"),
            p(class = "forecast-help", "Compare income at different price increases with this scenario's Expenses. Demand and the repriceable share stay at your selected assumptions."),
            plotOutput(ns("price_plot"), height = "400px")),
          col_widths = breakpoints(xs = c(12, 12), lg = c(4, 8)))
      ),
      tags$details(
        class = "forecast-details-card forecast-advanced",
        tags$summary("Explore the details", span("Monthly values, assumptions and forecast checks")),
        navset_card_tab(
          nav_panel("Monthly plan",
            div(class = "forecast-table-actions", downloadButton(ns("download_plan"), "Download monthly plan (CSV)"),
                downloadButton(ns("download_assumptions"), "Download assumptions (CSV)")),
            p(class = "forecast-help", "Income needed is equal to that month's scenario Expenses. The minimum price KPI covers the total selected period, not necessarily every individual month."),
            div(class = "forecast-table-scroll", tableOutput(ns("monthly_table")))),
          nav_panel("Forecast checks", uiOutput(ns("validation_note")),
            div(class = "forecast-table-scroll", tableOutput(ns("model_table")))),
          nav_panel("How it works",
            p("Baseline models use monthly Actual Income and Expenses from Monthly Site Reports. Organisation totals are preferred over site totals to avoid counting the same activity twice."),
            p("Automatic selection compares Seasonal naive, Naive and, with enough history, additive Holt-Winters on the same rolling-origin folds. The lowest out-of-sample RMSE wins separately for Income and Expenses. A missing month splits the training period."),
            p("Automatic casual payroll uses monthly reported Actual Casual Salaries from Venues Tracking Reports. It models the latest continuous history and aligns predictions to the finance plan's dates. Blank months and annual totals are not used as monthly actuals. The recorded payroll component is applied to Overall only, without assigning it to individual sites."),
            p("The casual payroll reference is the sum of monthly forecasts for your selected period, not a historical annual total or a headcount budget. The latest wage actual label identifies the last observed month."),
            p("Quick estimate: payroll change = predicted Venues payroll x your pay increase. No additional paid-hours change is applied; the historical reference may already contain trends in staffing and rates."),
            p("Advanced pay & hours: each category's current monthly cost is hourly wage x total monthly paid hours. Its future cost applies the pay increase and hours change, or uses an entered future-hours target. Only the future-minus-current difference is added. These are your roster assumptions, not predicted headcounts."),
            p("No hourly rate is estimated from Casual Hours: its coverage must first match the wage records. Future hiring and rostering are user assumptions, not automatically predicted headcounts."),
            p("Scenario income = baseline income x demand factor x (1 + repriceable share x price increase). Scenario expenses = baseline expenses + payroll change from ONE active mode. Advanced inputs replace the quick estimate; they do not rebuild the wages already inside the financial baseline."),
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
    dashboard <- reactive(if (shiny::is.reactive(data)) data() else data)
    custom <- reactive(isTRUE(input$custom_settings))
    selected_scope <- reactive(if (custom()) default(input$site, "Overall") else "Overall")
    payroll_mode <- reactive(default(input$payroll_mode, "quick"))
    payroll_group_ids <- reactiveVal(1L)
    next_payroll_group_id <- 1L
    confirmed_payroll_scope <- reactiveVal(NULL)
    observeEvent(input$add_payroll_group, {
      if (length(payroll_group_ids()) >= 20L) {
        showNotification("Use at most 20 staff categories.", type = "warning")
        return()
      }
      next_payroll_group_id <<- next_payroll_group_id + 1L
      payroll_group_ids(c(payroll_group_ids(), next_payroll_group_id))
    })
    observeEvent(input$remove_payroll_group, {
      id <- suppressWarnings(as.integer(input$remove_payroll_group))
      if (length(id) == 1L && !is.na(id) && id %in% payroll_group_ids()) {
        payroll_group_ids(setdiff(payroll_group_ids(), id))
      }
    })
    group_value <- function(id, field, fallback) {
      default(input[[paste0("group_", id, "_", field)]], fallback)
    }
    payroll_groups <- reactive({
      rows <- lapply(payroll_group_ids(), function(id) data.frame(
        category = group_value(id, "category", paste("Casual group", id)),
        hourly_wage = group_value(id, "hourly_wage", NA_real_),
        monthly_hours = group_value(id, "monthly_hours", NA_real_),
        pay_increase_pct = group_value(id, "pay_increase_pct", 0),
        hours_change_pct = group_value(id, "hours_change_pct", 0),
        future_monthly_hours = group_value(id, "future_monthly_hours", NA_real_),
        stringsAsFactors = FALSE))
      if (length(rows)) do.call(rbind, rows) else data.frame()
    })
    advanced_budget <- reactive({
      tryCatch(calculate_manual_casual_payroll(payroll_groups()),
               error = function(e) list(error = conditionMessage(e)))
    })
    observeEvent(input$payroll_in_expenses, {
      confirmed_payroll_scope(if (isTRUE(input$payroll_in_expenses)) selected_scope() else NULL)
    })
    output$payroll_scope <- renderUI({
      p(class = "forecast-help", paste("Categories apply to:", selected_scope(), "| AUD | Hours per month"))
    })
    output$payroll_group_inputs <- renderUI({
      ids <- payroll_group_ids()
      if (!length(ids)) return(p(class = "forecast-help", "No categories. Use Add category to start; an empty plan is not treated as zero payroll."))
      fields <- c("category", "hourly_wage", "monthly_hours", "pay_increase_pct",
                  "hours_change_pct", "future_monthly_hours")
      headers <- c("Staff category", "Current wage ($/hour)", "Current hours / month",
                   "Pay increase (%)", "Hours change (%)", "Future hours / month (optional)")
      rows <- lapply(ids, function(id) {
        cells <- lapply(seq_along(fields), function(j) {
          field <- fields[j]
          input_id <- session$ns(paste0("group_", id, "_", field))
          label <- span(class = "visually-hidden", paste("Category", id, headers[j]))
          fallback <- if (field == "category") paste("Casual group", id) else
            if (field %in% c("pay_increase_pct", "hours_change_pct")) 0 else NA_real_
          value <- isolate(group_value(id, field, fallback))
          control <- if (field == "category") textInput(input_id, label, value, width = "100%") else
            numericInput(input_id, label, value,
              min = if (field == "hours_change_pct") -100 else 0,
              max = if (field == "pay_increase_pct") 100 else if (field == "hours_change_pct") 200 else NA_real_,
              step = if (field %in% c("hourly_wage", "pay_increase_pct")) 0.5 else 1,
              width = "100%")
          tags$td(control)
        })
        remove <- tags$button(type = "button", class = "btn forecast-group-remove",
          title = paste("Remove category", id), `aria-label` = paste("Remove category", id),
          onclick = sprintf("Shiny.setInputValue(%s, %d, {priority: 'event'});",
                            encodeString(session$ns("remove_payroll_group"), quote = '"'), id),
          bs_icon("x-lg"))
        do.call(tags$tr, c(cells, list(tags$td(remove))))
      })
      tags$table(class = "forecast-payroll-table",
        tags$thead(tags$tr(lapply(c(headers, "Remove"), function(x) tags$th(scope = "col", x)))),
        do.call(tags$tbody, rows))
    })
    output$payroll_budget_summary <- renderUI({
      if (payroll_mode() != "advanced") return(NULL)
      x <- advanced_budget()
      if (!is.null(x$error)) return(p(class = "forecast-warning", x$error))
      div(class = "forecast-payroll-summary",
        div(span("Current monthly payroll"), strong(format_compact_currency(x$current_monthly, digits = 2L))),
        div(span("Planned monthly payroll"), strong(format_compact_currency(x$planned_monthly, digits = 2L))),
        div(span("Monthly payroll change"), strong(format_compact_currency(x$monthly_change, digits = 2L))))
    })
    reports <- reactive({
      forecast_monthly_reports(dashboard())
    })
    venues_reports <- reactive(forecast_venues_reports(dashboard()))
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
      cutoff <- if (custom()) input$cutoff else NULL
      if (is.null(cutoff) || !length(cutoff) || !nzchar(cutoff)) cutoff <- NULL
      year <- if (custom()) suppressWarnings(as.integer(input$from_year)) else NULL
      if (!length(year)) year <- NULL
      prepare_forecast_history(reports(), selected_scope(), cutoff, year,
                               custom() && isTRUE(input$include_trailing_zeros))
    })
    # A new scope or financial history needs a fresh user cost-inclusion check.
    observeEvent(prepared(), {
      confirmed_payroll_scope(NULL)
      updateCheckboxInput(session, "payroll_in_expenses", value = FALSE)
    }, ignoreInit = TRUE, priority = 100)
    # Business input edits do not refit models or reparse uploaded Excel.
    baseline <- reactive({
      fit_finance_forecasts(prepared(), as.integer(default(input$horizon, 12)), "auto")
    })
    casual_baseline <- reactive({
      b <- baseline()
      if (!is.null(b$error)) return(list(error = "Upload Monthly Site Reports to choose the planning period."))
      p <- prepared()
      year <- if (custom()) suppressWarnings(as.integer(input$from_year)) else NULL
      if (!length(year)) year <- NULL
      prepare_casual_wage_baseline(
        venues_reports(), b$forecast$date[b$forecast$metric == "income"],
        cutoff = max(p$training$date), from_year = year,
        include_trailing_zeros = custom() && isTRUE(input$include_trailing_zeros),
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
        if (!selected_mode %in% c("quick", "advanced")) stop("Choose Quick estimate or Advanced pay & hours.", call. = FALSE)
        mode <- if (selected_mode == "advanced") "manual_categories" else "casual_baseline"
        rise <- if (selected_mode == "advanced") 0 else forecast_check_number(
          default(input$casual_wage_pct, 0), "Casual pay increase (%)", 0, 100)
        casual_change <- mode == "casual_baseline" && rise != 0
        if (casual_change && selected_scope() != "Overall") {
          stop("Casual wages cannot be allocated to an individual site from these reports. Choose Overall or use Advanced pay & hours for that site.", call. = FALSE)
        }
        if (casual_change &&
            any(grepl("uses the sum of available site summaries", prepared()$notes, fixed = TRUE))) {
          stop("Overall is based on a partial set of site summaries. Upload the organisation summary or use Advanced pay & hours with matching categories before applying a payroll change.", call. = FALSE)
        }
        calculate_finance_scenario(
        baseline(), wage_mode = mode,
        wage_pct = rise, casual_hours_pct = 0,
        price_pct = default(input$price_pct, 0),
        price_share = if (custom()) default(input$price_share, 100) else 100,
        volume_pct = if (custom()) default(input$volume_pct, 0) else 0,
        effective_month = if (custom()) as.integer(default(input$effective_month, 1)) else 1L,
        casual_baseline = if (mode == "casual_baseline" && selected_scope() == "Overall") casual_baseline() else NULL,
        payroll_groups = if (mode == "manual_categories") payroll_groups() else NULL,
        payroll_in_expenses = isTRUE(input$payroll_in_expenses) &&
          identical(confirmed_payroll_scope(), selected_scope())
        )
      }, error = function(e) list(error = conditionMessage(e)))
    })
    observeEvent(input$reset_plan, {
      updateCheckboxInput(session, "custom_settings", value = FALSE)
      updateCheckboxInput(session, "include_trailing_zeros", value = FALSE)
      updateCheckboxInput(session, "payroll_in_expenses", value = FALSE)
      confirmed_payroll_scope(NULL)
      updateRadioButtons(session, "payroll_mode", selected = "quick")
      next_payroll_group_id <<- next_payroll_group_id + 1L
      payroll_group_ids(next_payroll_group_id)
      updateSelectInput(session, "horizon", selected = 12)
      updateSelectInput(session, "site", selected = "Overall")
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
    output$data_health <- renderUI({
      p <- prepared()
      b <- baseline()
      if (!is.null(p$error)) {
        return(div(class = "forecast-status forecast-status-empty", bs_icon("info-circle"), p$error))
      }
      training <- p$training
      w <- if (payroll_mode() == "quick") casual_baseline() else NULL
      notes <- unique(c(p$notes, b$notes, if (!is.null(w) && is.null(w$error)) w$notes))
      div(class = "forecast-status",
        layout_columns(
          div(div(class = "forecast-status-title", "Forecast starting point"),
              p(paste(selected_scope(), "—", nrow(training), "continuous months through", forecast_month_label(max(training$date)))),
              p(class = "forecast-help", "Forecast methods are selected automatically. Their names and historical checks are available under Explore the details."),
              if (nrow(training) < 24L) p(class = "forecast-warning", "Short history: treat these estimates as provisional, not a validated long-term forecast.")),
          div(div(class = "forecast-status-title",
                  if (payroll_mode() == "advanced") "Payroll reference: your inputs" else "Predicted casual payroll (before your changes)"),
              if (payroll_mode() == "advanced") p(class = "forecast-help", "Uses your hourly wages and monthly roster, not the Venues wage forecast. The budget above shows current costs, future costs and their difference. Only that difference is added to the financial reference; current costs are not subtracted and replaced.") else
              if (selected_scope() != "Overall") p(class = "forecast-help", "Not allocated to this site. Choose Overall or enter site-specific categories in Advanced pay & hours.") else
              if (is.null(w)) p(class = "forecast-help", "Choose a payroll planning mode to see its reference.") else
              if (!is.null(w$error)) p(class = "forecast-help", w$error) else tagList(
                p(paste(format_compact_currency(sum(w$forecast$baseline), digits = 2L), "estimated for",
                        forecast_month_label(min(w$forecast$date)), "to", forecast_month_label(max(w$forecast$date)))),
                p(class = "forecast-help", paste0("Historical wage actuals through ", forecast_month_label(w$last_actual),
                  ". Venues-reported component only, not all staff. This reference extrapolates past totals; it does not know future headcounts or rosters.")))),
          col_widths = breakpoints(xs = c(12, 12), lg = c(6, 6))),
        if (!is.null(b$error)) p(class = "forecast-warning", b$error),
        if (length(notes)) tags$details(class = "forecast-data-notes",
          tags$summary(paste("Data notes and limitations (", length(notes), ")", sep = "")),
          tags$ul(lapply(notes, tags$li)))
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
             subtitle = if (available) paste("Baseline:", format_compact_currency(result$baseline_income, digits = 2L)) else "Upload data to start", icon = "cash-stack"),
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
          chart_subtitle(if (is.null(b$error)) "Automatically selected forecast · methods and checks in Explore the details" else "Upload monthly actuals to start")
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
        p(class = "forecast-help", "Includes any existing shortfall. Minimum rates round up to 0.1 percentage points. This covers the whole plan, not necessarily every month."),
        if (!is.null(x$payroll_groups)) p(class = "forecast-help", "Advanced mode uses forecast Expenses plus your payroll difference. It is a conditional cost-impact estimate, not a rebuilt budget for wages and all other expenses.")
      )
    })
    output$monthly_table <- renderTable({
      x <- valid_scenario()$table
      columns <- c("baseline_income", "scenario_income", "baseline_expenses", "scenario_expenses", "extra_wage_cost")
      result <- data.frame(Month = forecast_month_label(x$date), x[columns], check.names = FALSE)
      result$income_needed <- x$required_income_to_break_even
      names(result) <- c("Month", "Baseline Income", "Scenario Income", "Baseline Expenses", "Scenario Expenses", "Payroll Change", "Income Needed")
      if (all(is.finite(x$baseline_casual_wages))) {
        result[[if (payroll_mode() == "advanced") "Current Casual Payroll Reference" else "Predicted Casual Payroll"]] <- x$baseline_casual_wages
        result[["Scenario Casual Payroll"]] <- x$scenario_casual_wages
      }
      for (column in names(result)[-1L]) result[[column]] <- paste0("$", formatC(result[[column]], format = "f", digits = 0, big.mark = ","))
      result
    }, striped = TRUE, bordered = FALSE, spacing = "m", rownames = FALSE)
    output$validation_note <- renderUI({
      b <- baseline()
      if (!is.null(b$error)) return(p(b$error))
      if (!nrow(b$scores)) return(p("Finance history is too short for rolling-origin evaluation. The finance seasonal baseline is provisional. Casual wage backtests appear below when that separate history is long enough."))
      p("RMSE and MAE are out-of-sample monthly errors in AUD. Models are scored on the same held-out months; a shorter validation horizon is used when history is limited. Low historical error does not establish future accuracy.")
    })
    output$model_table <- renderTable({
      b <- baseline()
      if (!is.null(b$error)) return(NULL)
      x <- b$scores
      w <- if (payroll_mode() == "quick") casual_baseline() else NULL
      if (!is.null(w) && is.null(w$error) && nrow(w$scores)) {
        wage_scores <- w$scores
        wage_scores$metric <- "casual wages"
        x <- if (nrow(x)) rbind(x, wage_scores[names(x)]) else wage_scores
      }
      if (!nrow(x)) return(NULL)
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
        extra <- data.frame(assumption = c("scope", "training_from", "training_to", "income_model", "expenses_model", "custom_settings", "model_selection", "payroll_mode"),
                            value = c(selected_scope(), format(min(p$training$date)),
                                      format(max(p$training$date)), unname(baseline()$models), as.character(custom()), "automatic", payroll_mode()))
        w <- if (payroll_mode() == "quick") casual_baseline() else NULL
        if (!is.null(w) && is.null(w$error) && selected_scope() == "Overall") {
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
                   casual_baseline = casual_baseline, payroll_groups = payroll_groups,
                   advanced_budget = advanced_budget, scenario = scenario))
  })
}
