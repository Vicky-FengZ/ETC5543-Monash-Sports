# Signed monthly changes have the same meaning in results, timing and exports.
manual_plus <- calculate_finance_scenario(baseline, wage_mode = "monthly_amount", wage_amount = 100)
manual_minus <- calculate_finance_scenario(baseline, wage_mode = "monthly_amount", wage_amount = -300)
manual_zero <- calculate_finance_scenario(baseline, wage_mode = "monthly_amount", wage_amount = 0)
stopifnot(manual_plus$expenses == 15600, manual_plus$extra_wage_cost == 1200,
          abs(manual_plus$min_price_increase - 0.3) < 1e-8,
          manual_minus$expenses == 10800, manual_minus$extra_wage_cost == -3600,
          manual_minus$covers_costs, manual_minus$min_price_increase == 0,
          manual_minus$min_income_growth == 0,
          manual_zero$expenses == zero$expenses, manual_zero$extra_wage_cost == 0,
          all(is.na(manual_plus$table$baseline_casual_wages)),
          all(is.na(manual_minus$table$scenario_casual_wages)))

for (h in c(3L, 6L, 12L)) {
  b <- baseline
  b$forecast <- b$forecast[b$forecast$date < seq(as.Date("2026-01-01"), by = "month", length.out = h + 1L)[h + 1L], ]
  for (amount in c(-100, 0, 100)) {
    for (start in c(1L, h)) {
      plan <- calculate_finance_scenario(b, wage_mode = "monthly_amount", wage_amount = amount,
        effective_month = start, wage_pct = NA_real_, casual_hours_pct = NA_real_,
        casual_baseline = list(error = "No wage data"))
      stopifnot(plan$extra_wage_cost == amount * (h - start + 1L),
                all(plan$table$extra_wage_cost[seq_len(h) < start] == 0),
                all(plan$table$extra_wage_cost[seq_len(h) >= start] == amount),
                plan$expenses == 1200 * h + amount * (h - start + 1L),
                all(plan$table$required_income_to_break_even == plan$table$scenario_expenses))
    }
  }
}
late_manual <- calculate_finance_scenario(baseline, wage_mode = "monthly_amount", wage_amount = 100,
                                        effective_month = 7, price_share = 50)
stopifnot(late_manual$extra_wage_cost == 600, late_manual$min_price_increase == 1)
at_minimum <- calculate_finance_scenario(baseline, wage_mode = "monthly_amount", wage_amount = 100,
  effective_month = 7, price_share = 50, price_pct = late_manual$min_price_increase * 100)
below_minimum <- calculate_finance_scenario(baseline, wage_mode = "monthly_amount", wage_amount = 100,
  effective_month = 7, price_share = 50, price_pct = late_manual$min_price_increase * 100 - 0.001)
stopifnot(at_minimum$covers_costs, !below_minimum$covers_costs)
cat("PASS signed amounts, all horizons, effective months, inactive wage data and break-even pricing\n")

for (bad_amount in list(NA_real_, NaN, Inf, -Inf, "100", numeric(), c(1, 2))) {
  err <- try(calculate_finance_scenario(baseline, wage_mode = "monthly_amount", wage_amount = bad_amount), silent = TRUE)
  stopifnot(inherits(err, "try-error"), grepl("Enter a finite number", as.character(err), fixed = TRUE))
}
for (bad_amount in c(-1201, 1e308)) {
  stopifnot(inherits(try(calculate_finance_scenario(baseline, wage_mode = "monthly_amount",
    wage_amount = bad_amount), silent = TRUE), "try-error"))
}
zero_cost <- calculate_finance_scenario(baseline, wage_mode = "monthly_amount", wage_amount = -1200)
stopifnot(zero_cost$expenses == 0, zero_cost$covers_costs,
          all(zero_cost$table$scenario_expenses_lower80 == 0))
variable_cost <- baseline
variable_cost$forecast$baseline[13] <- 1000
stopifnot(inherits(try(calculate_finance_scenario(variable_cost, wage_mode = "monthly_amount",
  wage_amount = -1100), silent = TRUE), "try-error"))
valid_later <- calculate_finance_scenario(variable_cost, wage_mode = "monthly_amount",
  wage_amount = -1100, effective_month = 2)
stopifnot(valid_later$table$scenario_expenses[1] == 1000,
          all(valid_later$table$scenario_expenses[-1] == 100))
cat("PASS missing/non-finite amounts, overflow and per-month expense floor\n")

manual_data <- reactiveVal(list(monthly = fixture, venues = partial))
shiny::testServer(mod_forecasting_server, args = list(data = manual_data), {
  session$setInputs(horizon = "12", from_year = "2022", cutoff = "2025-12-01",
    payroll_mode = "manual", monthly_payroll_adjustment = 100, casual_wage_pct = 99,
    price_pct = 0, price_share = 100, volume_pct = 0, effective_month = "1")
  saved_base <- baseline()
  stopifnot(is.null(scenario()$error), scenario()$extra_wage_cost == 1200,
            all(is.na(scenario()$table$baseline_casual_wages)))
  # Stale employee controls and site selections cannot add another adjustment.
  session$setInputs(site = "Site A", payroll_in_expenses = FALSE,
    group_1_hourly_wage = 9999, group_1_monthly_hours = 9999,
    group_1_pay_increase_pct = 100, casual_hours_pct = 100)
  stopifnot(selected_scope() == "Overall", scenario()$extra_wage_cost == 1200,
            identical(saved_base, baseline()))
  session$setInputs(monthly_payroll_adjustment = -100, casual_wage_pct = NA_real_, effective_month = "7")
  stopifnot(is.null(scenario()$error), scenario()$extra_wage_cost == -600,
            all(scenario()$table$extra_wage_cost[1:6] == 0), identical(saved_base, baseline()))
  session$setInputs(payroll_mode = "quick", casual_wage_pct = 5, monthly_payroll_adjustment = NA_real_)
  quick_wages <- casual_baseline()
  stopifnot(is.null(scenario()$error),
            abs(scenario()$extra_wage_cost - sum(quick_wages$forecast$baseline[7:12]) * 0.05) < 1e-8)
  # Manual planning does not need a Venues report, or a payroll-inclusion check.
  manual_data(list(monthly = fixture, venues = NULL))
  session$setInputs(payroll_mode = "manual", monthly_payroll_adjustment = 0)
  stopifnot(is.null(scenario()$error), scenario()$extra_wage_cost == 0,
            !grepl("Predicted casual payroll", output$data_health$html, fixed = TRUE))
  session$setInputs(monthly_payroll_adjustment = NA_real_)
  stopifnot(grepl("Monthly payroll adjustment", scenario()$error, fixed = TRUE),
            grepl("Use 0 for no change", output$scenario_notice$html, fixed = TRUE),
            grepl("Complete your plan", output$kpis$html, fixed = TRUE),
            inherits(try(output$download_plan, silent = TRUE), "try-error"),
            inherits(try(output$download_assumptions, silent = TRUE), "try-error"))
  session$setInputs(monthly_payroll_adjustment = -1e6)
  stopifnot(grepl("monthly Expenses negative", scenario()$error, fixed = TRUE))
  session$setInputs(monthly_payroll_adjustment = 100, payroll_mode = "invalid")
  stopifnot(grepl("Choose Quick estimate or Manual adjustment", scenario()$error, fixed = TRUE))
  session$setInputs(payroll_mode = "manual")
  # Check reset messages, then emulate their browser acknowledgement.
  updates <- list()
  mock_session <- session$rootScope()
  mock_session$sendInputMessage <- function(inputId, message) { updates[[inputId]] <<- message }
  session$setInputs(reset_plan = 1)
  stopifnot(updates[[session$ns("monthly_payroll_adjustment")]]$value == 0,
            updates[[session$ns("payroll_mode")]]$value == "quick",
            updates[[session$ns("casual_wage_pct")]]$value == 0,
            updates[[session$ns("effective_month")]]$value == "1")
  session$setInputs(payroll_mode = "quick", casual_wage_pct = 0,
                   monthly_payroll_adjustment = 0, effective_month = "1")
  stopifnot(is.null(scenario()$error), scenario()$extra_wage_cost == 0)
  session$setInputs(payroll_mode = "manual")
  stopifnot(is.null(scenario()$error), scenario()$extra_wage_cost == 0)
  manual_data(list(monthly = NULL))
  session$flushReact()
  stopifnot(!is.null(baseline()$error), !is.null(scenario()$error))
})
cat("PASS exclusive payroll modes, cached baseline, blank input feedback, downloads and reset\n")
