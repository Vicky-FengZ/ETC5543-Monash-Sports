# Default workflow and the recorded casual-payroll component.
make_casual_wage_fixture <- function(n = 48L) {
  dates <- seq(as.Date("2022-01-01"), by = "month", length.out = n)
  wages <- data.frame(date = dates, report_year = as.integer(format(dates, "%Y")),
    salary_year = as.integer(format(dates, "%Y")), period_type = "Month",
    metric = "Actual", value = 400 + seq_along(dates), is_reported = TRUE,
    source_file = paste0(format(dates, "%Y"), " Venues Tracking Report.xlsx"))
  list(casual_wages = wages, validation = data.frame())
}
wages <- make_casual_wage_fixture()
future <- seq(as.Date("2026-01-01"), by = "month", length.out = 12)
wage_base <- prepare_casual_wage_baseline(wages, future)
stopifnot(is.null(wage_base$error), nrow(wage_base$training) == 48L,
          identical(wage_base$forecast$date, future), nrow(wage_base$scores) >= 2L)

partial <- wages
partial$casual_wages$value[46:48] <- NA_real_
partial$casual_wages$is_reported[46:48] <- FALSE
partial_base <- prepare_casual_wage_baseline(partial, future, method = "seasonal_naive")
stopifnot(is.null(partial_base$error), nrow(partial_base$training) == 45L,
          partial_base$last_actual == as.Date("2025-09-01"), partial_base$bridge_months == 3L,
          identical(partial_base$forecast$date, future), partial_base$forecast$baseline[1] == 437)

total <- partial$casual_wages[1, ]
total$period_type <- "Reported Total"
total$value <- 999999
with_total <- partial
with_total$casual_wages <- rbind(partial$casual_wages, total)
stopifnot(identical(prepare_casual_wage_baseline(with_total, future, method = "seasonal_naive")$forecast,
                    partial_base$forecast))
dup_wages <- wages
dup_wages$casual_wages <- rbind(wages$casual_wages, wages$casual_wages[1, ])
stopifnot(grepl("Overlapping", prepare_casual_wage_baseline(dup_wages, future)$error),
          !is.null(prepare_casual_wage_baseline(make_casual_wage_fixture(11L), future)$error))
gapped_wages <- wages
gapped_wages$casual_wages$value[37] <- NA_real_
stopifnot(!is.null(prepare_casual_wage_baseline(gapped_wages, future)$error))
negative_wages <- wages
negative_wages$casual_wages$value[40] <- -1
stopifnot(grepl("Negative", prepare_casual_wage_baseline(negative_wages, future)$error))
wrong_year <- wages
wrong_year$casual_wages$salary_year[1] <- 2021
stopifnot(!is.null(prepare_casual_wage_baseline(wrong_year, future)$error))
future_wages <- make_casual_wage_fixture(60L)
stopifnot(nrow(prepare_casual_wage_baseline(future_wages, future)$training) == 48L)
zero_wages <- wages
zero_wages$casual_wages$value[46:48] <- 0
stopifnot(nrow(prepare_casual_wage_baseline(zero_wages, future)$training) == 45L,
          nrow(prepare_casual_wage_baseline(zero_wages, future, include_trailing_zeros = TRUE)$training) == 48L)
cat("PASS casual wage actual selection, partial years, date alignment and source safeguards\n")

casual <- calculate_finance_scenario(baseline, wage_mode = "casual_baseline", wage_pct = 5, casual_baseline = partial_base)
stopifnot(abs(casual$extra_wage_cost - sum(partial_base$forecast$baseline) * 0.05) < 1e-8,
          casual$expenses == zero$expenses + casual$extra_wage_cost)
casual_late <- calculate_finance_scenario(baseline, wage_mode = "casual_baseline", wage_pct = 5,
                                         casual_baseline = partial_base, effective_month = 7)
stopifnot(all(casual_late$table$extra_wage_cost[1:6] == 0),
          abs(casual_late$extra_wage_cost - sum(partial_base$forecast$baseline[7:12]) * 0.05) < 1e-8)
no_wage_zero <- calculate_finance_scenario(baseline, wage_mode = "casual_baseline", wage_pct = 0)
no_wage_rise <- try(calculate_finance_scenario(baseline, wage_mode = "casual_baseline", wage_pct = 5), silent = TRUE)
stopifnot(no_wage_zero$extra_wage_cost == 0, inherits(no_wage_rise, "try-error"))
wrong_dates <- partial_base
wrong_dates$forecast$date[1] <- as.Date("2025-12-01")
stopifnot(inherits(try(calculate_finance_scenario(baseline, wage_mode = "casual_baseline", wage_pct = 5,
                                                casual_baseline = wrong_dates), silent = TRUE), "try-error"))
oversized <- partial_base
oversized$forecast$baseline <- 2000
stopifnot(inherits(try(calculate_finance_scenario(baseline, wage_mode = "casual_baseline", wage_pct = 5,
                                                casual_baseline = oversized), silent = TRUE), "try-error"))
casual_threshold <- calculate_finance_scenario(baseline, wage_mode = "casual_baseline", wage_pct = 5,
  casual_baseline = partial_base, price_pct = casual$min_price_increase * 100)
stopifnot(casual_threshold$covers_costs)
cat("PASS casual wage increments without double counting, timing and break-even threshold\n")

# Paid hours are an extra scenario relative to the aggregate payroll forecast.
known_casual <- partial_base
known_casual$forecast$baseline <- 100
pay_and_hours <- calculate_finance_scenario(baseline, wage_mode = "casual_baseline",
  wage_pct = 5, casual_hours_pct = 20, casual_baseline = known_casual)
stopifnot(abs(pay_and_hours$extra_wage_cost - 1200 * 0.26) < 1e-8,
          abs(pay_and_hours$scenario_casual_wages - 1200 * 1.26) < 1e-8,
          abs(pay_and_hours$expenses - (zero$expenses + 312)) < 1e-8,
          pay_and_hours$assumptions$value[pay_and_hours$assumptions$assumption == "casual_paid_hours_change_pct"] == "20")
hours_only <- calculate_finance_scenario(baseline, wage_mode = "casual_baseline",
  casual_hours_pct = 20, casual_baseline = known_casual)
less_hours <- calculate_finance_scenario(baseline, wage_mode = "casual_baseline",
  wage_pct = 5, casual_hours_pct = -20, casual_baseline = known_casual)
no_hours <- calculate_finance_scenario(baseline, wage_mode = "casual_baseline",
  wage_pct = 5, casual_hours_pct = -100, casual_baseline = known_casual)
hours_late <- calculate_finance_scenario(baseline, wage_mode = "casual_baseline",
  wage_pct = 5, casual_hours_pct = 20, casual_baseline = known_casual, effective_month = 7)
stopifnot(abs(hours_only$extra_wage_cost - 240) < 1e-8,
          abs(less_hours$extra_wage_cost + 192) < 1e-8,
          no_hours$extra_wage_cost == -1200, no_hours$scenario_casual_wages == 0,
          all(hours_late$table$extra_wage_cost[1:6] == 0),
          all(hours_late$table$scenario_casual_wages[1:6] == 100),
          abs(hours_late$extra_wage_cost - 156) < 1e-8)
for (invalid_hours in c(-101, 201, NA_real_, Inf)) {
  stopifnot(inherits(try(calculate_finance_scenario(baseline, wage_mode = "casual_baseline",
    casual_hours_pct = invalid_hours, casual_baseline = known_casual), silent = TRUE), "try-error"))
}
for (hours_change in c(-10, 10)) {
  stopifnot(inherits(try(calculate_finance_scenario(baseline, wage_mode = "casual_baseline",
    casual_hours_pct = hours_change), silent = TRUE), "try-error"),
    inherits(try(calculate_finance_scenario(baseline, wage_mode = "casual_baseline",
    casual_hours_pct = hours_change, casual_baseline = oversized), silent = TRUE), "try-error"))
}
wrong_hours_dates <- try(calculate_finance_scenario(baseline, wage_mode = "casual_baseline",
  casual_hours_pct = 10, casual_baseline = wrong_dates), silent = TRUE)
stopifnot(inherits(wrong_hours_dates, "try-error"))
known_monthly <- calculate_finance_scenario(baseline, wage_mode = "monthly_amount",
  wage_amount = 100, wage_pct = 99, casual_hours_pct = 100, casual_baseline = known_casual, effective_month = 7)
stopifnot(known_monthly$extra_wage_cost == 600,
          all(known_monthly$table$extra_wage_cost[1:6] == 0),
          all(is.na(known_monthly$table$baseline_casual_wages)),
          !any(c("employee_fte_count", "payroll_share_pct", "casual_paid_hours_change_pct") %in% known_monthly$assumptions$assumption),
          known_monthly$assumptions$value[known_monthly$assumptions$assumption == "monthly_extra_payroll_cost"] == "100")
lower_bound <- known_casual
lower_bound$forecast$baseline <- 1100
reduced <- calculate_finance_scenario(baseline, wage_mode = "casual_baseline",
  casual_hours_pct = -100, casual_baseline = lower_bound)
stopifnot(all(reduced$table$scenario_expenses >= 0),
          all(reduced$table$scenario_expenses_lower80 == 0))
hours_threshold <- calculate_finance_scenario(baseline, wage_mode = "casual_baseline",
  wage_pct = 5, casual_hours_pct = 20, casual_baseline = known_casual,
  price_pct = pay_and_hours$min_price_increase * 100)
stopifnot(hours_threshold$covers_costs)
cat("PASS paid-hours/pay interaction, reductions, monthly alternative, dates and cost thresholds\n")

simple_data <- reactiveVal(list(monthly_reports = fixture, venues_reports = partial))
shiny::testServer(mod_forecasting_server, args = list(data = simple_data), {
  session$setInputs(horizon = "12", casual_wage_pct = 5, price_pct = 0)
  stopifnot(is.null(scenario()$error), nrow(prepared()$training) == 48L,
            scenario()$extra_wage_cost > 0)
  # Removed hours/legacy controls cannot silently add a second wage adjustment.
  session$setInputs(casual_hours_pct = 20, manual_payroll = TRUE,
                    monthly_payroll_amount = 999999, method = "naive",
                    wage_mode = "per_employee", employee_count = 1000)
  financial_base <- baseline()
  payroll_base <- casual_baseline()
  stopifnot(abs(scenario()$extra_wage_cost - sum(payroll_base$forecast$baseline) * 0.05) < 1e-8)
  session$setInputs(casual_wage_pct = NA_real_)
  stopifnot(grepl("Casual pay increase", scenario()$error, fixed = TRUE))
  session$setInputs(casual_wage_pct = 10, price_pct = 3)
  stopifnot(identical(financial_base, baseline()), identical(payroll_base, casual_baseline()),
            abs(scenario()$extra_wage_cost - sum(payroll_base$forecast$baseline) * 0.10) < 1e-8,
            grepl("Predicted casual payroll", output$data_health$html, fixed = TRUE))
  session$setInputs(site = "Site A", from_year = "2022",
                    cutoff = "2025-12-01", effective_month = "7")
  stopifnot(grepl("individual site", scenario()$error))
  session$setInputs(site = "Overall", effective_month = "1")
  stopifnot(is.null(scenario()$error), selected_scope() == "Overall",
            scenario()$table$extra_wage_cost[1] > 0,
            scenario()$assumptions$value[scenario()$assumptions$assumption == "repriceable_income_share_pct"] == "100")
  simple_data(list(monthly = fixture, venues = NULL))
  session$flushReact()
  stopifnot(!is.null(scenario()$error), is.null(baseline()$error))
  session$setInputs(casual_wage_pct = 0)
  stopifnot(is.null(scenario()$error), scenario()$extra_wage_cost == 0)
  incomplete_organisation <- fixture
  incomplete_organisation$finance <- fixture$finance[fixture$finance$site != "Monash Sport", ]
  simple_data(list(monthly = incomplete_organisation, venues = partial))
  session$setInputs(casual_wage_pct = 5)
  stopifnot(grepl("partial set of site summaries", scenario()$error))
  simple_data(list(monthly = NULL, venues = partial))
  session$flushReact()
  stopifnot(!is.null(baseline()$error), !is.null(scenario()$error))
})
ui_simple <- as.character(mod_forecasting_ui("simple"))
stopifnot(grepl("<details", ui_simple, fixed = TRUE),
          !grepl('id="simple-custom_settings"', ui_simple, fixed = TRUE),
          grepl('id="simple-payroll_mode"', ui_simple, fixed = TRUE),
          grepl('id="simple-payroll_in_expenses"', ui_simple, fixed = TRUE),
          !grepl('id="simple-casual_hours_pct"', ui_simple, fixed = TRUE),
          !grepl('id="simple-manual_payroll"', ui_simple, fixed = TRUE),
          !grepl('id="simple-monthly_payroll_amount"', ui_simple, fixed = TRUE),
          !grepl('id="simple-method"', ui_simple, fixed = TRUE),
          !grepl('id="simple-wage_mode"', ui_simple, fixed = TRUE),
          !grepl('id="simple-employee_count"', ui_simple, fixed = TRUE),
          !grepl('id="simple-wage_share"', ui_simple, fixed = TRUE),
          length(gregexpr('id="simple-price_plot"', ui_simple, fixed = TRUE)[[1]]) == 1L,
          regexpr("Income needed to cover costs", ui_simple, fixed = TRUE)[1] <
            regexpr("Price sensitivity", ui_simple, fixed = TRUE)[1],
          regexpr("Price sensitivity", ui_simple, fixed = TRUE)[1] <
            regexpr("Monthly plan", ui_simple, fixed = TRUE)[1])
cat("PASS quick-only inputs, inactive legacy settings, cached models and upload removal\n")
