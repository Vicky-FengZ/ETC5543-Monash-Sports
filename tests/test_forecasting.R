# Business invariants and time-series edge cases, without an extra test package.

make_forecast_fixture <- function(start = "2022-01-01", n = 48L) {
  dates <- seq(as.Date(start), by = "month", length.out = n)
  finance <- do.call(rbind, lapply(seq_along(dates), function(i) {
    seasonal <- 200 * sin(2 * pi * i / 12)
    data.frame(date = rep(dates[i], 6), report_year = as.integer(format(dates[i], "%Y")),
               site = rep(c("Monash Sport", "Site A", "Site B"), each = 2),
               is_summary = TRUE, metric = rep(c("Income", "Expenses"), 3),
               value_type = "Actual", value = c(1000 + seasonal + i * 2, 1200 + seasonal / 2,
                                                 600, 700, 400 + seasonal + i * 2, 500 + seasonal / 2),
               source_file = paste0(format(dates[i], "%Y"), ".xlsx"), stringsAsFactors = FALSE)
  }))
  list(finance = finance, validation = data.frame())
}

fixture <- make_forecast_fixture()
prepared <- prepare_forecast_history(fixture)
stopifnot(is.null(prepared$error), nrow(prepared$training) == 48L,
          prepared$training$income[1] == fixture$finance$value[1],
          !"net_result" %in% names(prepared$training))
cat("PASS forecasting summary selection without double counting\n")

fallback <- fixture
fallback$finance <- fallback$finance[fallback$finance$site != "Monash Sport", ]
stopifnot(isTRUE(all.equal(prepare_forecast_history(fallback)$training$income, prepared$training$income)))
fallback$finance <- fallback$finance[-1, ]
stopifnot(is.na(prepare_forecast_history(fallback)$history$income[1]))
site <- prepare_forecast_history(fixture, site = "Site A")
stopifnot(all(site$training$income == 600), all(site$training$expenses == 700))

duplicate <- fixture
duplicate$finance <- rbind(fixture$finance, fixture$finance[1, ])
stopifnot(grepl("overlapping", prepare_forecast_history(duplicate)$error))
gapped <- fixture
gapped$finance <- gapped$finance[gapped$finance$report_year != 2024, ]
gap_prepared <- prepare_forecast_history(gapped)
stopifnot(nrow(gap_prepared$training) == 12L,
          min(gap_prepared$training$date) == as.Date("2025-01-01"),
          sum(is.na(gap_prepared$history$income)) == 12L)
short <- prepare_forecast_history(fixture, cutoff = "2022-11-01")
stopifnot(!is.null(fit_finance_forecasts(short)$error))
zeros <- fixture
zeros$finance$value[zeros$finance$date >= as.Date("2025-11-01")] <- 0
stopifnot(nrow(prepare_forecast_history(zeros)$training) == 46L,
          nrow(prepare_forecast_history(zeros, include_trailing_zeros = TRUE)$training) == 48L)
cat("PASS forecasting gaps, overlapping files, short history and unconfirmed zeros\n")

seasonal <- fit_finance_forecasts(gap_prepared, 12L)
stopifnot(is.null(seasonal$error), all(seasonal$models == "Seasonal naive"),
          nrow(seasonal$scores) == 0L,
          all(is.na(seasonal$forecast$lower80)),
          seasonal$forecast$date[1] == as.Date("2026-01-01"),
          isTRUE(all.equal(seasonal$forecast$baseline[1:12], gap_prepared$training$income)))
full <- fit_finance_forecasts(prepared, 12L)
stopifnot(is.null(full$error), nrow(full$scores) >= 4L,
          all(full$scores$origins == full$scores$origins[1]),
          all(full$scores$horizon == full$scores$horizon[1]),
          all(is.finite(full$forecast$baseline)), all(full$forecast$baseline >= 0))
for (metric in c("income", "expenses")) {
  scores <- full$scores[full$scores$metric == metric, ]
  stopifnot(scores$rmse[scores$selected] == min(scores$rmse))
}
holt <- fit_finance_forecasts(prepared, 6L, "holt_winters")
stopifnot(is.null(holt$error), all(is.finite(holt$forecast$lower80)))
cat("PASS forecast benchmarks, equal-fold backtests and interval availability\n")

# A known cash-flow example: $1,000/month income, $1,200/month expenses.
baseline <- list(error = NULL, forecast = data.frame(
  date = rep(seq(as.Date("2026-01-01"), by = "month", length.out = 12), 2),
  metric = rep(c("income", "expenses"), each = 12),
  baseline = rep(c(1000, 1200), each = 12),
  lower80 = rep(c(800, 1000), each = 12), upper80 = rep(c(1200, 1400), each = 12)
))
zero <- calculate_finance_scenario(baseline)
stopifnot(zero$income == 12000, zero$expenses == 14400,
          abs(zero$min_income_growth - 0.2) < 1e-10,
          abs(zero$min_price_increase - 0.2) < 1e-10,
          !zero$covers_costs)
fixed <- calculate_finance_scenario(baseline, wage_amount = 1200, price_share = 50)
stopifnot(fixed$expenses == 15600, fixed$extra_wage_cost == 1200,
          abs(fixed$min_price_increase - 0.6) < 1e-10)
employee <- calculate_finance_scenario(baseline, wage_mode = "per_employee", wage_amount = 300, employee_count = 4)
stopifnot(employee$extra_wage_cost == 1200)
percent <- calculate_finance_scenario(baseline, wage_mode = "percentage", wage_pct = 10,
                                     wage_share = 50, other_cost_pct = 5,
                                     wage_amount = NA_real_, employee_count = NA_real_)
stopifnot(percent$expenses == 15480, percent$extra_wage_cost == 720)
late <- calculate_finance_scenario(baseline, wage_amount = 1200, effective_month = 7)
stopifnot(late$extra_wage_cost == 600, all(late$table$extra_wage_cost[1:6] == 0),
          abs(late$min_price_increase - 0.5) < 1e-10)
unachievable <- calculate_finance_scenario(baseline, price_share = 0)
stopifnot(is.infinite(unachievable$min_price_increase))
demand <- calculate_finance_scenario(baseline, wage_amount = 1200, volume_pct = -10, price_share = 50, effective_month = 7)
at_threshold <- calculate_finance_scenario(baseline, wage_amount = 1200, volume_pct = -10,
                                           price_share = 50, effective_month = 7,
                                           price_pct = demand$min_price_increase * 100)
below_threshold <- calculate_finance_scenario(baseline, wage_amount = 1200, volume_pct = -10,
                                              price_share = 50, effective_month = 7,
                                              price_pct = demand$min_price_increase * 100 - 0.001)
stopifnot(at_threshold$covers_costs, !below_threshold$covers_costs)
stopifnot(forecast_required_rate(0.00011) == "0.1%",
          forecast_required_rate(0.05001) == "5.1%",
          forecast_required_rate(0) == "0.0%")
missing_share <- try(calculate_finance_scenario(baseline, wage_mode = "percentage", wage_pct = 5), silent = TRUE)
invalid <- try(calculate_finance_scenario(baseline, volume_pct = -100), silent = TRUE)
stopifnot(inherits(missing_share, "try-error"), inherits(invalid, "try-error"))
cat("PASS wage scenarios, partial-year timing, demand and exact break-even pricing\n")

suppressPackageStartupMessages({ library(shiny); library(bslib); library(bsicons) })
ui_text <- as.character(mod_forecasting_ui("forecast_test"))
stopifnot(!grepl("Net Result|Forecast Prototype|Forecast vs Budget", ui_text),
          !grepl('id="forecast_test-method"', ui_text, fixed = TRUE),
          !grepl('id="forecast_test-wage_mode"', ui_text, fixed = TRUE))
data_source <- reactiveVal(list(monthly = fixture))
shiny::testServer(mod_forecasting_server, args = list(data = data_source), {
  session$setInputs(site = "Overall", horizon = "12",
                    cutoff = "2025-12-01", from_year = "2022", payroll_mode = "advanced",
                    group_1_category = "Casual staff", group_1_hourly_wage = 20,
                    group_1_monthly_hours = 20, group_1_pay_increase_pct = 0,
                    group_1_hours_change_pct = 0, payroll_in_expenses = TRUE,
                    effective_month = "1")
  stopifnot(is.null(baseline()$error), is.null(scenario()$error))
  previous_baseline <- baseline()
  session$setInputs(group_1_pay_increase_pct = 25, price_pct = 10)
  stopifnot(identical(previous_baseline, baseline()), scenario()$extra_wage_cost == 1200)
  data_source(list(monthly = NULL))
  session$flushReact()
  stopifnot(!is.null(baseline()$error), !is.null(scenario()$error))
})
cat("PASS forecasting Shiny input wiring, unchanged baseline and removed-data state\n")
