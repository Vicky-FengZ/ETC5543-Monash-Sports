# Contextual confirmations must not weaken the underlying data safeguards.
zero_finance <- make_forecast_fixture()
zero_finance$finance$value[zero_finance$finance$date >= as.Date("2025-11-01")] <- 0
zero_wage_data <- make_casual_wage_fixture()
zero_wage_data$casual_wages$value[46:48] <- 0
expected_finance_zeros <- as.Date(c("2025-11-01", "2025-12-01"))
expected_wage_zeros <- as.Date(c("2025-10-01", "2025-11-01", "2025-12-01"))
stopifnot(identical(prepare_forecast_history(zero_finance)$zero_months, expected_finance_zeros),
          identical(prepare_casual_wage_history(zero_wage_data)$zero_months, expected_wage_zeros))

# A blank between zero months must not be included in the zero-month question.
gap_zeros <- zero_finance
gap_zeros$finance$value[gap_zeros$finance$date == as.Date("2025-11-01")] <- NA_real_
p <- prepare_forecast_history(gap_zeros)
stopifnot(identical(p$zero_months, as.Date("2025-12-01")), p$excluded_months == 1L,
          max(p$training$date) == as.Date("2025-10-01"))
gap_wages <- zero_wage_data
gap_wages$casual_wages$is_reported[47] <- FALSE
w <- prepare_casual_wage_history(gap_wages)
stopifnot(identical(w$zero_months, as.Date(c("2025-10-01", "2025-12-01"))),
          max(w$training$date) == as.Date("2025-09-01"))
confirmed_gap <- prepare_casual_wage_history(gap_wages, include_trailing_zeros = TRUE)
stopifnot(!is.null(confirmed_gap$error)) # one month after the gap, not 48 months

# Middle-series zeros are legitimate actuals; blank final months stay unreported.
middle_zeros <- make_forecast_fixture()
middle_zeros$finance$value[middle_zeros$finance$date == as.Date("2024-07-01")] <- 0
stopifnot(!length(prepare_forecast_history(middle_zeros)$zero_months),
          nrow(prepare_forecast_history(middle_zeros)$training) == 48L,
          !length(prepare_casual_wage_history(partial)$zero_months))
all_zeros <- zero_finance
all_zeros$finance$value[] <- 0
stopifnot(!is.null(prepare_forecast_history(all_zeros)$error),
          length(prepare_forecast_history(all_zeros)$zero_months) == 48L,
          is.null(prepare_forecast_history(all_zeros, include_trailing_zeros = TRUE)$error))
cat("PASS exact zero months, missing-versus-zero distinction and all-zero safeguards\n")

options_data <- reactiveVal(list(monthly = fixture, venues = wages))
shiny::testServer(mod_forecasting_server, args = list(data = options_data), {
  session$setInputs(horizon = "12", site = "Overall", from_year = "2022",
    cutoff = "2025-12-01", payroll_mode = "quick", casual_wage_pct = 0)
  stopifnot(is.null(output$finance_zero_review), is.null(output$wage_zero_review))
  options_data(list(monthly = zero_finance, venues = zero_wage_data))
  session$flushReact()
  stopifnot(grepl("Nov 2025 to Dec 2025", output$finance_zero_review$html, fixed = TRUE),
            grepl("Not yet reported", output$finance_zero_review$html, fixed = TRUE),
            grepl("Confirmed zero actuals", output$finance_zero_review$html, fixed = TRUE),
            !finance_zeros_confirmed(), !wage_zeros_confirmed(),
            nrow(prepared()$training) == 46L,
            max(casual_baseline()$training$date) == as.Date("2025-09-01"))
  wage_token <- paste0(wage_zero_state()$version, ":confirmed")
  session$setInputs(wage_zero_status = wage_token)
  stopifnot(wage_zeros_confirmed(), !finance_zeros_confirmed(),
            nrow(prepared()$training) == 46L,
            max(casual_baseline()$training$date) == as.Date("2025-10-01"))
  finance_token <- paste0(finance_zero_state()$version, ":confirmed")
  session$setInputs(finance_zero_status = finance_token)
  stopifnot(finance_zeros_confirmed(), nrow(prepared()$training) == 48L,
            !wage_zeros_confirmed(), # finance cutoff changed, so recheck wage months
            grepl("Oct 2025 to Dec 2025", output$wage_zero_review$html, fixed = TRUE))
  wage_token <- paste0(wage_zero_state()$version, ":confirmed")
  session$setInputs(wage_zero_status = wage_token)
  stopifnot(wage_zeros_confirmed(), nrow(casual_baseline()$training) == 48L)
  # Price edits cannot invalidate actual reporting status or refit baselines.
  saved_baseline <- baseline()
  saved_wage_baseline <- casual_baseline()
  session$setInputs(price_pct = 5)
  stopifnot(finance_zeros_confirmed(), wage_zeros_confirmed(),
            identical(baseline(), saved_baseline), identical(casual_baseline(), saved_wage_baseline))
  exported <- utils::read.csv(output$download_assumptions, stringsAsFactors = FALSE)
  stopifnot(exported$value[exported$assumption == "finance_final_zeros_confirmed"] == "TRUE",
            exported$value[exported$assumption == "casual_wage_final_zeros_confirmed"] == "TRUE")
  session$setInputs(site = "Site A")
  stopifnot(finance_zeros_confirmed(), wage_zeros_confirmed(), selected_scope() == "Overall",
            identical(baseline(), saved_baseline), identical(casual_baseline(), saved_wage_baseline))
  session$setInputs(from_year = "2023")
  stopifnot(!finance_zeros_confirmed(), !wage_zeros_confirmed())
  # Delayed browser events cannot confirm a changed historical period.
  session$setInputs(finance_zero_status = finance_token, wage_zero_status = wage_token)
  stopifnot(!finance_zeros_confirmed(), !wage_zeros_confirmed())
  session$setInputs(site = "Overall")
  stopifnot(!finance_zeros_confirmed(), !wage_zeros_confirmed())
  session$setInputs(finance_zero_status = paste0(finance_zero_state()$version, ":confirmed"))
  session$setInputs(from_year = "2024")
  stopifnot(!finance_zeros_confirmed())
  session$setInputs(finance_zero_status = paste0(finance_zero_state()$version, ":confirmed"))
  session$setInputs(cutoff = "2025-11-01")
  stopifnot(!finance_zeros_confirmed())
  session$setInputs(finance_zero_status = paste0(finance_zero_state()$version, ":confirmed"))
  changed_finance <- zero_finance
  changed_finance$finance$value[1] <- changed_finance$finance$value[1] + 1
  options_data(list(monthly = changed_finance, venues = zero_wage_data))
  session$flushReact()
  stopifnot(!finance_zeros_confirmed())
  session$setInputs(finance_zero_status = paste0(finance_zero_state()$version, ":confirmed"))
  session$setInputs(reset_plan = 1)
  stopifnot(!finance_zeros_confirmed(), !wage_zeros_confirmed())
})
cat("PASS conditional zero review, independent payroll/finance choices and stale-context reset\n")

# Wage reporting questions are relevant only to the Quick method.
shiny::testServer(mod_forecasting_server, args = list(data = list(monthly = fixture, venues = zero_wage_data)), {
  session$setInputs(horizon = "12", from_year = "2022", cutoff = "2025-12-01",
                   payroll_mode = "quick", casual_wage_pct = 5)
  token <- paste0(wage_zero_state()$version, ":confirmed")
  session$setInputs(wage_zero_status = token)
  stopifnot(wage_zeros_confirmed(), !is.null(output$wage_zero_review))
  session$setInputs(payroll_mode = "manual", monthly_payroll_adjustment = -100)
  stopifnot(is.null(output$wage_zero_review), !wage_zeros_confirmed(),
            is.null(scenario()$error), scenario()$extra_wage_cost == -1200)
  session$setInputs(payroll_mode = "quick")
  stopifnot(!wage_zeros_confirmed(), !is.null(output$wage_zero_review))
  session$setInputs(wage_zero_status = token)
  stopifnot(!wage_zeros_confirmed())
})
cat("PASS payroll-method changes hide irrelevant wage questions and invalidate stale confirmations\n")
