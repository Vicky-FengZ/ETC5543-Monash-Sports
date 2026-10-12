# Tiered payroll: explicit units, one active reference and no duplicate costs.
groups <- data.frame(
  category = c("Casual attendants", "Casual coaches"), hourly_wage = c(20, 30),
  monthly_hours = c(20, 10), pay_increase_pct = c(5, 0), hours_change_pct = c(20, -10))
budget <- calculate_manual_casual_payroll(groups)
stopifnot(budget$current_monthly == 700, abs(budget$planned_monthly - 774) < 1e-8,
          abs(budget$monthly_change - 74) < 1e-8,
          abs(budget$groups$planned_monthly_cost[1] - 504) < 1e-8)
manual <- calculate_finance_scenario(baseline, wage_mode = "manual_categories",
  payroll_groups = groups, payroll_in_expenses = TRUE,
  wage_pct = NA_real_, casual_hours_pct = NA_real_, casual_baseline = oversized)
stopifnot(abs(manual$extra_wage_cost - 888) < 1e-8,
          abs(manual$expenses - 15288) < 1e-8,
          manual$baseline_casual_wages == 8400,
          abs(manual$scenario_casual_wages - 9288) < 1e-8,
          !any(c("wage_increase_pct", "casual_paid_hours_change_pct") %in% manual$assumptions$assumption),
          nrow(manual$payroll_groups) == 2L)
manual_late <- calculate_finance_scenario(baseline, wage_mode = "manual_categories",
  payroll_groups = groups, payroll_in_expenses = TRUE, effective_month = 7)
stopifnot(all(manual_late$table$extra_wage_cost[1:6] == 0),
          abs(manual_late$extra_wage_cost - 444) < 1e-8,
          all(manual_late$table$scenario_casual_wages[1:6] == 700))
balanced <- calculate_finance_scenario(baseline, wage_mode = "manual_categories",
  payroll_groups = groups, payroll_in_expenses = TRUE,
  price_pct = manual$min_price_increase * 100)
stopifnot(balanced$covers_costs,
          abs(balanced$income - balanced$expenses) < 1e-8)
reduction <- groups
reduction$hours_change_pct <- -100
reduced_manual <- calculate_finance_scenario(baseline, wage_mode = "manual_categories",
  payroll_groups = reduction, payroll_in_expenses = TRUE)
stopifnot(reduced_manual$extra_wage_cost == -8400,
          reduced_manual$scenario_casual_wages == 0,
          all(reduced_manual$table$scenario_expenses >= 0),
          all(reduced_manual$table$scenario_expenses_lower80 >= 0))
new_staff <- groups[1, ]
new_staff$monthly_hours <- 0
new_staff$future_monthly_hours <- 10
new_staff$pay_increase_pct <- 0
new_staff$hours_change_pct <- 200
new_budget <- calculate_manual_casual_payroll(new_staff)
stopifnot(new_budget$current_monthly == 0, new_budget$planned_monthly == 200,
          new_budget$groups$hours_basis == "Future-hours target")
target_staff <- groups[1, ]
target_staff$future_monthly_hours <- 0
stopifnot(calculate_manual_casual_payroll(target_staff)$planned_monthly == 0)

expect_payroll_error <- function(x, pattern = NULL) {
  result <- try(calculate_manual_casual_payroll(x), silent = TRUE)
  stopifnot(inherits(result, "try-error"))
  if (!is.null(pattern)) stopifnot(grepl(pattern, as.character(result), fixed = TRUE))
}
expect_payroll_error(data.frame())
expect_payroll_error(groups[rep(1, 21), ])
for (name in c("hourly_wage", "monthly_hours", "pay_increase_pct", "hours_change_pct")) {
  invalid_groups <- groups
  invalid_groups[[name]][1] <- NA_real_
  expect_payroll_error(invalid_groups)
  invalid_groups[[name]][1] <- Inf
  expect_payroll_error(invalid_groups)
  invalid_groups[[name]] <- as.character(invalid_groups[[name]])
  expect_payroll_error(invalid_groups)
}
for (bad in c("", NA_character_, "  CASUAL COACHES  ")) {
  invalid_groups <- groups
  invalid_groups$category[1] <- bad
  expect_payroll_error(invalid_groups, "unique name")
}
for (name in c("hourly_wage", "monthly_hours", "pay_increase_pct")) {
  invalid_groups <- groups
  invalid_groups[[name]][1] <- -1
  expect_payroll_error(invalid_groups)
}
invalid_groups <- groups
invalid_groups$pay_increase_pct[1] <- 101
expect_payroll_error(invalid_groups)
for (bad in c(-101, 201)) {
  invalid_groups$pay_increase_pct[1] <- 0
  invalid_groups$hours_change_pct[1] <- bad
  expect_payroll_error(invalid_groups)
}
invalid_groups <- groups
invalid_groups$hourly_wage[1] <- 0
expect_payroll_error(invalid_groups, "positive hourly wage")
new_staff$future_monthly_hours <- NA_real_
expect_payroll_error(new_staff, "cannot create hours from zero")
for (bad in c(-1, Inf)) {
  new_staff$future_monthly_hours <- bad
  expect_payroll_error(new_staff)
}
invalid_groups <- groups
invalid_groups$hourly_wage[1] <- 1e308
expect_payroll_error(invalid_groups, "too large")
unconfirmed <- try(calculate_finance_scenario(baseline, wage_mode = "manual_categories",
  payroll_groups = groups), silent = TRUE)
over_costs <- groups
over_costs$monthly_hours <- 10000
over_limit <- try(calculate_finance_scenario(baseline, wage_mode = "manual_categories",
  payroll_groups = over_costs, payroll_in_expenses = TRUE), silent = TRUE)
stopifnot(inherits(unconfirmed, "try-error"), grepl("Confirm", unconfirmed),
          inherits(over_limit, "try-error"), grepl("exceeds", over_limit))
cat("PASS category costs, pay-hours interaction, targets, timing, confirmation and input safeguards\n")

tiered_data <- reactiveVal(list(monthly = fixture, venues = partial))
shiny::testServer(mod_forecasting_server, args = list(data = tiered_data), {
  session$setInputs(horizon = "12", casual_wage_pct = 5, price_pct = 0)
  quick_cost <- scenario()$extra_wage_cost
  original_finance <- baseline()
  original_wages <- casual_baseline()
  stopifnot(is.null(scenario()$error), quick_cost > 0)
  session$setInputs(payroll_mode = "advanced",
    group_1_category = "Attendants", group_1_hourly_wage = 20,
    group_1_monthly_hours = 20, group_1_pay_increase_pct = 5,
    group_1_hours_change_pct = 20, payroll_in_expenses = FALSE)
  stopifnot(grepl("Confirm", scenario()$error, fixed = TRUE),
            abs(advanced_budget()$monthly_change - 104) < 1e-8,
            grepl("Planned monthly payroll", output$payroll_budget_summary$html, fixed = TRUE))
  session$setInputs(payroll_in_expenses = TRUE, casual_wage_pct = NA_real_)
  stopifnot(is.null(scenario()$error),
            abs(scenario()$extra_wage_cost - 1248) < 1e-8,
            identical(original_finance, baseline()),
            grepl("Payroll reference: your inputs", output$data_health$html, fixed = TRUE))
  # Adding/removing category controls preserves values and never reuses removed IDs.
  session$setInputs(add_payroll_group = 1)
  stopifnot(identical(payroll_group_ids(), c(1L, 2L)),
            grepl('value="Attendants"', output$payroll_group_inputs$html, fixed = TRUE),
            grepl('value="20"', output$payroll_group_inputs$html, fixed = TRUE),
            !is.null(scenario()$error)) # new row is incomplete, not silently zero
  session$setInputs(group_2_category = "Coaches", group_2_hourly_wage = 30,
    group_2_monthly_hours = 10, group_2_pay_increase_pct = 0, group_2_hours_change_pct = -10)
  stopifnot(is.null(scenario()$error), abs(scenario()$extra_wage_cost - 888) < 1e-8)
  session$setInputs(remove_payroll_group = 2)
  stopifnot(identical(payroll_group_ids(), 1L), abs(scenario()$extra_wage_cost - 1248) < 1e-8)
  session$setInputs(group_2_hourly_wage = 999999, add_payroll_group = 2)
  stopifnot(identical(payroll_group_ids(), c(1L, 3L)))
  session$setInputs(group_3_category = "New recruits", group_3_hourly_wage = 20,
    group_3_monthly_hours = 0, group_3_pay_increase_pct = 0,
    group_3_hours_change_pct = 0, group_3_future_monthly_hours = 10)
  stopifnot(abs(scenario()$extra_wage_cost - 3648) < 1e-8)
  session$setInputs(remove_payroll_group = 3)
  session$setInputs(custom_settings = TRUE, site = "Site A",
                    from_year = "2022", cutoff = "2025-12-01", effective_month = "7")
  stopifnot(is.null(confirmed_payroll_scope()),
            grepl("Confirm", scenario()$error, fixed = TRUE))
  session$setInputs(payroll_in_expenses = FALSE)
  session$setInputs(payroll_in_expenses = TRUE)
  stopifnot(is.null(scenario()$error), abs(scenario()$extra_wage_cost - 624) < 1e-8,
            identical(confirmed_payroll_scope(), "Site A"))
  session$setInputs(payroll_mode = "quick", casual_wage_pct = 5)
  stopifnot(grepl("individual site", scenario()$error))
  session$setInputs(custom_settings = FALSE)
  stopifnot(is.null(scenario()$error), abs(scenario()$extra_wage_cost - quick_cost) < 1e-8,
            identical(original_finance, baseline()), identical(original_wages, casual_baseline()))
  # No Venues data is needed in manual mode, but a changed finance basis must be confirmed.
  tiered_data(list(monthly = fixture, venues = NULL))
  session$setInputs(payroll_mode = "advanced")
  session$setInputs(payroll_in_expenses = FALSE)
  session$setInputs(payroll_in_expenses = TRUE)
  stopifnot(is.null(scenario()$error), abs(scenario()$extra_wage_cost - 1248) < 1e-8,
            !any(grepl("Casual payroll", output$model_table, fixed = TRUE)))
  modified_finance <- fixture
  modified_finance$finance$value[1] <- modified_finance$finance$value[1] + 1
  tiered_data(list(monthly = modified_finance, venues = NULL))
  session$flushReact()
  stopifnot(is.null(confirmed_payroll_scope()), grepl("Confirm", scenario()$error, fixed = TRUE))
  session$setInputs(payroll_mode = "invalid")
  stopifnot(grepl("Choose Quick estimate", scenario()$error, fixed = TRUE))
  session$setInputs(payroll_mode = "advanced", payroll_in_expenses = TRUE)
  session$setInputs(remove_payroll_group = 1)
  stopifnot(length(payroll_group_ids()) == 0L, grepl("at least one", scenario()$error, fixed = TRUE))
  session$setInputs(reset_plan = 1)
  stopifnot(length(payroll_group_ids()) == 1L, payroll_group_ids() == 4L,
            is.null(confirmed_payroll_scope()), is.na(payroll_groups()$hourly_wage),
            !is.null(advanced_budget()$error))
})
cat("PASS mode exclusivity, group editing, confirmation reset, missing wage data and reset wiring\n")
