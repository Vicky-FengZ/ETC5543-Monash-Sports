# Page structure without an additional HTML/browser testing dependency.
layout_html <- as.character(mod_forecasting_ui("layout"))
options_start <- regexpr('<details id="layout-more_options"', layout_html, fixed = TRUE)[1]
stopifnot(options_start > 0L)
options_tail <- substring(layout_html, options_start)
options_end <- regexpr("</details>", options_tail, fixed = TRUE)[1]
stopifnot(options_end > 0L)
options_html <- substr(options_tail, 1L, options_end + nchar("</details>") - 1L)
outside_options <- paste(substr(layout_html, 1L, options_start - 1L),
  substring(options_tail, options_end + nchar("</details>")))
for (control in c("payroll_mode", "from_year", "cutoff",
                   "price_share", "volume_pct", "effective_month", "finance_zero_review", "wage_zero_review")) {
  control_id <- paste0('id="layout-', control, '"')
  stopifnot(grepl(control_id, options_html, fixed = TRUE),
            !grepl(control_id, outside_options, fixed = TRUE))
}
for (control in c("horizon", "casual_wage_pct", "monthly_payroll_adjustment", "price_pct", "monthly_table",
                  "download_plan", "download_assumptions")) {
  stopifnot(grepl(paste0('id="layout-', control, '"'), outside_options, fixed = TRUE))
}
stopifnot(length(gregexpr("<details", layout_html, fixed = TRUE)[[1]]) == 1L,
          !grepl("Treat final zero months as confirmed actuals", layout_html, fixed = TRUE),
          !grepl("payroll_in_expenses|payroll_group_inputs|add_payroll_group", layout_html),
          !grepl('id="layout-site"', layout_html, fixed = TRUE),
          !grepl("Scope and history|site-specific", layout_html),
          grepl("Payroll, history, pricing and timing", layout_html, fixed = TRUE),
          grepl("Monash Sport overall", layout_html, fixed = TRUE),
          !grepl("Data notes and limitations|Explore the details|Forecast checks|How it works", layout_html),
          !grepl('id="layout-custom_settings"', layout_html, fixed = TRUE),
          !grepl('id="layout-model_table"', layout_html, fixed = TRUE),
          !grepl('id="layout-validation_note"', layout_html, fixed = TRUE),
          grepl("forecast-monthly-plan-header", layout_html, fixed = TRUE),
          regexpr('id="layout-monthly_table"', layout_html, fixed = TRUE)[1] >
            options_start + options_end)
cat("PASS grouped optional controls, direct monthly plan and removed technical UI\n")

layout_data <- reactiveVal(list(monthly = fixture, venues = partial))
shiny::testServer(mod_forecasting_server, args = list(data = layout_data), {
  session$setInputs(horizon = "12", site = "Overall", from_year = "2022",
    cutoff = "2025-12-01", payroll_mode = "quick", casual_wage_pct = 5,
    price_pct = 3, price_share = 100, volume_pct = 0, effective_month = "1")
  saved_finance <- baseline()
  saved_wages <- casual_baseline()
  session$setInputs(price_share = 50, volume_pct = -10, effective_month = "7")
  expected <- calculate_finance_scenario(saved_finance, wage_mode = "casual_baseline",
    wage_pct = 5, casual_baseline = saved_wages, price_pct = 3,
    price_share = 50, volume_pct = -10, effective_month = 7)
  stopifnot(is.null(scenario()$error), identical(scenario()$table, expected$table),
            identical(saved_finance, baseline()), identical(saved_wages, casual_baseline()),
            all(scenario()$table$extra_wage_cost[1:6] == 0),
            grepl("Quick payroll", output$options_state$html, fixed = TRUE),
            !grepl("Data notes and limitations", output$data_health$html, fixed = TRUE))
  # Obsolete enable flags do not silently cancel visible settings.
  session$setInputs(custom_settings = FALSE)
  stopifnot(identical(scenario()$table, expected$table), selected_scope() == "Overall")
  quick_plan <- utils::read.csv(output$download_plan, stringsAsFactors = FALSE)
  quick_assumptions <- utils::read.csv(output$download_assumptions, stringsAsFactors = FALSE)
  stopifnot(nrow(quick_plan) == 12L,
            isTRUE(all.equal(quick_plan$scenario_income, expected$table$scenario_income)),
            quick_assumptions$value[quick_assumptions$assumption == "payroll_mode"] == "quick",
            quick_assumptions$value[quick_assumptions$assumption == "scope"] == "Overall",
            quick_assumptions$value[quick_assumptions$assumption == "repriceable_income_share_pct"] == "50",
            "casual_wage_model" %in% quick_assumptions$assumption,
            !"custom_settings" %in% quick_assumptions$assumption,
            grepl("Income Needed", output$monthly_table, fixed = TRUE))
  session$setInputs(payroll_mode = "manual", casual_wage_pct = NA_real_,
    monthly_payroll_adjustment = -100)
  stopifnot(is.null(scenario()$error), scenario()$extra_wage_cost == -600,
            identical(saved_finance, baseline()),
            grepl("Manual adjustment", output$options_state$html, fixed = TRUE))
  manual_plan <- utils::read.csv(output$download_plan, stringsAsFactors = FALSE)
  manual_assumptions <- utils::read.csv(output$download_assumptions, stringsAsFactors = FALSE)
  stopifnot(nrow(manual_plan) == 12L,
            sum(manual_plan$extra_wage_cost) == -600,
            all(manual_plan$extra_wage_cost[1:6] == 0),
            all(manual_plan$extra_wage_cost[7:12] == -100),
            all(is.na(manual_plan$baseline_casual_wages)),
            all(is.na(manual_plan$scenario_casual_wages)),
            manual_assumptions$value[manual_assumptions$assumption == "payroll_mode"] == "manual",
            manual_assumptions$value[manual_assumptions$assumption == "scope"] == "Overall",
            manual_assumptions$value[manual_assumptions$assumption == "wage_mode"] == "monthly_amount",
            manual_assumptions$value[manual_assumptions$assumption == "monthly_payroll_adjustment"] == "-100",
            manual_assumptions$value[manual_assumptions$assumption == "effective_from"] == "2026-07",
            !any(grepl("payroll_group_|payroll_in_expenses|wage_increase_pct", manual_assumptions$assumption)),
            !"casual_wage_model" %in% manual_assumptions$assumption,
            grepl("-$100", output$monthly_table, fixed = TRUE),
            !grepl("Predicted Casual Payroll|Scenario Casual Payroll", output$monthly_table))
})
cat("PASS direct settings, unchanged models, mode badges and quick/manual CSV downloads\n")
