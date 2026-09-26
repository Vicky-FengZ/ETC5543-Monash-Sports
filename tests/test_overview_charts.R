suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(bsicons)
})

make_finance_rows <- function(
  year,
  site,
  is_summary,
  income_actual,
  income_budget,
  expenses_actual,
  expenses_budget,
  source_file
) {
  data.frame(
    report_year = rep(as.integer(year), 4L),
        date = rep( as.Date(sprintf("%04d-01-01", year)), 4L),
    site = rep(site, 4L),
    is_summary = rep(is_summary, 4L),
    metric = c("Income", "Income", "Expenses", "Expenses"),
    value_type = c("Actual", "Budget", "Actual", "Budget"),
    value = c(
      income_actual,
      income_budget,
      expenses_actual,
      expenses_budget
    ),
    source_file = rep(source_file, 4L),
    stringsAsFactors = FALSE
  )
}

make_monthly_finance_rows <- function(
  year,
  month,
  site,
  is_summary,
  income_actual,
  income_budget,
  expenses_actual,
  expenses_budget,
  source_file
) {

  month_date <- as.Date(
    sprintf("%04d-%02d-01", year, month)
  )

  data.frame(
    report_year = rep(as.integer(year), 4L),
    date = rep(month_date, 4L),
    month = rep(as.integer(month), 4L),
    month_label = rep(month.abb[[month]], 4L),
    site = rep(site, 4L),
    is_summary = rep(is_summary, 4L),
    metric = c(
      "Income",
      "Income",
      "Expenses",
      "Expenses"
    ),
    value_type = c(
      "Actual",
      "Budget",
      "Actual",
      "Budget"
    ),
    value = c(
      income_actual,
      income_budget,
      expenses_actual,
      expenses_budget
    ),
    source_file = rep(source_file, 4L),
    stringsAsFactors = FALSE
  )
}

make_revenue_row <- function(
  year,
  site,
  actual_income,
  discount_opportunity,
  reported_total_revenue,
  validation_status,
  source_file
) {
  potential_revenue <- actual_income + discount_opportunity

  data.frame(
    report_year = as.integer(year),
    site = site,
    is_summary = TRUE,
    actual_income = actual_income,
    discount_opportunity = discount_opportunity,
    calculated_total_revenue = potential_revenue,
    reported_total_revenue = reported_total_revenue,
    validation_difference = reported_total_revenue - potential_revenue,
    validation_status = validation_status,
    source_file = source_file,
    stringsAsFactors = FALSE
  )
}

synthetic_finance <- rbind(
  make_finance_rows(2024, "Monash Sport", TRUE, 120, 100, 90, 80, "2024.xlsx"),
  make_finance_rows(2024, "Clayton", TRUE, 999, 999, 999, 999, "2024.xlsx"),
  make_finance_rows(2024, "Monash Sport", FALSE, 888, 888, 888, 888, "2024.xlsx"),
  make_finance_rows(2025, "Monash Sport", TRUE, 150, 140, 110, 100, "2025.xlsx")
)

synthetic_reports <- structure(
  list(
    finance = synthetic_finance,
    total_revenue = rbind(
      make_revenue_row(2024, "Monash Sport", 120, 30, 150, "pass", "2024.xlsx"),
      make_revenue_row(2024, "Clayton", 999, 999, 1998, "pass", "2024.xlsx"),
      make_revenue_row(2025, "Monash Sport", 150, 50, 200, "pass", "2025.xlsx")
    ),
    files = data.frame(
      source_file = c("2024.xlsx", "2025.xlsx"),
      report_year = c(2024L, 2025L),
      stringsAsFactors = FALSE
    )
  ),
  class = "monthly_site_report_data"
)

annual <- calculate_overview_annual_finance(synthetic_reports)
stopifnot(
  identical(annual$report_year, c(2024L, 2025L)),
  identical(annual$income_actual, c(120, 150)),
  identical(annual$expenses_actual, c(90, 110)),
  identical(annual$net_actual, c(30, 40)),
  identical(annual$net_budget, c(20, 40)),
  identical(annual$budget_variance, c(10, 0)),
  all(annual$aggregation_source == "Monash Sport YTD Summary"),
  length(overview_duplicate_report_years(synthetic_reports)) == 0L
)

# -------------------------------------------------------------------------
# Monthly Overview finance calculation
# -------------------------------------------------------------------------

monthly_reports_fixture <- list(
  finance = rbind(

    # Organisation-wide values that should be used.
    make_monthly_finance_rows(
      2025, 1,
      "Monash Sport", TRUE,
      120, 110,
      80, 75,
      "2025.xlsx"
    ),

    make_monthly_finance_rows(
      2025, 2,
      "Monash Sport", TRUE,
      150, 140,
      90, 85,
      "2025.xlsx"
    ),

    # Site values deliberately made very large.
    # These must NOT be added to the Monash Sport total.
    make_monthly_finance_rows(
      2025, 1,
      "Clayton", TRUE,
      999, 999,
      999, 999,
      "2025.xlsx"
    ),

    make_monthly_finance_rows(
      2025, 2,
      "Clayton", TRUE,
      999, 999,
      999, 999,
      "2025.xlsx"
    )
  )
)

monthly_finance <- calculate_overview_monthly_finance(
  monthly_reports_fixture
)

stopifnot(
  nrow(monthly_finance) == 2L,

  identical(
    monthly_finance$date,
    as.Date(c(
      "2025-01-01",
      "2025-02-01"
    ))
  ),

  identical(
    monthly_finance$income_actual,
    c(120, 150)
  ),

  identical(
    monthly_finance$income_budget,
    c(110, 140)
  ),

  identical(
    monthly_finance$expenses_actual,
    c(80, 90)
  ),

  identical(
    monthly_finance$expenses_budget,
    c(75, 85)
  ),

  identical(
    monthly_finance$net_actual,
    c(40, 60)
  ),

  identical(
    monthly_finance$net_budget,
    c(35, 55)
  ),

  all(
    monthly_finance$aggregation_source ==
      "Monash Sport Monthly Summary"
  )
)

monthly_fallback_fixture <- list(
  finance = rbind(

    make_monthly_finance_rows(
      2026, 1,
      "Clayton", TRUE,
      50, 45,
      30, 28,
      "2026.xlsx"
    ),

    make_monthly_finance_rows(
      2026, 1,
      "Caulfield", TRUE,
      70, 65,
      40, 38,
      "2026.xlsx"
    )
  )
)

monthly_fallback <- calculate_overview_monthly_finance(
  monthly_fallback_fixture
)

stopifnot(
  nrow(monthly_fallback) == 1L,
  monthly_fallback$income_actual[[1L]] == 120,
  monthly_fallback$income_budget[[1L]] == 110,
  monthly_fallback$expenses_actual[[1L]] == 70,
  monthly_fallback$expenses_budget[[1L]] == 66,
  monthly_fallback$net_actual[[1L]] == 50,
  monthly_fallback$net_budget[[1L]] == 44,
  monthly_fallback$aggregation_source[[1L]] ==
    "Sum of Site Monthly Summaries"
)

annual_revenue <- calculate_overview_annual_revenue(synthetic_reports)
stopifnot(
  identical(annual_revenue$report_year, c(2024L, 2025L)),
  identical(annual_revenue$actual_income, c(120, 150)),
  identical(annual_revenue$discount_opportunity, c(30, 50)),
  identical(annual_revenue$potential_revenue, c(150, 200)),
  isTRUE(all.equal(annual_revenue$capture_rate, c(0.8, 0.75))),
  all(annual_revenue$validation_status == "pass"),
  all(annual_revenue$aggregation_source == "Monash Sport YTD Summary")
)

fallback_reports <- list(
  finance = rbind(
    make_finance_rows(2026, "Clayton", TRUE, 50, 45, 30, 28, "2026.xlsx"),
    make_finance_rows(2026, "Caulfield", TRUE, 70, 65, 40, 38, "2026.xlsx")
  ),
  total_revenue = rbind(
    make_revenue_row(2026, "Clayton", 50, 10, 60, "pass", "2026.xlsx"),
    make_revenue_row(2026, "Caulfield", 70, 20, 85, "fail", "2026.xlsx")
  )
)
fallback <- calculate_overview_annual_finance(fallback_reports)
fallback_revenue <- calculate_overview_annual_revenue(fallback_reports)
stopifnot(
  fallback$income_actual[[1L]] == 120,
  fallback$expenses_actual[[1L]] == 70,
  fallback$aggregation_source[[1L]] == "Sum of Site YTD Summaries",
  fallback_revenue$actual_income[[1L]] == 120,
  fallback_revenue$discount_opportunity[[1L]] == 30,
  fallback_revenue$potential_revenue[[1L]] == 150,
  fallback_revenue$reported_total_revenue[[1L]] == 145,
  fallback_revenue$validation_status[[1L]] == "fail",
  fallback_revenue$aggregation_source[[1L]] == "Sum of Site YTD Summaries"
)

duplicate_reports <- synthetic_reports
duplicate_reports$files <- rbind(
  duplicate_reports$files,
  data.frame(
    source_file = "2025 revised.xlsx",
    report_year = 2025L,
    stringsAsFactors = FALSE
  )
)
stopifnot(identical(overview_duplicate_report_years(duplicate_reports), 2025L))

stopifnot(
  identical(overview_metric_values(annual, "Income"), c(120, 150)),
  identical(overview_metric_values(annual, "Expenses"), c(90, 110)),
  identical(overview_metric_values(annual, "Net Result"), c(30, 40)),
  identical(overview_metric_values(annual, "Budget Variance"), c(10, 0))
)

ui_html <- as.character(mod_overview_ui("overview-test"))
stopifnot(
  grepl("overview-test-monthly_income_expenses",ui_html,fixed = TRUE ),
  grepl("overview-test-annual_performance_trend", ui_html, fixed = TRUE),
  grepl("overview-test-budget_vs_actual", ui_html, fixed = TRUE),
  grepl("overview-test-revenue_capture_plot", ui_html, fixed = TRUE),
  grepl("Annual Revenue Opportunity &amp; Capture", ui_html, fixed = TRUE),
  grepl("height:460px", ui_html, fixed = TRUE),
  grepl("height:480px", ui_html, fixed = TRUE),
  grepl("height:520px", ui_html, fixed = TRUE),
  !grepl("Year-by-year performance", ui_html, fixed = TRUE),
  !grepl("Budget performance", ui_html, fixed = TRUE),
  !grepl("once Discounts are imported", ui_html, fixed = TRUE),
  !grepl("$12.4M", ui_html, fixed = TRUE)
)

dashboard_data <- shiny::reactiveVal(
  list(monthly_reports = synthetic_reports)
)

shiny::testServer(
  mod_overview_server,
  args = list(data = dashboard_data),
  {
    session$setInputs(
      from_year = "2024",
      to_year = "2025",
      metric = "Net Result"
    )
    session$flushReact()

    stopifnot(
      identical(available_years(), c(2024L, 2025L)),
      nrow(annual_finance()) == 2L,
      nrow(monthly_finance()) == 2L,
      nrow(annual_revenue()) == 2L,
      nrow(filtered_monthly_finance()) == 2L,
      nrow(filtered_annual_finance()) == 2L,
      nrow(filtered_annual_finance()) == 2L,
      nrow(filtered_annual_revenue()) == 2L,
      output$income_value == "$150",
      output$expense_value == "$110",
      output$net_value == "$40",
      output$variance_value == "$0",
      output$discount_opportunity_value == "$50",
      output$potential_revenue_value == "$200",
      output$capture_rate_value == "75.0%",
      length(duplicate_years()) == 0L
    )

    # Accessing the outputs forces all renderPlot expressions to execute.
    stopifnot(
      !is.null(output$monthly_income_expenses),
      !is.null(output$annual_performance_trend),
      !is.null(output$budget_vs_actual),
      !is.null(output$revenue_capture_plot)
    )
  }
)

empty_dashboard_data <- shiny::reactiveVal(
  list(monthly_reports = NULL)
)

shiny::testServer(
  mod_overview_server,
  args = list(data = empty_dashboard_data),
  {
    session$setInputs(
      from_year = "2021",
      to_year = "2025",
      metric = "Net Result"
    )
    session$flushReact()

    stopifnot(
      output$income_value == "—",
      output$expense_value == "—",
      output$net_value == "—",
      output$variance_value == "—",
      output$discount_opportunity_value == "—",
      output$potential_revenue_value == "—",
      output$capture_rate_value == "—",
      grepl(
        "No Monthly Site Report data loaded",
        output$snapshot_heading$html,
        fixed = TRUE
      )
    )
  }
)

monthly_file <- tempfile(fileext = ".png")

grDevices::png(
  monthly_file,
  width = 1000,
  height = 520,
  res = 96
)

plot_monthly_income_expenses(
  monthly_finance
)

grDevices::dev.off()

monthly_budget_file <- tempfile(fileext = ".png")

grDevices::png(
  monthly_budget_file,
  width = 1000,
  height = 520,
  res = 96
)

plot_monthly_budget_vs_actual_net_result(
  monthly_finance
)

grDevices::dev.off()

trend_file <- tempfile(fileext = ".png")
grDevices::png(trend_file, width = 900, height = 500, res = 96)
plot_annual_performance_trend(annual, "Net Result")
grDevices::dev.off()

budget_file <- tempfile(fileext = ".png")
grDevices::png(budget_file, width = 900, height = 500, res = 96)
plot_budget_vs_actual_net_result(annual)
grDevices::dev.off()

revenue_file <- tempfile(fileext = ".png")
grDevices::png(revenue_file, width = 900, height = 500, res = 96)
plot_revenue_capture_over_time(annual_revenue)
grDevices::dev.off()

stopifnot(
  file.exists(monthly_file),
  file.info(monthly_file)$size > 0L,
  file.info(monthly_file)$size > 0L,
  file.exists(trend_file),
  file.info(trend_file)$size > 0L,
  file.exists(budget_file),
  file.info(budget_file)$size > 0L,
  file.exists(revenue_file),
  file.info(revenue_file)$size > 0L
)
unlink(
  c(
    monthly_file,
    monthly_budget_file,
    trend_file,
    budget_file,
    revenue_file
  )
)

cat("PASS test_overview_charts.R\n")
