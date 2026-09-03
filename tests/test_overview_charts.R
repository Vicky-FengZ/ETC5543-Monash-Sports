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

synthetic_finance <- rbind(
  make_finance_rows(2024, "Monash Sport", TRUE, 120, 100, 90, 80, "2024.xlsx"),
  make_finance_rows(2024, "Clayton", TRUE, 999, 999, 999, 999, "2024.xlsx"),
  make_finance_rows(2024, "Monash Sport", FALSE, 888, 888, 888, 888, "2024.xlsx"),
  make_finance_rows(2025, "Monash Sport", TRUE, 150, 140, 110, 100, "2025.xlsx")
)

synthetic_reports <- structure(
  list(
    finance = synthetic_finance,
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

fallback_reports <- list(
  finance = rbind(
    make_finance_rows(2026, "Clayton", TRUE, 50, 45, 30, 28, "2026.xlsx"),
    make_finance_rows(2026, "Caulfield", TRUE, 70, 65, 40, 38, "2026.xlsx")
  )
)
fallback <- calculate_overview_annual_finance(fallback_reports)
stopifnot(
  fallback$income_actual[[1L]] == 120,
  fallback$expenses_actual[[1L]] == 70,
  fallback$aggregation_source[[1L]] == "Sum of Site YTD Summaries"
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
  grepl("overview-test-annual_performance_trend", ui_html, fixed = TRUE),
  grepl("overview-test-budget_vs_actual", ui_html, fixed = TRUE),
  !grepl("Year-by-year performance", ui_html, fixed = TRUE),
  !grepl("Budget performance", ui_html, fixed = TRUE)
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
      nrow(filtered_annual_finance()) == 2L,
      length(duplicate_years()) == 0L
    )

    # Accessing the outputs forces both renderPlot expressions to execute.
    stopifnot(
      !is.null(output$annual_performance_trend),
      !is.null(output$budget_vs_actual)
    )
  }
)

trend_file <- tempfile(fileext = ".png")
grDevices::png(trend_file, width = 900, height = 500, res = 96)
plot_annual_performance_trend(annual, "Net Result")
grDevices::dev.off()

budget_file <- tempfile(fileext = ".png")
grDevices::png(budget_file, width = 900, height = 500, res = 96)
plot_budget_vs_actual_net_result(annual)
grDevices::dev.off()

stopifnot(
  file.exists(trend_file),
  file.info(trend_file)$size > 0L,
  file.exists(budget_file),
  file.info(budget_file)$size > 0L
)
unlink(c(trend_file, budget_file))

cat("PASS test_overview_charts.R\n")
