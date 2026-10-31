# The same figure must be the same on every page. Uses the real importers
# on sample workbooks (two Monthly Site Report years, one Detailed Site
# Report, one Venues Tracking Report) and compares, for the latest year:
# - Overview snapshot, Executive Dashboard KPIs and the shared calculation;
# - Overview monthly totals against its annual totals;
# - Forecasting history (Overall) against Overview monthly Actuals;
# - Executive Dashboard for one site against Site Comparison for that site.

consistency_files <- c(
  file.path("data", "sample", "Monthly Site Report", "Monthly Site Report 2024 -December.xlsx"),
  file.path("data", "sample", "Monthly Site Report", "Monthly Site Report 2025 -December.xlsx"),
  file.path("data", "sample", "Business Unit Templates that feed master reports", "2025",
    "Aquatics Site Report 2025.xlsx"),
  file.path("data", "sample", "Business Unit Tracking Reports", "2025 Venues Tracking Report.xlsx")
)
stopifnot(file.exists(consistency_files))

consistency_state <- new.env()
shiny::testServer(mod_data_upload_server, {
  session$flushReact()
  session$setInputs(all_reports = data.frame(
    name = basename(consistency_files),
    size = file.info(consistency_files)$size,
    type = "application/octet-stream",
    datapath = consistency_files,
    stringsAsFactors = FALSE
  ))
  session$flushReact()
  stopifnot(all(session$returned()$uploads$status == "ready"))
  consistency_state$data <- session$returned()
})

consistency_data <- shiny::reactiveVal(consistency_state$data)
consistency_monthly <- consistency_state$data$monthly_reports
finance_columns <- c("income_actual", "expenses_actual", "net_actual", "budget_variance")

same_values <- function(a, b, label) {
  a <- unlist(a[finance_columns])
  b <- unlist(b[finance_columns])
  if (!isTRUE(all.equal(unname(a), unname(b), tolerance = 1e-9))) {
    stop(label, " differ:\n  ", paste(finance_columns, a, b, sep = " ", collapse = "\n  "),
      call. = FALSE)
  }
}

annual <- calculate_overview_annual_finance(consistency_monthly)
expected <- annual[annual$report_year == 2025L, , drop = FALSE]
stopifnot(nrow(expected) == 1L, is.finite(expected$income_actual))

# Overview monthly totals add up to the annual totals.
monthly <- calculate_overview_monthly_finance(consistency_monthly)
monthly_2025 <- monthly[monthly$report_year == 2025L, , drop = FALSE]
stopifnot(
  nrow(monthly_2025) == 12L,
  isTRUE(all.equal(sum(monthly_2025$income_actual), expected$income_actual)),
  isTRUE(all.equal(sum(monthly_2025$expenses_actual), expected$expenses_actual))
)

# Forecasting starts from the same monthly Actuals.
history <- prepare_forecast_history(consistency_monthly, "Overall")$history
history_2025 <- history[format(history$date, "%Y") == "2025", , drop = FALSE]
stopifnot(
  nrow(history_2025) == 12L,
  isTRUE(all.equal(sum(history_2025$income), expected$income_actual)),
  isTRUE(all.equal(sum(history_2025$expenses), expected$expenses_actual))
)

shiny::testServer(mod_overview_server, args = list(data = consistency_data), {
  session$setInputs(from_year = "2024", to_year = "2025", metric = "Net Result")
  session$flushReact()
  same_values(snapshot_finance(), expected, "Overview snapshot and annual calculation")
  stopifnot(
    output$income_value == format_compact_currency(expected$income_actual),
    output$net_value == format_compact_currency(expected$net_actual)
  )
})

site_finance <- calculate_site_annual_finance(consistency_monthly)
site_finance_2025 <- site_finance[site_finance$report_year == 2025L, , drop = FALSE]
consistency_site <- site_finance_2025$site[[1L]]

shiny::testServer(mod_executive_dashboard_server, args = list(data = consistency_data), {
  session$setInputs(
    view_mode = "Year Review", year = "2025",
    comparison_year = "2024", site = "All Monash Sport"
  )
  session$flushReact()
  same_values(executive_kpi_finance(), expected, "Executive Dashboard and Overview figures")
  stopifnot(
    output$kpi_total_income == format_compact_currency(expected$income_actual),
    output$kpi_net_result == format_compact_currency(expected$net_actual)
  )

  # One site on the Executive Dashboard matches that site everywhere else.
  session$setInputs(site = consistency_site)
  session$flushReact()
  same_values(
    executive_kpi_finance(),
    site_finance_2025[site_finance_2025$site == consistency_site, , drop = FALSE],
    paste("Executive Dashboard and site calculation for", consistency_site)
  )
})

shiny::testServer(mod_site_comparison_server, args = list(data = consistency_data), {
  session$setInputs(
    from_year = "2024", to_year = "2025", metric = "Income",
    selected_site = consistency_site
  )
  session$flushReact()
  snapshot <- snapshot_site_finance()
  same_values(
    snapshot[snapshot$site == consistency_site, , drop = FALSE],
    site_finance_2025[site_finance_2025$site == consistency_site, , drop = FALSE],
    paste("Site Comparison and site calculation for", consistency_site)
  )
})

cat("PASS test_cross_page_consistency.R\n")
