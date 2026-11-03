# Hover read-outs on the detailed pages must show the exact value that the
# chart draws, and nothing for a period that has no data. For each chart this
# hovers over its first reported point and checks the read-out against the
# chart's own data, then hovers away from every point and expects the hint.
# Uses the real importers on sample workbooks (two Monthly Site Report years,
# two Detailed Site Reports and one Venues Tracking Report).

hover_files <- c(
  file.path("data", "sample", "Monthly Site Report", "Monthly Site Report 2024 -December.xlsx"),
  file.path("data", "sample", "Monthly Site Report", "Monthly Site Report 2025 -December.xlsx"),
  file.path("data", "sample", "Business Unit Templates that feed master reports", "2025",
    "Aquatics Site Report 2025.xlsx"),
  file.path("data", "sample", "Business Unit Templates that feed master reports", "2025",
    "Venues Site Report 2025.xlsx"),
  file.path("data", "sample", "Business Unit Tracking Reports", "2025 Venues Tracking Report.xlsx")
)
stopifnot(file.exists(hover_files))

hover_state <- new.env()
shiny::testServer(mod_data_upload_server, {
  session$flushReact()
  session$setInputs(all_reports = data.frame(
    name = basename(hover_files), size = file.info(hover_files)$size,
    type = "application/octet-stream", datapath = hover_files,
    stringsAsFactors = FALSE
  ))
  session$flushReact()
  stopifnot(all(session$returned()$uploads$status == "ready"))
  hover_state$data <- session$returned()
})
hover_data <- shiny::reactiveVal(hover_state$data)

hover_hint <- "Hover over a plotted period to inspect exact values."

# The value as dashboard_hover_text() writes it.
hover_format <- function(value, kind) {
  if (identical(kind, "currency")) {
    return(paste0(if (value < 0) "-" else "", "$",
      formatC(abs(value), digits = 2L, format = "f", big.mark = ",")))
  }
  format(round(value), big.mark = ",", scientific = FALSE, trim = TRUE)
}

# Hovers over the first point of `column` in `data`, expects its exact value
# (and the group label, if given), then hovers far from every point and
# expects the hint. Returns 1, so the caller can count the read-outs checked.
check_hover <- function(session, output, plot, data, x_col, column, kind,
                        group = NULL) {
  label <- paste("hover read-out for", plot)
  if (!is.data.frame(data) || !nrow(data)) stop(label, ": the chart has no data", call. = FALSE)
  rows <- which(is.finite(suppressWarnings(as.numeric(data[[column]]))))
  if (!length(rows)) stop(label, ": no reported values in ", column, call. = FALSE)
  row <- data[rows[[1L]], , drop = FALSE]
  x <- as.numeric(row[[x_col]])
  hover <- stats::setNames(list(list(x = x)), paste0(plot, "_hover"))
  do.call(session$setInputs, hover)
  session$flushReact()
  text <- output[[paste0(plot, "_hover_value")]]
  expected <- hover_format(as.numeric(row[[column]]), kind)
  if (identical(text, hover_hint) || !grepl(expected, text, fixed = TRUE) ||
      (!is.null(group) && !grepl(as.character(row[[group]]), text, fixed = TRUE))) {
    stop(label, " shows '", text, "', expected it to include ", expected, call. = FALSE)
  }
  # Far beyond the last point: never the nearest other period.
  away <- max(as.numeric(data[[x_col]]), na.rm = TRUE) + if (inherits(data[[x_col]], "Date")) 400 else 5
  hover <- stats::setNames(list(list(x = away)), paste0(plot, "_hover"))
  do.call(session$setInputs, hover)
  session$flushReact()
  if (!identical(output[[paste0(plot, "_hover_value")]], hover_hint)) {
    stop(label, " shows a value away from every point", call. = FALSE)
  }
  1L
}

hover_checked <- 0L

shiny::testServer(mod_overview_server, args = list(data = hover_data), {
  session$setInputs(from_year = "2024", to_year = "2025", metric = "Income")
  session$flushReact()
  monthly <- filtered_monthly_finance()
  hover_checked <<- hover_checked + check_hover(session, output, "monthly_income_expenses",
    monthly, "date", "income_actual", "currency")
  hover_checked <<- hover_checked + check_hover(session, output, "budget_vs_actual",
    monthly, "date", "net_actual", "currency")
  annual <- filtered_annual_finance()
  annual$hover_metric <- overview_metric_values(annual, "Income")
  hover_checked <<- hover_checked + check_hover(session, output, "annual_performance_trend",
    annual, "report_year", "hover_metric", "currency")
})

shiny::testServer(mod_site_comparison_server, args = list(data = hover_data), {
  sites <- site_comparison_available_sites(hover_state$data$site_reports)
  session$setInputs(from_year = "2024", to_year = "2025", metric = "Income",
    selected_site = sites[[1L]])
  session$flushReact()
  sites_data <- filtered_site_finance()
  sites_data$hover_value <- site_finance_values(sites_data, "Income", "Actual")
  hover_checked <<- hover_checked + check_hover(session, output, "site_performance_trend",
    sites_data, "report_year", "hover_value", "currency", group = "site")
  facilities <- filtered_facility_finance()
  facilities$hover_value <- facility_finance_values(facilities, "Income", "Actual")
  hover_checked <<- hover_checked + check_hover(session, output, "facility_performance_trend",
    facilities, "report_year", "hover_value", "currency", group = "business_unit")
})

shiny::testServer(mod_site_detail_server, args = list(data = hover_data), {
  # Not every site reports every measure (the sample Aquatics report has no
  # memberships sheet), so each chart is checked on the first site that has
  # data for it.
  sites <- site_detail_available_sites(hover_state$data$site_reports, year = 2025)
  choose_site <- function(area, values) {
    for (site in sites) {
      session$setInputs(site = site, year = "2025", area = area)
      session$flushReact()
      data <- values()
      if (is.data.frame(data) && nrow(data) > 0L) return(data)
    }
    stop("no sample site has data for the ", area, " hover charts", call. = FALSE)
  }
  visits <- choose_site("Visits", monthly_visits)
  hover_checked <<- hover_checked + check_hover(session, output, "visits_over_time",
    visits, "date", "direct", "count")
  enrolments <- choose_site("Enrolments", enrolment_trend)
  hover_checked <<- hover_checked + check_hover(session, output, "enrolments_over_time",
    enrolments, "date", "total_active", "count")
  members <- choose_site("Memberships", membership_trend)
  hover_checked <<- hover_checked + check_hover(session, output, "memberships_over_time",
    members, "date", "total_active", "count")
  movement <- choose_site("Memberships", membership_movement)
  hover_checked <<- hover_checked + check_hover(session, output, "membership_movement",
    movement, "date", "new_members", "count")
})

shiny::testServer(mod_venues_operations_server, args = list(data = hover_data), {
  session$setInputs(year = "2025", area = "Attendance")
  session$flushReact()
  hover_checked <<- hover_checked + check_hover(session, output, "attendance_over_time",
    monthly_attendance(), "date", "value", "count")
  session$setInputs(area = "Venue Finances")
  session$flushReact()
  hover_checked <<- hover_checked + check_hover(session, output, "attendance_over_time",
    finance_monthly(), "date", "income_actual", "currency")
  session$setInputs(area = "Workforce")
  session$flushReact()
  hover_checked <<- hover_checked + check_hover(session, output, "attendance_over_time",
    workforce_hours_monthly(), "date", "paid_hours", "count")
})

shiny::testServer(mod_forecasting_server, args = list(data = hover_data), {
  session$setInputs(site = "Overall", horizon = "12", payroll_mode = "manual",
    monthly_payroll_adjustment = 0, price_pct = 0, effective_month = "1")
  session$flushReact()
  rows <- forecast_hover_rows("income")
  hover_checked <<- hover_checked + check_hover(session, output, "income_plot",
    rows, "date", "Actual", "currency")
  forecast <- rows[is.finite(rows$Baseline), , drop = FALSE]
  hover_checked <<- hover_checked + check_hover(session, output, "income_plot",
    forecast, "date", "Baseline", "currency")
})

cat("PASS hover read-outs show the charted value and nothing away from the data (",
  hover_checked, " read-outs)\n", sep = "")
