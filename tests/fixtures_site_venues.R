# Small, hand-checkable fixtures for the Site Detail and Venues Operations
# pages, shared by their calculation tests and the plot smoke tests. Values
# are chosen so preference rules are visible (e.g. a reported YTD total that
# differs from the sum of months, or a facility Total next to its Details).

month_dates <- function(year, months) {
  as.Date(sprintf("%04d-%02d-01", year, months))
}

# Site Detail ------------------------------------------------------------

make_site_visit_rows <- function(report_unit, campus, year, months, period_type,
                                 row_type, visit_type, audience, values,
                                 activity = NA_character_) {
  n <- length(values)
  dates <- if (identical(period_type, "Month")) month_dates(year, months) else rep(as.Date(NA), n)
  data.frame(
    report_year = rep(as.integer(year), n),
    report_unit = rep(report_unit, n),
    campus = rep(campus, n),
    date = dates,
    month = if (identical(period_type, "Month")) as.integer(months) else rep(NA_integer_, n),
    period_type = rep(period_type, n),
    row_type = rep(row_type, n),
    visit_type = rep(visit_type, n),
    audience = rep(audience, n),
    activity = rep(activity, n),
    value = values,
    stringsAsFactors = FALSE
  )
}

make_site_detail_fixture <- function() {
  hf <- function(...) make_site_visit_rows("Health & Fitness", "Clayton", 2025, 1:3, ...)

  visits <- rbind(
    hf("Month", "Total", "Direct", "Total", c(100, 120, 90)),
    hf("Month", "Total", "Indirect", "Total", c(10, 5, 15)),
    hf("Month", "Total", "Direct", "Student", c(60, 70, 50)),
    hf("Month", "Total", "Direct", "Staff", c(30, 35, 30)),
    hf("Month", "Total", "Direct", "Community", c(10, 15, 10)),
    hf("Month", "Total", "Indirect", "Student", c(6, 3, 9)),
    hf("Month", "Total", "Indirect", "Staff", c(3, 1, 4)),
    hf("Month", "Total", "Indirect", "Community", c(1, 1, 2)),
    # Reported YTD differs from the month sum (310) so the preference shows.
    make_site_visit_rows("Health & Fitness", "Clayton", 2025, NA, "YTD", "Total", "Direct", "Total", 315),
    hf("Month", "Activity", NA_character_, "Total", c(50, 60, 40), activity = "Gym"),
    hf("Month", "Activity", NA_character_, "Total", c(30, 40, 30), activity = "Classes"),
    # Another site and another year, for the filters.
    make_site_visit_rows("Peninsula", "Peninsula", 2025, 1, "Month", "Total", "Direct", "Total", 40),
    make_site_visit_rows("Health & Fitness", "Clayton", 2024, 12, "Month", "Total", "Direct", "Total", 80)
  )

  membership_row <- function(metric, months, values) {
    data.frame(
      report_year = 2025L,
      report_unit = "Health & Fitness",
      campus = "Clayton",
      date = month_dates(2025, months),
      month = as.integer(months),
      metric = metric,
      value = values,
      stringsAsFactors = FALSE
    )
  }
  memberships <- rbind(
    membership_row("Total Active", 1:3, c(500, 520, 510)),
    membership_row("New Members", 1:3, c(40, 30, 25)),
    membership_row("Expired/Cancelled", 1:3, c(20, 10, 35)),
    membership_row("Active Student Members", 3, 300),
    membership_row("Active Staff Members", 3, 150),
    membership_row("Active Community Members", 3, 60)
  )

  # Enrolment templates zero-fill months that have not been reported yet.
  enrollments <- rbind(
    membership_row("Total Active", 1:4, c(0, 80, 90, 0)),
    membership_row("Target", 1:4, c(0, 100, 100, 0)),
    membership_row("Active Student Members", 1:4, c(0, 60, 70, 0)),
    membership_row("Student Share", 1:4, c(0, 0.75, 0.78, 0))
  )

  list(visits = visits, memberships = memberships, enrollments = enrollments)
}

# Venues Operations ------------------------------------------------------

make_venues_fixture <- function() {
  labels <- month.abb
  attendance_row <- function(period_type, row_type, values, months = NA,
                             activity = NA_character_, area = NA_character_,
                             category = NA_character_) {
    n <- length(values)
    is_month <- identical(period_type, "Month")
    data.frame(
      report_year = rep(2025L, n),
      date = if (is_month) month_dates(2025, months) else rep(as.Date(NA), n),
      month = if (is_month) as.integer(months) else rep(NA_integer_, n),
      month_label = if (is_month) labels[months] else rep(NA_character_, n),
      period_type = rep(period_type, n),
      row_type = rep(row_type, n),
      activity = rep(activity, n),
      area = rep(area, n),
      category = rep(category, n),
      value = values,
      stringsAsFactors = FALSE
    )
  }
  attendance <- rbind(
    attendance_row("Month", "Overall Total", c(1000, 1500, 1200), months = 1:3),
    # Reported YTD (3800) differs from the month sum (3700).
    attendance_row("YTD", "Overall Total", 3800),
    attendance_row("YTD", "Section Total", 900, activity = "Badminton", area = "Indoor"),
    attendance_row("YTD", "Section Total", 700, activity = "Tennis", area = "Outdoor"),
    attendance_row("YTD", "Section Total", 3800, activity = "Overall", area = "All"),
    attendance_row("YTD", "Summary Total", 2000, area = "Indoor"),
    attendance_row("YTD", "Summary Total", 1700, area = "Outdoor"),
    attendance_row("YTD", "Summary Total", 3800, area = "All"),
    attendance_row("YTD", "Detail", 500, activity = "Badminton", category = "Student"),
    attendance_row("YTD", "Detail", 400, activity = "Badminton", category = "Community"),
    attendance_row("YTD", "Detail", 900, activity = "Badminton", category = "Total")
  )

  hire_row <- function(period_type, facility, row_type, metric, values, months = NA) {
    n <- length(values)
    is_month <- identical(period_type, "Month")
    data.frame(
      report_year = rep(2025L, n),
      date = if (is_month) month_dates(2025, months) else rep(as.Date(NA), n),
      month = if (is_month) as.integer(months) else rep(NA_integer_, n),
      month_label = if (is_month) labels[months] else rep(NA_character_, n),
      period_type = rep(period_type, n),
      facility = rep(facility, n),
      row_type = rep(row_type, n),
      metric = rep(metric, n),
      value = values,
      stringsAsFactors = FALSE
    )
  }
  facility_hire <- rbind(
    # Courts reports a Total, so its Details must not be added on top.
    hire_row("YTD", "Courts", "Total", "Revenue", 5000),
    hire_row("YTD", "Courts", "Detail", "Revenue", c(3000, 2500)),
    hire_row("YTD", "Courts", "Total", "Transactions", 100),
    # Rooms has Details only, so they are summed.
    hire_row("YTD", "Rooms", "Detail", "Revenue", c(1000, 500)),
    hire_row("YTD", "Rooms", "Detail", "Transactions", c(20, 10)),
    hire_row("Month", "Courts", "Total", "Revenue", c(2000, 3000), months = 1:2),
    hire_row("Month", "Courts", "Detail", "Revenue", 1500, months = 1),
    hire_row("Month", "Rooms", "Detail", "Revenue", c(300, 200), months = c(1, 1)),
    hire_row("Month", "Rooms", "Detail", "Revenue", 1000, months = 2)
  )

  hours_row <- function(period_type, metric, values, months = NA, reported = TRUE) {
    n <- length(values)
    is_month <- identical(period_type, "Month")
    data.frame(
      report_year = rep(2025L, n),
      date = if (is_month) month_dates(2025, months) else rep(as.Date(NA), n),
      month = if (is_month) as.integer(months) else rep(NA_integer_, n),
      month_label = if (is_month) labels[months] else rep(NA_character_, n),
      period_type = rep(period_type, n),
      record_group = rep("Monthly Summary", n),
      metric = rep(metric, n),
      value = values,
      is_reported = rep(reported, n),
      stringsAsFactors = FALSE
    )
  }
  casual_hours <- rbind(
    hours_row("Month", "Budgeted Hours", c(100, 100), months = 1:2),
    hours_row("Month", "Paid Hours", c(90, 110), months = 1:2),
    hours_row("Month", "Variance", c(-10, 10), months = 1:2),
    # March is not reported yet and must not appear.
    hours_row("Month", "Paid Hours", 0, months = 3, reported = FALSE),
    hours_row("Reported Total", "Paid Hours", 205)
  )
  casual_wages <- rbind(
    hours_row("Month", "Actual", c(1000, 1100), months = 1:2),
    hours_row("Reported Total", "Actual", 2100),
    hours_row("Reported Total", "Variance", 50)
  )

  finance_row <- function(row_type, metric, value_type, values, months, fund = NA_character_) {
    n <- length(values)
    data.frame(
      report_year = rep(2025L, n),
      date = month_dates(2025, months),
      month = as.integer(months),
      month_label = labels[months],
      fund = rep(fund, n),
      row_type = rep(row_type, n),
      metric = rep(metric, n),
      value_type = rep(value_type, n),
      value = values,
      stringsAsFactors = FALSE
    )
  }
  finance <- rbind(
    finance_row("MTD Total", "Income", "Actual", c(100, 120), 1:2),
    finance_row("MTD Total", "Income", "Budget", c(90, 100), 1:2),
    finance_row("MTD Total", "Expenses", "Actual", c(80, 130), 1:2),
    finance_row("MTD Total", "Expenses", "Budget", c(85, 95), 1:2),
    # Latest reported YTD (225) differs from the MTD sum (220).
    finance_row("YTD Total", "Income", "Actual", 225, 2),
    finance_row("YTD Total", "Income", "Budget", 190, 2),
    finance_row("YTD Total", "Expenses", "Actual", 210, 2),
    finance_row("YTD Total", "Expenses", "Budget", 180, 2),
    finance_row("Fund", "Income", "Actual", c(60, 70), 1:2, fund = "Fund A"),
    finance_row("Fund", "Expenses", "Actual", c(50, 60), 1:2, fund = "Fund A")
  )

  list(
    attendance = attendance,
    facility_hire = facility_hire,
    casual_hours = casual_hours,
    casual_wages = casual_wages,
    finance = finance
  )
}
