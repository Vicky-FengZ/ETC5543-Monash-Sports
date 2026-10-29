# Venues Operations calculations: attendance, facility hire, workforce and
# finance values shown on the page. Fixtures: tests/fixtures_site_venues.R.

venues <- make_venues_fixture()

# Years and filters ------------------------------------------------------

stopifnot(
  identical(venues_operations_available_years(venues), 2025L),
  identical(venues_operations_available_years(NULL), integer()),
  nrow(venues_operations_filter_year(venues$attendance, 2025)) == nrow(venues$attendance),
  nrow(venues_operations_filter_year(venues$attendance, 2024)) == 0L
)
cat("PASS Venues Operations years and filters\n")

# Attendance ------------------------------------------------------------

attendance_kpis <- venues_operations_attendance_kpis(venues$attendance)
stopifnot(
  # Reported YTD (3800) is preferred over the month sum (3700).
  attendance_kpis$total_attendance == 3800,
  isTRUE(all.equal(attendance_kpis$average_monthly, 3700 / 3)),
  attendance_kpis$peak_value == 1500,
  identical(attendance_kpis$peak_month, "Feb"),
  attendance_kpis$reporting_months == 3L
)

no_ytd <- venues$attendance[
  !(venues$attendance$period_type == "YTD" & venues$attendance$row_type == "Overall Total"),
]
stopifnot(venues_operations_attendance_kpis(no_ytd)$total_attendance == 3700)

sections <- venues_operations_attendance_section_totals(venues$attendance)
details <- venues_operations_attendance_detail_totals(venues$attendance, "Badminton")
areas <- venues_operations_attendance_area_totals(venues$attendance)
stopifnot(
  # The "Overall" section and "All" area are totals, not activities/areas.
  identical(sections$activity, c("Badminton", "Tennis")),
  identical(sections$value, c(900, 700)),
  identical(details$category, c("Student", "Community")),
  identical(details$value, c(500, 400)),
  identical(areas$area, c("Indoor", "Outdoor")),
  identical(areas$value, c(2000, 1700)),
  nrow(venues_operations_attendance_detail_totals(venues$attendance, NULL)) == 0L
)
cat("PASS Venues attendance KPIs, sections, details and areas\n")

# Facility hire ---------------------------------------------------------

hire_kpis <- venues_operations_facility_hire_kpis(venues$facility_hire)
stopifnot(
  # Courts uses its reported Total (5000), not Total + Details.
  hire_kpis$revenue == 5000 + 1500,
  hire_kpis$transactions == 100 + 30,
  hire_kpis$revenue_per_transaction == 50
)

by_facility <- venues_operations_facility_hire_by_facility(venues$facility_hire)
monthly_hire <- venues_operations_monthly_facility_hire(venues$facility_hire)
stopifnot(
  identical(by_facility$facility, c("Courts", "Rooms")),
  identical(by_facility$value, c(5000, 1500)),
  identical(monthly_hire$month_label, c("Jan", "Feb")),
  identical(monthly_hire$value, c(2000 + 300 + 200, 3000 + 1000))
)
cat("PASS Venues facility hire never double counts Totals and Details\n")

# Workforce -------------------------------------------------------------

hours <- venues_operations_workforce_hours_monthly(venues$casual_hours)
stopifnot(
  # March is not reported yet, so it is left out.
  identical(hours$month_label, c("Jan", "Feb")),
  identical(hours$budget_hours, c(100, 100)),
  identical(hours$paid_hours, c(90, 110)),
  identical(hours$variance_hours, c(-10, 10))
)

workforce_kpis <- venues_operations_workforce_kpis(venues$casual_hours, venues$casual_wages)
no_total <- venues$casual_hours[venues$casual_hours$period_type != "Reported Total", ]
stopifnot(
  workforce_kpis$paid_hours == 205,
  workforce_kpis$actual_wages == 2100,
  workforce_kpis$wage_variance == 50,
  # Without a reported total, only reported months are summed.
  venues_operations_workforce_annual_value(no_total, "Paid Hours", "Monthly Summary") == 200
)
cat("PASS Venues workforce hours and KPIs\n")

# Finance ---------------------------------------------------------------

finance_monthly <- venues_operations_finance_monthly(venues$finance)
all_funds <- venues_operations_finance_ytd(venues$finance)
fund_a <- venues_operations_finance_ytd(venues$finance, fund = "Fund A")
no_ytd_finance <- venues$finance[venues$finance$row_type != "YTD Total", ]
stopifnot(
  identical(finance_monthly$net_actual, c(20, -10)),
  identical(finance_monthly$net_budget, c(5, 5)),
  # All Funds prefers the latest reported YTD Total.
  all_funds$income_actual == 225,
  all_funds$expenses_actual == 210,
  all_funds$net_actual == 15,
  all_funds$net_budget == 10,
  # A single fund sums its monthly rows.
  fund_a$income_actual == 130,
  fund_a$expenses_actual == 110,
  fund_a$net_actual == 20,
  is.na(fund_a$net_budget),
  venues_operations_finance_ytd(no_ytd_finance)$income_actual == 220
)
cat("PASS Venues finance monthly and year-to-date values\n")

# Formatting ------------------------------------------------------------

stopifnot(
  identical(venues_operations_format_count(1234.6), "1,235"),
  identical(venues_operations_format_count(NA_real_), "—"),
  identical(venues_operations_format_currency(1234.4), "$1,234"),
  identical(venues_operations_format_currency(1234.5, digits = 2L), "$1,234.50"),
  identical(venues_operations_format_currency(NA_real_), "—")
)
cat("PASS Venues number formatting\n")

cat("PASS test_venues_operations.R\n")
