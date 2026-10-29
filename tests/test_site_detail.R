# Site Detail calculations: filters, available choices and the KPI and trend
# values shown on the page. Fixtures: tests/fixtures_site_venues.R.

site_reports <- make_site_detail_fixture()

# Filters and choices ----------------------------------------------------

stopifnot(
  identical(site_detail_available_years(site_reports), c(2024L, 2025L)),
  identical(site_detail_available_years(NULL), integer()),
  identical(
    site_detail_available_sites(site_reports, year = 2025),
    c("H&F Clayton", "Peninsula")
  ),
  identical(
    site_detail_available_areas(site_reports, year = 2025, site = "H&F Clayton"),
    c("Visits", "Memberships", "Enrolments")
  ),
  # Peninsula only has visits, so the page hides the area switcher.
  identical(
    site_detail_available_areas(site_reports, year = 2025, site = "Peninsula"),
    "Visits"
  ),
  nrow(site_detail_filter_rows(site_reports$visits, year = 2025, site = "Unknown site")) == 0L
)

hf_visits <- site_detail_filter_rows(site_reports$visits, year = 2025, site = "H&F Clayton")
stopifnot(
  all(hf_visits$report_year == 2025L),
  all(hf_visits$report_unit == "Health & Fitness")
)
cat("PASS Site Detail filters, years, sites and areas\n")

# Visit KPIs ------------------------------------------------------------

kpis <- site_detail_visit_kpis(hf_visits)
stopifnot(
  # Reported YTD (315) is preferred over the month sum (310).
  kpis[["direct"]] == 315,
  # Indirect has no YTD row, so months are summed.
  kpis[["indirect"]] == 30,
  kpis[["total"]] == 345,
  kpis[["student"]] == 180 + 18,
  kpis[["staff"]] == 95 + 8,
  kpis[["community"]] == 35 + 4,
  all(is.na(site_detail_visit_kpis(hf_visits[0, ])))
)
stopifnot(
  identical(site_detail_format_visit_count(212402.4), "212,402"),
  identical(site_detail_format_visit_count(NA_real_), "—")
)

monthly_visits <- site_detail_monthly_visit_totals(hf_visits)
stopifnot(
  identical(monthly_visits$month_label, c("Jan", "Feb", "Mar")),
  identical(monthly_visits$direct, c(100, 120, 90)),
  identical(monthly_visits$indirect, c(10, 5, 15)),
  identical(monthly_visits$total, c(110, 125, 105))
)

activities <- site_detail_activity_totals(hf_visits)
stopifnot(
  identical(activities$activity, c("Gym", "Classes")),
  identical(activities$value, c(150, 100))
)
cat("PASS Site Detail visit KPIs, monthly totals and activities\n")

# Memberships -----------------------------------------------------------

membership_kpis <- site_detail_membership_kpis(site_reports$memberships)
stopifnot(
  membership_kpis$total_active == 510,
  membership_kpis$new_members == 25,
  membership_kpis$expired_cancelled == 35,
  membership_kpis$reporting_months == 3L,
  identical(membership_kpis$latest_month, "Mar 2025"),
  is.na(site_detail_membership_kpis(NULL)$total_active)
)

membership_trend <- site_detail_membership_trend(site_reports$memberships)
movement <- site_detail_membership_movement(site_reports$memberships)
mix <- site_detail_membership_active_mix(site_reports$memberships)
stopifnot(
  identical(membership_trend$total_active, c(500, 520, 510)),
  identical(movement$new_members, c(40, 30, 25)),
  identical(movement$expired_cancelled, c(20, 10, 35)),
  identical(mix$audience, c("Student", "Staff", "Community")),
  identical(mix$value, c(300, 150, 60)),
  isTRUE(all.equal(sum(mix$share), 1))
)
cat("PASS Site Detail membership KPIs, trend, movement and mix\n")

# Enrolments ------------------------------------------------------------

enrolment_kpis <- site_detail_enrolment_kpis(site_reports$enrollments)
stopifnot(
  # Zero-filled April is ignored: the latest meaningful month is March.
  enrolment_kpis$total_active == 90,
  identical(enrolment_kpis$total_active_month, "Mar 2025"),
  enrolment_kpis$target == 100,
  enrolment_kpis$active_students == 70,
  enrolment_kpis$student_share == 0.78
)

enrolment_trend <- site_detail_enrolment_trend(site_reports$enrollments)
share_trend <- site_detail_enrolment_student_share(site_reports$enrollments)
stopifnot(
  nrow(enrolment_trend) == 4L,
  is.na(enrolment_trend$total_active[[1L]]),
  is.na(enrolment_trend$total_active[[4L]]),
  identical(enrolment_trend$total_active[2:3], c(80, 90)),
  identical(share_trend$student_share, c(0.75, 0.78))
)
cat("PASS Site Detail enrolment KPIs and trends ignore zero-filled months\n")

cat("PASS test_site_detail.R\n")
