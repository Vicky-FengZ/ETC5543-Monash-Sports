# Plot smoke tests: draw every Site Detail and Venues Operations chart to a
# temporary PNG, at a wide and a narrow size, using the shared fixtures.
# These charts are not otherwise exercised by the test suite; this catches
# charts that error when the page opens (e.g. after styling changes).

site_reports <- make_site_detail_fixture()
hf_visits <- site_detail_filter_rows(site_reports$visits, year = 2025, site = "H&F Clayton")
visit_kpis <- site_detail_visit_kpis(hf_visits)
venues <- make_venues_fixture()

share_table <- function(label_column, values) {
  table <- data.frame(names(values), as.numeric(values), as.numeric(values) / sum(values))
  names(table) <- c(label_column, "value", "share")
  table
}

plot_cases <- list(
  site_visits_over_time = function() plot_site_detail_visits_over_time(
    site_detail_monthly_visit_totals(hf_visits)),
  site_visit_type_breakdown = function() plot_site_detail_visit_type_breakdown(
    share_table("visit_type", c(
      Direct = visit_kpis[["direct"]],
      Indirect = visit_kpis[["indirect"]]
    ))),
  site_audience_mix = function() plot_site_detail_audience_mix(
    share_table("audience", c(
      Student = visit_kpis[["student"]],
      Staff = visit_kpis[["staff"]],
      Community = visit_kpis[["community"]]
    ))),
  site_activity_breakdown = function() plot_site_detail_activity_breakdown(
    site_detail_activity_totals(hf_visits)),
  site_memberships_over_time = function() plot_site_detail_memberships_over_time(
    site_detail_membership_trend(site_reports$memberships)),
  site_membership_movement = function() plot_site_detail_membership_movement(
    site_detail_membership_movement(site_reports$memberships)),
  site_membership_mix = function() plot_site_detail_audience_mix(
    site_detail_membership_active_mix(site_reports$memberships)),
  site_enrolments_over_time = function() plot_site_detail_enrolments_over_time(
    site_detail_enrolment_trend(site_reports$enrollments)),
  site_enrolment_breakdown = function() plot_site_detail_enrolment_breakdown(
    site_detail_enrolment_breakdown(site_reports$enrollments)),
  site_enrolment_student_share = function() plot_site_detail_enrolment_student_share(
    site_detail_enrolment_student_share(site_reports$enrollments)),
  venues_attendance_over_time = function() plot_venues_attendance_over_time(
    venues_operations_monthly_attendance(venues$attendance)),
  venues_facility_hire_over_time = function() plot_venues_facility_hire_revenue_over_time(
    venues_operations_monthly_facility_hire(venues$facility_hire)),
  venues_facility_hire_by_facility = function() plot_venues_facility_hire_by_facility(
    venues_operations_facility_hire_by_facility(venues$facility_hire), currency = TRUE),
  venues_attendance_by_activity = function() plot_venues_attendance_by_activity(
    venues_operations_attendance_section_totals(venues$attendance)),
  venues_attendance_by_area = function() plot_venues_attendance_by_area(
    venues_operations_attendance_area_totals(venues$attendance)),
  workforce_hours_over_time = function() plot_workforce_hours_over_time(
    venues_operations_workforce_hours_monthly(venues$casual_hours)),
  workforce_hours_variance = function() plot_workforce_hours_variance(
    venues_operations_workforce_hours_monthly(venues$casual_hours))
)

draw_plot_case <- function(draw, width, height) {
  file <- tempfile(fileext = ".png")
  grDevices::png(file, width = width, height = height, res = 96)
  on.exit({
    grDevices::dev.off()
    unlink(file)
  })
  draw()
  invisible(TRUE)
}

failures <- character()
for (name in names(plot_cases)) {
  for (size in list(c(1100, 420), c(420, 360))) {
    result <- tryCatch(
      draw_plot_case(plot_cases[[name]], size[[1L]], size[[2L]]),
      error = function(error) conditionMessage(error)
    )
    if (!isTRUE(result)) {
      failures <- c(failures, sprintf("%s (%dx%d): %s", name, size[[1L]], size[[2L]], result))
    }
  }
}

if (length(failures) > 0L) {
  stop(paste(c("Plot smoke test failures:", failures), collapse = "\n"), call. = FALSE)
}
cat(sprintf("PASS %d Site Detail and Venues charts draw at wide and narrow sizes\n",
            length(plot_cases)))

cat("PASS test_plot_smoke.R\n")
