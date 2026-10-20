# Validate the shipped blank templates through the production loaders.
local({
  root <- file.path("www", "templates")
  files <- c(monthly_site = "Monthly Site Report 2026.xlsx",
    site_detail = "New Sport Site Report 2026.xlsx",
    venues_tracking = "2026 Venues Tracking Report.xlsx")
  loaders <- list(monthly_site = load_monthly_site_reports,
    site_detail = load_business_unit_template_reports, venues_tracking = load_venues_tracking_reports)
  for (family in names(files)) {
    path <- file.path(root, files[[family]])
    stopifnot(file.exists(path))
    report <- loaders[[family]](paths = path, source_names = basename(path), strict = FALSE)
    stopifnot(!any(report$validation$level == "error"), nrow(report$files) == 1L,
      report$files$report_year == 2026L, nrow(report$finance) > 0L,
      all(is.na(report$finance$value)),
      !any(grepl("no configured data layout", report$validation$message, fixed = TRUE)))
    if (family == "monthly_site") {
      stopifnot(nrow(report$discounts) > 0L, nrow(report$total_revenue) == 2L)
    } else if (family == "site_detail") {
      stopifnot(nrow(report$visits) > 0L, nrow(report$memberships) > 0L,
        nrow(report$enrollments) > 0L)
    } else {
      stopifnot(nrow(report$casual_hours) == 39L, nrow(report$casual_wages) == 39L,
        all(report$casual_wages$salary_year == 2026L), nrow(report$court_hire) > 0L)
    }
  }
})
cat("PASS blank templates: production import, required structure and unreported values\n")
