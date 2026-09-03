real_data_root <- Sys.getenv("MONASH_SPORT_REAL_DATA_ROOT", unset = "")

if (!nzchar(real_data_root)) {
  cat("SKIP test_real_upload_parsers.R (MONASH_SPORT_REAL_DATA_ROOT is not set)\n")
} else {
  monthly_files <- discover_monthly_site_report_files(
    file.path(real_data_root, "Monthly Site Report")
  )
  venue_files <- discover_venues_tracking_report_files(
    file.path(real_data_root, "Business Unit Tracking Reports", "Venues")
  )
  site_files <- discover_business_unit_template_files(
    file.path(real_data_root, "Business Unit Templates that feed master reports")
  )

  monthly <- load_monthly_site_reports(
    paths = tail(monthly_files, 1L),
    source_names = basename(tail(monthly_files, 1L)),
    strict = FALSE
  )
  venues <- load_venues_tracking_reports(
    paths = tail(venue_files, 1L),
    source_names = basename(tail(venue_files, 1L)),
    strict = FALSE
  )
  sites <- load_business_unit_template_reports(
    paths = head(site_files, 1L),
    source_names = basename(head(site_files, 1L)),
    strict = FALSE
  )

  stopifnot(
    inherits(monthly, "monthly_site_report_data"),
    nrow(monthly$files) == 1L,
    nrow(monthly$finance) > 0L,
    inherits(venues, "venues_tracking_report_data"),
    nrow(venues$files) == 1L,
    nrow(venues$finance) > 0L,
    inherits(sites, "business_unit_template_report_data"),
    nrow(sites$files) == 1L,
    nrow(sites$finance) > 0L
  )

  cat("PASS test_real_upload_parsers.R\n")
}
