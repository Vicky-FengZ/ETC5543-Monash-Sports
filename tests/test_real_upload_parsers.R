real_data_root <- Sys.getenv("MONASH_SPORT_REAL_DATA_ROOT", unset = "")

if (!nzchar(real_data_root)) {
  cat("SKIP test_real_upload_parsers.R (MONASH_SPORT_REAL_DATA_ROOT is not set)\n")
} else {
  monthly_files <- discover_monthly_site_report_files(
    file.path(real_data_root, "Monthly Site Report")
  )
  # Accept both the documented Venues subfolder and a flat tracking folder.
  # Discovery still filters filenames; no workbooks are moved or modified.
  venue_dir <- file.path(real_data_root, "Business Unit Tracking Reports", "Venues")
  if (!dir.exists(venue_dir)) {
    venue_dir <- file.path(real_data_root, "Business Unit Tracking Reports")
  }
  venue_files <- discover_venues_tracking_report_files(venue_dir)
  site_files <- discover_business_unit_template_files(
    file.path(real_data_root, "Business Unit Templates that feed master reports")
  )
  stopifnot(length(monthly_files) > 0L, length(venue_files) > 0L, length(site_files) > 0L)
  cat(sprintf("REAL DATA: %d Monthly, %d Venues, %d Detailed Site workbooks\n",
              length(monthly_files), length(venue_files), length(site_files)))

  monthly <- load_monthly_site_reports(
    paths = monthly_files,
    source_names = basename(monthly_files),
    strict = FALSE
  )
  cat("PASS real Monthly Site Reports parsing\n")
  venues <- load_venues_tracking_reports(
    paths = venue_files,
    source_names = basename(venue_files),
    strict = FALSE
  )
  cat("PASS real Venues Tracking Reports parsing\n")
  sites <- load_business_unit_template_reports(
    paths = site_files,
    source_names = basename(site_files),
    strict = FALSE
  )
  cat("PASS real Detailed Site Reports parsing\n")

  monthly_preview <- basic_preview_monthly_site_data(monthly)
  monthly_preview_sheets <- basic_preview_source_sheets(
    monthly_preview,
    attr(monthly, "source_sheet_order", exact = TRUE)
  )
  venues_preview <- basic_preview_combined_report_data(
    venues,
    "venues_tracking"
  )
  sites_preview <- basic_preview_combined_report_data(
    sites,
    "site_detail"
  )

  preview_tables_have_source_sheet <- function(report, report_type) {
    table_names <- basic_preview_table_names(report, report_type)
    all(vapply(
      table_names,
      function(table_name) {
        "source_sheet" %in% names(report[[table_name]])
      },
      logical(1L)
    ))
  }

  stopifnot(
    inherits(monthly, "monthly_site_report_data"),
    length(attr(monthly, "source_sheet_order")) > 0L,
    nrow(monthly$files) == length(monthly_files),
    nrow(monthly$finance) > 0L,
    !any(
      !is.na(monthly$validation$sheet) &
        normalise_excel_label(monthly$validation$sheet) == "chart1"
    ),
    !any(grepl("rId1", monthly$validation$message, fixed = TRUE)),
    nrow(monthly_preview) ==
      nrow(monthly$finance) +
      nrow(monthly$discounts) +
      4L * nrow(monthly$total_revenue),
    setequal(
      unique(monthly_preview$data_section),
      c("Finance", "Discounts", "Total Revenue")
    ),
    length(monthly_preview_sheets) > 0L,
    all(monthly_preview$data_section %in%
      c("Finance", "Discounts", "Total Revenue")),
    inherits(monthly_preview$period_date, "Date"),
    identical(
      basic_preview_table_names(monthly, "monthly_site"),
      BASIC_PREVIEW_TABLES$monthly_site
    ),
    preview_tables_have_source_sheet(monthly, "monthly_site"),
    inherits(venues, "venues_tracking_report_data"),
    length(attr(venues, "source_sheet_order")) > 0L,
    nrow(venues$files) == length(venue_files),
    nrow(venues$finance) > 0L,
    nrow(venues$court_hire) > 0L,
    nrow(venues$online_sales) > 0L,
    nrow(venues$merchandise) > 0L,
    nrow(venues$casual_hours) > 0L,
    nrow(venues$casual_wages) > 0L,
    nrow(venues$online_sales_checks) > 0L,
    nrow(venues$merchandise_checks) > 0L,
    nrow(venues$casual_hours_checks) > 0L,
    nrow(venues$court_hire_checks) > 0L,
    nrow(venues$casual_wages_checks) > 0L,
    !any(venues$validation$level == "error"),
    identical(
      sort(unique(venues$files$report_year)),
      sort(unique(venues$casual_wages$salary_year))
    ),
    all(
      venues$casual_wages$salary_year == venues$casual_wages$report_year
    ),
    identical(
      sort(unique(venues$court_hire$report_year)),
      sort(unique(venues$files$report_year))
    ),
    all(venues$casual_hours$record_group == "Monthly Summary"),
    !any(venues$casual_hours$period_type == "Pay Period"),
    nrow(venues$casual_hours) == 39L * nrow(venues$files),
    identical(
      BASIC_PREVIEW_TABLES$venues_tracking,
      c(
        "finance",
        "attendance",
        "facility_hire",
        "court_hire",
        "membership",
        "online_sales",
        "merchandise",
        "casual_hours",
        "casual_wages"
      )
    ),
    identical(
      basic_preview_table_names(venues, "venues_tracking"),
      BASIC_PREVIEW_TABLES$venues_tracking
    ),
    preview_tables_have_source_sheet(venues, "venues_tracking"),
    nrow(venues_preview) == sum(vapply(
      venues[BASIC_PREVIEW_TABLES$venues_tracking],
      nrow,
      integer(1L)
    )),
    setequal(
      basic_preview_source_sheets(
        venues_preview,
        attr(venues, "source_sheet_order", exact = TRUE)
      ),
      unique(basic_preview_sheet_label(venues_preview$source_sheet))
    ),
    sum(
      basic_preview_source_sheets(
        venues_preview,
        attr(venues, "source_sheet_order", exact = TRUE)
      ) == "Casual Hours"
    ) == 1L,
    sum(
      basic_preview_source_sheets(
        venues_preview,
        attr(venues, "source_sheet_order", exact = TRUE)
      ) == "Casual Hire"
    ) == 1L,
    inherits(venues_preview$date, "Date"),
    !any(grepl("^\\.preview_", names(venues_preview))),
    inherits(sites, "business_unit_template_report_data"),
    length(attr(sites, "source_sheet_order")) > 0L,
    nrow(sites$files) == length(site_files),
    nrow(sites$finance) > 0L,
    identical(
      basic_preview_table_names(sites, "site_detail"),
      BASIC_PREVIEW_TABLES$site_detail
    ),
    preview_tables_have_source_sheet(sites, "site_detail"),
    nrow(sites_preview) == sum(vapply(
      sites[BASIC_PREVIEW_TABLES$site_detail],
      nrow,
      integer(1L)
    )),
    setequal(
      basic_preview_source_sheets(
        sites_preview,
        attr(sites, "source_sheet_order", exact = TRUE)
      ),
      unique(basic_preview_sheet_label(sites_preview$source_sheet))
    ),
    inherits(sites_preview$date, "Date"),
    !any(grepl("^\\.preview_", names(sites_preview)))
  )

  cat("PASS test_real_upload_parsers.R\n")
}
