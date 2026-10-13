# Exercise extensions through cleaning, validation, preview, export and the UI.
local({
  make_raw <- function(rows, columns) {
    as.data.frame(matrix(NA_character_, rows, columns), stringsAsFactors = FALSE)
  }
  wb <- list(report_year = 2028L, source_file = "New Sport Site Report 2028.xlsx",
    report_unit = "New Sport", campus = "Unspecified", site_code = NA_character_,
    sheet_names = list(direct_visits = "Direct Visits", memberships = "Memberships"))
  raw <- make_raw(5L, 66L)
  for (period in seq_len(13L)) {
    start <- 2L + (period - 1L) * 5L
    raw[1L, start] <- if (period <= 12L) month.abb[[period]] else "YTD Total"
    raw[2L, start:(start + 4L)] <- c("Students", "Staff", "Community", "Alumni", "Total")
    multiplier <- if (period <= 12L) 1 else 12
    raw[3L, start:(start + 4L)] <- as.character(c(10, 20, 30, 40, 100) * multiplier)
    raw[4L, start:(start + 4L)] <- as.character(c(5, 10, 15, 20, 50) * multiplier)
    raw[5L, start:(start + 4L)] <- as.character(c(15, 30, 45, 60, 150) * multiplier)
  }
  raw[3:5, 1L] <- c("Padel", "Pickleball", "Total")
  wb$sheets <- list(direct_visits = raw)
  visits <- clean_business_unit_template_visits(wb, "direct_visits", "Direct")
  stopifnot(nrow(visits) == 3L * 13L * 5L,
    all(c("Padel", "Pickleball") %in% visits$activity),
    all(c("Alumni", "Total") %in% visits$audience))
  structure_issues <- validate_business_unit_visit_structure(wb, "direct_visits", "Direct Visits")
  stopifnot(!any(structure_issues$level == "error"))
  checks <- build_business_unit_visit_checks(visits)
  stopifnot(nrow(checks) > 0L, all(checks$status == "pass"))

  # New metric names survive without assuming every new metric is a count.
  wb$sheets$memberships <- data.frame(
    a = c("Month", "Jan"), b = c("New Members", "10"),
    c = c("Paused Memberships", "3"), d = c("Average Membership Fee", "42"),
    e = c(NA, "99"), f = c("Status Note", "Awaiting confirmation"),
    stringsAsFactors = FALSE)
  member <- clean_business_unit_template_member_sheet(wb, "memberships", "Membership")
  stopifnot(member$metric_type[member$metric == "New Members"] == "Count",
    member$metric_type[member$metric == "Average Membership Fee"] == "Unknown",
    member$value[member$metric == "Paused Memberships"] == 3,
    any(member$raw_metric == "Status Note"))
  wb$workbook_sheets <- c("Memberships", "Direct Visits", "New Sport Detail")
  report <- structure(list(memberships = member, visits = visits,
    validation = empty_import_issue_table()), class = "business_unit_template_report_data")
  report <- finalise_extensible_report(report, list(wb), "site_detail")
  stopifnot(any(report$additional_data$raw_value == "99"),
    any(report$additional_data$raw_value == "Awaiting confirmation"),
    any(grepl("no configured data layout", report$validation$message, fixed = TRUE)),
    all(c("source_sheet", "source_row", "source_column") %in% names(report$additional_data)))
  configured <- data.frame(family = "site_detail", dataset = "memberships",
    metric = "Average Membership Fee", unit = "Currency", aggregation = "Mean")
  updated <- define_import_report_metrics(report, "site_detail", configured)
  stopifnot(updated$memberships$unit[updated$memberships$metric == "Average Membership Fee"] == "Currency",
    identical(updated$memberships$value, report$memberships$value))
  preview <- basic_preview_combined_report_data(updated, "site_detail")
  exports <- basic_cleaned_export_sheets(preview)
  stopifnot(all(c("Memberships", "Direct Visits") %in% names(exports)),
    any(exports$Memberships$raw_value == "Awaiting confirmation", na.rm = TRUE),
    !any(c("unit", "aggregation") %in% names(exports$Memberships)),
    any(exports$Memberships$value == 42, na.rm = TRUE))

  # Venues also retain unfamiliar metric columns.
  venue_raw <- make_raw(3L, 13L)
  venue_raw[1L, 2:13] <- month.abb
  venue_raw[2L, 2:13] <- rep("Paused Memberships", 12L)
  venue_raw[3L, ] <- c("Fitness", rep("2", 12L))
  venue_wb <- list(report_year = 2028L, source_file = "2028 Venues Tracking Report.xlsx",
    membership_sheet_name = "Membership Tracking", membership_sheet = venue_raw)
  venue_members <- clean_venues_tracking_report_membership(venue_wb)
  stopifnot(nrow(venue_members) == 12L,
    all(venue_members$raw_metric == "Paused Memberships"),
    all(venue_members$value == 2))
  stopifnot(canonical_venues_merchandise_metric("Returned Items") == "Returned Items")
  expanded_merchandise <- merchandise_raw
  expanded_merchandise[[ncol(expanded_merchandise) + 1L]] <- NA_character_
  expanded_merchandise[2L, ncol(expanded_merchandise)] <- "Returned Items"
  expanded_merchandise[3L, ncol(expanded_merchandise)] <- "2"
  merchandise <- clean_venues_tracking_report_merchandise(list(
    report_year = 2028L, source_file = "2028 Venues Tracking Report.xlsx",
    merchandise_sheet_name = "Merchandise Tracking", merchandise_sheet = expanded_merchandise))
  stopifnot(any(merchandise$metric == "Returned Items" & merchandise$value == 2, na.rm = TRUE))

  # A new discount category at the start of a group must participate in
  # reconciliation even when Students is no longer the first column.
  finance_raw <- make_raw(11L, 5L)
  finance_raw[1L, 2L] <- "New Sport"
  finance_raw[2L, ] <- c("INCOME", "Budget", "Actual", "Variance", NA)
  finance_raw[3L, ] <- c("Jan", "100", "110", "10", "99")
  finance_raw[4L, 1L] <- "Total"
  finance_raw[5L, ] <- c("EXPENSES", "Budget", "Actual", "Variance", NA)
  finance_raw[6L, ] <- c("Jan", "50", "50", "0", NA)
  finance_raw[7L, 1L] <- "Total"
  finance_raw[8L, ] <- c("DISCOUNTS", "VIP", "Students", "Staff", "Other")
  finance_raw[9L, ] <- c("Jan", "5", "10", "3", "2")
  finance_raw[10L, ] <- c("Total", "5", "10", "3", "2")
  finance_raw[11L, ] <- c("Total Revenue (actual + opp)", "130", NA, NA, NA)
  finance_raw[[6L]] <- NA_character_
  finance_raw[8:10, 6L] <- c("Total", "20", "20")
  paths <- c(tempfile(fileext = ".xlsx"), tempfile(fileext = ".xlsx"))
  tryCatch({
    writexl::write_xlsx(list("Monash Sport - Finance Summary" = finance_raw),
      paths[[1L]], col_names = FALSE)
    monthly <- load_monthly_site_reports(paths = paths[[1L]],
      source_names = "Monthly Site Report 2028.xlsx", strict = FALSE)
    stopifnot(!any(monthly$validation$level == "error"),
      "VIP" %in% monthly$discounts$discount_type,
      monthly$total_revenue$calculated_total_revenue == 130,
      monthly$total_revenue$validation_status == "pass",
      "99" %in% monthly$additional_data$raw_value,
      sum(monthly$additional_data$interpretation == "Reported discount total") == 1L,
      any(grepl("1 value(s) could not be interpreted", monthly$validation$message, fixed = TRUE)))
    monthly_preview <- basic_preview_monthly_site_data(monthly)
    sheets <- basic_cleaned_export_sheets(monthly_preview)
    writexl::write_xlsx(sheets, paths[[2L]])
    roundtrip <- readxl::read_excel(paths[[2L]], sheet = 1L)
    stopifnot(any(roundtrip$raw_value == "99", na.rm = TRUE),
      any(roundtrip$value_type == "VIP", na.rm = TRUE))
  }, finally = unlink(paths))

  # Default definitions remain in imported data. Users can preview/download
  # unfamiliar metrics directly without any per-session configuration.
  fake_parser <- function(path, source_name) report
  shiny::testServer(mod_data_upload_server,
    args = list(parsers = list(site_detail = fake_parser)), {
      session$flushReact()
      upload_path <- tempfile(fileext = ".xlsx")
      writeLines("test fixture", upload_path)
      process_upload_batch("site_detail", data.frame(name = "New Sport Site Report 2028.xlsx",
        size = file.info(upload_path)$size, datapath = upload_path))
      session$setInputs(preview_report_type = "site_detail")
      session$flushReact()
      data <- session$returned()$site_reports$memberships
      stopifnot(data$unit[data$metric == "Average Membership Fee"] == "Unknown",
        data$aggregation[data$metric == "Average Membership Fee"] == "None",
        data$value[data$metric == "Average Membership Fee"] == 42,
        data$aggregation[data$metric == "New Members"] == "Sum",
        data$unit[data$metric == "Paused Memberships"] == "Unknown")
      downloaded <- preview_export_sheets()
      stopifnot(!any(c("unit", "aggregation") %in% names(downloaded$Memberships)),
        any(downloaded$Memberships$metric == "Average Membership Fee" &
          downloaded$Memberships$value == 42, na.rm = TRUE),
        any(downloaded$Memberships$raw_value == "Awaiting confirmation", na.rm = TRUE))
      remove_session_files(registry()$file_id)
      session$flushReact()
      stopifnot(is.null(preview_report()))
      unlink(upload_path)
    })
  stopifnot(import_metric_definition("site_detail", "memberships", "Average Membership Fee")$unit == "Unknown")
})
cat("PASS extensible columns, default definitions, reconciliation and simplified preview/export\n")
