# -------------------------------------------------------------------------
# Monthly Site Report validation
# -------------------------------------------------------------------------

validate_monthly_site_report_workbook <- function(workbook) {

  issues <- list()

  if (!inherits(workbook, "monthly_site_report_workbook")) {
    return(
      make_import_issue(
        level = "error",
        source_file = NA_character_,
        message = "Object is not a Monthly Site Report workbook."
      )
    )
  }

  issues <- list(
    workbook$sheet_read_issues
  )

  normalised_sheets <- normalise_excel_label(
    workbook$workbook_sheets
  )

  if (!"monash sport - finance summary" %in% normalised_sheets) {
    issues[[length(issues) + 1L]] <- make_import_issue(
      level = "warning",
      source_file = workbook$source_file,
      message = paste(
        "The Monash Sport - Finance Summary sheet was not found.",
        "Detail sheets will still be imported when valid."
      )
    )
  }

  valid_sheet_count <- 0L

  for (sheet_name in names(workbook$sheets)) {

    raw_sheet <- workbook$sheets[[sheet_name]]

    if (sheet_has_finance_sections(raw_sheet)) {
      valid_sheet_count <- valid_sheet_count + 1L

      if (!sheet_has_discount_section(raw_sheet)) {
        issues[[length(issues) + 1L]] <- make_import_issue(
          level = "warning",
          source_file = workbook$source_file,
          sheet = sheet_name,
          message = paste(
            "The DISCOUNTS section was not found.",
            "Finance data will still be imported."
          )
        )
      }

      if (!sheet_has_total_revenue_section(raw_sheet)) {
        issues[[length(issues) + 1L]] <- make_import_issue(
          level = "warning",
          source_file = workbook$source_file,
          sheet = sheet_name,
          message = paste(
            "The Total Revenue (actual + opp) row was not found.",
            "Revenue validation will not be available for this sheet."
          )
        )
      }
    } else {
      issues[[length(issues) + 1L]] <- make_import_issue(
        level = "warning",
        source_file = workbook$source_file,
        sheet = sheet_name,
        message = paste(
          "Sheet was skipped because both INCOME and EXPENSES",
          "sections could not be found."
        )
      )
    }
  }

  if (valid_sheet_count == 0L) {
    issues[[length(issues) + 1L]] <- make_import_issue(
      level = "error",
      source_file = workbook$source_file,
      message = paste(
        "No sheet contained the expected INCOME and EXPENSES",
        "finance sections."
      )
    )
  }

  bind_import_tables(
    issues,
    empty_import_issue_table()
  )
}


# Warnings for business units whose reported Total Revenue differs from Actual Income plus
# discount opportunity, or could not be checked.
validate_monthly_total_revenue_table <- function(total_revenue) {

  if (
    !is.data.frame(total_revenue) ||
      nrow(total_revenue) == 0L
  ) {
    return(empty_import_issue_table())
  }

  issues <- list()
  source_files <- unique(total_revenue$source_file)

  for (source_file in source_files) {

    file_rows <- total_revenue[
      total_revenue$source_file == source_file,
      ,
      drop = FALSE
    ]

    failed_rows <- file_rows$validation_status == "fail"

    if (any(failed_rows)) {

      maximum_difference <- max(
        abs(file_rows$validation_difference[failed_rows]),
        na.rm = TRUE
      )

      issues[[length(issues) + 1L]] <- make_import_issue(
        level = "warning",
        source_file = source_file,
        message = paste0(
          "Total Revenue validation failed for ",
          sum(failed_rows),
          " business-unit row(s); maximum absolute difference: ",
          format(
            round(maximum_difference, 2),
            big.mark = ",",
            scientific = FALSE,
            trim = TRUE,
            nsmall = 2
          ),
          ". Use calculated_total_revenue for dashboard analysis and ",
          "retain reported_total_revenue for audit."
        )
      )
    }

    incomplete_rows <- !file_rows$validation_status %in% c(
      "pass",
      "fail"
    )

    if (any(incomplete_rows)) {

      statuses <- sort(
        unique(file_rows$validation_status[incomplete_rows])
      )

      issues[[length(issues) + 1L]] <- make_import_issue(
        level = "warning",
        source_file = source_file,
        message = paste0(
          "Total Revenue validation was incomplete for ",
          sum(incomplete_rows),
          " business-unit row(s): ",
          paste(statuses, collapse = ", "),
          "."
        )
      )
    }
  }

  bind_import_tables(
    issues,
    empty_import_issue_table()
  )
}


# Warnings across all uploaded years: two files for one year, or missing years in the series.
validate_monthly_site_report_collection <- function(workbooks) {

  if (length(workbooks) == 0L) {
    return(empty_import_issue_table())
  }

  issues <- list()
  years <- vapply(
    workbooks,
    function(x) x$report_year,
    integer(1)
  )

  duplicate_years <- sort(
    unique(years[duplicated(years)])
  )

  if (length(duplicate_years) > 0L) {
    issues[[length(issues) + 1L]] <- make_import_issue(
      level = "warning",
      source_file = NA_character_,
      message = paste0(
        "Multiple Monthly Site Report files were found for year(s): ",
        paste(duplicate_years, collapse = ", "),
        ". Check for duplicate reporting periods before aggregation."
      )
    )
  }

  expected_years <- seq.int(
    min(years),
    max(years)
  )

  missing_years <- setdiff(
    expected_years,
    years
  )

  if (length(missing_years) > 0L) {
    issues[[length(issues) + 1L]] <- make_import_issue(
      level = "warning",
      source_file = NA_character_,
      message = paste0(
        "The reporting series has missing year(s): ",
        paste(missing_years, collapse = ", "),
        ". This should be addressed before forecasting."
      )
    )
  }

  bind_import_tables(
    issues,
    empty_import_issue_table()
  )
}
