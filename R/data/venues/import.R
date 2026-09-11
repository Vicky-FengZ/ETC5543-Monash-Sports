# -------------------------------------------------------------------------
# Venues Tracking Report import
# -------------------------------------------------------------------------

require_venues_report_packages <- function() {

  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop(
      paste(
        "The readxl package is required to import Venues Tracking Reports.",
        "Install it with install.packages('readxl')."
      ),
      call. = FALSE
    )
  }
}


discover_venues_tracking_report_files <- function(
  data_dir = file.path(
    "data",
    "raw",
    "Business Unit Tracking Reports",
    "Venues"
  )
) {

  if (!dir.exists(data_dir)) {
    stop(
      paste0(
        "Venues Tracking Report directory does not exist: ",
        data_dir
      ),
      call. = FALSE
    )
  }

  files <- list.files(
    path = data_dir,
    pattern = "\\.(xlsx|xls)$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )

  file_names <- basename(files)
  keep <- !startsWith(file_names, "~$") &
    grepl(
      "Venues[[:space:]_-]*Tracking[[:space:]_-]*Report",
      file_names,
      ignore.case = TRUE
    )

  files <- files[keep]

  if (length(files) == 0L) {
    stop(
      paste0(
        "No Venues Tracking Report Excel files were found in: ",
        data_dir
      ),
      call. = FALSE
    )
  }

  sort(
    normalizePath(
      files,
      winslash = "/",
      mustWork = TRUE
    )
  )
}


extract_venues_tracking_report_year <- function(source_name) {

  year_matches <- regmatches(
    source_name,
    gregexpr(
      "(?<![0-9])(19|20)[0-9]{2}(?![0-9])",
      source_name,
      perl = TRUE
    )
  )[[1]]

  year_matches <- unique(year_matches[nzchar(year_matches)])

  if (length(year_matches) != 1L) {
    stop(
      paste0(
        "Could not identify exactly one report year from file name: ",
        source_name
      ),
      call. = FALSE
    )
  }

  as.integer(year_matches)
}


read_venues_tracking_sheet <- function(path, sheet_name) {

  raw_sheet <- suppressMessages(
    suppressWarnings(
      readxl::read_excel(
        path = path,
        sheet = sheet_name,
        col_names = FALSE,
        na = c("", "NA"),
        .name_repair = "minimal"
      )
    )
  )

  raw_sheet <- as.data.frame(
    raw_sheet,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(raw_sheet) <- paste0(
    "column_",
    seq_len(ncol(raw_sheet))
  )

  raw_sheet
}


find_venues_tracking_sheet <- function(
  sheet_names,
  expected_name,
  source_name,
  required = FALSE,
  allow_year_prefix = FALSE,
  allow_year_comparison_suffix = FALSE
) {

  sheet_keys <- normalise_excel_label(sheet_names)
  expected_keys <- normalise_excel_label(expected_name)

  if (isTRUE(allow_year_prefix)) {
    sheet_keys <- sub(
      "^(19|20)[0-9]{2}[[:space:]_-]+",
      "",
      sheet_keys
    )
  }

  if (isTRUE(allow_year_comparison_suffix)) {
    sheet_keys <- sub(
      paste0(
        "[[:space:]_-]+(19|20)[0-9]{2}",
        "[[:space:]_-]+vs[[:space:]_-]+(19|20)[0-9]{2}$"
      ),
      "",
      sheet_keys
    )
  }

  matches <- sheet_names[
    sheet_keys %in% expected_keys
  ]

  expected_label <- paste(expected_name, collapse = " or ")

  if (length(matches) > 1L) {
    stop(
      paste0(
        "Expected at most one ",
        expected_label,
        " sheet in ",
        source_name,
        "; found ",
        length(matches),
        "."
      ),
      call. = FALSE
    )
  }

  if (length(matches) == 0L) {
    if (isTRUE(required)) {
      stop(
        paste0(
          "Expected exactly one ",
          expected_label,
          " sheet in ",
          source_name,
          "; found 0."
        ),
        call. = FALSE
      )
    }

    return(NA_character_)
  }

  matches[[1]]
}


read_venues_tracking_report_file <- function(
  path,
  source_name = basename(path)
) {

  require_venues_report_packages()

  if (!file.exists(path)) {
    stop(
      paste0("Venues Tracking Report file does not exist: ", path),
      call. = FALSE
    )
  }

  extension <- tolower(tools::file_ext(path))

  if (!extension %in% c("xlsx", "xls")) {
    stop(
      paste0("Unsupported Venues Tracking Report file type: .", extension),
      call. = FALSE
    )
  }

  report_year <- extract_venues_tracking_report_year(source_name)
  sheet_names <- readxl::excel_sheets(path)
  finance_sheet_name <- find_venues_tracking_sheet(
    sheet_names = sheet_names,
    expected_name = "MTD - Venues Finance",
    source_name = source_name,
    required = TRUE
  )
  attendance_sheet_name <- find_venues_tracking_sheet(
    sheet_names = sheet_names,
    expected_name = "Facility Attendance",
    source_name = source_name
  )
  facility_hire_sheet_name <- find_venues_tracking_sheet(
    sheet_names = sheet_names,
    expected_name = "Facility Hire Tracking",
    source_name = source_name
  )
  court_hire_sheet_name <- find_venues_tracking_sheet(
    sheet_names = sheet_names,
    expected_name = "Court Hire",
    source_name = source_name,
    allow_year_comparison_suffix = TRUE
  )
  membership_sheet_name <- find_venues_tracking_sheet(
    sheet_names = sheet_names,
    expected_name = "Membership Tracking",
    source_name = source_name
  )
  online_sales_sheet_name <- find_venues_tracking_sheet(
    sheet_names = sheet_names,
    expected_name = "Online Sales",
    source_name = source_name
  )
  merchandise_sheet_name <- find_venues_tracking_sheet(
    sheet_names = sheet_names,
    expected_name = "Merchandise Tracking",
    source_name = source_name
  )
  casual_hours_sheet_name <- find_venues_tracking_sheet(
    sheet_names = sheet_names,
    expected_name = "Casual Hours",
    source_name = source_name,
    allow_year_prefix = TRUE
  )
  casual_wages_sheet_name <- find_venues_tracking_sheet(
    sheet_names = sheet_names,
    expected_name = "Casual Wages",
    source_name = source_name
  )

  finance_sheet <- read_venues_tracking_sheet(
    path,
    finance_sheet_name
  )
  attendance_sheet <- if (is.na(attendance_sheet_name)) {
    NULL
  } else {
    read_venues_tracking_sheet(path, attendance_sheet_name)
  }
  facility_hire_sheet <- if (is.na(facility_hire_sheet_name)) {
    NULL
  } else {
    read_venues_tracking_sheet(path, facility_hire_sheet_name)
  }
  court_hire_sheet <- if (is.na(court_hire_sheet_name)) {
    NULL
  } else {
    read_venues_tracking_sheet(path, court_hire_sheet_name)
  }
  membership_sheet <- if (is.na(membership_sheet_name)) {
    NULL
  } else {
    read_venues_tracking_sheet(path, membership_sheet_name)
  }
  online_sales_sheet <- if (is.na(online_sales_sheet_name)) {
    NULL
  } else {
    read_venues_tracking_sheet(path, online_sales_sheet_name)
  }
  merchandise_sheet <- if (is.na(merchandise_sheet_name)) {
    NULL
  } else {
    read_venues_tracking_sheet(path, merchandise_sheet_name)
  }
  casual_hours_sheet <- if (is.na(casual_hours_sheet_name)) {
    NULL
  } else {
    read_venues_tracking_sheet(path, casual_hours_sheet_name)
  }
  casual_wages_sheet <- if (is.na(casual_wages_sheet_name)) {
    NULL
  } else {
    read_venues_tracking_sheet(path, casual_wages_sheet_name)
  }

  structure(
    list(
      path = normalizePath(
        path,
        winslash = "/",
        mustWork = TRUE
      ),
      source_file = source_name,
      report_year = report_year,
      workbook_sheets = sheet_names,
      finance_sheet_name = finance_sheet_name,
      finance_sheet = finance_sheet,
      attendance_sheet_name = attendance_sheet_name,
      attendance_sheet = attendance_sheet,
      facility_hire_sheet_name = facility_hire_sheet_name,
      facility_hire_sheet = facility_hire_sheet,
      court_hire_sheet_name = court_hire_sheet_name,
      court_hire_sheet = court_hire_sheet,
      membership_sheet_name = membership_sheet_name,
      membership_sheet = membership_sheet,
      online_sales_sheet_name = online_sales_sheet_name,
      online_sales_sheet = online_sales_sheet,
      merchandise_sheet_name = merchandise_sheet_name,
      merchandise_sheet = merchandise_sheet,
      casual_hours_sheet_name = casual_hours_sheet_name,
      casual_hours_sheet = casual_hours_sheet,
      casual_wages_sheet_name = casual_wages_sheet_name,
      casual_wages_sheet = casual_wages_sheet
    ),
    class = "venues_tracking_report_workbook"
  )
}


load_venues_tracking_reports <- function(
  data_dir = file.path(
    "data",
    "raw",
    "Business Unit Tracking Reports",
    "Venues"
  ),
  paths = NULL,
  source_names = NULL,
  strict = TRUE,
  validation_tolerance = 0.01
) {

  if (
    length(validation_tolerance) != 1L ||
      is.na(validation_tolerance) ||
      !is.numeric(validation_tolerance) ||
      validation_tolerance < 0
  ) {
    stop(
      "validation_tolerance must be one non-negative number.",
      call. = FALSE
    )
  }

  if (is.null(paths)) {
    paths <- discover_venues_tracking_report_files(data_dir)
  }

  if (length(paths) == 0L) {
    stop("No Venues Tracking Report paths were supplied.", call. = FALSE)
  }

  if (is.null(source_names)) {
    source_names <- basename(paths)
  }

  if (length(source_names) != length(paths)) {
    stop(
      "source_names must have the same length as paths.",
      call. = FALSE
    )
  }

  workbooks <- list()
  read_issues <- list()

  for (index in seq_along(paths)) {

    workbook <- tryCatch(
      read_venues_tracking_report_file(
        path = paths[[index]],
        source_name = source_names[[index]]
      ),
      error = function(error) {

        read_issues[[length(read_issues) + 1L]] <<- make_import_issue(
          level = "error",
          source_file = source_names[[index]],
          message = conditionMessage(error)
        )

        NULL
      }
    )

    if (!is.null(workbook)) {
      workbooks[[length(workbooks) + 1L]] <- workbook
    }
  }

  workbook_issues <- lapply(
    workbooks,
    validate_venues_tracking_report_workbook
  )

  validation <- bind_import_tables(
    c(
      read_issues,
      workbook_issues,
      list(validate_venues_tracking_report_collection(workbooks))
    ),
    empty_import_issue_table()
  )

  error_rows <- validation$level == "error"

  if (isTRUE(strict) && any(error_rows)) {

    error_messages <- paste0(
      ifelse(
        is.na(validation$source_file[error_rows]),
        "",
        paste0("[", validation$source_file[error_rows], "] ")
      ),
      validation$message[error_rows]
    )

    stop(
      paste(
        "Venues Tracking Report import failed:",
        paste(error_messages, collapse = "; ")
      ),
      call. = FALSE
    )
  }

  invalid_files <- unique(
    validation$source_file[error_rows]
  )
  valid_workbooks <- Filter(
    function(workbook) {
      !workbook$source_file %in% invalid_files
    },
    workbooks
  )

  finance <- bind_import_tables(
    lapply(
      valid_workbooks,
      clean_venues_tracking_report_finance
    ),
    empty_venues_finance_table()
  )

  attendance <- bind_import_tables(
    lapply(
      valid_workbooks,
      clean_venues_tracking_report_attendance
    ),
    empty_venues_attendance_table()
  )

  facility_hire <- bind_import_tables(
    lapply(
      valid_workbooks,
      clean_venues_tracking_report_facility_hire
    ),
    empty_venues_facility_hire_table()
  )

  court_hire <- bind_import_tables(
    lapply(
      valid_workbooks,
      clean_venues_tracking_report_court_hire
    ),
    empty_venues_court_hire_table()
  )

  membership <- bind_import_tables(
    lapply(
      valid_workbooks,
      clean_venues_tracking_report_membership
    ),
    empty_venues_membership_table()
  )

  online_sales <- bind_import_tables(
    lapply(
      valid_workbooks,
      clean_venues_tracking_report_online_sales
    ),
    empty_venues_online_sales_table()
  )

  merchandise <- bind_import_tables(
    lapply(
      valid_workbooks,
      clean_venues_tracking_report_merchandise
    ),
    empty_venues_merchandise_table()
  )

  casual_hours <- bind_import_tables(
    lapply(
      valid_workbooks,
      clean_venues_tracking_report_casual_hours
    ),
    empty_venues_casual_hours_table()
  )

  casual_wages <- bind_import_tables(
    lapply(
      valid_workbooks,
      clean_venues_tracking_report_casual_wages
    ),
    empty_venues_casual_wages_table()
  )

  checks <- build_venues_finance_checks(
    finance,
    validation_tolerance = validation_tolerance
  )
  attendance_checks <- build_venues_attendance_checks(
    attendance,
    validation_tolerance = validation_tolerance
  )
  facility_hire_checks <- build_venues_facility_hire_checks(
    facility_hire,
    validation_tolerance = validation_tolerance
  )
  court_hire_checks <- build_venues_court_hire_checks(
    court_hire,
    validation_tolerance = validation_tolerance
  )
  membership_checks <- build_venues_membership_checks(
    membership,
    validation_tolerance = validation_tolerance
  )
  online_sales_checks <- build_venues_online_sales_checks(
    online_sales,
    validation_tolerance = validation_tolerance
  )
  merchandise_checks <- build_venues_merchandise_checks(
    merchandise,
    validation_tolerance = validation_tolerance
  )
  casual_hours_checks <- build_venues_casual_hours_checks(
    casual_hours,
    validation_tolerance = validation_tolerance
  )
  casual_wages_checks <- build_venues_casual_wages_checks(
    casual_wages,
    validation_tolerance = validation_tolerance
  )

  validation <- bind_import_tables(
    list(
      validation,
      validate_venues_finance_checks(checks),
      validate_venues_operational_checks(attendance_checks),
      validate_venues_operational_checks(facility_hire_checks),
      validate_venues_operational_checks(court_hire_checks),
      validate_venues_operational_checks(membership_checks),
      validate_venues_operational_checks(online_sales_checks),
      validate_venues_operational_checks(merchandise_checks),
      validate_venues_operational_checks(casual_hours_checks),
      validate_venues_operational_checks(casual_wages_checks)
    ),
    empty_import_issue_table()
  )

  file_summary <- if (length(workbooks) == 0L) {
    data.frame(
      source_file = character(),
      report_year = integer(),
      finance_sheet = character(),
      attendance_sheet = character(),
      facility_hire_sheet = character(),
      court_hire_sheet = character(),
      membership_sheet = character(),
      online_sales_sheet = character(),
      merchandise_sheet = character(),
      casual_hours_sheet = character(),
      casual_wages_sheet = character(),
      path = character(),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      source_file = vapply(
        workbooks,
        function(x) x$source_file,
        character(1)
      ),
      report_year = vapply(
        workbooks,
        function(x) x$report_year,
        integer(1)
      ),
      finance_sheet = vapply(
        workbooks,
        function(x) x$finance_sheet_name,
        character(1)
      ),
      attendance_sheet = vapply(
        workbooks,
        function(x) x$attendance_sheet_name,
        character(1)
      ),
      facility_hire_sheet = vapply(
        workbooks,
        function(x) x$facility_hire_sheet_name,
        character(1)
      ),
      court_hire_sheet = vapply(
        workbooks,
        function(x) x$court_hire_sheet_name,
        character(1)
      ),
      membership_sheet = vapply(
        workbooks,
        function(x) x$membership_sheet_name,
        character(1)
      ),
      online_sales_sheet = vapply(
        workbooks,
        function(x) x$online_sales_sheet_name,
        character(1)
      ),
      merchandise_sheet = vapply(
        workbooks,
        function(x) x$merchandise_sheet_name,
        character(1)
      ),
      casual_hours_sheet = vapply(
        workbooks,
        function(x) x$casual_hours_sheet_name,
        character(1)
      ),
      casual_wages_sheet = vapply(
        workbooks,
        function(x) x$casual_wages_sheet_name,
        character(1)
      ),
      path = vapply(
        workbooks,
        function(x) x$path,
        character(1)
      ),
      stringsAsFactors = FALSE
    )
  }

  structure(
    list(
      finance = finance,
      checks = checks,
      attendance = attendance,
      attendance_checks = attendance_checks,
      facility_hire = facility_hire,
      facility_hire_checks = facility_hire_checks,
      court_hire = court_hire,
      court_hire_checks = court_hire_checks,
      membership = membership,
      membership_checks = membership_checks,
      online_sales = online_sales,
      online_sales_checks = online_sales_checks,
      merchandise = merchandise,
      merchandise_checks = merchandise_checks,
      casual_hours = casual_hours,
      casual_hours_checks = casual_hours_checks,
      casual_wages = casual_wages,
      casual_wages_checks = casual_wages_checks,
      validation = validation,
      files = file_summary
    ),
    class = "venues_tracking_report_data",
    source_sheet_order = workbook_source_sheet_order(workbooks)
  )
}


print.venues_tracking_report_data <- function(x, ...) {

  years <- sort(unique(x$files$report_year))

  cat("Venues Tracking Report data\n")
  cat("  Files:", nrow(x$files), "\n")
  cat(
    "  Years:",
    if (length(years) == 0L) "none" else paste(years, collapse = ", "),
    "\n"
  )
  cat("  Finance rows:", nrow(x$finance), "\n")
  cat("  Attendance rows:", nrow(x$attendance), "\n")
  cat("  Facility hire rows:", nrow(x$facility_hire), "\n")
  cat("  Court hire rows:", nrow(x$court_hire), "\n")
  cat("  Membership rows:", nrow(x$membership), "\n")
  cat("  Online sales rows:", nrow(x$online_sales), "\n")
  cat("  Merchandise rows:", nrow(x$merchandise), "\n")
  cat("  Casual hours rows:", nrow(x$casual_hours), "\n")
  cat("  Casual wages rows:", nrow(x$casual_wages), "\n")
  cat(
    "  Finance checks passed:",
    sum(x$checks$status == "pass"),
    "of",
    nrow(x$checks),
    "\n"
  )
  cat(
    "  Attendance checks passed:",
    sum(x$attendance_checks$status == "pass"),
    "of",
    nrow(x$attendance_checks),
    "\n"
  )
  cat(
    "  Facility hire checks passed:",
    sum(x$facility_hire_checks$status == "pass"),
    "of",
    nrow(x$facility_hire_checks),
    "\n"
  )
  cat(
    "  Court hire checks passed:",
    sum(x$court_hire_checks$status == "pass"),
    "of",
    nrow(x$court_hire_checks),
    "\n"
  )
  cat(
    "  Membership checks passed:",
    sum(x$membership_checks$status == "pass"),
    "of",
    nrow(x$membership_checks),
    "\n"
  )
  cat(
    "  Online sales checks passed:",
    sum(x$online_sales_checks$status == "pass"),
    "of",
    nrow(x$online_sales_checks),
    "\n"
  )
  cat(
    "  Merchandise checks passed:",
    sum(x$merchandise_checks$status == "pass"),
    "of",
    nrow(x$merchandise_checks),
    "\n"
  )
  cat(
    "  Casual hours checks passed:",
    sum(x$casual_hours_checks$status == "pass"),
    "of",
    nrow(x$casual_hours_checks),
    "\n"
  )
  cat(
    "  Casual wages checks passed:",
    sum(x$casual_wages_checks$status == "pass"),
    "of",
    nrow(x$casual_wages_checks),
    "\n"
  )
  cat("  Validation issues:", nrow(x$validation), "\n")

  invisible(x)
}
