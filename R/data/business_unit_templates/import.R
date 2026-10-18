# -------------------------------------------------------------------------
# Business Unit Template report import
# -------------------------------------------------------------------------

require_business_unit_template_packages <- function() {

  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop(
      paste(
        "The readxl package is required to import Business Unit Templates.",
        "Install it with install.packages('readxl')."
      ),
      call. = FALSE
    )
  }
}


discover_business_unit_template_files <- function(
  data_dir = file.path(
    "data",
    "raw",
    "Business Unit Templates that feed master reports"
  )
) {

  if (!dir.exists(data_dir)) {
    stop(
      paste0(
        "Business Unit Template directory does not exist: ",
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
  keep <-
    !startsWith(file_names, "~$") &
    grepl(
      "(Site|Program)[[:space:]_-]*Report",
      file_names,
      ignore.case = TRUE
    )
  files <- files[keep]

  if (length(files) == 0L) {
    stop(
      paste0(
        "No Business Unit Template Excel files were found in: ",
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


extract_business_unit_template_year <- function(source_name) {

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


extract_business_unit_report_name <- function(source_name) {

  label <- tools::file_path_sans_ext(basename(source_name))
  label <- gsub(
    "(?<![0-9])(19|20)[0-9]{2}(?![0-9])",
    "",
    label,
    perl = TRUE
  )
  label <- gsub(
    "[[:space:]_-]*(Site|Program)[[:space:]_-]*Report[[:space:]_-]*",
    " ",
    label,
    ignore.case = TRUE
  )
  label <- trimws(gsub("[[:space:]]+", " ", label))
  key <- normalise_excel_label(label)
  known_names <- c(
    "aquatics" = "Aquatics",
    "caulfield" = "Caulfield",
    "health & fitness" = "Health & Fitness",
    "participation" = "Participation Programs",
    "participation programs" = "Participation Programs",
    "peninsula" = "Peninsula",
    "venues" = "Venues"
  )

  if (key %in% names(known_names)) {
    return(unname(known_names[[key]]))
  }

  if (!nzchar(label)) {
    stop(
      paste0("Could not identify report unit from file name: ", source_name),
      call. = FALSE
    )
  }

  label
}


business_unit_template_campus <- function(report_unit) {

  key <- normalise_excel_label(report_unit)

  if (key %in% c("aquatics", "health & fitness", "venues")) {
    return("Clayton")
  }
  if (key == "caulfield") {
    return("Caulfield")
  }
  if (key == "peninsula") {
    return("Peninsula")
  }
  if (key == "participation programs") {
    return("Multi-campus")
  }

  "Unspecified"
}


read_business_unit_template_sheet <- function(path, sheet_name) {

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
  names(raw_sheet) <- paste0("column_", seq_len(ncol(raw_sheet)))
  raw_sheet
}


find_business_unit_template_sheet <- function(
  sheet_names,
  expected_name,
  source_name,
  required = FALSE
) {

  matches <- sheet_names[
    normalise_excel_label(sheet_names) ==
      normalise_excel_label(expected_name)
  ]

  if (length(matches) > 1L) {
    stop(
      paste0(
        "Expected at most one ",
        expected_name,
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
          expected_name,
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


read_business_unit_template_file <- function(
  path,
  source_name = basename(path)
) {

  require_business_unit_template_packages()

  if (!file.exists(path)) {
    stop(
      paste0("Business Unit Template file does not exist: ", path),
      call. = FALSE
    )
  }

  extension <- tolower(tools::file_ext(path))
  if (!extension %in% c("xlsx", "xls")) {
    stop(
      paste0("Unsupported Business Unit Template file type: .", extension),
      call. = FALSE
    )
  }

  report_year <- extract_business_unit_template_year(source_name)
  report_unit <- extract_business_unit_report_name(source_name)
  sheet_names <- readxl::excel_sheets(path)
  expected_sheets <- c(
    finance = "Finance",
    direct_visits = "Direct Visits",
    indirect_visits = "Indirect Visits",
    memberships = "Memberships",
    enrollments = "Enrollments"
  )
  required_sheets <- c("finance", "direct_visits", "indirect_visits")
  resolved_names <- lapply(
    names(expected_sheets),
    function(field) {
      find_business_unit_template_sheet(
        sheet_names = sheet_names,
        expected_name = expected_sheets[[field]],
        source_name = source_name,
        required = field %in% required_sheets
      )
    }
  )
  names(resolved_names) <- names(expected_sheets)
  raw_sheets <- lapply(
    resolved_names,
    function(sheet_name) {
      if (is.na(sheet_name)) {
        NULL
      } else {
        read_business_unit_template_sheet(path, sheet_name)
      }
    }
  )

  finance_sheet <- raw_sheets$finance
  site_code <- extract_site_code("Finance", finance_sheet)

  structure(
    list(
      path = normalizePath(path, winslash = "/", mustWork = TRUE),
      source_file = source_name,
      report_year = report_year,
      report_unit = report_unit,
      campus = business_unit_template_campus(report_unit),
      site_code = site_code,
      workbook_sheets = sheet_names,
      sheet_names = resolved_names,
      sheets = raw_sheets
    ),
    class = "business_unit_template_workbook"
  )
}


load_business_unit_template_reports <- function(
  data_dir = file.path(
    "data",
    "raw",
    "Business Unit Templates that feed master reports"
  ),
  paths = NULL,
  source_names = NULL,
  strict = TRUE,
  validation_tolerance = 0.01,
  visit_validation_tolerance = 1
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
  if (
    length(visit_validation_tolerance) != 1L ||
      is.na(visit_validation_tolerance) ||
      !is.numeric(visit_validation_tolerance) ||
      visit_validation_tolerance < 0
  ) {
    stop(
      "visit_validation_tolerance must be one non-negative number.",
      call. = FALSE
    )
  }

  if (is.null(paths)) {
    paths <- discover_business_unit_template_files(data_dir)
  }
  if (length(paths) == 0L) {
    stop("No Business Unit Template paths were supplied.", call. = FALSE)
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
      read_business_unit_template_file(
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

  validation <- bind_import_tables(
    c(
      read_issues,
      lapply(workbooks, validate_business_unit_template_workbook),
      list(validate_business_unit_template_collection(workbooks))
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
        "Business Unit Template import failed:",
        paste(error_messages, collapse = "; ")
      ),
      call. = FALSE
    )
  }

  invalid_files <- unique(validation$source_file[error_rows])
  valid_workbooks <- Filter(
    function(workbook) !workbook$source_file %in% invalid_files,
    workbooks
  )
  cleaned <- lapply(
    valid_workbooks,
    clean_business_unit_template_workbook,
    validation_tolerance = validation_tolerance
  )
  finance <- bind_import_tables(
    lapply(cleaned, function(x) x$finance),
    empty_business_unit_template_finance_table()
  )
  discounts <- bind_import_tables(
    lapply(cleaned, function(x) x$discounts),
    empty_business_unit_template_discount_table()
  )
  total_revenue <- bind_import_tables(
    lapply(cleaned, function(x) x$total_revenue),
    empty_business_unit_template_total_revenue_table()
  )
  visits <- bind_import_tables(
    lapply(cleaned, function(x) x$visits),
    empty_business_unit_template_visit_table()
  )
  memberships <- bind_import_tables(
    lapply(cleaned, function(x) x$memberships),
    empty_business_unit_template_member_table()
  )
  enrollments <- bind_import_tables(
    lapply(cleaned, function(x) x$enrollments),
    empty_business_unit_template_member_table()
  )
  finance_checks <- build_business_unit_finance_checks(
    finance,
    validation_tolerance
  )
  discount_checks <- build_business_unit_discount_checks(
    discounts,
    validation_tolerance
  )
  visit_checks <- build_business_unit_visit_checks(
    visits,
    visit_validation_tolerance
  )
  membership_checks <- build_business_unit_member_checks(
    memberships,
    validation_tolerance
  )
  enrollment_checks <- build_business_unit_member_checks(
    enrollments,
    validation_tolerance
  )
  validation <- bind_import_tables(
    list(
      validation,
      validate_business_unit_check_table(finance_checks),
      validate_business_unit_check_table(discount_checks),
      validate_business_unit_check_table(visit_checks),
      validate_business_unit_check_table(membership_checks),
      validate_business_unit_check_table(enrollment_checks),
      validate_business_unit_total_revenue(total_revenue)
    ),
    empty_import_issue_table()
  )

  files <- if (length(workbooks) == 0L) {
    empty_business_unit_template_file_table()
  } else {
    data.frame(
      source_file = vapply(workbooks, function(x) x$source_file, character(1)),
      report_year = vapply(workbooks, function(x) x$report_year, integer(1)),
      report_unit = vapply(workbooks, function(x) x$report_unit, character(1)),
      campus = vapply(workbooks, function(x) x$campus, character(1)),
      site_code = vapply(
        workbooks,
        function(x) if (is.na(x$site_code)) NA_character_ else x$site_code,
        character(1)
      ),
      finance_sheet = vapply(
        workbooks,
        function(x) x$sheet_names$finance,
        character(1)
      ),
      direct_visits_sheet = vapply(
        workbooks,
        function(x) x$sheet_names$direct_visits,
        character(1)
      ),
      indirect_visits_sheet = vapply(
        workbooks,
        function(x) x$sheet_names$indirect_visits,
        character(1)
      ),
      memberships_sheet = vapply(
        workbooks,
        function(x) x$sheet_names$memberships,
        character(1)
      ),
      enrollments_sheet = vapply(
        workbooks,
        function(x) x$sheet_names$enrollments,
        character(1)
      ),
      path = vapply(workbooks, function(x) x$path, character(1)),
      stringsAsFactors = FALSE
    )
  }

  result <- structure(
    list(
      finance = finance,
      discounts = discounts,
      total_revenue = total_revenue,
      visits = visits,
      memberships = memberships,
      enrollments = enrollments,
      finance_checks = finance_checks,
      discount_checks = discount_checks,
      visit_checks = visit_checks,
      membership_checks = membership_checks,
      enrollment_checks = enrollment_checks,
      validation = validation,
      files = files
    ),
    class = "business_unit_template_report_data",
    source_sheet_order = workbook_source_sheet_order(workbooks)
  )
  finalise_extensible_report(result, valid_workbooks, "site_detail")
}


empty_business_unit_template_file_table <- function() {

  data.frame(
    source_file = character(),
    report_year = integer(),
    report_unit = character(),
    campus = character(),
    site_code = character(),
    finance_sheet = character(),
    direct_visits_sheet = character(),
    indirect_visits_sheet = character(),
    memberships_sheet = character(),
    enrollments_sheet = character(),
    path = character(),
    stringsAsFactors = FALSE
  )
}


print.business_unit_template_report_data <- function(x, ...) {

  years <- sort(unique(x$files$report_year))
  cat("Business Unit Template report data\n")
  cat("  Files:", nrow(x$files), "\n")
  cat(
    "  Years:",
    if (length(years) == 0L) "none" else paste(years, collapse = ", "),
    "\n"
  )
  cat("  Finance rows:", nrow(x$finance), "\n")
  cat("  Discount rows:", nrow(x$discounts), "\n")
  cat("  Total revenue rows:", nrow(x$total_revenue), "\n")
  cat("  Visit rows:", nrow(x$visits), "\n")
  cat("  Membership rows:", nrow(x$memberships), "\n")
  cat("  Enrollment rows:", nrow(x$enrollments), "\n")
  all_checks <- bind_import_tables(
    list(
      x$finance_checks,
      x$discount_checks,
      x$visit_checks,
      x$membership_checks,
      x$enrollment_checks
    ),
    empty_business_unit_check_table()
  )
  cat(
    "  Checks passed:",
    sum(all_checks$status == "pass"),
    "of",
    nrow(all_checks),
    "\n"
  )
  cat("  Validation issues:", nrow(x$validation), "\n")
  invisible(x)
}
