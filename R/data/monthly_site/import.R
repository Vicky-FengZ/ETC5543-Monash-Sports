# -------------------------------------------------------------------------
# Monthly Site Report import
# -------------------------------------------------------------------------
# This file only deals with locating and reading Excel workbooks. Validation
# and conversion to dashboard-ready tables live in the next two data files.

require_monthly_report_packages <- function() {

  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop(
      paste(
        "The readxl package is required to import Monthly Site Reports.",
        "Install it with install.packages('readxl')."
      ),
      call. = FALSE
    )
  }
}


discover_monthly_site_report_files <- function(
  data_dir = file.path(
    "data",
    "raw",
    "Monthly Site Report"
  )
) {

  if (!dir.exists(data_dir)) {
    stop(
      paste0(
        "Monthly Site Report directory does not exist: ",
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
      "^Monthly[[:space:]_-]*Site[[:space:]_-]*Report",
      file_names,
      ignore.case = TRUE
    )

  files <- files[keep]

  if (length(files) == 0L) {
    stop(
      paste0(
        "No Monthly Site Report Excel files were found in: ",
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


extract_monthly_site_report_year <- function(source_name) {

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


read_monthly_site_report_file <- function(
  path,
  source_name = basename(path)
) {

  require_monthly_report_packages()

  if (!file.exists(path)) {
    stop(
      paste0("Monthly Site Report file does not exist: ", path),
      call. = FALSE
    )
  }

  extension <- tolower(tools::file_ext(path))

  if (!extension %in% c("xlsx", "xls")) {
    stop(
      paste0("Unsupported Monthly Site Report file type: .", extension),
      call. = FALSE
    )
  }

  report_year <- extract_monthly_site_report_year(source_name)
  sheet_names <- readxl::excel_sheets(path)

  # DASHBOARD contains presentation cells rather than the source finance
  # blocks. All other sheets are retained here and checked during validation.
  import_sheets <- sheet_names[
    normalise_excel_label(sheet_names) != "dashboard"
  ]

  sheet_results <- lapply(
    import_sheets,
    function(sheet_name) {

      tryCatch(
        {
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

          list(
            sheet = raw_sheet,
            issue = NULL
          )
        },
        error = function(error) {
          list(
            sheet = NULL,
            issue = data.frame(
              level = "warning",
              source_file = source_name,
              sheet = sheet_name,
              message = paste0(
                "Sheet could not be read as tabular data and was skipped: ",
                conditionMessage(error)
              ),
              stringsAsFactors = FALSE
            )
          )
        }
      )
    }
  )

  readable <- vapply(
    sheet_results,
    function(result) !is.null(result$sheet),
    logical(1)
  )

  sheets <- lapply(
    sheet_results[readable],
    function(result) result$sheet
  )

  names(sheets) <- import_sheets[readable]

  sheet_read_issues <- bind_import_tables(
    lapply(
      sheet_results,
      function(result) result$issue
    ),
    empty_import_issue_table()
  )

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
      sheets = sheets,
      sheet_read_issues = sheet_read_issues
    ),
    class = "monthly_site_report_workbook"
  )
}


load_monthly_site_reports <- function(
  data_dir = file.path(
    "data",
    "raw",
    "Monthly Site Report"
  ),
  paths = NULL,
  source_names = NULL,
  strict = TRUE,
  revenue_validation_tolerance = 0.01
) {

  if (
    length(revenue_validation_tolerance) != 1L ||
      is.na(revenue_validation_tolerance) ||
      !is.numeric(revenue_validation_tolerance) ||
      revenue_validation_tolerance < 0
  ) {
    stop(
      "revenue_validation_tolerance must be one non-negative number.",
      call. = FALSE
    )
  }

  if (is.null(paths)) {
    paths <- discover_monthly_site_report_files(data_dir)
  }

  if (length(paths) == 0L) {
    stop("No Monthly Site Report paths were supplied.", call. = FALSE)
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
      read_monthly_site_report_file(
        path = paths[[index]],
        source_name = source_names[[index]]
      ),
      error = function(error) {

        read_issues[[length(read_issues) + 1L]] <<- data.frame(
          level = "error",
          source_file = source_names[[index]],
          sheet = NA_character_,
          message = conditionMessage(error),
          stringsAsFactors = FALSE
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
    validate_monthly_site_report_workbook
  )

  validation <- bind_import_tables(
    c(
      read_issues,
      workbook_issues,
      list(validate_monthly_site_report_collection(workbooks))
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
        "Monthly Site Report import failed:",
        paste(
          error_messages,
          collapse = "; "
        )
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

  cleaned_reports <- lapply(
    valid_workbooks,
    function(workbook) {

      finance <- clean_monthly_site_report_finance(workbook)
      discounts <- clean_monthly_site_report_discounts(workbook)
      total_revenue <- clean_monthly_site_report_total_revenue(
        workbook = workbook,
        finance = finance,
        discounts = discounts,
        validation_tolerance = revenue_validation_tolerance
      )

      list(
        finance = finance,
        discounts = discounts,
        total_revenue = total_revenue
      )
    }
  )

  finance <- bind_import_tables(
    lapply(cleaned_reports, function(x) x$finance),
    empty_monthly_finance_table()
  )

  discounts <- bind_import_tables(
    lapply(cleaned_reports, function(x) x$discounts),
    empty_monthly_discount_table()
  )

  total_revenue <- bind_import_tables(
    lapply(cleaned_reports, function(x) x$total_revenue),
    empty_monthly_total_revenue_table()
  )

  validation <- bind_import_tables(
    list(
      validation,
      validate_monthly_total_revenue_table(total_revenue)
    ),
    empty_import_issue_table()
  )

  file_summary <- if (length(workbooks) == 0L) {
    data.frame(
      source_file = character(),
      report_year = integer(),
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
      discounts = discounts,
      total_revenue = total_revenue,
      validation = validation,
      files = file_summary
    ),
    class = "monthly_site_report_data"
  )
}


print.monthly_site_report_data <- function(x, ...) {

  years <- sort(unique(x$files$report_year))

  cat("Monthly Site Report data\n")
  cat("  Files:", nrow(x$files), "\n")
  cat(
    "  Years:",
    if (length(years) == 0L) "none" else paste(years, collapse = ", "),
    "\n"
  )
  cat("  Finance rows:", nrow(x$finance), "\n")
  cat("  Discount rows:", nrow(x$discounts), "\n")
  cat("  Total Revenue rows:", nrow(x$total_revenue), "\n")
  cat(
    "  Total Revenue checks passed:",
    sum(x$total_revenue$validation_status == "pass"),
    "of",
    nrow(x$total_revenue),
    "\n"
  )
  cat("  Validation issues:", nrow(x$validation), "\n")

  invisible(x)
}
