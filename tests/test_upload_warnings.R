suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(bsicons)
})

# Count only validation warnings, preserve messages, and tolerate missing
# worksheet/message metadata without losing a warning or changing the parser.
warning_fixture <- list(validation = data.frame(
  level = c("warning", "WARNING", "warning", "info", "error", NA),
  sheet = c(" Finance ", NA, "", "Other", "Other", "Other"),
  message = c("Original note", "File note", NA, "Not a warning", "Blocking", "Other"),
  stringsAsFactors = FALSE
))
warning_rows <- basic_upload_warning_rows(warning_fixture)
stopifnot(
  nrow(warning_rows) == basic_upload_result_issues(warning_fixture)$warnings,
  identical(warning_rows$sheet, c("Finance", "File-level notes", "File-level notes")),
  identical(warning_rows$message[1:2], c("Original note", "File note")),
  !anyNA(warning_rows$impact),
  nzchar(warning_rows$message[[3L]]),
  nrow(basic_upload_warning_rows(list())) == 0L,
  nrow(basic_upload_warning_rows(list(validation = data.frame()))) == 0L,
  nrow(basic_upload_warning_rows(list(validation = data.frame(level = "info")))) == 0L,
  nrow(basic_upload_warning_rows(list(validation = data.frame(level = "warning")))) == 1L,
  is.null(basic_upload_warning_details_ui(NULL, "empty.xlsx")),
  is.null(basic_upload_warning_details_ui(warning_rows[FALSE, ], "empty.xlsx")),
  grepl("Calculated and reported figures differ", basic_upload_warning_impact(
    "Total Revenue validation failed for 1 business-unit row(s)."
  ), fixed = TRUE),
  grepl("Some checks could not be completed", basic_upload_warning_impact(
    "Venues finance validation was incomplete for 2 check(s)."
  ), fixed = TRUE),
  grepl("Coverage may be incomplete", basic_upload_warning_impact(
    "The Casual Hours summary is missing month(s): Jan."
  ), fixed = TRUE),
  grepl("source workbook", basic_upload_warning_impact("An unfamiliar note."), fixed = TRUE),
  grepl("This part of the worksheet", basic_upload_warning_impact(
    "The Casual Wages sheet has overlapping blocks; extraction was skipped to avoid double counting."
  ), fixed = TRUE),
  grepl("This worksheet was skipped", basic_upload_warning_impact(
    "Sheet could not be read as tabular data and was skipped: unreadable."
  ), fixed = TRUE)
)

warning_ui <- as.character(basic_upload_warning_details_ui(warning_rows, "test.xlsx"))
stopifnot(
  grepl("3 warnings", warning_ui, fixed = TRUE),
  grepl("View details", warning_ui, fixed = TRUE),
  grepl("Original note", warning_ui, fixed = TRUE),
  grepl("File-level notes", warning_ui, fixed = TRUE),
  !grepl("open=", warning_ui, fixed = TRUE),
  !grepl("Blocking", warning_ui, fixed = TRUE)
)
cat("PASS warning normalization, guidance and collapsed grouping\n")

make_warning_upload_result <- function(source_name, result_class, payload) {
  validation <- data.frame(
    level = c("warning", "warning", "warning", "info"),
    source_file = source_name,
    sheet = c("Finance & <Summary>", "Finance & <Summary>", NA, "Ignored"),
    message = c(
      paste("Original", payload, "<img src=x onerror=alert(1)>"),
      "The DISCOUNTS section was not found. Finance data will still be imported.",
      "Total Revenue validation failed for 1 business-unit row(s).",
      "Information only"
    ),
    stringsAsFactors = FALSE
  )
  if (payload == "clean") {
    validation <- validation[FALSE, ]
  }
  if (payload == "blocked") {
    validation <- rbind(validation, data.frame(
      level = "error", source_file = source_name, sheet = "Finance",
      message = "A blocking validation error", stringsAsFactors = FALSE
    ))
  }
  structure(list(
    finance = data.frame(
      # The clean file is another year: two files for one year are refused.
      report_year = if (payload == "clean") 2024L else 2025L,
      site = "Clayton", source_sheet = "Finance",
      source_file = source_name, metric = "Income", value_type = "Actual", value = 100,
      stringsAsFactors = FALSE
    ),
    validation = validation,
    files = data.frame(source_file = source_name, stringsAsFactors = FALSE)
  ), class = result_class)
}

warning_parsers <- lapply(c(
  monthly_site = "monthly_site_report_data",
  site_detail = "business_unit_template_report_data",
  venues_tracking = "venues_tracking_report_data"
), function(result_class) {
  force(result_class)
  function(path, source_name) {
    make_warning_upload_result(source_name, result_class, readLines(path, warn = FALSE)[[1L]])
  }
})

shiny::testServer(mod_data_upload_server, args = list(parsers = warning_parsers), {
  session$flushReact()
  upload_path <- tempfile(fileext = ".xlsx")
  upload_info <- function(name, payload) {
    writeLines(payload, upload_path)
    data.frame(name = name, size = file.info(upload_path)$size,
      type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      datapath = upload_path, stringsAsFactors = FALSE)
  }
  for (family in names(warning_parsers)) {
    process_upload_batch(family, upload_info(paste0(family, ".xlsx"), family))
  }
  process_upload_batch("monthly_site", upload_info("clean.xlsx", "clean"))
  session$flushReact()
  rows <- registry()
  notes <- warning_details()
  stopifnot(
    nrow(rows) == 4L,
    all(rows$status == "ready"),
    identical(rows$warning_count, c(3L, 3L, 3L, 0L)),
    length(notes) == 3L
  )
  for (family in names(warning_parsers)) {
    file_id <- rows$file_id[rows$original_name == paste0(family, ".xlsx")]
    stopifnot(
      nrow(notes[[file_id]]) == 3L,
      grepl(paste("Original", family), notes[[file_id]]$message[[1L]], fixed = TRUE)
    )
  }
  status_html <- output$file_status$html
  stopifnot(
    grepl("basic-status-badge ready", status_html, fixed = TRUE),
    !grepl("READY WITH WARNINGS", status_html, fixed = TRUE),
    grepl("&lt;img src=x onerror=alert(1)&gt;", status_html, fixed = TRUE),
    grepl("Finance &amp; &lt;Summary&gt;", status_html, fixed = TRUE),
    !grepl("<img src=x", status_html, fixed = TRUE),
    !grepl("Information only", status_html, fixed = TRUE),
    length(gregexpr('class="basic-upload-warning-details"', status_html, fixed = TRUE)[[1L]]) == 3L,
    length(gregexpr('class="basic-upload-warning-group-heading"', status_html, fixed = TRUE)[[1L]]) == 6L
  )

  # Failed validation retains its own notes, but not analysis data, and cannot
  # replace the earlier ready file with the same family and filename.
  old_id <- rows$file_id[rows$original_name == "monthly_site.xlsx"]
  process_upload_batch("monthly_site", upload_info("monthly_site.xlsx", "blocked"))
  session$flushReact()
  blocked_id <- registry()$file_id[registry()$status == "failed"]
  stopifnot(
    length(blocked_id) == 1L,
    old_id %in% names(parsed_results()),
    !blocked_id %in% names(parsed_results()),
    nrow(warning_details()[[blocked_id]]) == 3L,
    grepl("Original blocked", output$file_status$html, fixed = TRUE)
  )
  session$setInputs(remove_file = blocked_id)
  session$flushReact()
  stopifnot(!blocked_id %in% names(warning_details()))

  # Successful replacement updates notes atomically with the uploaded data.
  process_upload_batch("monthly_site", upload_info("monthly_site.xlsx", "replacement"))
  session$flushReact()
  new_id <- registry()$file_id[registry()$original_name == "monthly_site.xlsx"]
  stopifnot(
    length(new_id) == 1L,
    new_id != old_id,
    !old_id %in% names(warning_details()),
    !old_id %in% names(parsed_results()),
    grepl("Original replacement", warning_details()[[new_id]]$message[[1L]], fixed = TRUE),
    !grepl("Original monthly_site", output$file_status$html, fixed = TRUE)
  )

  session$setInputs(remove_file = new_id)
  session$flushReact()
  stopifnot(
    !new_id %in% names(warning_details()),
    !grepl("Original replacement", output$file_status$html, fixed = TRUE),
    grepl("Original site_detail", output$file_status$html, fixed = TRUE),
    grepl("Original venues_tracking", output$file_status$html, fixed = TRUE)
  )
  remove_session_files(registry()$file_id)
  session$flushReact()
  stopifnot(length(warning_details()) == 0L, length(parsed_results()) == 0L,
    grepl("No files have been uploaded", output$file_status$html, fixed = TRUE))
  unlink(upload_path)
})
cat("PASS all upload families: READY status, escaped details, delete and safe replacement\n")
