suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(bsicons)
})

source(file.path("R", "mod_data_upload.R"))

ui_html <- as.character(mod_data_upload_ui("basic-upload-test"))
stopifnot(
  lengths(regmatches(
    ui_html,
    gregexpr("basic-upload-source-card", ui_html, fixed = TRUE)
  )) == 3L,
  grepl("File parsing status", ui_html, fixed = TRUE),
  grepl("Delete selected file", ui_html, fixed = TRUE),
  grepl("Session-only upload", ui_html, fixed = TRUE)
)

empty_registry <- empty_basic_upload_registry()
stopifnot(
  is.data.frame(empty_registry),
  nrow(empty_registry) == 0L,
  all(c("file_id", "status", "progress", "error_message") %in% names(empty_registry))
)

make_fake_result <- function(source_name, result_class, warning = FALSE) {
  validation <- if (warning) {
    data.frame(
      level = "warning",
      source_file = source_name,
      sheet = NA_character_,
      message = "Synthetic warning",
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      level = character(),
      source_file = character(),
      sheet = character(),
      message = character(),
      stringsAsFactors = FALSE
    )
  }

  structure(
    list(
      values = data.frame(
        source_file = source_name,
        value = 1,
        stringsAsFactors = FALSE
      ),
      validation = validation,
      files = data.frame(
        source_file = source_name,
        stringsAsFactors = FALSE
      )
    ),
    class = result_class
  )
}

combined <- combine_basic_upload_results(
  list(
    make_fake_result("one.xlsx", "monthly_site_report_data"),
    make_fake_result("two.xlsx", "monthly_site_report_data")
  ),
  "monthly_site_report_data"
)
stopifnot(
  inherits(combined, "monthly_site_report_data"),
  nrow(combined$values) == 2L,
  identical(combined$values$source_file, c("one.xlsx", "two.xlsx"))
)

fake_parsers <- list(
  monthly_site = function(path, source_name) {
    stopifnot(file.exists(path))
    make_fake_result(source_name, "monthly_site_report_data", warning = TRUE)
  },
  site_detail = function(path, source_name) {
    stop("Synthetic parser failure")
  },
  venues_tracking = function(path, source_name) {
    make_fake_result(source_name, "venues_tracking_report_data")
  }
)

shiny::testServer(
  mod_data_upload_server,
  args = list(parsers = fake_parsers),
  {
    session$flushReact()

    upload_path <- tempfile(fileext = ".xlsx")
    writeBin(charToRaw("synthetic upload"), upload_path)

    session$setInputs(
      monthly_reports = data.frame(
        name = "Monthly Site Report 2099.xlsx",
        size = file.info(upload_path)$size,
        type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
        datapath = upload_path,
        stringsAsFactors = FALSE
      )
    )
    session$flushReact()

    returned_data <- session$returned()
    stopifnot(
      nrow(returned_data$uploads) == 1L,
      returned_data$uploads$status[[1L]] == "ready",
      returned_data$uploads$progress[[1L]] == 1,
      returned_data$uploads$warning_count[[1L]] == 1L,
      inherits(returned_data$monthly_reports, "monthly_site_report_data")
    )

    stored_path <- returned_data$uploads$stored_path[[1L]]
    file_id <- returned_data$uploads$file_id[[1L]]
    stopifnot(file.exists(stored_path))

    session$setInputs(selected_file = file_id, delete_file = 1)
    session$flushReact()
    after_delete <- session$returned()
    stopifnot(
      nrow(after_delete$uploads) == 0L,
      is.null(after_delete$monthly_reports),
      !file.exists(stored_path)
    )

    failed_path <- tempfile(fileext = ".xlsx")
    writeBin(charToRaw("synthetic failed upload"), failed_path)
    session$setInputs(
      site_reports = data.frame(
        name = "Synthetic Site Report 2099.xlsx",
        size = file.info(failed_path)$size,
        type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
        datapath = failed_path,
        stringsAsFactors = FALSE
      )
    )
    session$flushReact()

    failed_data <- session$returned()
    stopifnot(
      nrow(failed_data$uploads) == 1L,
      failed_data$uploads$status[[1L]] == "failed",
      grepl("Synthetic parser failure", failed_data$uploads$error_message[[1L]], fixed = TRUE),
      is.null(failed_data$site_reports)
    )

    unlink(c(upload_path, failed_path))
  }
)

cat("PASS test_basic_upload.R\n")
