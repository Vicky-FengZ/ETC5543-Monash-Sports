# Upload identity and saved-data integrity must hold even when a file's
# name, size and outer R class all look unchanged.
(function() {
  empty_definitions <- import_metric_definition("site_detail", "memberships", character())
  stopifnot(is.character(empty_definitions$unit), is.character(empty_definitions$aggregation))
  paths <- c(
    monthly_site = "data/sample/Monthly Site Report/Monthly Site Report 2025 -December.xlsx",
    site_detail = "data/sample/Business Unit Templates that feed master reports/2025/Aquatics Site Report 2025.xlsx",
    venues_tracking = "data/sample/Business Unit Tracking Reports/2025 Venues Tracking Report.xlsx"
  )
  parsers <- basic_upload_parsers()
  reports <- lapply(names(paths), function(type) parsers[[type]](paths[[type]], basename(paths[[type]])))
  names(reports) <- names(paths)
  rows <- data.frame(
    file_id = names(paths), report_type = names(paths), original_name = basename(paths),
    stored_path = unname(paths), size = unname(file.info(paths)$size),
    content_hash = basic_upload_content_hash(paths), status = "ready", progress = 1,
    detail = "Parsed successfully",
    warning_count = vapply(reports, function(report) basic_upload_result_issues(report)$warnings, integer(1L)),
    error_message = NA_character_, stringsAsFactors = FALSE
  )
  warnings <- lapply(reports, function(report) {
    notes <- basic_upload_warning_rows(report)
    if (nrow(notes)) notes else NULL
  })
  payload <- build_basic_saved_data(rows, reports, warnings)
  scratch <- tempfile("upload-integrity-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  saved_path <- file.path(scratch, "saved.rds")
  saveRDS(payload, saved_path)
  restored <- read_basic_saved_data(saved_path)
  stopifnot(identical(restored$results, payload$results),
    identical(restored$files$content_hash, rows$content_hash))

  refuse <- function(changed) {
    path <- file.path(scratch, "damaged.rds")
    saveRDS(changed, path)
    error <- tryCatch({ read_basic_saved_data(path); NULL }, error = identity)
    stopifnot(inherits(error, "error"), grepl("incomplete or damaged", conditionMessage(error), fixed = TRUE))
  }
  for (index in seq_along(reports)) {
    damaged <- payload
    damaged$results[[index]]$finance <- NULL
    refuse(damaged)
    damaged <- payload
    damaged$results[[index]]$finance$value <- NULL
    refuse(damaged)
    damaged <- payload
    damaged$results[[index]]$finance$value <- as.character(damaged$results[[index]]$finance$value)
    refuse(damaged)
    damaged <- payload
    damaged$results[[index]]$finance$value[[1L]] <- Inf
    refuse(damaged)
    damaged <- payload
    damaged$results[[index]]$finance$date <- as.character(damaged$results[[index]]$finance$date)
    refuse(damaged)
    damaged <- payload
    damaged$results[[index]]$validation <- rbind(damaged$results[[index]]$validation,
      make_import_issue("error", rows$original_name[[index]], "Must not become ready"))
    refuse(damaged)
  }
  for (field in c("size", "warning_count")) {
    damaged <- payload
    damaged$files[[field]][[1L]] <- NA_real_
    refuse(damaged)
  }
  damaged <- payload
  damaged$saved_at <- as.POSIXct(NA)
  refuse(damaged)
  damaged <- payload
  damaged$files$years[[1L]] <- "2099"
  refuse(damaged)
  damaged <- payload
  damaged$warnings[[1L]] <- data.frame(message = "Missing worksheet and impact")
  refuse(damaged)
  damaged <- payload
  damaged$files$content_hash[[1L]] <- "not a fingerprint"
  refuse(damaged)
  damaged <- payload
  damaged$files <- damaged$files[FALSE, ]
  damaged$results <- list()
  damaged$warnings <- list()
  refuse(damaged)
  damaged <- payload
  duplicate <- damaged$results[[1L]]
  for (name in names(duplicate)) {
    if (is.data.frame(duplicate[[name]]) && "source_file" %in% names(duplicate[[name]])) {
      duplicate[[name]]$source_file[] <- "Monthly Site Report 2025 -November.xlsx"
    }
  }
  damaged$files <- rbind(damaged$files, damaged$files[1L, ])
  damaged$files$original_name[[4L]] <- "Monthly Site Report 2025 -November.xlsx"
  damaged$results[[4L]] <- duplicate
  damaged$warnings[4L] <- payload$warnings[1L]
  refuse(damaged)
  cat("PASS saved-data contracts for all report families and invalid metadata/overlap\n")

  count <- 0L
  mock_parsers <- list(monthly_site = function(path, source_name) {
    count <<- count + 1L
    bytes <- rawToChar(readBin(path, "raw", n = file.info(path)$size))
    if (bytes == "bad") stop("Replacement failed")
    result <- reports$monthly_site
    result$finance$value[[1L]] <- as.numeric(bytes)
    result
  })
  upload <- function(path, name = rows$original_name[[1L]]) {
    data.frame(name = name, size = file.info(path)$size,
      type = "application/octet-stream", datapath = path, stringsAsFactors = FALSE)
  }
  first <- file.path(scratch, "first.xlsx")
  updated <- file.path(scratch, "updated.xlsx")
  failed <- file.path(scratch, "failed.xlsx")
  writeBin(charToRaw("100"), first)
  writeBin(charToRaw("900"), updated)
  writeBin(charToRaw("bad"), failed)
  stopifnot(file.info(first)$size == file.info(updated)$size)
  shiny::testServer(mod_data_upload_server, args = list(parsers = mock_parsers), {
    session$flushReact()
    choose <- function(path, name = rows$original_name[[1L]]) {
      session$setInputs(all_reports = upload(path, name))
      session$flushReact()
    }
    choose(first)
    stopifnot(count == 1L, session$returned()$monthly_reports$finance$value[[1L]] == 100)
    choose(first)
    stopifnot(count == 1L)
    choose(updated)
    stopifnot(count == 2L, session$returned()$monthly_reports$finance$value[[1L]] == 900)
    choose(failed)
    stopifnot(count == 3L, session$returned()$monthly_reports$finance$value[[1L]] == 900)
    ready_payload <- saved_data_payload()
    before <- session$returned()$uploads
    corrupt <- ready_payload
    corrupt$results[[1L]]$finance <- NULL
    path <- file.path(scratch, "corrupt.rds")
    saveRDS(corrupt, path)
    choose(path, "corrupt.rds")
    stopifnot(identical(session$returned()$uploads, before),
      session$returned()$monthly_reports$finance$value[[1L]] == 900,
      grepl("was not loaded", paste(as.character(output$saved_data_notice), collapse = ""), fixed = TRUE))

    path <- file.path(scratch, "current.rds")
    saveRDS(ready_payload, path)
    choose(path, "current.rds")
    choose(updated)
    stopifnot(count == 3L)
    legacy <- ready_payload
    legacy$files$content_hash <- NULL
    saveRDS(legacy, path)
    choose(path, "legacy.rds")
    stopifnot(all(is.na(session$returned()$uploads$content_hash)))
    choose(updated)
    stopifnot(count == 4L)
    choose(updated)
    stopifnot(count == 4L)
  })
  cat("PASS same-size edits, failed replacements, atomic invalid restores and old-save fingerprints\n")

  export_count <- 0L
  original_export <- basic_cleaned_export_sheets
  assign("basic_cleaned_export_sheets", function(...) {
    export_count <<- export_count + 1L
    original_export(...)
  }, envir = globalenv())
  on.exit(assign("basic_cleaned_export_sheets", original_export, envir = globalenv()), add = TRUE)
  shiny::testServer(mod_data_upload_server, args = list(parsers = mock_parsers), {
    session$flushReact()
    session$setInputs(all_reports = upload(first), preview_report_type = "monthly_site",
      preview_search = "", preview_year = character(), preview_measure = character(),
      preview_value_type = character(), preview_sort_column = "__none__", preview_sort_direction = "asc")
    session$flushReact()
    stopifnot(grepl("Download Excel", paste(as.character(output$preview_download_ui), collapse = ""), fixed = TRUE),
      export_count == 0L, length(preview_export_sheets()) > 0L, export_count == 1L)
    session$setInputs(preview_search = "no-row-can-match-this-search")
    session$flushReact()
    stopifnot(grepl("disabled", paste(as.character(output$preview_download_ui), collapse = ""), fixed = TRUE),
      export_count == 1L)
    session$setInputs(preview_search = "")
    session$flushReact()
    stopifnot(grepl("download_cleaned_data", paste(as.character(output$preview_download_ui), collapse = ""), fixed = TRUE),
      export_count == 1L)
  })
  cat("PASS cleaned-data export is built only when requested\n")
})()
