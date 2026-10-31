# Upload situations met in practice, run with the real importers on the
# synthetic workbooks in data/sample (part of the repository): duplicate
# report years, a wrong year in the file name, blank templates, unreadable
# files, saved data that overlaps the session, and a new report year.

realworld_dir <- file.path(tempdir(), "monash-upload-realworld")
dir.create(realworld_dir, showWarnings = FALSE, recursive = TRUE)

sample_file <- function(...) {
  path <- file.path("data", "sample", ...)
  stopifnot(file.exists(path))
  path
}

# Copy a sample workbook under another file name, as staff might save it.
copy_as <- function(path, name) {
  target <- file.path(realworld_dir, name)
  stopifnot(file.copy(path, target, overwrite = TRUE))
  target
}

upload_frame <- function(paths, names = basename(paths)) {
  data.frame(
    name = names,
    size = file.info(paths)$size,
    type = "application/octet-stream",
    datapath = paths,
    stringsAsFactors = FALSE
  )
}

has_error <- function(result, pattern) {
  issues <- result$validation
  any(issues$level == "error" & grepl(pattern, issues$message, fixed = TRUE))
}

monthly_2024 <- sample_file("Monthly Site Report", "Monthly Site Report 2024 -December.xlsx")
monthly_2025 <- sample_file("Monthly Site Report", "Monthly Site Report 2025 -December.xlsx")
venues_2025 <- sample_file("Business Unit Tracking Reports", "2025 Venues Tracking Report.xlsx")
aquatics_2025 <- sample_file(
  "Business Unit Templates that feed master reports", "2025", "Aquatics Site Report 2025.xlsx"
)
parsers <- basic_upload_parsers()

# 1. The year inside the workbook must match the year in the file name.
stopifnot(
  identical(basic_upload_content_year(monthly_2025, "monthly_site"), 2025L),
  identical(basic_upload_content_year(monthly_2024, "monthly_site"), 2024L),
  identical(basic_upload_content_year(venues_2025, "venues_tracking"), 2025L),
  is.na(basic_upload_content_year(aquatics_2025, "site_detail")),
  is.na(basic_upload_content_year(tempfile(fileext = ".xlsx"), "monthly_site"))
)

for (path in list.files(file.path("data", "sample"), "\\.xlsx$", recursive = TRUE, full.names = TRUE)) {
  name <- basename(path)
  type <- classify_report_upload(name)
  if (type %in% c("monthly_site", "venues_tracking")) {
    content <- basic_upload_content_year(path, type)
    named <- as.integer(regmatches(name, regexpr("(19|20)[0-9]{2}", name)))
    if (!identical(content, named)) {
      stop("Content year check disagrees with the file name for ", name, call. = FALSE)
    }
  }
}

renamed_monthly <- parsers$monthly_site(
  copy_as(monthly_2024, "Monthly Site Report 2026 -December.xlsx"),
  "Monthly Site Report 2026 -December.xlsx"
)
renamed_venues <- parsers$venues_tracking(
  copy_as(venues_2025, "2023 Venues Tracking Report.xlsx"),
  "2023 Venues Tracking Report.xlsx"
)
correct_monthly <- parsers$monthly_site(monthly_2025, basename(monthly_2025))
stopifnot(
  has_error(renamed_monthly, "The file name says 2026, but the workbook holds 2024 figures"),
  has_error(renamed_venues, "The file name says 2023, but the workbook holds 2025 figures"),
  !any(correct_monthly$validation$level == "error")
)

# 2. Blank templates are refused instead of adding an empty year.
templates <- c(
  monthly_site = "Monthly Site Report 2026.xlsx",
  site_detail = "New Sport Site Report 2026.xlsx",
  venues_tracking = "2026 Venues Tracking Report.xlsx"
)
for (type in names(templates)) {
  result <- parsers[[type]](file.path("www", "templates", templates[[type]]), templates[[type]])
  if (!has_error(result, "no figures yet")) {
    stop("A blank ", templates[[type]], " was not refused.", call. = FALSE)
  }
}
stopifnot(
  basic_upload_reported_values(correct_monthly, "monthly_site") > 0L,
  !has_error(correct_monthly, "no figures yet")
)

# 3. Uploads in the dashboard: duplicates, replacements, unreadable files.
shiny::testServer(
  mod_data_upload_server,
  {
    session$flushReact()
    upload <- function(input_id, paths, names = basename(paths)) {
      do.call(session$setInputs, stats::setNames(list(upload_frame(paths, names)), input_id))
      session$flushReact()
      session$returned()
    }
    row_for <- function(data, name) data$uploads[data$uploads$original_name == name, , drop = FALSE]

    first <- upload("monthly_reports", monthly_2025)
    single_rows <- nrow(first$monthly_reports$finance)
    stopifnot(row_for(first, basename(monthly_2025))$status == "ready")

    # Another file for the same year is not used, so nothing is counted twice.
    november <- copy_as(monthly_2025, "Monthly Site Report 2025 -November.xlsx")
    second <- upload("monthly_reports", november)
    rejected <- row_for(second, "Monthly Site Report 2025 -November.xlsx")
    stopifnot(
      rejected$status == "failed",
      grepl("Monthly Site Reports for 2025 are already loaded", rejected$error_message, fixed = TRUE),
      grepl("Monthly Site Report 2025 -December.xlsx", rejected$error_message, fixed = TRUE),
      nrow(second$monthly_reports$finance) == single_rows
    )

    # The same file name still replaces the earlier upload.
    replaced <- upload("monthly_reports", monthly_2025)
    stopifnot(
      sum(replaced$uploads$status == "ready") == 1L,
      nrow(replaced$monthly_reports$finance) == single_rows
    )

    # Once the first file is removed, the other one can be used.
    session$setInputs(remove_file = row_for(replaced, basename(monthly_2025))$file_id)
    session$flushReact()
    after_switch <- upload("monthly_reports", november)
    stopifnot(
      row_for(after_switch, "Monthly Site Report 2025 -November.xlsx")$status[[
        nrow(row_for(after_switch, "Monthly Site Report 2025 -November.xlsx"))
      ]] == "ready",
      nrow(after_switch$monthly_reports$finance) == single_rows
    )

    # Venues and Detailed Site Reports follow the same rule.
    upload("venues_reports", venues_2025)
    venues_copy <- upload(
      "venues_reports",
      copy_as(venues_2025, "2025 Venues Tracking Report (2).xlsx")
    )
    upload("site_reports", aquatics_2025)
    site_copy <- upload(
      "site_reports",
      copy_as(aquatics_2025, "Aquatics_Site_Report_2025.xlsx")
    )
    other_unit <- upload(
      "site_reports",
      sample_file("Business Unit Templates that feed master reports", "2025", "Caulfield Site Report 2025.xlsx")
    )
    stopifnot(
      row_for(venues_copy, "2025 Venues Tracking Report (2).xlsx")$status == "failed",
      row_for(site_copy, "Aquatics_Site_Report_2025.xlsx")$status == "failed",
      grepl("Detailed Site Reports for Aquatics 2025",
        row_for(site_copy, "Aquatics_Site_Report_2025.xlsx")$error_message, fixed = TRUE),
      row_for(other_unit, "Caulfield Site Report 2025.xlsx")$status == "ready"
    )

    # Unreadable and empty workbooks fail with a message, not a crash.
    broken <- file.path(realworld_dir, "Monthly Site Report 2027.xlsx")
    writeLines("not an Excel workbook", broken)
    empty <- file.path(realworld_dir, "Monthly Site Report 2028.xlsx")
    writexl::write_xlsx(list(Sheet1 = data.frame()), empty)
    bad <- upload("monthly_reports", c(broken, empty))
    for (name in basename(c(broken, empty))) {
      row <- row_for(bad, name)
      stopifnot(row$status == "failed", nzchar(row$error_message))
    }
  }
)

# Upload all reports with two files for one year: the first is used.
shiny::testServer(
  mod_data_upload_server,
  {
    session$flushReact()
    paths <- c(monthly_2025, copy_as(monthly_2025, "Monthly Site Report 2025 -November.xlsx"))
    session$setInputs(all_reports = upload_frame(paths))
    session$flushReact()
    uploads <- session$returned()$uploads
    stopifnot(
      nrow(uploads) == 2L,
      sum(uploads$status == "ready") == 1L,
      sum(uploads$status == "failed") == 1L,
      nrow(session$returned()$monthly_reports$finance) == nrow(correct_monthly$finance)
    )
  }
)

# 4. Saved data that overlaps the session under another name is not used.
realworld_saved <- file.path(realworld_dir, "saved.rds")
shiny::testServer(
  mod_data_upload_server,
  {
    session$flushReact()
    session$setInputs(monthly_reports = upload_frame(monthly_2025))
    session$flushReact()
    saveRDS(saved_data_payload(), realworld_saved)
  }
)
shiny::testServer(
  mod_data_upload_server,
  {
    session$flushReact()
    november <- copy_as(monthly_2025, "Monthly Site Report 2025 -November.xlsx")
    session$setInputs(monthly_reports = upload_frame(november))
    session$flushReact()
    rows_before <- nrow(session$returned()$monthly_reports$finance)
    session$setInputs(saved_data = upload_frame(realworld_saved, "saved.rds"))
    session$flushReact()
    uploads <- session$returned()$uploads
    notice <- paste(as.character(output$saved_data_notice), collapse = "")
    saved_row <- uploads[uploads$original_name == basename(monthly_2025), , drop = FALSE]
    stopifnot(
      saved_row$status == "failed",
      grepl("already loaded", saved_row$error_message, fixed = TRUE),
      nrow(session$returned()$monthly_reports$finance) == rows_before,
      grepl("Not used, because another file in this session", notice, fixed = TRUE)
    )
  }
)

# 5. A new report year (2026) reaches the year choices of the pages that
# read their years from the data.
shift_report_year <- function(report, from, to) {
  for (name in names(report)) {
    table <- report[[name]]
    if (!is.data.frame(table) || !"report_year" %in% names(table)) next
    rows <- !is.na(table$report_year) & table$report_year == from
    table$report_year[rows] <- to
    for (column in names(table)) {
      if (inherits(table[[column]], "Date")) {
        dated <- rows & !is.na(table[[column]])
        table[[column]][dated] <- as.Date(paste0(
          to, substr(format(table[[column]][dated]), 5L, 10L)
        ))
      }
    }
    report[[name]] <- table
  }
  report
}
monthly_2026 <- shift_report_year(correct_monthly, 2025L, 2026L)
monthly_two_years <- combine_basic_upload_results(
  list(correct_monthly, monthly_2026),
  "monthly_site_report_data"
)
new_year_data <- shiny::reactiveVal(list(
  monthly_reports = monthly_two_years,
  monthly = monthly_two_years
))
shiny::testServer(
  mod_overview_server,
  args = list(data = new_year_data),
  {
    session$flushReact()
    stopifnot(identical(available_years(), c(2025L, 2026L)))
  }
)
shiny::testServer(
  mod_site_comparison_server,
  args = list(data = new_year_data),
  {
    session$flushReact()
    stopifnot(2026L %in% available_years())
  }
)

unlink(realworld_dir, recursive = TRUE)
cat("PASS test_upload_realworld.R\n")
