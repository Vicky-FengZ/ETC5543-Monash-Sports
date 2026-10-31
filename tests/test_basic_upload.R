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
  grepl("data-section-navigation", ui_html, fixed = TRUE),
  grepl("Upload &amp; Manage", ui_html, fixed = TRUE),
  grepl("View Cleaned Data", ui_html, fixed = TRUE),
  grepl("basic-upload-test-preview_report_type", ui_html, fixed = TRUE),
  !grepl("basic-upload-test-preview_table_control", ui_html, fixed = TRUE),
  grepl("basic-upload-test-preview_measure", ui_html, fixed = TRUE),
  grepl("basic-upload-test-preview_value_type", ui_html, fixed = TRUE),
  grepl("basic-upload-test-preview_source_sheet", ui_html, fixed = TRUE),
  grepl("basic-upload-test-preview_sort_column", ui_html, fixed = TRUE),
  grepl("basic-upload-test-preview_download_ui", ui_html, fixed = TRUE),
  grepl("basic-upload-test-cleaned_data_table", ui_html, fixed = TRUE),
  grepl("Uploaded files", ui_html, fixed = TRUE),
  !grepl("basic-upload-test-selected_file", ui_html, fixed = TRUE),
  !grepl("basic-upload-test-delete_file", ui_html, fixed = TRUE),
  grepl("Session-only upload", ui_html, fixed = TRUE)
)

empty_registry <- empty_basic_upload_registry()
stopifnot(
  is.data.frame(empty_registry),
  nrow(empty_registry) == 0L,
  all(c("file_id", "status", "progress", "error_message") %in% names(empty_registry))
)

make_fake_result <- function(
  source_name,
  result_class,
  warning = FALSE,
  row_count = 1L
) {
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
      finance = data.frame(
        report_year = rep(c(2098L, 2099L), length.out = row_count),
        site = paste("Site", seq_len(row_count)),
        source_sheet = rep(
          c("Finance Summary", "Aquatics Clayton"),
          length.out = row_count
        ),
        source_file = rep(source_name, row_count),
        metric = rep(
          c("Income", "Expenses", "Income"),
          length.out = row_count
        ),
        value_type = rep(
          c("Actual", "Budget", "Variance"),
          length.out = row_count
        ),
        value = seq_len(row_count),
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

first_fake_result <- make_fake_result("one.xlsx", "monthly_site_report_data")
second_fake_result <- make_fake_result("two.xlsx", "monthly_site_report_data")
attr(first_fake_result, "source_sheet_order") <- c(
  "Finance Summary",
  "Aquatics Clayton"
)
attr(second_fake_result, "source_sheet_order") <- c(
  "Finance Summary",
  "Caulfield"
)

combined <- combine_basic_upload_results(
  list(first_fake_result, second_fake_result),
  "monthly_site_report_data"
)
stopifnot(
  inherits(combined, "monthly_site_report_data"),
  nrow(combined$finance) == 2L,
  identical(combined$finance$source_file, c("one.xlsx", "two.xlsx")),
  identical(
    attr(combined, "source_sheet_order"),
    c("Finance Summary", "Aquatics Clayton", "Caulfield")
  )
)

preview_report_fixture <- structure(
  list(
    finance = data.frame(
      report_year = c(2025L, 2023L, 2025L),
      date = as.Date(c("2025-01-01", "2023-01-01", "2025-02-01")),
      month = c(1L, 1L, 2L),
      month_label = c("Jan", "Jan", "Feb"),
      site = c("Clayton", "Caulfield", "Peninsula"),
      site_code = c("Z07601", "Z07602", "Z07603"),
      business_unit = c("Aquatics", "Venues", "Aquatics"),
      is_summary = c(FALSE, FALSE, FALSE),
      metric = c("Income", "Expenses", "Income"),
      value_type = c("Actual", "Budget", "Actual"),
      source_sheet = c(
        "Aquatics Clayton",
        "Caulfield",
        " Aquatics Clayton "
      ),
      source_file = c("2025.xlsx", "2023.xlsx", "2025.xlsx"),
      source_row = c(6L, 21L, 7L),
      source_column = c(3L, 4L, 3L),
      value = c(10, 20, 30),
      stringsAsFactors = FALSE
    ),
    discounts = data.frame(
      report_year = c(2025L, 2025L),
      date = as.Date(c("2025-01-01", "2025-01-01")),
      month = c(1L, 1L),
      month_label = c("Jan", "Jan"),
      site = c("Clayton", "Caulfield"),
      site_code = c("Z07601", "Z07602"),
      business_unit = c("Aquatics", "Venues"),
      is_summary = c(FALSE, FALSE),
      discount_type = c("Student", "Staff"),
      value = c(5, 6),
      source_file = c("2025.xlsx", "2025.xlsx"),
      source_sheet = c("Aquatics Clayton", "Caulfield"),
      source_row = c(38L, 38L),
      source_column = c(3L, 4L),
      stringsAsFactors = FALSE
    ),
    total_revenue = data.frame(
      report_year = c(2025L, 2023L),
      period_end = as.Date(c("2025-12-31", "2023-12-31")),
      site = c("Clayton", "Peninsula"),
      site_code = c("Z07601", "Z07603"),
      business_unit = c("Aquatics", "Venues"),
      is_summary = c(FALSE, FALSE),
      actual_income = c(110, 90),
      discount_opportunity = c(5, 4),
      calculated_total_revenue = c(115, 94),
      reported_total_revenue = c(115, 94),
      validation_difference = c(0, 0),
      validation_relative_difference = c(0, 0),
      validation_tolerance = c(0.01, 0.01),
      validation_status = c("pass", "pass"),
      source_file = c("2025.xlsx", "2023.xlsx"),
      source_sheet = c("Aquatics Clayton", "Peninsula"),
      source_row = c(52L, 52L),
      source_column = c(3L, 3L),
      stringsAsFactors = FALSE
    ),
    checks = data.frame(report_year = 2025L, difference = 0),
    finance_checks = data.frame(report_year = 2025L, difference = 0),
    validation = data.frame(level = character()),
    files = data.frame(source_file = character()),
    internal_table = data.frame(value = 1)
  ),
  class = "monthly_site_report_data"
)
attr(preview_report_fixture, "source_sheet_order") <- c(
  "Monash Sport - Finance Summary",
  "Aquatics Clayton",
  "Caulfield",
  "Peninsula"
)

preview_dashboard_fixture <- list(monthly_reports = preview_report_fixture)
stopifnot(
  identical(
    basic_preview_report(preview_dashboard_fixture, "monthly_site"),
    preview_report_fixture
  ),
  is.null(basic_preview_report(preview_dashboard_fixture, "not_a_report")),
  identical(
    basic_preview_table_names(
      preview_report_fixture,
      report_type = "monthly_site"
    ),
    c("finance", "discounts", "total_revenue")
  ),
  identical(
    basic_preview_years(preview_report_fixture$finance),
    c("2023", "2025")
  ),
  identical(
    basic_preview_source_sheets(preview_report_fixture$finance),
    c("Aquatics Clayton", "Caulfield")
  ),
  identical(
    basic_preview_sheet_key(c(" Finance ", "FINANCE")),
    c("finance", "finance")
  ),
  identical(
    basic_preview_sheet_label(" MTD - Venues Finance "),
    "MTD - Venues Finance"
  ),
  identical(
    basic_preview_sheet_label(c(
      "2024 Casual Hours",
      "2025 Casual Hours",
      "Court Hire 2023 vs 2024",
      "Court Hire 2024 vs 2025",
      "Casual Hire 2024 vs 2025"
    )),
    c(
      "Casual Hours",
      "Casual Hours",
      "Casual Hire",
      "Casual Hire",
      "Casual Hire"
    )
  ),
  identical(
    basic_preview_source_sheets(
      data.frame(
        source_sheet = c(
          "2024 Casual Hours",
          "2025 Casual Hours",
          "Court Hire 2023 vs 2024",
          "Court Hire 2024 vs 2025"
        )
      )
    ),
    c("Casual Hours", "Casual Hire")
  ),
  nrow(filter_basic_preview_table(
    data.frame(
      source_sheet = c("2024 Casual Hours", "2025 Casual Hours"),
      value = c(1, 2)
    ),
    source_sheet = "Casual Hours"
  )) == 2L,
  nrow(filter_basic_preview_table(
    data.frame(
      source_sheet = c(
        "Court Hire 2023 vs 2024",
        "Court Hire 2024 vs 2025"
      ),
      value = c(1, 2)
    ),
    source_sheet = "Casual Hire"
  )) == 2L,
  identical(
    basic_preview_source_sheets(
      preview_report_fixture$finance,
      source_sheet_order = c("Caulfield", "Aquatics Clayton")
    ),
    c("Caulfield", "Aquatics Clayton")
  ),
  identical(
    basic_preview_valid_selections(
      c("2023", "2099", NA_character_),
      c("2023", "2025")
    ),
    "2023"
  ),
  nrow(filter_basic_preview_table(
    preview_report_fixture$finance,
    year = "2025"
  )) == 2L,
  nrow(filter_basic_preview_table(
    preview_report_fixture$finance,
    source_sheet = "Aquatics Clayton"
  )) == 2L,
  identical(
    filter_basic_preview_table(
      preview_report_fixture$finance,
      search = "FIELD"
    )$site,
    "Caulfield"
  ),
  identical(
    sort_basic_preview_table(
      preview_report_fixture$finance,
      column = "value",
      direction = "desc"
    )$value,
    c(30, 20, 10)
  )
)

monthly_site_preview <- basic_preview_monthly_site_data(
  preview_report_fixture
)
aquatics_2025_preview <- filter_basic_preview_table(
  monthly_site_preview,
  year = "2025",
  source_sheet = "Aquatics Clayton"
)
aquatics_revenue_preview <- aquatics_2025_preview[
  aquatics_2025_preview$data_section == "Total Revenue",
  ,
  drop = FALSE
]
formatted_monthly_preview <- format_basic_preview_table(
  monthly_site_preview
)
first_header_sort <- next_basic_preview_sort("value", "__none__", "asc")
second_header_sort <- next_basic_preview_sort(
  "value",
  first_header_sort$column,
  first_header_sort$direction
)
other_header_sort <- next_basic_preview_sort("measure", "value", "asc")
sortable_table_html <- as.character(basic_preview_table_ui(
  formatted_monthly_preview[1:2, c("report_year", "measure", "value")],
  sort_column = "measure",
  sort_direction = "desc",
  sort_input_id = "preview-column-sort-test"
))

stopifnot(
  nrow(monthly_site_preview) == 13L,
  setequal(
    basic_preview_field_values(monthly_site_preview, "measure"),
    c("Income", "Expenses", "Discount Opportunity", "Total Revenue")
  ),
  identical(
    as.integer(table(monthly_site_preview$data_section)[
      c("Finance", "Discounts", "Total Revenue")
    ]),
    c(3L, 2L, 8L)
  ),
  identical(
    basic_preview_source_sheets(
      monthly_site_preview,
      attr(preview_report_fixture, "source_sheet_order")
    ),
    c("Aquatics Clayton", "Caulfield", "Peninsula")
  ),
  nrow(aquatics_2025_preview) == 7L,
  nrow(filter_basic_preview_table(
    monthly_site_preview,
    year = c("2023", "2099")
  )) == 5L,
  nrow(filter_basic_preview_table(
    monthly_site_preview,
    measures = c("Income", "Total Revenue"),
    value_types = c("Actual", "Calculated Total Revenue")
  )) == 4L,
  identical(
    aquatics_2025_preview$source_row,
    c(6L, 7L, 38L, 52L, 52L, 52L, 52L)
  ),
  identical(
    aquatics_revenue_preview$value_type,
    c(
      "Actual Income",
      "Discount Opportunity",
      "Calculated Total Revenue",
      "Reported Total Revenue"
    )
  ),
  identical(aquatics_revenue_preview$value, c(110, 5, 115, 115)),
  all(aquatics_revenue_preview$validation_status == "pass"),
  inherits(monthly_site_preview$period_date, "Date"),
  is.character(formatted_monthly_preview$period_date),
  "2025-12-31" %in% formatted_monthly_preview$period_date,
  identical(first_header_sort, list(column = "value", direction = "desc")),
  identical(second_header_sort, list(column = "value", direction = "asc")),
  identical(other_header_sort, list(column = "measure", direction = "desc")),
  identical(basic_preview_cell_text(1234.5), "1234.5"),
  identical(basic_preview_cell_text(NA_real_), ""),
  grepl("cleaned-preview-sort-button active", sortable_table_html, fixed = TRUE),
  grepl("data-column=\"measure\"", sortable_table_html, fixed = TRUE),
  grepl("aria-sort=\"descending\"", sortable_table_html, fixed = TRUE),
  grepl("Shiny.setInputValue", sortable_table_html, fixed = TRUE),
  nrow(preview_report_fixture$finance) == 3L,
  nrow(preview_report_fixture$discounts) == 2L,
  nrow(preview_report_fixture$total_revenue) == 2L,
  !any(grepl("^\\.preview_", names(monthly_site_preview)))
)

site_preview_fixture <- preview_report_fixture
site_preview_fixture$finance$source_sheet <- "Finance"
site_preview_fixture$discounts$source_sheet <- "Finance"
site_preview_fixture$total_revenue$source_sheet <- "Finance"
site_preview_fixture$visits <- data.frame(
  report_year = 2025L,
  date = as.Date("2025-03-01"),
  month = 3L,
  month_label = "Mar",
  report_unit = "Aquatics",
  visit_type = "Direct",
  activity = "Swim",
  audience = "Student",
  value = 42,
  source_file = "2025-site.xlsx",
  source_sheet = "Direct Visits",
  source_row = 8L,
  source_column = 3L,
  stringsAsFactors = FALSE
)
site_preview_fixture$memberships <- data.frame(
  report_year = 2025L,
  date = as.Date("2025-03-01"),
  month = 3L,
  month_label = "Mar",
  report_unit = "Aquatics",
  dataset_type = "Membership",
  metric = "Active members",
  value = 20,
  source_file = "2025-site.xlsx",
  source_sheet = "Memberships",
  source_row = 9L,
  source_column = 3L,
  stringsAsFactors = FALSE
)
site_preview_fixture$enrollments <- data.frame(
  report_year = 2025L,
  date = as.Date("2025-03-01"),
  month = 3L,
  month_label = "Mar",
  report_unit = "Aquatics",
  dataset_type = "Enrollment",
  metric = "New enrollments",
  value = 7,
  source_file = "2025-site.xlsx",
  source_sheet = "Enrollments",
  source_row = 10L,
  source_column = 3L,
  stringsAsFactors = FALSE
)
class(site_preview_fixture) <- "business_unit_template_report_data"
attr(site_preview_fixture, "source_sheet_order") <- c(
  "Finance",
  "Direct Visits",
  "Memberships",
  "Enrollments"
)
combined_site_preview <- basic_preview_combined_report_data(
  site_preview_fixture,
  "site_detail"
)
direct_visits_preview <- filter_basic_preview_table(
  combined_site_preview,
  source_sheet = "Direct Visits"
)
direct_visits_display <- basic_preview_drop_empty_columns(
  direct_visits_preview
)

venue_preview_fixture <- structure(
  list(
    finance = data.frame(
      report_year = 2025L,
      date = as.Date("2025-01-01"),
      metric = "Income",
      value = 100,
      source_file = "2025-venues.xlsx",
      source_sheet = "MTD - Venues Finance ",
      source_row = 6L,
      source_column = 3L,
      stringsAsFactors = FALSE
    ),
    attendance = data.frame(
      report_year = 2025L,
      date = as.Date("2025-01-01"),
      area = "Pool",
      activity = "Lap swimming",
      value = 80,
      source_file = "2025-venues.xlsx",
      source_sheet = "Facility Attendance",
      source_row = 7L,
      source_column = 3L,
      stringsAsFactors = FALSE
    )
  ),
  class = "venues_tracking_report_data",
  source_sheet_order = c("MTD - Venues Finance", "Facility Attendance")
)
combined_venue_preview <- basic_preview_combined_report_data(
  venue_preview_fixture,
  "venues_tracking"
)

stopifnot(
  nrow(combined_site_preview) ==
    nrow(preview_report_fixture$finance) +
    nrow(preview_report_fixture$discounts) +
    nrow(preview_report_fixture$total_revenue) + 3L,
  all(c("metric", "discount_type", "activity", "dataset_type") %in%
    names(combined_site_preview)),
  inherits(combined_site_preview$date, "Date"),
  identical(
    basic_preview_source_sheets(
      combined_site_preview,
      attr(site_preview_fixture, "source_sheet_order")
    ),
    c("Finance", "Direct Visits", "Memberships", "Enrollments")
  ),
  nrow(direct_visits_display) == 1L,
  "activity" %in% names(direct_visits_display),
  !"discount_type" %in% names(direct_visits_display),
  !"actual_income" %in% names(direct_visits_display),
  nrow(combined_venue_preview) == 2L,
  all(c("metric", "area", "activity", "source_sheet") %in%
    names(combined_venue_preview)),
  identical(
    basic_preview_source_sheets(
      combined_venue_preview,
      attr(venue_preview_fixture, "source_sheet_order")
    ),
    c("MTD - Venues Finance", "Facility Attendance")
  ),
  !any(grepl("^\\.preview_", names(combined_site_preview))),
  !any(grepl("^\\.preview_", names(combined_venue_preview)))
)

pagination_fixture <- data.frame(row_id = seq_len(27L))
page_two <- paginate_basic_preview_table(
  pagination_fixture,
  page = 2L,
  page_size = 10L
)
last_page <- paginate_basic_preview_table(
  pagination_fixture,
  page = 99L,
  page_size = 10L
)
stopifnot(
  page_two$page == 2L,
  page_two$pages == 3L,
  identical(page_two$data$row_id, 11:20),
  last_page$page == 3L,
  identical(last_page$data$row_id, 21:27)
)

export_fixture <- data.frame(
  report_year = c(2023L, 2025L, 2025L, 2025L),
  period_date = as.Date(c(
    "2023-01-01",
    "2025-01-01",
    "2025-02-01",
    "2025-01-01"
  )),
  source_sheet = c(
    "Aquatics/Clayton:Monthly Results That Exceed Thirty One Characters",
    "Aquatics/Clayton:Monthly Results That Exceed Thirty One Characters",
    "Aquatics Clayton Monthly Results That Exceed Thirty One Characters",
    "Caulfield"
  ),
  measure = c("Income", "Income", "Expenses", "Income"),
  value_type = c("Actual", "Actual", "Budget", "Actual"),
  value = c(5, 30, 20, 10),
  stringsAsFactors = FALSE
)
export_sheets <- basic_cleaned_export_sheets(
  export_fixture,
  source_sheets = c(
    "Caulfield",
    "Aquatics/Clayton:Monthly Results That Exceed Thirty One Characters",
    "Aquatics Clayton Monthly Results That Exceed Thirty One Characters"
  ),
  years = "2025",
  measures = c("Income", "Expenses"),
  value_types = c("Actual", "Budget"),
  sort_column = "value",
  sort_direction = "desc"
)
stopifnot(
  length(export_sheets) == 3L,
  identical(export_sheets[[1L]]$value, 10),
  identical(export_sheets[[2L]]$value, 30),
  identical(export_sheets[[3L]]$value, 20),
  all(nchar(names(export_sheets)) <= 31L),
  !anyDuplicated(tolower(names(export_sheets))),
  inherits(export_sheets[[1L]]$period_date, "Date"),
  identical(
    basic_cleaned_export_sheet_names(c("A/B", "A:B", "")),
    c("A B", "A B (2)", "Cleaned Data")
  ),
  identical(
    basic_cleaned_export_filename("monthly_site", c("2025", "2023")),
    "cleaned-monthly-site-reports-2023-2025.xlsx"
  ),
  length(basic_cleaned_export_sheets(export_fixture, search = "missing")) == 0L
)

export_path <- tempfile(fileext = ".xlsx")
writexl::write_xlsx(export_sheets, export_path, format_headers = TRUE)
stopifnot(
  file.exists(export_path),
  identical(readxl::excel_sheets(export_path), names(export_sheets))
)
unlink(export_path)

fake_parsers <- list(
  monthly_site = function(path, source_name) {
    stopifnot(file.exists(path))
    payload <- rawToChar(readBin(path, what = "raw", n = file.info(path)$size))

    if (identical(payload, "synthetic failed replacement")) {
      stop("Synthetic replacement failure")
    }

    make_fake_result(
      source_name,
      "monthly_site_report_data",
      warning = TRUE,
      row_count = 30L
    )
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
      inherits(returned_data$monthly_reports, "monthly_site_report_data"),
      nrow(returned_data$monthly_reports$finance) == 30L
    )

    session$setInputs(
      preview_report_type = "monthly_site",
      preview_year = character(),
      preview_source_sheet = BASIC_PREVIEW_ALL_SHEETS,
      preview_measure = character(),
      preview_value_type = character(),
      preview_search = "",
      preview_sort_column = "__none__",
      preview_sort_direction = "asc",
      preview_page_size = "10"
    )
    session$flushReact()
    stopifnot(
      nrow(preview_table_data()) == 30L,
      !"data_section" %in% names(preview_table_data()),
      identical(selected_preview_source_sheet(), "Finance Summary"),
      setequal(preview_measure_choices(), c("Income", "Expenses")),
      setequal(
        preview_value_type_choices(),
        c("Actual", "Budget", "Variance")
      ),
      grepl(
        "cleaned-preview-sort-button",
        paste(as.character(output$cleaned_data_table), collapse = ""),
        fixed = TRUE
      ),
      !grepl(
        "data-column=\"data_section\"",
        paste(as.character(output$cleaned_data_table), collapse = ""),
        fixed = TRUE
      ),
      nrow(preview_filtered_data()) == 15L,
      preview_page_state()$total == 15L,
      preview_page_state()$pages == 2L,
      nrow(preview_page_state()$data) == 10L
    )

    stopifnot(length(preview_export_sheets()) == 2L)
    stopifnot(grepl(
      "Download Excel",
      paste(as.character(output$preview_download_ui), collapse = ""),
      fixed = TRUE
    ))

    session$setInputs(
      preview_measure = c("Income", "Expenses"),
      preview_value_type = c("Actual", "Budget")
    )
    session$flushReact()
    stopifnot(
      all(preview_filtered_data()$measure %in% c("Income", "Expenses")),
      all(preview_filtered_data()$value_type %in% c("Actual", "Budget")),
      nrow(preview_filtered_data()) == 10L
    )

    session$setInputs(
      preview_source_sheet = "Aquatics Clayton",
      preview_measure = character(),
      preview_value_type = character()
    )
    session$flushReact()
    stopifnot(
      nrow(preview_filtered_data()) == 15L,
      all(
        preview_filtered_data()$source_sheet == "Aquatics Clayton"
      )
    )

    session$setInputs(preview_year = "2099")
    session$flushReact()
    stopifnot(
      identical(preview_source_sheets(), "Aquatics Clayton"),
      identical(selected_preview_source_sheet(), "Aquatics Clayton"),
      nrow(preview_filtered_data()) == 15L
    )

    session$setInputs(
      preview_year = character(),
      preview_source_sheet = "Aquatics Clayton",
      preview_search = "Site 2"
    )
    session$flushReact()
    stopifnot(nrow(preview_filtered_data()) == 6L)

    session$setInputs(
      preview_search = "",
      preview_sort_column = "value",
      preview_sort_direction = "desc"
    )
    session$flushReact()
    stopifnot(preview_page_state()$data$value[[1L]] == 30L)

    session$setInputs(preview_next = 1)
    session$flushReact()
    stopifnot(preview_page_state()$page == 2L)

    original_stored_path <- returned_data$uploads$stored_path[[1L]]
    original_file_id <- returned_data$uploads$file_id[[1L]]
    failed_replacement_path <- tempfile(fileext = ".xlsx")
    writeBin(
      charToRaw("synthetic failed replacement"),
      failed_replacement_path
    )
    session$setInputs(
      monthly_reports = data.frame(
        name = "Monthly Site Report 2099.xlsx",
        size = file.info(failed_replacement_path)$size,
        type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
        datapath = failed_replacement_path,
        stringsAsFactors = FALSE
      )
    )
    session$flushReact()

    after_failed_replacement <- session$returned()
    stopifnot(
      nrow(after_failed_replacement$uploads) == 2L,
      sum(after_failed_replacement$uploads$status == "ready") == 1L,
      sum(after_failed_replacement$uploads$status == "failed") == 1L,
      original_file_id %in% after_failed_replacement$uploads$file_id,
      file.exists(original_stored_path),
      inherits(
        after_failed_replacement$monthly_reports,
        "monthly_site_report_data"
      ),
      nrow(after_failed_replacement$monthly_reports$finance) == 30L
    )

    successful_replacement_path <- tempfile(fileext = ".xlsx")
    writeBin(
      charToRaw("synthetic successful replacement"),
      successful_replacement_path
    )
    session$setInputs(
      monthly_reports = data.frame(
        name = "Monthly Site Report 2099.xlsx",
        size = file.info(successful_replacement_path)$size,
        type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
        datapath = successful_replacement_path,
        stringsAsFactors = FALSE
      )
    )
    session$flushReact()

    returned_data <- session$returned()
    stopifnot(
      nrow(returned_data$uploads) == 1L,
      returned_data$uploads$status[[1L]] == "ready",
      returned_data$uploads$file_id[[1L]] != original_file_id,
      !file.exists(original_stored_path),
      inherits(returned_data$monthly_reports, "monthly_site_report_data"),
      nrow(returned_data$monthly_reports$finance) == 30L
    )

    stored_path <- returned_data$uploads$stored_path[[1L]]
    file_id <- returned_data$uploads$file_id[[1L]]
    stopifnot(file.exists(stored_path))

    status_html <- as.character(output$file_status)
    stopifnot(
      any(grepl("basic-upload-remove-button", status_html, fixed = TRUE)),
      any(grepl(file_id, status_html, fixed = TRUE))
    )

    session$setInputs(remove_file = file_id)
    session$flushReact()
    after_delete <- session$returned()
    stopifnot(
      nrow(after_delete$uploads) == 0L,
      is.null(after_delete$monthly_reports),
      is.null(preview_table_data()),
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

    unlink(c(
      upload_path,
      failed_replacement_path,
      successful_replacement_path,
      failed_path
    ))
  }
)

# Upload all reports: files are routed by name; others are reported.
stopifnot(
  identical(
    classify_report_upload(c(
      "Monthly Site Report 2025 -December.xlsx",
      "Aquatics Site Report 2025.xlsx",
      "Participation Program Report 2025.xlsx",
      "Venues Site Report 2025.xlsx",
      "2025 Venues Tracking Report.xlsx",
      "Budget notes 2025.xlsx",
      "~$Monthly Site Report 2025.xlsx",
      "desktop.ini",
      "Monthly Site Report 2025.csv"
    )),
    c("monthly_site", "site_detail", "site_detail", "site_detail",
      "venues_tracking", NA, NA, NA, NA)
  ),
  identical(basic_upload_mismatch_hint("Aquatics Site Report 2025.xlsx", "site_detail"), ""),
  grepl("Venues Tracking Reports",
    basic_upload_mismatch_hint("2025 Venues Tracking Report.xlsx", "monthly_site"),
    fixed = TRUE)
)

shiny::testServer(
  mod_data_upload_server,
  args = list(parsers = fake_parsers),
  {
    session$flushReact()
    paths <- replicate(4L, tempfile(fileext = ".xlsx"))
    for (path in paths) writeBin(charToRaw("synthetic upload"), path)

    session$setInputs(
      all_reports = data.frame(
        name = c(
          "Monthly Site Report 2099.xlsx",
          "2099 Venues Tracking Report.xlsx",
          "Unrelated notes 2099.xlsx",
          "readme.txt"
        ),
        size = file.info(paths)$size,
        type = "application/octet-stream",
        datapath = paths,
        stringsAsFactors = FALSE
      )
    )
    session$flushReact()

    uploaded <- session$returned()$uploads
    notice <- paste(as.character(output$upload_notice), collapse = "")
    stopifnot(
      nrow(uploaded) == 2L,
      setequal(uploaded$report_type, c("monthly_site", "venues_tracking")),
      all(uploaded$status == "ready"),
      grepl("Matched 2 files", notice, fixed = TRUE),
      grepl("Unrelated notes 2099.xlsx", notice, fixed = TRUE),
      grepl("1 other file was ignored", notice, fixed = TRUE)
    )

    # A file uploaded to the wrong area fails with a hint to the right one.
    session$setInputs(
      site_reports = data.frame(
        name = "2099 Venues Tracking Report.xlsx",
        size = file.info(paths[[2L]])$size,
        type = "application/octet-stream",
        datapath = paths[[2L]],
        stringsAsFactors = FALSE
      )
    )
    session$flushReact()
    failed <- session$returned()$uploads
    failed <- failed[failed$status == "failed", , drop = FALSE]
    stopifnot(
      nrow(failed) == 1L,
      grepl("try Upload all reports", failed$error_message[[1L]], fixed = TRUE)
    )
    unlink(paths)
  }
)

# Saved data: year labels, file name, save/load round trip and refusals.
stopifnot(
  identical(format_basic_year_span(c(2021L, 2022L, 2023L, 2025L)), "2021–2023, 2025"),
  identical(format_basic_year_span(c(2025L, 2025L)), "2025"),
  identical(format_basic_year_span(integer()), ""),
  identical(format_basic_year_span(c(2021L, 2022L), dash = "-", join = "+"), "2021-2022"),
  identical(
    basic_saved_result_years(list(), "Monthly Site Report 2024 -June.xlsx"),
    2024L
  ),
  basic_saved_data_days_old(as.POSIXct("2026-01-01 10:00:00"), as.Date("2026-01-31")) == 30L,
  # 30 Oct 22:00 UTC is already 31 Oct in Melbourne (offset -660 minutes).
  identical(
    basic_saved_local_date(as.POSIXct("2026-10-30 22:00:00", tz = "UTC"), -660),
    as.Date("2026-10-31")
  ),
  identical(
    basic_saved_local_date(as.POSIXct("2026-10-30 22:00:00", tz = "UTC"), 0),
    as.Date("2026-10-30")
  )
)

saved_state <- new.env()
saved_state$path <- tempfile(fileext = ".rds")

shiny::testServer(
  mod_data_upload_server,
  args = list(parsers = fake_parsers),
  {
    session$flushReact()
    stopifnot(grepl(
      "Upload or load reports first",
      paste(as.character(output$saved_data_actions), collapse = ""),
      fixed = TRUE
    ))

    paths <- replicate(2L, tempfile(fileext = ".xlsx"))
    for (path in paths) writeBin(charToRaw("synthetic upload"), path)
    session$setInputs(
      all_reports = data.frame(
        name = c("Monthly Site Report 2099.xlsx", "2099 Venues Tracking Report.xlsx"),
        size = file.info(paths)$size,
        type = "application/octet-stream",
        datapath = paths,
        stringsAsFactors = FALSE
      )
    )
    session$flushReact()

    payload <- saved_data_payload()
    actions <- paste(as.character(output$saved_data_actions), collapse = "")
    stopifnot(
      identical(payload$format, BASIC_SAVED_DATA_FORMAT),
      nrow(payload$files) == 2L,
      length(payload$results) == 2L,
      identical(payload$files$years[payload$files$report_type == "monthly_site"], "2098, 2099"),
      identical(
        basic_saved_data_filename(payload$files, as.POSIXct("2026-10-31 12:00:00")),
        "monash-sport-saved-data_2098-2099_2-files_saved-2026-10-31.rds"
      ),
      grepl("Will save 2 ready files", actions, fixed = TRUE),
      grepl("Monthly Site Reports: 1 file (2098–2099)", actions, fixed = TRUE),
      grepl("download_saved_data", actions, fixed = TRUE)
    )

    saveRDS(payload, saved_state$path)
    saved_state$monthly_finance <- session$returned()$monthly_reports$finance
    unlink(paths)
  }
)

shiny::testServer(
  mod_data_upload_server,
  args = list(parsers = fake_parsers),
  {
    session$flushReact()
    load_file <- function(path, name = "saved.rds") {
      session$setInputs(saved_data = data.frame(
        name = name,
        size = file.info(path)$size,
        type = "application/octet-stream",
        datapath = path,
        stringsAsFactors = FALSE
      ))
      session$flushReact()
      paste(as.character(output$saved_data_notice), collapse = "")
    }

    notice <- load_file(saved_state$path)
    loaded <- session$returned()
    stopifnot(
      nrow(loaded$uploads) == 2L,
      all(loaded$uploads$status == "ready"),
      all(is.na(loaded$uploads$stored_path)),
      all(grepl("Loaded from saved data", loaded$uploads$detail, fixed = TRUE)),
      identical(loaded$monthly_reports$finance, saved_state$monthly_finance),
      inherits(loaded$venues_reports, "venues_tracking_report_data"),
      loaded$uploads$warning_count[loaded$uploads$report_type == "monthly_site"] == 1L,
      grepl("Saved data loaded", notice, fixed = TRUE),
      grepl("(today)", notice, fixed = TRUE),
      grepl("Venues Tracking Reports: 1 file (2098)", notice, fixed = TRUE),
      grepl("2099 Venues Tracking Report.xlsx", notice, fixed = TRUE),
      grepl("Synthetic warning", paste(as.character(output$file_status), collapse = ""), fixed = TRUE)
    )

    # A newer upload of the same report replaces the saved copy.
    path <- tempfile(fileext = ".xlsx")
    writeBin(charToRaw("synthetic upload"), path)
    session$setInputs(monthly_reports = data.frame(
      name = "Monthly Site Report 2099.xlsx",
      size = file.info(path)$size,
      type = "application/octet-stream",
      datapath = path,
      stringsAsFactors = FALSE
    ))
    session$flushReact()
    uploads <- session$returned()$uploads
    stopifnot(
      nrow(uploads) == 2L,
      !is.na(uploads$stored_path[uploads$report_type == "monthly_site"])
    )

    # Loading the same saved file again replaces the session copies.
    notice <- load_file(saved_state$path)
    stopifnot(
      nrow(session$returned()$uploads) == 2L,
      grepl("2 files already in this session", notice, fixed = TRUE)
    )

    # Loaded files can be removed like uploads.
    venues_id <- session$returned()$uploads$file_id[
      session$returned()$uploads$report_type == "venues_tracking"
    ]
    session$setInputs(remove_file = venues_id)
    session$flushReact()
    stopifnot(
      nrow(session$returned()$uploads) == 1L,
      is.null(session$returned()$venues_reports)
    )

    # Old saves are flagged; wrong, newer, older or damaged files are refused.
    saved <- readRDS(saved_state$path)
    variant <- function(change) {
      path <- tempfile(fileext = ".rds")
      saveRDS(change(saved), path)
      path
    }
    stale <- load_file(variant(function(x) {
      x$saved_at <- as.POSIXct(
        paste(
          Sys.Date() - 40,"12:00:00"),
          tz = Sys.timezone())
      x
    }))
    stopifnot(
      grepl("40 days ago", stale, fixed = TRUE),
      grepl("more than 30 days old", stale, fixed = TRUE)
    )

    before <- session$returned()$uploads
    not_rds <- tempfile(fileext = ".rds")
    writeBin(charToRaw("not an rds file"), not_rds)
    refusals <- list(
      list(not_rds, "is not saved dashboard data"),
      list(variant(function(x) list(a = 1)), "is not saved dashboard data"),
      list(variant(function(x) { x$version <- BASIC_SAVED_DATA_VERSION + 1L; x }), "newer version"),
      list(variant(function(x) { x$version <- BASIC_SAVED_DATA_VERSION - 1L; x }), "older version"),
      list(variant(function(x) { x$results <- x$results[1L]; x }), "incomplete or damaged")
    )
    for (refusal in refusals) {
      notice <- load_file(refusal[[1L]], "bad.rds")
      stopifnot(
        grepl("bad.rds was not loaded", notice, fixed = TRUE),
        grepl(refusal[[2L]], notice, fixed = TRUE),
        identical(session$returned()$uploads, before)
      )
    }
    unlink(c(path, not_rds))
  }
)
unlink(saved_state$path)

cat("PASS test_basic_upload.R\n")
