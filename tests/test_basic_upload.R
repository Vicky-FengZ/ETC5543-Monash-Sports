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
  grepl("Upload &amp; Manage", ui_html, fixed = TRUE),
  grepl("Cleaned Data Preview", ui_html, fixed = TRUE),
  grepl("basic-upload-test-preview_report_type", ui_html, fixed = TRUE),
  !grepl("basic-upload-test-preview_table_control", ui_html, fixed = TRUE),
  grepl("basic-upload-test-preview_measure", ui_html, fixed = TRUE),
  grepl("basic-upload-test-preview_value_type", ui_html, fixed = TRUE),
  grepl("basic-upload-test-preview_source_sheet", ui_html, fixed = TRUE),
  grepl("basic-upload-test-preview_sort_column", ui_html, fixed = TRUE),
  grepl("basic-upload-test-cleaned_data_table", ui_html, fixed = TRUE),
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

fake_parsers <- list(
  monthly_site = function(path, source_name) {
    stopifnot(file.exists(path))
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

    stored_path <- returned_data$uploads$stored_path[[1L]]
    file_id <- returned_data$uploads$file_id[[1L]]
    stopifnot(file.exists(stored_path))

    session$setInputs(selected_file = file_id, delete_file = 1)
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

    unlink(c(upload_path, failed_path))
  }
)

cat("PASS test_basic_upload.R\n")
