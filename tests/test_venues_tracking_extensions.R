make_venues_raw_sheet <- function(rows, columns) {
  result <- as.data.frame(
    matrix(NA_character_, nrow = rows, ncol = columns),
    stringsAsFactors = FALSE
  )
  names(result) <- paste0("column_", seq_len(columns))
  result
}


set_venues_test_cell <- function(sheet, row, column, value) {
  sheet[[column]][[row]] <- as.character(value)
  sheet
}


stopifnot(
  identical(
    find_venues_tracking_sheet(
      c("Finance", "2028 Casual Hours", "Casual Wages "),
      expected_name = "Casual Hours",
      source_name = "2028 Venues Tracking Report.xlsx",
      allow_year_prefix = TRUE
    ),
    "2028 Casual Hours"
  ),
  identical(
    find_venues_tracking_sheet(
      c("Finance", "2028 Casual Hours", "Casual Wages "),
      expected_name = "Casual Wages",
      source_name = "2028 Venues Tracking Report.xlsx"
    ),
    "Casual Wages "
  ),
  identical(
    find_venues_tracking_sheet(
      c("Finance", "Court Hire 2027 vs 2028", "Casual Wages "),
      expected_name = "Court Hire",
      source_name = "2028 Venues Tracking Report.xlsx",
      allow_year_comparison_suffix = TRUE
    ),
    "Court Hire 2027 vs 2028"
  )
)


online_raw <- make_venues_raw_sheet(5L, 14L)
online_raw <- set_venues_test_cell(online_raw, 1L, 1L, "Clayton")

for (month in seq_len(12L)) {
  online_raw <- set_venues_test_cell(
    online_raw,
    1L,
    month + 1L,
    month.name[[month]]
  )
}

online_raw <- set_venues_test_cell(online_raw, 1L, 14L, "YTD")
online_categories <- c(
  "Facility Hire",
  "Membership/Health & Fitness",
  "DD Rejection Fees",
  "Total"
)

for (row_index in seq_along(online_categories)) {
  source_row <- row_index + 1L
  online_raw <- set_venues_test_cell(
    online_raw,
    source_row,
    1L,
    online_categories[[row_index]]
  )
  monthly_value <- c(10, 5, NA, 15)[[row_index]]

  if (!is.na(monthly_value)) {
    for (month in seq_len(12L)) {
      online_raw <- set_venues_test_cell(
        online_raw,
        source_row,
        month + 1L,
        monthly_value
      )
    }
  }

  online_raw <- set_venues_test_cell(
    online_raw,
    source_row,
    14L,
    c(120, 60, 0, 180)[[row_index]]
  )
}


merchandise_raw <- make_venues_raw_sheet(7L, 28L)

for (month in seq_len(12L)) {
  start_column <- 2L * month + 1L
  merchandise_raw <- set_venues_test_cell(
    merchandise_raw,
    1L,
    start_column,
    month.name[[month]]
  )
  merchandise_raw <- set_venues_test_cell(
    merchandise_raw,
    2L,
    start_column,
    "MTD"
  )
  merchandise_raw <- set_venues_test_cell(
    merchandise_raw,
    2L,
    start_column + 1L,
    "# Transactions"
  )
}

merchandise_raw <- set_venues_test_cell(
  merchandise_raw,
  1L,
  27L,
  "YTD"
)
merchandise_raw <- set_venues_test_cell(
  merchandise_raw,
  2L,
  1L,
  "EQUIPMENT HIRE"
)
merchandise_raw <- set_venues_test_cell(merchandise_raw, 2L, 2L, "Cost")
merchandise_raw <- set_venues_test_cell(merchandise_raw, 2L, 27L, "YTD")
merchandise_raw <- set_venues_test_cell(
  merchandise_raw,
  2L,
  28L,
  "# Transactions"
)
merchandise_raw <- set_venues_test_cell(merchandise_raw, 3L, 1L, "Racquet Hire")
merchandise_raw <- set_venues_test_cell(merchandise_raw, 3L, 2L, 2)
merchandise_raw <- set_venues_test_cell(merchandise_raw, 4L, 1L, "TOTAL")
merchandise_raw <- set_venues_test_cell(
  merchandise_raw,
  5L,
  1L,
  "EQUIPMENT SALES"
)
merchandise_raw <- set_venues_test_cell(merchandise_raw, 6L, 1L, "Squash Ball")
merchandise_raw <- set_venues_test_cell(merchandise_raw, 6L, 2L, 5)
merchandise_raw <- set_venues_test_cell(merchandise_raw, 7L, 1L, "TOTAL")

for (source_row in c(3L, 4L, 6L, 7L)) {
  revenue <- if (source_row %in% c(3L, 4L)) 2 else 5

  for (month in seq_len(12L)) {
    start_column <- 2L * month + 1L
    merchandise_raw <- set_venues_test_cell(
      merchandise_raw,
      source_row,
      start_column,
      revenue
    )
    merchandise_raw <- set_venues_test_cell(
      merchandise_raw,
      source_row,
      start_column + 1L,
      1
    )
  }

  merchandise_raw <- set_venues_test_cell(
    merchandise_raw,
    source_row,
    27L,
    revenue * 12
  )
  merchandise_raw <- set_venues_test_cell(
    merchandise_raw,
    source_row,
    28L,
    12
  )
}


court_hire_raw <- make_venues_raw_sheet(31L, 4L)

for (block in list(
  list(title_row = 1L, header_row = 2L, court_type = "Badminton"),
  list(title_row = 17L, header_row = 18L, court_type = "Total Court Hire")
)) {
  court_hire_raw <- set_venues_test_cell(
    court_hire_raw,
    block$title_row,
    1L,
    block$court_type
  )

  for (column in seq_along(c("Month", "2027", "2028", "Growth Achieved"))) {
    court_hire_raw <- set_venues_test_cell(
      court_hire_raw,
      block$header_row,
      column,
      c("Month", "2027", "2028", "Growth Achieved")[[column]]
    )
  }

  for (month in seq_len(12L)) {
    source_row <- block$header_row + month
    court_hire_raw <- set_venues_test_cell(
      court_hire_raw,
      source_row,
      1L,
      month.abb[[month]]
    )
    court_hire_raw <- set_venues_test_cell(court_hire_raw, source_row, 2L, 100)
    court_hire_raw <- set_venues_test_cell(court_hire_raw, source_row, 3L, 120)
    court_hire_raw <- set_venues_test_cell(court_hire_raw, source_row, 4L, 1.2)
  }

  total_row <- block$header_row + 13L
  court_hire_raw <- set_venues_test_cell(court_hire_raw, total_row, 1L, "Total")
  court_hire_raw <- set_venues_test_cell(court_hire_raw, total_row, 2L, 1200)
  court_hire_raw <- set_venues_test_cell(court_hire_raw, total_row, 3L, 1440)
  court_hire_raw <- set_venues_test_cell(court_hire_raw, total_row, 4L, 1.2)
}


casual_hours_raw <- make_venues_raw_sheet(14L, 15L)
casual_hours_raw <- set_venues_test_cell(
  casual_hours_raw,
  1L,
  1L,
  "2028 Roster Dates"
)
casual_hours_raw <- set_venues_test_cell(casual_hours_raw, 1L, 12L, "Summary")
casual_hours_raw <- set_venues_test_cell(
  casual_hours_raw,
  1L,
  13L,
  "Budgeted Hours"
)
casual_hours_raw <- set_venues_test_cell(
  casual_hours_raw,
  1L,
  14L,
  "Hours Paid"
)
casual_hours_raw <- set_venues_test_cell(casual_hours_raw, 1L, 15L, "Variance")
detail_headers <- c(
  "P/F",
  "Roster",
  "Pay Period",
  NA,
  "Timesheet Deadline",
  "Pay Date",
  "Budgeted Hours",
  "Paid Hours",
  "Variance",
  "Notes"
)

for (column in seq_along(detail_headers)) {
  if (!is.na(detail_headers[[column]])) {
    casual_hours_raw <- set_venues_test_cell(
      casual_hours_raw,
      2L,
      column,
      detail_headers[[column]]
    )
  }
}

for (month in seq_len(12L)) {
  summary_row <- month + 1L
  casual_hours_raw <- set_venues_test_cell(
    casual_hours_raw,
    summary_row,
    12L,
    month.name[[month]]
  )
  casual_hours_raw <- set_venues_test_cell(
    casual_hours_raw,
    summary_row,
    13L,
    100
  )
  casual_hours_raw <- set_venues_test_cell(
    casual_hours_raw,
    summary_row,
    14L,
    90
  )
  casual_hours_raw <- set_venues_test_cell(
    casual_hours_raw,
    summary_row,
    15L,
    -10
  )

  detail_row <- if (month == 1L) 14L else month + 1L
  casual_hours_raw <- set_venues_test_cell(
    casual_hours_raw,
    detail_row,
    1L,
    paste0("F", month)
  )
  casual_hours_raw <- set_venues_test_cell(
    casual_hours_raw,
    detail_row,
    2L,
    month.abb[[month]]
  )
  casual_hours_raw <- set_venues_test_cell(
    casual_hours_raw,
    detail_row,
    3L,
    sprintf("2028-%02d-01", month)
  )
  casual_hours_raw <- set_venues_test_cell(
    casual_hours_raw,
    detail_row,
    4L,
    sprintf("2028-%02d-14", month)
  )
  casual_hours_raw <- set_venues_test_cell(
    casual_hours_raw,
    detail_row,
    5L,
    sprintf("2028-%02d-15", month)
  )
  casual_hours_raw <- set_venues_test_cell(
    casual_hours_raw,
    detail_row,
    6L,
    sprintf("2028-%02d-20", month)
  )
  casual_hours_raw <- set_venues_test_cell(casual_hours_raw, detail_row, 7L, 100)
  casual_hours_raw <- set_venues_test_cell(casual_hours_raw, detail_row, 8L, 90)
  casual_hours_raw <- set_venues_test_cell(casual_hours_raw, detail_row, 9L, -10)
}

casual_hours_raw <- set_venues_test_cell(casual_hours_raw, 14L, 12L, "Total")
casual_hours_raw <- set_venues_test_cell(casual_hours_raw, 14L, 13L, 1200)
casual_hours_raw <- set_venues_test_cell(casual_hours_raw, 14L, 14L, 1080)
casual_hours_raw <- set_venues_test_cell(casual_hours_raw, 14L, 15L, -120)


casual_wages_raw <- make_venues_raw_sheet(15L, 9L)
casual_wages_raw <- set_venues_test_cell(
  casual_wages_raw,
  1L,
  1L,
  "2027 Casual Salaries"
)
casual_wages_raw <- set_venues_test_cell(
  casual_wages_raw,
  1L,
  6L,
  "2028 Casual Salaries"
)

for (start_column in c(1L, 6L)) {
  for (offset in seq_along(c("Month", "Budget", "Actual", "Variation"))) {
    casual_wages_raw <- set_venues_test_cell(
      casual_wages_raw,
      2L,
      start_column + offset - 1L,
      c("Month", "Budget", "Actual", "Variation")[[offset]]
    )
  }
}

for (month in seq_len(12L)) {
  source_row <- month + 2L
  casual_wages_raw <- set_venues_test_cell(
    casual_wages_raw,
    source_row,
    1L,
    month.name[[month]]
  )
  casual_wages_raw <- set_venues_test_cell(casual_wages_raw, source_row, 2L, 999)
  casual_wages_raw <- set_venues_test_cell(casual_wages_raw, source_row, 3L, 999)
  casual_wages_raw <- set_venues_test_cell(casual_wages_raw, source_row, 4L, 0)
  casual_wages_raw <- set_venues_test_cell(
    casual_wages_raw,
    source_row,
    6L,
    month.name[[month]]
  )
  casual_wages_raw <- set_venues_test_cell(casual_wages_raw, source_row, 7L, 100)
  casual_wages_raw <- set_venues_test_cell(casual_wages_raw, source_row, 8L, 110)
  casual_wages_raw <- set_venues_test_cell(casual_wages_raw, source_row, 9L, 10)
}

casual_wages_raw <- set_venues_test_cell(casual_wages_raw, 15L, 1L, "TOTAL")
casual_wages_raw <- set_venues_test_cell(casual_wages_raw, 15L, 6L, "TOTAL")
casual_wages_raw <- set_venues_test_cell(casual_wages_raw, 15L, 7L, 1200)
casual_wages_raw <- set_venues_test_cell(casual_wages_raw, 15L, 8L, 1320)
casual_wages_raw <- set_venues_test_cell(casual_wages_raw, 15L, 9L, 120)


future_workbook <- structure(
  list(
    source_file = "2028 Venues Tracking Report.xlsx",
    report_year = 2028L,
    online_sales_sheet_name = "Online Sales",
    online_sales_sheet = online_raw,
    merchandise_sheet_name = "Merchandise Tracking",
    merchandise_sheet = merchandise_raw,
    court_hire_sheet_name = "Court Hire 2027 vs 2028",
    court_hire_sheet = court_hire_raw,
    casual_hours_sheet_name = "2028 Casual Hours",
    casual_hours_sheet = casual_hours_raw,
    casual_wages_sheet_name = "Casual Wages ",
    casual_wages_sheet = casual_wages_raw
  ),
  class = "venues_tracking_report_workbook"
)

online_result <- clean_venues_tracking_report_online_sales(future_workbook)
merchandise_result <- clean_venues_tracking_report_merchandise(future_workbook)
court_hire_result <- clean_venues_tracking_report_court_hire(future_workbook)
casual_hours_result <- clean_venues_tracking_report_casual_hours(future_workbook)
casual_wages_result <- clean_venues_tracking_report_casual_wages(future_workbook)

stopifnot(
  nrow(online_result) == 52L,
  identical(unique(online_result$report_year), 2028L),
  all(online_result$is_reported[online_result$period_type == "Month"]),
  all(build_venues_online_sales_checks(online_result)$status == "pass"),
  nrow(merchandise_result) == 104L,
  identical(
    unique(merchandise_result$section),
    c("EQUIPMENT HIRE", "EQUIPMENT SALES")
  ),
  all(build_venues_merchandise_checks(merchandise_result)$status == "pass"),
  nrow(court_hire_result) == 78L,
  setequal(
    unique(court_hire_result$court_type),
    c("Badminton", "Total Court Hire")
  ),
  setequal(unique(court_hire_result$data_year), c(2027L, 2028L)),
  sum(court_hire_result$comparison_role == "Growth Achieved") == 26L,
  all(build_venues_court_hire_checks(court_hire_result)$status == "pass"),
  nrow(casual_hours_result) == 39L,
  sum(casual_hours_result$period_type == "Pay Period") == 0L,
  sum(casual_hours_result$period_type == "Month") == 36L,
  all(casual_hours_result$is_reported),
  all(build_venues_casual_hours_checks(casual_hours_result)$status == "pass"),
  nrow(casual_wages_result) == 39L,
  identical(unique(casual_wages_result$salary_year), 2028L),
  !any(casual_wages_result$value == 999, na.rm = TRUE),
  all(build_venues_casual_wages_checks(casual_wages_result)$status == "pass"),
  all(c(
    "source_file",
    "source_sheet",
    "source_row",
    "source_column"
  ) %in% names(online_result)),
  all(c(
    "source_file",
    "source_sheet",
    "source_row",
    "source_column"
  ) %in% names(casual_wages_result))
)

duplicate_month_online <- online_result
target_month_rows <- which(
  duplicate_month_online$period_type == "Month" &
    duplicate_month_online$site == duplicate_month_online$site[[1]] &
    duplicate_month_online$category == duplicate_month_online$category[[1]]
)
december_row <- target_month_rows[
  duplicate_month_online$month[target_month_rows] == 12L
]
duplicate_month_online$month[december_row] <- 11L
duplicate_month_checks <- build_venues_online_sales_checks(
  duplicate_month_online
)
target_check <-
  duplicate_month_checks$group == duplicate_month_online$site[[1]] &
  duplicate_month_checks$item == duplicate_month_online$category[[1]]

duplicate_wages_workbook <- future_workbook
duplicate_wages_workbook$casual_wages_sheet <- set_venues_test_cell(
  duplicate_wages_workbook$casual_wages_sheet,
  1L,
  1L,
  "2028 Casual Salaries"
)
duplicate_wages_layout <- find_venues_casual_wages_layout(
  duplicate_wages_workbook$casual_wages_sheet,
  2028L
)
duplicate_wages_issues <- validate_venues_casual_wages_sheet(
  duplicate_wages_workbook
)

blank_hours_total_workbook <- future_workbook
blank_hours_total_workbook$casual_hours_sheet <- set_venues_test_cell(
  blank_hours_total_workbook$casual_hours_sheet,
  14L,
  13L,
  NA_real_
)
blank_hours_total <- clean_venues_tracking_report_casual_hours(
  blank_hours_total_workbook
)

blank_wages_total_workbook <- future_workbook
blank_wages_total_workbook$casual_wages_sheet <- set_venues_test_cell(
  blank_wages_total_workbook$casual_wages_sheet,
  15L,
  8L,
  NA_real_
)
blank_wages_total <- clean_venues_tracking_report_casual_wages(
  blank_wages_total_workbook
)

stopifnot(
  length(december_row) == 1L,
  identical(
    duplicate_month_checks$status[target_check],
    "incomplete_periods"
  ),
  nrow(duplicate_wages_layout) == 2L,
  nrow(clean_venues_tracking_report_casual_wages(
    duplicate_wages_workbook
  )) == 0L,
  nrow(duplicate_wages_issues) == 1L,
  grepl("contains 2 recognisable", duplicate_wages_issues$message[[1]]),
  !blank_hours_total$is_reported[
    blank_hours_total$period_type == "Reported Total" &
      blank_hours_total$metric == "Budgeted Hours"
  ],
  !blank_wages_total$is_reported[
    blank_wages_total$period_type == "Reported Total" &
      blank_wages_total$metric == "Actual"
  ]
)

cat("PASS test_venues_tracking_extensions.R\n")
