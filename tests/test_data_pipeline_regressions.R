numeric_cells <- data.frame(
  value = c("82.8%", "($1,234.50)", " 2,500 "),
  stringsAsFactors = FALSE
)

stopifnot(
  isTRUE(all.equal(cell_as_number(numeric_cells, 1L, 1L), 0.828)),
  isTRUE(all.equal(cell_as_number(numeric_cells, 2L, 1L), -1234.5)),
  isTRUE(all.equal(cell_as_number(numeric_cells, 3L, 1L), 2500))
)

stopifnot(
  identical(
    monthly_site_import_sheet_names(
      c(
        "Chart1",
        "Chart 2",
        "Chart",
        "DASHBOARD",
        "Monash Sport - Finance Summary",
        "Aquatics Clayton (Z07601)"
      )
    ),
    c(
      "Monash Sport - Finance Summary",
      "Aquatics Clayton (Z07601)"
    )
  )
)

source_order <- workbook_source_sheet_order(
  list(
    list(workbook_sheets = c("Dashboard", "Finance")),
    list(workbook_sheets = c("Finance", "Attendance"))
  )
)
stopifnot(identical(source_order, c("Dashboard", "Finance", "Attendance")))

growth_check <- data.frame(
  report_year = 2028L,
  dataset = "Court Hire",
  check_type = "growth_identity",
  group = "Stadium",
  item = "2027 vs 2028",
  row_type = "Month",
  metric = "Growth Achieved",
  reported_total = 1.25,
  calculated_total = 1.2,
  difference = 0.05,
  status = "fail",
  source_file = "2028 Venues Tracking Report.xlsx",
  source_sheet = "Court Hire 2027 vs 2028",
  source_row = 10L,
  stringsAsFactors = FALSE
)
growth_issue <- validate_venues_operational_checks(growth_check)
stopifnot(
  nrow(growth_issue) == 1L,
  grepl("growth identity", growth_issue$message[[1L]], fixed = TRUE),
  !grepl("monthly-to-total", growth_issue$message[[1L]], fixed = TRUE)
)

cat("PASS test_data_pipeline_regressions.R\n")
