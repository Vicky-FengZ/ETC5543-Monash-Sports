# The data checks catch problems in real workbooks. Each case reads a
# synthetic sample workbook from data/sample through the production loader,
# breaks one thing in it (a total that no longer adds up, a missing month,
# a missing section or sheet, a repeated month heading) and checks that the
# loader reports it with the right level and message. A final case adds a
# new business unit and checks that it reaches the Site Detail and Site
# Comparison choices. The sample workbooks themselves pass every check, so
# without these cases the checks would never be exercised.

sample_path <- function(...) {
  path <- file.path("data", "sample", ...)
  stopifnot(file.exists(path))
  path
}

# Runs `code` with the reader `reader_name` replaced by one that returns the
# workbook after `tamper(workbook)`. Each file is read once per test run.
check_read_cache <- new.env()
with_tampered_workbook <- function(reader_name, tamper, code) {
  original <- get(reader_name, envir = globalenv())
  assign(reader_name, function(path, source_name = basename(path)) {
    key <- paste(reader_name, path, source_name, sep = "|")
    if (is.null(check_read_cache[[key]])) {
      check_read_cache[[key]] <- original(path = path, source_name = source_name)
    }
    tamper(check_read_cache[[key]])
  }, envir = globalenv())
  on.exit(assign(reader_name, original, envir = globalenv()), add = TRUE)
  force(code)
}

sheet_labels <- function(sheet) normalise_excel_label(sheet[[1]])

# Rebuilds a sheet's cell cache after an edit (the old cache is dropped
# first, or it would be reused).
recache <- function(sheet) {
  attr(sheet, "cell_text") <- NULL
  attr(sheet, "cell_number") <- NULL
  attr(sheet, "cell_row_names") <- NULL
  cache_sheet_cells(sheet)
}

# Adds `by` to one numeric cell and rebuilds the sheet's cell cache.
bump_cell <- function(sheet, row, column, by = 1000) {
  number <- suppressWarnings(as.numeric(sheet[[column]][[row]]))
  stopifnot(length(number) == 1L, !is.na(number))
  sheet[[column]][[row]] <- if (is.numeric(sheet[[column]])) number + by else as.character(number + by)
  recache(sheet)
}

# Blanks the first-column label of the given rows.
blank_labels <- function(sheet, rows) {
  sheet[[1]][rows] <- NA
  recache(sheet)
}

# Column of the first cell in `row` whose label is `label`.
column_labelled <- function(sheet, row, label) {
  texts <- vapply(seq_len(ncol(sheet)), function(column) cell_as_text(sheet, row, column), character(1))
  which(normalise_excel_label(texts) == label)[[1L]]
}

issues_matching <- function(result, level, pattern) {
  issues <- result$validation
  issues[issues$level == level & grepl(pattern, issues$message, fixed = TRUE), , drop = FALSE]
}

# Monthly Site Report ------------------------------------------------------

monthly_file <- sample_path("Monthly Site Report", "Monthly Site Report 2024 -December.xlsx")
load_monthly <- function(tamper = identity, paths = monthly_file) {
  with_tampered_workbook("read_monthly_site_report_file", tamper,
    load_monthly_site_reports(paths = paths, source_names = basename(paths), strict = FALSE))
}
aquatics_sheet <- function(workbook) grep("Aquatics", names(workbook$sheets), value = TRUE)[[1L]]
edit_aquatics <- function(edit) {
  function(workbook) {
    name <- aquatics_sheet(workbook)
    workbook$sheets[[name]] <- edit(workbook$sheets[[name]])
    workbook
  }
}

failed_rows <- function(result) sum(result$total_revenue$validation_status == "fail", na.rm = TRUE)

clean_monthly <- load_monthly()
stopifnot(
  !any(clean_monthly$validation$level == "error"),
  nrow(issues_matching(clean_monthly, "warning", "section was not found")) == 0L,
  nrow(issues_matching(clean_monthly, "warning", "Sheet was skipped")) == 0L
)

# A reported Total Revenue that no longer equals Actual Income plus discounts.
revenue_off <- load_monthly(edit_aquatics(function(sheet) {
  row <- which(grepl("^total revenue.*actual.*opp", sheet_labels(sheet)))[[1L]]
  for (column in 2:ncol(sheet)) {
    if (!is.na(suppressWarnings(as.numeric(sheet[[column]][[row]])))) {
      sheet <- bump_cell(sheet, row, column)
    }
  }
  sheet
}))
stopifnot(
  nrow(issues_matching(revenue_off, "warning", "Total Revenue validation failed")) == 1L,
  failed_rows(revenue_off) > failed_rows(clean_monthly),
  # The finance figures are still used.
  nrow(revenue_off$finance) == nrow(clean_monthly$finance)
)

# A sheet without its DISCOUNTS heading: a warning, finance still imported.
no_discounts <- load_monthly(edit_aquatics(function(sheet) {
  blank_labels(sheet, which(sheet_labels(sheet) == "discounts"))
}))
stopifnot(
  nrow(issues_matching(no_discounts, "warning", "The DISCOUNTS section was not found")) == 1L,
  nrow(no_discounts$finance) == nrow(clean_monthly$finance)
)

# A sheet without INCOME and EXPENSES headings is skipped with a warning;
# the other sheets are still imported.
no_finance <- load_monthly(edit_aquatics(function(sheet) {
  blank_labels(sheet, which(sheet_labels(sheet) %in% c("income", "expenses")))
}))
stopifnot(
  nrow(issues_matching(no_finance, "warning", "Sheet was skipped")) == 1L,
  !any(grepl("Aquatics", no_finance$finance$source_sheet)),
  nrow(no_finance$finance) > 0L
)

# No Finance Summary sheet: a warning.
no_summary <- load_monthly(function(workbook) {
  keep <- normalise_excel_label(names(workbook$sheets)) != "monash sport - finance summary"
  workbook$sheets <- workbook$sheets[keep]
  workbook$workbook_sheets <- workbook$workbook_sheets[
    normalise_excel_label(workbook$workbook_sheets) != "monash sport - finance summary"
  ]
  workbook
})
stopifnot(nrow(issues_matching(no_summary, "warning", "Finance Summary sheet was not found")) == 1L)

# No sheet with finance sections at all: an error, and nothing is imported.
no_sheets <- load_monthly(function(workbook) {
  for (name in names(workbook$sheets)) {
    sheet <- workbook$sheets[[name]]
    workbook$sheets[[name]] <- blank_labels(sheet, which(sheet_labels(sheet) %in% c("income", "expenses")))
  }
  workbook
})
stopifnot(
  nrow(issues_matching(no_sheets, "error", "No sheet contained the expected INCOME and EXPENSES")) == 1L,
  nrow(no_sheets$finance) == 0L
)

# A gap in the years: 2022 and 2024 without 2023.
gap <- load_monthly(paths = c(
  sample_path("Monthly Site Report", "Monthly Site Report 2022 -Dec.xlsx"),
  monthly_file
))
stopifnot(nrow(issues_matching(gap, "warning", "missing year(s): 2023")) == 1L)
cat("PASS Monthly Site Report checks: Total Revenue, missing sections and sheets, year gaps\n")

# Detailed Site Report -----------------------------------------------------

site_file <- sample_path("Business Unit Templates that feed master reports", "2025", "Aquatics Site Report 2025.xlsx")
load_site <- function(tamper = identity, paths = site_file, names = basename(paths)) {
  with_tampered_workbook("read_business_unit_template_file", tamper,
    load_business_unit_template_reports(paths = paths, source_names = names, strict = FALSE))
}
edit_sheet <- function(field, edit) {
  function(workbook) {
    workbook$sheets[[field]] <- edit(workbook$sheets[[field]])
    workbook
  }
}

failed_checks <- function(result, dataset) {
  sum(result$finance_checks$dataset == dataset & result$finance_checks$status == "fail", na.rm = TRUE)
}

clean_site <- load_site()
stopifnot(
  !any(clean_site$validation$level == "error"),
  nrow(issues_matching(clean_site, "warning", "missing month row")) == 0L
)

# January Actual Income changed: Actual - Budget no longer equals Variance
# and the months no longer add up to the total.
actual_off <- load_site(edit_sheet("finance", function(sheet) {
  labels <- sheet_labels(sheet)
  income <- which(labels == "income")[[1L]]
  january <- which(labels == "jan" & seq_along(labels) > income)[[1L]]
  bump_cell(sheet, january, column_labelled(sheet, income, "actual"))
}))
stopifnot(
  nrow(issues_matching(actual_off, "warning", "Finance validation failed")) == 1L,
  failed_checks(actual_off, "Finance") > failed_checks(clean_site, "Finance")
)

# Reported Total Revenue off.
site_revenue_off <- load_site(edit_sheet("finance", function(sheet) {
  row <- which(grepl("^total revenue.*actual.*opp", sheet_labels(sheet)))[[1L]]
  for (column in 2:ncol(sheet)) {
    if (!is.na(suppressWarnings(as.numeric(sheet[[column]][[row]])))) {
      sheet <- bump_cell(sheet, row, column)
    }
  }
  sheet
}))
stopifnot(
  nrow(issues_matching(site_revenue_off, "warning", "Total Revenue validation failed")) == 1L,
  sum(site_revenue_off$total_revenue$validation_status == "fail", na.rm = TRUE) >
    sum(clean_site$total_revenue$validation_status == "fail", na.rm = TRUE)
)

# March missing from the Finance sheet.
no_march <- load_site(edit_sheet("finance", function(sheet) {
  months <- vapply(sheet[[1]], parse_month_number, integer(1))
  blank_labels(sheet, which(months %in% 3L))
}))
stopifnot(nrow(issues_matching(no_march, "warning", "missing month row(s): Mar")) == 1L)

# No DISCOUNTS section: an error, so the file is not used.
site_no_discounts <- load_site(edit_sheet("finance", function(sheet) {
  blank_labels(sheet, which(sheet_labels(sheet) == "discounts"))
}))
stopifnot(
  nrow(issues_matching(site_no_discounts, "error", "missing section(s): discounts")) == 1L,
  nrow(site_no_discounts$finance) == 0L
)

# February's heading on Direct Visits changed to January: an error.
repeated_month <- load_site(edit_sheet("direct_visits", function(sheet) {
  header <- find_business_unit_visit_header(sheet)
  column <- column_labelled(sheet, header, "february")
  sheet[[column]][[header]] <- "January"
  recache(sheet)
}))
stopifnot(
  nrow(issues_matching(repeated_month, "error", "Direct Visits contains duplicate month header(s): Jan")) == 1L,
  nrow(repeated_month$visits) == 0L
)
cat("PASS Detailed Site Report checks: Finance and Total Revenue totals, missing months and sections, repeated months\n")

# Venues Tracking Report ---------------------------------------------------

venues_file <- sample_path("Business Unit Tracking Reports", "2025 Venues Tracking Report.xlsx")
load_venues <- function(tamper = identity) {
  with_tampered_workbook("read_venues_tracking_report_file", tamper,
    load_venues_tracking_reports(paths = venues_file, source_names = basename(venues_file), strict = FALSE))
}
failed_finance_checks <- function(result) sum(result$checks$status == "fail", na.rm = TRUE)

clean_venues <- load_venues()

# January's Indoor Actual Income changed: the MTD Total no longer adds up.
indoor_off <- load_venues(function(workbook) {
  sheet <- workbook$finance_sheet
  labels <- sheet_labels(sheet)
  header <- which(labels == "funds")[[1L]]
  indoor <- which(labels == "indoor" & seq_along(labels) > header)[[1L]]
  workbook$finance_sheet <- bump_cell(sheet, indoor, column_labelled(sheet, header, "actuals"))
  workbook
})
stopifnot(
  failed_finance_checks(indoor_off) > failed_finance_checks(clean_venues),
  nrow(issues_matching(indoor_off, "warning", "Venues finance validation failed")) >= 1L
)
cat("PASS Venues Tracking Report checks: finance totals\n")

# A new business unit -------------------------------------------------------

# A copy of a site report for a unit the dashboard has not seen before,
# named and titled "Tennis", reaches the Site Detail and Site Comparison
# choices and its figures can be filtered by that name.
tennis_name <- "Tennis Site Report 2025.xlsx"
with_tennis <- load_site(
  function(workbook) {
    if (!identical(workbook$source_file, tennis_name)) return(workbook)
    sheet <- workbook$sheets$finance
    for (row in seq_len(min(3L, nrow(sheet)))) {
      for (column in seq_len(ncol(sheet))) {
        if (identical(cell_as_text(sheet, row, column), "AQUATICS (CLAYTON)")) {
          sheet[[column]][[row]] <- "TENNIS"
        }
      }
    }
    workbook$sheets$finance <- recache(sheet)
    # The unit is resolved when the file is read; read it again as Tennis.
    workbook$report_unit <- resolve_business_unit_report_name(tennis_name, workbook$sheets$finance)$unit
    workbook$report_unit_note <- NULL
    workbook$campus <- business_unit_template_campus(workbook$report_unit)
    workbook
  },
  paths = c(site_file, site_file),
  names = c(basename(site_file), tennis_name)
)
detail_sites <- site_detail_available_sites(with_tennis, year = 2025)
tennis_visits <- site_detail_filter_rows(with_tennis$visits, year = 2025, site = "Tennis")
stopifnot(
  !any(with_tennis$validation$level == "error"),
  setequal(unique(with_tennis$finance$report_unit), c("Aquatics", "Tennis")),
  "Tennis" %in% detail_sites,
  any(grepl("Aquatics", detail_sites)),
  "Tennis" %in% site_comparison_available_sites(with_tennis),
  nrow(tennis_visits) > 0L,
  all(tennis_visits$report_unit == "Tennis"),
  site_detail_visit_kpis(tennis_visits)[["total"]] > 0
)
cat("PASS a new business unit reaches the Site Detail and Site Comparison choices\n")

cat("PASS test_data_checks.R\n")
