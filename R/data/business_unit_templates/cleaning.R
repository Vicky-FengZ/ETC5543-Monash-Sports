# -------------------------------------------------------------------------
# Business Unit Template report cleaning
# -------------------------------------------------------------------------

empty_business_unit_template_finance_table <- function() {

  data.frame(
    report_year = integer(),
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    period_type = character(),
    report_unit = character(),
    campus = character(),
    site_code = character(),
    business_unit = character(),
    raw_business_unit = character(),
    fund_code = character(),
    is_summary = logical(),
    metric = character(),
    value_type = character(),
    value = double(),
    source_file = character(),
    source_sheet = character(),
    source_row = integer(),
    source_column = integer(),
    stringsAsFactors = FALSE
  )
}


empty_business_unit_template_discount_table <- function() {

  data.frame(
    report_year = integer(),
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    period_type = character(),
    report_unit = character(),
    campus = character(),
    site_code = character(),
    business_unit = character(),
    raw_business_unit = character(),
    fund_code = character(),
    is_summary = logical(),
    discount_type = character(),
    value = double(),
    source_file = character(),
    source_sheet = character(),
    source_row = integer(),
    source_column = integer(),
    stringsAsFactors = FALSE
  )
}


empty_business_unit_template_total_revenue_table <- function() {

  data.frame(
    report_year = integer(),
    period_end = as.Date(character()),
    report_unit = character(),
    campus = character(),
    site_code = character(),
    business_unit = character(),
    raw_business_unit = character(),
    fund_code = character(),
    is_summary = logical(),
    actual_income = double(),
    discount_opportunity = double(),
    calculated_total_revenue = double(),
    reported_total_revenue = double(),
    validation_difference = double(),
    validation_relative_difference = double(),
    validation_tolerance = double(),
    validation_status = character(),
    source_file = character(),
    source_sheet = character(),
    source_row = integer(),
    source_column = integer(),
    stringsAsFactors = FALSE
  )
}


empty_business_unit_template_visit_table <- function() {

  data.frame(
    report_year = integer(),
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    period_type = character(),
    report_unit = character(),
    campus = character(),
    site_code = character(),
    visit_type = character(),
    activity = character(),
    row_type = character(),
    audience = character(),
    value = double(),
    source_file = character(),
    source_sheet = character(),
    source_row = integer(),
    source_column = integer(),
    stringsAsFactors = FALSE
  )
}


empty_business_unit_template_member_table <- function() {

  data.frame(
    report_year = integer(),
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    report_unit = character(),
    campus = character(),
    site_code = character(),
    dataset_type = character(),
    metric = character(),
    raw_metric = character(),
    metric_type = character(),
    value = double(),
    source_file = character(),
    source_sheet = character(),
    source_row = integer(),
    source_column = integer(),
    stringsAsFactors = FALSE
  )
}


business_unit_template_fund_code <- function(value) {

  if (length(value) == 0L || is.na(value)) {
    return(NA_character_)
  }
  match <- regexpr("\\([0-9]+\\)", value)
  if (match[[1]] < 0L) {
    return(NA_character_)
  }
  gsub("[()]", "", regmatches(value, match))
}


business_unit_template_business_unit <- function(value) {

  if (length(value) == 0L || is.na(value)) {
    return(NA_character_)
  }
  trimws(
    gsub(
      "[[:space:]]*\\([0-9]+\\)[[:space:]]*$",
      "",
      as.character(value)
    )
  )
}


standardise_business_unit_finance_rows <- function(base, workbook) {

  if (!is.data.frame(base) || nrow(base) == 0L) {
    return(empty_business_unit_template_finance_table())
  }
  raw_business_unit <- as.character(base$business_unit)

  data.frame(
    report_year = as.integer(base$report_year),
    date = as.Date(base$date),
    month = as.integer(base$month),
    month_label = as.character(base$month_label),
    period_type = "Month",
    report_unit = workbook$report_unit,
    campus = workbook$campus,
    site_code = workbook$site_code,
    business_unit = vapply(
      raw_business_unit,
      business_unit_template_business_unit,
      character(1)
    ),
    raw_business_unit = raw_business_unit,
    fund_code = vapply(
      raw_business_unit,
      business_unit_template_fund_code,
      character(1)
    ),
    is_summary = base$is_summary,
    metric = as.character(base$metric),
    value_type = as.character(base$value_type),
    value = as.numeric(base$value),
    source_file = workbook$source_file,
    source_sheet = workbook$sheet_names$finance,
    source_row = as.integer(base$source_row),
    source_column = as.integer(base$source_column),
    stringsAsFactors = FALSE
  )
}


extract_business_unit_finance_reported_totals <- function(workbook) {

  raw_sheet <- workbook$sheets$finance
  first_column <- first_column_labels(raw_sheet)
  records <- list()

  for (section_label in c("income", "expenses")) {
    header_rows <- which(first_column == section_label)
    if (length(header_rows) == 0L) {
      next
    }
    header_row <- header_rows[[1]]
    candidate_totals <- which(
      first_column == "total" &
        seq_along(first_column) > header_row
    )
    if (length(candidate_totals) == 0L) {
      next
    }
    total_row <- candidate_totals[[1]]
    header_labels <- vapply(
      seq_len(ncol(raw_sheet)),
      function(column) normalise_excel_label(
        cell_as_text(raw_sheet, header_row, column)
      ),
      character(1)
    )
    value_type_map <- c(
      budget = "Budget",
      actual = "Actual",
      actuals = "Actual",
      variance = "Variance"
    )
    value_columns <- which(header_labels %in% names(value_type_map))
    group_headers <- fill_group_headers(raw_sheet, header_row)

    for (source_column in value_columns) {
      raw_business_unit <- group_headers[[source_column]]
      if (is.na(raw_business_unit)) {
        raw_business_unit <- "Unlabelled"
      }
      records[[length(records) + 1L]] <- data.frame(
        report_year = as.integer(workbook$report_year),
        date = as.Date(NA),
        month = NA_integer_,
        month_label = "Reported Total",
        period_type = "Reported Total",
        report_unit = workbook$report_unit,
        campus = workbook$campus,
        site_code = workbook$site_code,
        business_unit = business_unit_template_business_unit(
          raw_business_unit
        ),
        raw_business_unit = raw_business_unit,
        fund_code = business_unit_template_fund_code(raw_business_unit),
        is_summary = normalise_excel_label(raw_business_unit) ==
          "ytd summary",
        metric = if (section_label == "income") "Income" else "Expenses",
        value_type = unname(value_type_map[[header_labels[[source_column]]]]),
        value = cell_as_number(raw_sheet, total_row, source_column),
        source_file = workbook$source_file,
        source_sheet = workbook$sheet_names$finance,
        source_row = as.integer(total_row),
        source_column = as.integer(source_column),
        stringsAsFactors = FALSE
      )
    }
  }

  profit_rows <- which(first_column %in% c("total p/l", "profit/loss"))
  income_rows <- which(first_column == "income")

  if (length(profit_rows) > 0L && length(income_rows) > 0L) {
    profit_row <- profit_rows[[1]]
    header_row <- income_rows[[1]]
    header_labels <- vapply(
      seq_len(ncol(raw_sheet)),
      function(column) normalise_excel_label(
        cell_as_text(raw_sheet, header_row, column)
      ),
      character(1)
    )
    value_type_map <- c(
      budget = "Budget",
      actual = "Actual",
      actuals = "Actual",
      variance = "Variance"
    )
    value_columns <- which(header_labels %in% names(value_type_map))
    group_headers <- fill_group_headers(raw_sheet, header_row)

    for (source_column in value_columns) {
      raw_business_unit <- group_headers[[source_column]]
      if (is.na(raw_business_unit)) {
        raw_business_unit <- "Unlabelled"
      }
      records[[length(records) + 1L]] <- data.frame(
        report_year = as.integer(workbook$report_year),
        date = as.Date(NA),
        month = NA_integer_,
        month_label = "Reported Total",
        period_type = "Reported Total",
        report_unit = workbook$report_unit,
        campus = workbook$campus,
        site_code = workbook$site_code,
        business_unit = business_unit_template_business_unit(
          raw_business_unit
        ),
        raw_business_unit = raw_business_unit,
        fund_code = business_unit_template_fund_code(raw_business_unit),
        is_summary = normalise_excel_label(raw_business_unit) ==
          "ytd summary",
        metric = "Profit/Loss",
        value_type = unname(value_type_map[[header_labels[[source_column]]]]),
        value = cell_as_number(raw_sheet, profit_row, source_column),
        source_file = workbook$source_file,
        source_sheet = workbook$sheet_names$finance,
        source_row = as.integer(profit_row),
        source_column = as.integer(source_column),
        stringsAsFactors = FALSE
      )
    }
  }

  bind_import_tables(
    records,
    empty_business_unit_template_finance_table()
  )
}


clean_business_unit_template_finance <- function(workbook) {

  raw_sheet <- workbook$sheets$finance
  monthly_base <- bind_import_tables(
    lapply(
      c("INCOME", "EXPENSES"),
      function(section_label) {
        extract_monthly_finance_section(
          raw_sheet = raw_sheet,
          report_year = workbook$report_year,
          source_file = workbook$source_file,
          sheet_name = workbook$sheet_names$finance,
          section_label = section_label
        )
      }
    ),
    empty_monthly_finance_table()
  )
  monthly <- standardise_business_unit_finance_rows(monthly_base, workbook)
  reported_totals <- extract_business_unit_finance_reported_totals(workbook)
  result <- bind_import_tables(
    list(monthly, reported_totals),
    empty_business_unit_template_finance_table()
  )

  if (nrow(result) > 0L) {
    result <- result[
      order(
        result$report_year,
        is.na(result$month),
        result$month,
        result$report_unit,
        result$is_summary,
        result$business_unit,
        result$metric,
        result$value_type
      ),
      ,
      drop = FALSE
    ]
    rownames(result) <- NULL
  }
  result
}


standardise_business_unit_discount_rows <- function(base, workbook) {

  if (!is.data.frame(base) || nrow(base) == 0L) {
    return(empty_business_unit_template_discount_table())
  }
  raw_business_unit <- as.character(base$business_unit)

  data.frame(
    report_year = as.integer(base$report_year),
    date = as.Date(base$date),
    month = as.integer(base$month),
    month_label = as.character(base$month_label),
    period_type = "Month",
    report_unit = workbook$report_unit,
    campus = workbook$campus,
    site_code = workbook$site_code,
    business_unit = vapply(
      raw_business_unit,
      business_unit_template_business_unit,
      character(1)
    ),
    raw_business_unit = raw_business_unit,
    fund_code = vapply(
      raw_business_unit,
      business_unit_template_fund_code,
      character(1)
    ),
    is_summary = base$is_summary,
    discount_type = as.character(base$discount_type),
    value = as.numeric(base$value),
    source_file = workbook$source_file,
    source_sheet = workbook$sheet_names$finance,
    source_row = as.integer(base$source_row),
    source_column = as.integer(base$source_column),
    stringsAsFactors = FALSE
  )
}


extract_business_unit_discount_reported_totals <- function(workbook) {

  raw_sheet <- workbook$sheets$finance
  first_column <- first_column_labels(raw_sheet)
  header_rows <- which(first_column == "discounts")
  if (length(header_rows) == 0L) {
    return(empty_business_unit_template_discount_table())
  }
  header_row <- header_rows[[1]]
  total_rows <- which(
    first_column == "total" &
      seq_along(first_column) > header_row
  )
  if (length(total_rows) == 0L) {
    return(empty_business_unit_template_discount_table())
  }
  total_row <- total_rows[[1]]
  header_labels <- vapply(
    seq_len(ncol(raw_sheet)),
    function(column) normalise_excel_label(
      cell_as_text(raw_sheet, header_row, column)
    ),
    character(1)
  )
  value_columns <- discount_value_columns(header_labels)
  group_headers <- fill_group_headers(raw_sheet, header_row)
  records <- list()

  for (source_column in value_columns) {
    raw_business_unit <- group_headers[[source_column]]
    if (is.na(raw_business_unit)) {
      raw_business_unit <- "Unlabelled"
    }
    records[[length(records) + 1L]] <- data.frame(
      report_year = as.integer(workbook$report_year),
      date = as.Date(NA),
      month = NA_integer_,
      month_label = "Reported Total",
      period_type = "Reported Total",
      report_unit = workbook$report_unit,
      campus = workbook$campus,
      site_code = workbook$site_code,
      business_unit = business_unit_template_business_unit(raw_business_unit),
      raw_business_unit = raw_business_unit,
      fund_code = business_unit_template_fund_code(raw_business_unit),
      is_summary = normalise_excel_label(raw_business_unit) == "ytd summary",
      discount_type = canonical_discount_type(
        cell_as_text(raw_sheet, header_row, source_column)
      ),
      value = cell_as_number(raw_sheet, total_row, source_column),
      source_file = workbook$source_file,
      source_sheet = workbook$sheet_names$finance,
      source_row = as.integer(total_row),
      source_column = as.integer(source_column),
      stringsAsFactors = FALSE
    )
  }

  bind_import_tables(
    records,
    empty_business_unit_template_discount_table()
  )
}


clean_business_unit_template_discounts <- function(workbook) {

  raw_sheet <- workbook$sheets$finance
  base <- extract_monthly_discount_section(
    raw_sheet = raw_sheet,
    report_year = workbook$report_year,
    source_file = workbook$source_file,
    sheet_name = workbook$sheet_names$finance
  )
  monthly <- standardise_business_unit_discount_rows(base, workbook)
  reported_totals <- extract_business_unit_discount_reported_totals(workbook)
  result <- bind_import_tables(
    list(monthly, reported_totals),
    empty_business_unit_template_discount_table()
  )
  if (nrow(result) > 0L) {
    result <- result[
      order(
        result$report_year,
        is.na(result$month),
        result$month,
        result$report_unit,
        result$is_summary,
        result$business_unit,
        result$discount_type
      ),
      ,
      drop = FALSE
    ]
    rownames(result) <- NULL
  }
  result
}


clean_business_unit_template_total_revenue <- function(
  workbook,
  monthly_finance_base,
  monthly_discount_base,
  validation_tolerance
) {

  base <- extract_monthly_total_revenue_section(
    raw_sheet = workbook$sheets$finance,
    report_year = workbook$report_year,
    source_file = workbook$source_file,
    sheet_name = workbook$sheet_names$finance,
    finance = monthly_finance_base,
    discounts = monthly_discount_base,
    validation_tolerance = validation_tolerance
  )
  if (nrow(base) == 0L) {
    return(empty_business_unit_template_total_revenue_table())
  }
  raw_business_unit <- as.character(base$business_unit)

  result <- data.frame(
    report_year = as.integer(base$report_year),
    period_end = as.Date(base$period_end),
    report_unit = workbook$report_unit,
    campus = workbook$campus,
    site_code = workbook$site_code,
    business_unit = vapply(
      raw_business_unit,
      business_unit_template_business_unit,
      character(1)
    ),
    raw_business_unit = raw_business_unit,
    fund_code = vapply(
      raw_business_unit,
      business_unit_template_fund_code,
      character(1)
    ),
    is_summary = base$is_summary,
    actual_income = as.numeric(base$actual_income),
    discount_opportunity = as.numeric(base$discount_opportunity),
    calculated_total_revenue = as.numeric(base$calculated_total_revenue),
    reported_total_revenue = as.numeric(base$reported_total_revenue),
    validation_difference = as.numeric(base$validation_difference),
    validation_relative_difference = as.numeric(
      base$validation_relative_difference
    ),
    validation_tolerance = as.numeric(base$validation_tolerance),
    validation_status = as.character(base$validation_status),
    source_file = workbook$source_file,
    source_sheet = workbook$sheet_names$finance,
    source_row = as.integer(base$source_row),
    source_column = as.integer(base$source_column),
    stringsAsFactors = FALSE
  )

  # These finance templates use blank discount cells to represent zero.
  # Treating them as zero keeps the independent Total Revenue calculation
  # consistent with the workbook convention while preserving genuinely
  # missing Actual Income and reported Total Revenue values.
  result$discount_opportunity[is.na(result$discount_opportunity)] <- 0
  result$calculated_total_revenue <- ifelse(
    is.na(result$actual_income),
    NA_real_,
    result$actual_income + result$discount_opportunity
  )
  result$validation_difference <- ifelse(
    is.na(result$reported_total_revenue) |
      is.na(result$calculated_total_revenue),
    NA_real_,
    result$reported_total_revenue - result$calculated_total_revenue
  )
  result$validation_relative_difference <- ifelse(
    is.na(result$validation_difference) |
      is.na(result$calculated_total_revenue) |
      result$calculated_total_revenue == 0,
    NA_real_,
    result$validation_difference / result$calculated_total_revenue
  )
  result$validation_status <- ifelse(
    is.na(result$reported_total_revenue),
    "missing_reported_value",
    ifelse(
      is.na(result$actual_income),
      "missing_actual_income",
      ifelse(
        abs(result$validation_difference) <= result$validation_tolerance,
        "pass",
        "fail"
      )
    )
  )

  result
}


business_unit_template_month_from_label <- function(value) {

  key <- normalise_excel_label(value)
  if (!nzchar(key)) {
    return(NA_integer_)
  }
  key <- substr(gsub("[^a-z]", "", key), 1L, 3L)
  match(key, tolower(month.abb))
}


find_business_unit_visit_header <- function(raw_sheet) {

  find_month_header_row(raw_sheet, 10L)
}


business_unit_visit_periods <- function(raw_sheet, header_row) {

  if (is.na(header_row)) {
    return(
      data.frame(
        period_type = character(),
        month = integer(),
        start_column = integer(),
        stringsAsFactors = FALSE
      )
    )
  }
  records <- list()

  for (source_column in seq_len(ncol(raw_sheet))) {
    label <- cell_as_text(raw_sheet, header_row, source_column)
    month <- business_unit_template_month_from_label(label)
    key <- normalise_excel_label(label)

    if (!is.na(month)) {
      records[[length(records) + 1L]] <- data.frame(
        period_type = "Month",
        month = as.integer(month),
        start_column = as.integer(source_column),
        stringsAsFactors = FALSE
      )
    } else if (key %in% c("ytd", "ytd total")) {
      records[[length(records) + 1L]] <- data.frame(
        period_type = "YTD",
        month = NA_integer_,
        start_column = as.integer(source_column),
        stringsAsFactors = FALSE
      )
    }
  }

  bind_import_tables(
    records,
    data.frame(
      period_type = character(),
      month = integer(),
      start_column = integer(),
      stringsAsFactors = FALSE
    )
  )
}


clean_business_unit_template_visits <- function(
  workbook,
  sheet_field,
  visit_type
) {

  raw_sheet <- workbook$sheets[[sheet_field]]
  sheet_name <- workbook$sheet_names[[sheet_field]]
  if (!is.list(raw_sheet) || nrow(raw_sheet) == 0L) {
    return(empty_business_unit_template_visit_table())
  }
  header_row <- find_business_unit_visit_header(raw_sheet)
  periods <- business_unit_visit_periods(raw_sheet, header_row)
  audience_row <- header_row + 1L
  audience_map <- c(
    student = "Student",
    students = "Student",
    staff = "Staff",
    community = "Community",
    total = "Total"
  )
  records <- list()

  for (source_row in seq.int(audience_row + 1L, nrow(raw_sheet))) {
    raw_activity <- cell_as_text(raw_sheet, source_row, 1L)
    activity_key <- normalise_excel_label(raw_activity)

    if (grepl("^assumptions used", activity_key)) {
      break
    }
    if (!nzchar(activity_key)) {
      next
    }
    row_type <- if (activity_key == "total") "Total" else "Activity"
    activity <- if (row_type == "Total") "Total" else trimws(raw_activity)

    for (period_index in seq_len(nrow(periods))) {
      period <- periods[period_index, , drop = FALSE]
      start_column <- period$start_column[[1]]
      end_column <- if (period_index < nrow(periods)) {
        periods$start_column[[period_index + 1L]] - 1L
      } else {
        ncol(raw_sheet)
      }
      candidate_columns <- seq.int(start_column, end_column)

      for (source_column in candidate_columns) {
        raw_audience <- cell_as_text(
          raw_sheet,
          audience_row,
          source_column
        )
        audience_key <- normalise_excel_label(raw_audience)
        if (!nzchar(audience_key)) {
          next
        }
        period_type <- period$period_type[[1]]
        month <- period$month[[1]]
        records[[length(records) + 1L]] <- list(
          report_year = as.integer(workbook$report_year),
          date = if (period_type == "Month") {
            as.Date(sprintf("%04d-%02d-01", workbook$report_year, month))
          } else {
            as.Date(NA)
          },
          month = as.integer(month),
          month_label = if (period_type == "Month") {
            month.abb[[month]]
          } else {
            "YTD"
          },
          period_type = period_type,
          report_unit = workbook$report_unit,
          campus = workbook$campus,
          site_code = workbook$site_code,
          visit_type = visit_type,
          activity = activity,
          row_type = row_type,
          audience = if (audience_key %in% names(audience_map)) {
            unname(audience_map[[audience_key]])
          } else {
            trimws(raw_audience)
          },
          value = cell_as_number(raw_sheet, source_row, source_column),
          source_file = workbook$source_file,
          source_sheet = sheet_name,
          source_row = as.integer(source_row),
          source_column = as.integer(source_column)
        )
      }
    }

    if (row_type == "Total") {
      break
    }
  }

  result <- records_to_table(records,
    empty_business_unit_template_visit_table()
  )
  if (nrow(result) > 0L) {
    result <- result[
      order(
        result$report_year,
        result$report_unit,
        result$visit_type,
        is.na(result$month),
        result$month,
        result$row_type,
        result$activity,
        result$audience
      ),
      ,
      drop = FALSE
    ]
    rownames(result) <- NULL
  }
  result
}


canonical_business_unit_member_metric <- function(value) {

  key <- normalise_excel_label(value)
  replacements <- c(
    "target" = "Target",
    "new members" = "New Members",
    "new student members" = "New Student Members",
    "new staff members" = "New Staff Members",
    "new community members" = "New Community Members",
    "member expiring" = "Expired/Cancelled",
    "member cancelled" = "Expired/Cancelled",
    "members cancelled" = "Expired/Cancelled",
    "total active" = "Total Active",
    "active student members" = "Active Student Members",
    "% of total membership" = "Membership Share",
    "% of toal membership" = "Membership Share",
    "% of total membership (students)" = "Student Share",
    "% of total membership (staff)" = "Staff Share",
    "% of total membership (community)" = "Community Share",
    "active staff members" = "Active Staff Members",
    "active community members" = "Active Community Members",
    "free trials" = "Free Trials",
    "students" = "Students",
    "staff" = "Staff",
    "community" = "Community",
    "total enrollments" = "Total Enrollments"
  )

  if (key %in% names(replacements)) {
    return(unname(replacements[[key]]))
  }
  trimws(as.character(value))
}


clean_business_unit_template_member_sheet <- function(
  workbook,
  sheet_field,
  dataset_type
) {

  raw_sheet <- workbook$sheets[[sheet_field]]
  sheet_name <- workbook$sheet_names[[sheet_field]]
  if (!is.data.frame(raw_sheet) || nrow(raw_sheet) == 0L) {
    return(empty_business_unit_template_member_table())
  }
  first_column <- first_column_labels(raw_sheet)
  header_rows <- which(first_column == "month")
  if (length(header_rows) == 0L) {
    return(empty_business_unit_template_member_table())
  }
  records <- list()

  for (header_index in seq_along(header_rows)) {
    header_row <- header_rows[[header_index]]
    block_end <- if (header_index < length(header_rows)) {
      header_rows[[header_index + 1L]] - 1L
    } else {
      nrow(raw_sheet)
    }
    metric_columns <- which(
      vapply(
        seq_len(ncol(raw_sheet)),
        function(source_column) {
          !is.na(cell_as_text(raw_sheet, header_row, source_column))
        },
        logical(1)
      )
    )
    metric_columns <- setdiff(metric_columns, 1L)

    for (source_row in seq.int(header_row + 1L, block_end)) {
      month <- parse_month_number(raw_sheet[[1]][source_row])
      if (is.na(month)) {
        next
      }

      for (source_column in metric_columns) {
        raw_metric <- cell_as_text(raw_sheet, header_row, source_column)
        metric <- canonical_business_unit_member_metric(raw_metric)
        if (metric == "Membership Share" && source_column > 1L) {
          preceding_metric <- canonical_business_unit_member_metric(
            cell_as_text(raw_sheet, header_row, source_column - 1L)
          )
          metric <- switch(
            preceding_metric,
            "Active Student Members" = "Student Share",
            "Active Staff Members" = "Staff Share",
            "Active Community Members" = "Community Share",
            metric
          )
        }
        metric_type <- import_metric_definition(
          "site_detail", sheet_field, metric
        )$unit[[1L]]
        records[[length(records) + 1L]] <- data.frame(
          report_year = as.integer(workbook$report_year),
          date = as.Date(sprintf("%04d-%02d-01", workbook$report_year, month)),
          month = as.integer(month),
          month_label = month.abb[[month]],
          report_unit = workbook$report_unit,
          campus = workbook$campus,
          site_code = workbook$site_code,
          dataset_type = dataset_type,
          metric = metric,
          raw_metric = raw_metric,
          metric_type = metric_type,
          value = cell_as_number(raw_sheet, source_row, source_column),
          source_file = workbook$source_file,
          source_sheet = sheet_name,
          source_row = as.integer(source_row),
          source_column = as.integer(source_column),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  result <- bind_import_tables(
    records,
    empty_business_unit_template_member_table()
  )
  if (nrow(result) > 0L) {
    result <- result[
      order(
        result$report_year,
        result$report_unit,
        result$month,
        result$metric
      ),
      ,
      drop = FALSE
    ]
    rownames(result) <- NULL
  }
  result
}


clean_business_unit_template_workbook <- function(
  workbook,
  validation_tolerance = 0.01
) {

  raw_finance <- workbook$sheets$finance
  monthly_finance_base <- bind_import_tables(
    lapply(
      c("INCOME", "EXPENSES"),
      function(section_label) {
        extract_monthly_finance_section(
          raw_sheet = raw_finance,
          report_year = workbook$report_year,
          source_file = workbook$source_file,
          sheet_name = workbook$sheet_names$finance,
          section_label = section_label
        )
      }
    ),
    empty_monthly_finance_table()
  )
  monthly_discount_base <- extract_monthly_discount_section(
    raw_sheet = raw_finance,
    report_year = workbook$report_year,
    source_file = workbook$source_file,
    sheet_name = workbook$sheet_names$finance
  )

  list(
    finance = clean_business_unit_template_finance(workbook),
    discounts = clean_business_unit_template_discounts(workbook),
    total_revenue = clean_business_unit_template_total_revenue(
      workbook = workbook,
      monthly_finance_base = monthly_finance_base,
      monthly_discount_base = monthly_discount_base,
      validation_tolerance = validation_tolerance
    ),
    visits = bind_import_tables(
      list(
        clean_business_unit_template_visits(
          workbook,
          "direct_visits",
          "Direct"
        ),
        clean_business_unit_template_visits(
          workbook,
          "indirect_visits",
          "Indirect"
        )
      ),
      empty_business_unit_template_visit_table()
    ),
    memberships = clean_business_unit_template_member_sheet(
      workbook,
      "memberships",
      "Membership"
    ),
    enrollments = clean_business_unit_template_member_sheet(
      workbook,
      "enrollments",
      "Enrollment"
    )
  )
}
