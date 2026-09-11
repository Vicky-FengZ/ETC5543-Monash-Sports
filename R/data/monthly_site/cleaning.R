# -------------------------------------------------------------------------
# Monthly Site Report cleaning
# -------------------------------------------------------------------------

first_column_labels <- function(raw_sheet) {

  if (!is.data.frame(raw_sheet) || ncol(raw_sheet) == 0L) {
    return(character())
  }

  normalise_excel_label(raw_sheet[[1]])
}


sheet_has_finance_sections <- function(raw_sheet) {

  labels <- first_column_labels(raw_sheet)

  all(c("income", "expenses") %in% labels)
}


sheet_has_discount_section <- function(raw_sheet) {

  "discounts" %in% first_column_labels(raw_sheet)
}


sheet_has_total_revenue_section <- function(raw_sheet) {

  labels <- first_column_labels(raw_sheet)

  any(grepl("^total revenue.*actual.*opp", labels))
}


empty_monthly_finance_table <- function() {

  data.frame(
    report_year = integer(),
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    site = character(),
    site_code = character(),
    business_unit = character(),
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


empty_monthly_discount_table <- function() {

  data.frame(
    report_year = integer(),
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    site = character(),
    site_code = character(),
    business_unit = character(),
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


empty_monthly_total_revenue_table <- function() {

  data.frame(
    report_year = integer(),
    period_end = as.Date(character()),
    site = character(),
    site_code = character(),
    business_unit = character(),
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


canonical_site_name <- function(sheet_name) {

  clean_name <- trimws(sheet_name)
  clean_name <- trimws(
    sub(
      "[[:space:]]*\\(Z[0-9]+\\)[[:space:]]*$",
      "",
      clean_name,
      ignore.case = TRUE
    )
  )

  name_key <- normalise_excel_label(clean_name)

  known_names <- c(
    "monash sport - finance summary" = "Monash Sport",
    "h&f clayton" = "H&F Clayton",
    "venues clayton" = "Venues Clayton",
    "aquatics clayton" = "Aquatics Clayton",
    "caulfield" = "Caulfield",
    "peninsula" = "Peninsula",
    "participation programs" = "Participation Programs"
  )

  if (name_key %in% names(known_names)) {
    return(unname(known_names[[name_key]]))
  }

  clean_name
}


extract_site_code <- function(sheet_name, raw_sheet) {

  candidates <- c(
    sheet_name,
    if (ncol(raw_sheet) >= 1L) {
      vapply(
        seq_len(min(20L, nrow(raw_sheet))),
        function(row) cell_as_text(raw_sheet, row, 1L),
        character(1)
      )
    }
  )

  candidates <- candidates[!is.na(candidates)]

  for (candidate in candidates) {

    match <- regexpr(
      "Z[0-9]+",
      candidate,
      ignore.case = TRUE
    )

    if (match[[1]] > 0L) {
      return(
        toupper(
          regmatches(candidate, match)
        )
      )
    }
  }

  NA_character_
}


fill_group_headers <- function(raw_sheet, header_row) {

  # INCOME and EXPENSES share the business-unit headings placed immediately
  # above the INCOME section. For EXPENSES, scanning only the nearby rows would
  # incorrectly pick up the INCOME total row and use totals as group names.
  first_column <- first_column_labels(raw_sheet)
  income_rows <- which(
    first_column == "income" &
      seq_along(first_column) <= header_row
  )

  anchor_row <- if (length(income_rows) > 0L) {
    max(income_rows)
  } else {
    header_row
  }

  group_row <- max(1L, anchor_row - 1L)

  result <- rep(NA_character_, ncol(raw_sheet))
  current_group <- NA_character_

  for (column in seq_len(ncol(raw_sheet))) {

    candidate <- cell_as_text(
      raw_sheet,
      group_row,
      column
    )

    if (!is.na(candidate)) {
      current_group <- candidate
    }

    result[[column]] <- current_group
  }

  result
}


extract_monthly_finance_section <- function(
  raw_sheet,
  report_year,
  source_file,
  sheet_name,
  section_label
) {

  first_column <- first_column_labels(raw_sheet)
  header_rows <- which(
    first_column == normalise_excel_label(section_label)
  )

  if (length(header_rows) == 0L) {
    return(empty_monthly_finance_table())
  }

  header_row <- header_rows[[1]]
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

  value_columns <- which(
    header_labels %in% names(value_type_map)
  )

  if (length(value_columns) == 0L) {
    return(empty_monthly_finance_table())
  }

  group_headers <- fill_group_headers(
    raw_sheet,
    header_row
  )

  site <- canonical_site_name(sheet_name)
  site_code <- extract_site_code(
    sheet_name,
    raw_sheet
  )

  rows <- list()

  for (source_row in seq.int(header_row + 1L, nrow(raw_sheet))) {

    row_label <- cell_as_text(
      raw_sheet,
      source_row,
      1L
    )

    if (normalise_excel_label(row_label) == "total") {
      break
    }

    month <- parse_month_number(
      raw_sheet[[1]][source_row]
    )

    if (is.na(month)) {
      next
    }

    for (source_column in value_columns) {

      business_unit <- group_headers[[source_column]]

      if (is.na(business_unit)) {
        business_unit <- "Unlabelled"
      }

      rows[[length(rows) + 1L]] <- data.frame(
        report_year = as.integer(report_year),
        date = as.Date(
          sprintf(
            "%04d-%02d-01",
            report_year,
            month
          )
        ),
        month = as.integer(month),
        month_label = month.abb[[month]],
        site = site,
        site_code = site_code,
        business_unit = business_unit,
        is_summary = normalise_excel_label(business_unit) == "ytd summary",
        metric = if (
          normalise_excel_label(section_label) == "income"
        ) {
          "Income"
        } else {
          "Expenses"
        },
        value_type = unname(
          value_type_map[[header_labels[[source_column]]]]
        ),
        value = cell_as_number(
          raw_sheet,
          source_row,
          source_column
        ),
        source_file = source_file,
        source_sheet = sheet_name,
        source_row = as.integer(source_row),
        source_column = as.integer(source_column),
        stringsAsFactors = FALSE
      )
    }
  }

  bind_import_tables(
    rows,
    empty_monthly_finance_table()
  )
}


extract_monthly_discount_section <- function(
  raw_sheet,
  report_year,
  source_file,
  sheet_name
) {

  first_column <- first_column_labels(raw_sheet)
  header_rows <- which(first_column == "discounts")

  if (length(header_rows) == 0L) {
    return(empty_monthly_discount_table())
  }

  header_row <- header_rows[[1]]
  header_labels <- vapply(
    seq_len(ncol(raw_sheet)),
    function(column) normalise_excel_label(
      cell_as_text(raw_sheet, header_row, column)
    ),
    character(1)
  )

  discount_type_map <- c(
    students = "Student",
    staff = "Staff",
    other = "Other"
  )

  value_columns <- which(
    header_labels %in% names(discount_type_map)
  )

  if (length(value_columns) == 0L) {
    return(empty_monthly_discount_table())
  }

  group_headers <- fill_group_headers(
    raw_sheet,
    header_row
  )

  site <- canonical_site_name(sheet_name)
  site_code <- extract_site_code(
    sheet_name,
    raw_sheet
  )

  rows <- list()

  for (source_row in seq.int(header_row + 1L, nrow(raw_sheet))) {

    row_label <- cell_as_text(
      raw_sheet,
      source_row,
      1L
    )

    if (normalise_excel_label(row_label) == "total") {
      break
    }

    month <- parse_month_number(
      raw_sheet[[1]][source_row]
    )

    if (is.na(month)) {
      next
    }

    for (source_column in value_columns) {

      business_unit <- group_headers[[source_column]]

      if (is.na(business_unit)) {
        business_unit <- "Unlabelled"
      }

      rows[[length(rows) + 1L]] <- data.frame(
        report_year = as.integer(report_year),
        date = as.Date(
          sprintf(
            "%04d-%02d-01",
            report_year,
            month
          )
        ),
        month = as.integer(month),
        month_label = month.abb[[month]],
        site = site,
        site_code = site_code,
        business_unit = business_unit,
        is_summary = normalise_excel_label(business_unit) == "ytd summary",
        discount_type = unname(
          discount_type_map[[header_labels[[source_column]]]]
        ),
        value = cell_as_number(
          raw_sheet,
          source_row,
          source_column
        ),
        source_file = source_file,
        source_sheet = sheet_name,
        source_row = as.integer(source_row),
        source_column = as.integer(source_column),
        stringsAsFactors = FALSE
      )
    }
  }

  bind_import_tables(
    rows,
    empty_monthly_discount_table()
  )
}


sum_available_values <- function(values) {

  values <- suppressWarnings(
    as.numeric(values)
  )

  if (length(values) == 0L || all(is.na(values))) {
    return(NA_real_)
  }

  sum(values, na.rm = TRUE)
}


extract_monthly_total_revenue_section <- function(
  raw_sheet,
  report_year,
  source_file,
  sheet_name,
  finance,
  discounts,
  validation_tolerance = 0.01
) {

  first_column <- first_column_labels(raw_sheet)
  discount_header_rows <- which(first_column == "discounts")
  total_revenue_rows <- which(
    grepl(
      "^total revenue.*actual.*opp",
      first_column
    )
  )

  if (
    length(discount_header_rows) == 0L ||
      length(total_revenue_rows) == 0L
  ) {
    return(empty_monthly_total_revenue_table())
  }

  discount_header_row <- discount_header_rows[[1]]
  total_revenue_row <- total_revenue_rows[[1]]
  header_labels <- vapply(
    seq_len(ncol(raw_sheet)),
    function(column) normalise_excel_label(
      cell_as_text(raw_sheet, discount_header_row, column)
    ),
    character(1)
  )

  # Each business-unit group begins with the Students discount column. The
  # source Total Revenue value is stored in this first column of the group.
  reported_columns <- which(header_labels == "students")

  if (length(reported_columns) == 0L) {
    return(empty_monthly_total_revenue_table())
  }

  group_headers <- fill_group_headers(
    raw_sheet,
    discount_header_row
  )

  site <- canonical_site_name(sheet_name)
  site_code <- extract_site_code(
    sheet_name,
    raw_sheet
  )

  rows <- lapply(
    reported_columns,
    function(source_column) {

      business_unit <- group_headers[[source_column]]

      if (is.na(business_unit)) {
        business_unit <- "Unlabelled"
      }

      finance_rows <-
        finance$business_unit == business_unit &
        finance$metric == "Income" &
        finance$value_type == "Actual"

      discount_rows <-
        discounts$business_unit == business_unit

      actual_income <- sum_available_values(
        finance$value[finance_rows]
      )

      discount_opportunity <- sum_available_values(
        discounts$value[discount_rows]
      )

      calculated_total_revenue <- if (
        is.na(actual_income) || is.na(discount_opportunity)
      ) {
        NA_real_
      } else {
        actual_income + discount_opportunity
      }

      reported_total_revenue <- cell_as_number(
        raw_sheet,
        total_revenue_row,
        source_column
      )

      validation_difference <- if (
        is.na(reported_total_revenue) ||
          is.na(calculated_total_revenue)
      ) {
        NA_real_
      } else {
        reported_total_revenue - calculated_total_revenue
      }

      validation_relative_difference <- if (
        is.na(validation_difference) ||
          is.na(calculated_total_revenue) ||
          calculated_total_revenue == 0
      ) {
        NA_real_
      } else {
        validation_difference / calculated_total_revenue
      }

      validation_status <- if (is.na(reported_total_revenue)) {
        "missing_reported_value"
      } else if (is.na(actual_income)) {
        "missing_actual_income"
      } else if (is.na(discount_opportunity)) {
        "missing_discounts"
      } else if (
        abs(validation_difference) <= validation_tolerance
      ) {
        "pass"
      } else {
        "fail"
      }

      data.frame(
        report_year = as.integer(report_year),
        period_end = as.Date(
          sprintf("%04d-12-31", report_year)
        ),
        site = site,
        site_code = site_code,
        business_unit = business_unit,
        is_summary = normalise_excel_label(business_unit) == "ytd summary",
        actual_income = actual_income,
        discount_opportunity = discount_opportunity,
        calculated_total_revenue = calculated_total_revenue,
        reported_total_revenue = reported_total_revenue,
        validation_difference = validation_difference,
        validation_relative_difference = validation_relative_difference,
        validation_tolerance = as.numeric(validation_tolerance),
        validation_status = validation_status,
        source_file = source_file,
        source_sheet = sheet_name,
        source_row = as.integer(total_revenue_row),
        source_column = as.integer(source_column),
        stringsAsFactors = FALSE
      )
    }
  )

  bind_import_tables(
    rows,
    empty_monthly_total_revenue_table()
  )
}


clean_monthly_site_report_finance <- function(workbook) {

  if (!inherits(workbook, "monthly_site_report_workbook")) {
    stop(
      "workbook must be a monthly_site_report_workbook object.",
      call. = FALSE
    )
  }

  finance_tables <- list()

  for (sheet_name in names(workbook$sheets)) {

    raw_sheet <- workbook$sheets[[sheet_name]]

    if (!sheet_has_finance_sections(raw_sheet)) {
      next
    }

    for (section_label in c("INCOME", "EXPENSES")) {
      finance_tables[[length(finance_tables) + 1L]] <-
        extract_monthly_finance_section(
          raw_sheet = raw_sheet,
          report_year = workbook$report_year,
          source_file = workbook$source_file,
          sheet_name = sheet_name,
          section_label = section_label
        )
    }
  }

  result <- bind_import_tables(
    finance_tables,
    empty_monthly_finance_table()
  )

  if (nrow(result) > 0L) {
    result <- result[
      order(
        result$date,
        result$site,
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


clean_monthly_site_report_discounts <- function(workbook) {

  if (!inherits(workbook, "monthly_site_report_workbook")) {
    stop(
      "workbook must be a monthly_site_report_workbook object.",
      call. = FALSE
    )
  }

  discount_tables <- list()

  for (sheet_name in names(workbook$sheets)) {

    raw_sheet <- workbook$sheets[[sheet_name]]

    if (!sheet_has_discount_section(raw_sheet)) {
      next
    }

    discount_tables[[length(discount_tables) + 1L]] <-
      extract_monthly_discount_section(
        raw_sheet = raw_sheet,
        report_year = workbook$report_year,
        source_file = workbook$source_file,
        sheet_name = sheet_name
      )
  }

  result <- bind_import_tables(
    discount_tables,
    empty_monthly_discount_table()
  )

  if (nrow(result) > 0L) {
    result <- result[
      order(
        result$date,
        result$site,
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


clean_monthly_site_report_total_revenue <- function(
  workbook,
  finance,
  discounts,
  validation_tolerance = 0.01
) {

  if (!inherits(workbook, "monthly_site_report_workbook")) {
    stop(
      "workbook must be a monthly_site_report_workbook object.",
      call. = FALSE
    )
  }

  revenue_tables <- list()

  for (sheet_name in names(workbook$sheets)) {

    raw_sheet <- workbook$sheets[[sheet_name]]
    sheet_finance <- finance[
      finance$source_sheet == sheet_name,
      ,
      drop = FALSE
    ]
    sheet_discounts <- discounts[
      discounts$source_sheet == sheet_name,
      ,
      drop = FALSE
    ]

    if (!sheet_has_total_revenue_section(raw_sheet)) {
      next
    }

    revenue_tables[[length(revenue_tables) + 1L]] <-
      extract_monthly_total_revenue_section(
        raw_sheet = raw_sheet,
        report_year = workbook$report_year,
        source_file = workbook$source_file,
        sheet_name = sheet_name,
        finance = sheet_finance,
        discounts = sheet_discounts,
        validation_tolerance = validation_tolerance
      )
  }

  result <- bind_import_tables(
    revenue_tables,
    empty_monthly_total_revenue_table()
  )

  if (nrow(result) > 0L) {
    result <- result[
      order(
        result$report_year,
        result$site,
        result$business_unit
      ),
      ,
      drop = FALSE
    ]

    rownames(result) <- NULL
  }

  result
}
