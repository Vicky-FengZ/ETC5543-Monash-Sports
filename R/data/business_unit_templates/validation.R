# -------------------------------------------------------------------------
# Business Unit Template report validation
# -------------------------------------------------------------------------

empty_business_unit_check_table <- function() {

  data.frame(
    report_year = integer(),
    dataset = character(),
    check_type = character(),
    report_unit = character(),
    item = character(),
    metric = character(),
    value_type = character(),
    month = integer(),
    expected_value = double(),
    reported_value = double(),
    difference = double(),
    tolerance = double(),
    status = character(),
    source_file = character(),
    source_sheet = character(),
    source_row = integer(),
    source_column = integer(),
    stringsAsFactors = FALSE
  )
}


validate_business_unit_visit_structure <- function(
  workbook,
  sheet_field,
  label
) {

  raw_sheet <- workbook$sheets[[sheet_field]]
  sheet_name <- workbook$sheet_names[[sheet_field]]
  if (!is.data.frame(raw_sheet) || nrow(raw_sheet) == 0L) {
    return(
      make_import_issue(
        level = "error",
        source_file = workbook$source_file,
        sheet = sheet_name,
        message = paste0("The ", label, " sheet is missing or empty.")
      )
    )
  }
  header_row <- find_business_unit_visit_header(raw_sheet)
  if (is.na(header_row)) {
    return(
      make_import_issue(
        level = "error",
        source_file = workbook$source_file,
        sheet = sheet_name,
        message = paste0(
          "The ",
          label,
          " sheet has no recognisable month header."
        )
      )
    )
  }
  periods <- business_unit_visit_periods(raw_sheet, header_row)
  months <- periods$month[periods$period_type == "Month"]
  issues <- list()
  missing_months <- setdiff(seq_len(12L), months)
  duplicate_months <- unique(months[duplicated(months)])

  if (length(missing_months) > 0L) {
    issues[[length(issues) + 1L]] <- make_import_issue(
      level = "warning",
      source_file = workbook$source_file,
      sheet = sheet_name,
      message = paste0(
        label,
        " is missing month header(s): ",
        paste(month.abb[missing_months], collapse = ", "),
        "."
      )
    )
  }
  if (length(duplicate_months) > 0L) {
    issues[[length(issues) + 1L]] <- make_import_issue(
      level = "error",
      source_file = workbook$source_file,
      sheet = sheet_name,
      message = paste0(
        label,
        " contains duplicate month header(s): ",
        paste(month.abb[duplicate_months], collapse = ", "),
        "."
      )
    )
  }
  if (!any(periods$period_type == "YTD")) {
    issues[[length(issues) + 1L]] <- make_import_issue(
      level = "warning",
      source_file = workbook$source_file,
      sheet = sheet_name,
      message = paste0(label, " has no recognised YTD Total block.")
    )
  }
  audiences <- normalise_excel_label(
    vapply(
      seq_len(ncol(raw_sheet)),
      function(source_column) {
        cell_as_text(raw_sheet, header_row + 1L, source_column)
      },
      character(1)
    )
  )
  if (!any(nzchar(audiences[-1L]) & audiences[-1L] != "total")) {
    issues[[length(issues) + 1L]] <- make_import_issue(
      level = "error",
      source_file = workbook$source_file,
      sheet = sheet_name,
      message = paste0(label, " has no labelled audience columns.")
    )
  }
  if (!"total" %in% audiences) {
    issues[[length(issues) + 1L]] <- make_import_issue("warning",
      workbook$source_file, paste0(label, " has no Total audience column; audience rollups cannot be checked."),
      sheet_name)
  }
  bind_import_tables(issues, empty_import_issue_table())
}


validate_business_unit_member_structure <- function(
  workbook,
  sheet_field,
  label
) {

  raw_sheet <- workbook$sheets[[sheet_field]]
  sheet_name <- workbook$sheet_names[[sheet_field]]
  if (!is.data.frame(raw_sheet)) {
    return(empty_import_issue_table())
  }
  if (nrow(raw_sheet) == 0L) {
    return(
      make_import_issue(
        level = "warning",
        source_file = workbook$source_file,
        sheet = sheet_name,
        message = paste0("The optional ", label, " sheet is empty.")
      )
    )
  }
  first_column <- first_column_labels(raw_sheet)
  header_rows <- which(first_column == "month")
  if (length(header_rows) == 0L) {
    return(
      make_import_issue(
        level = "warning",
        source_file = workbook$source_file,
        sheet = sheet_name,
        message = paste0(
          "The optional ",
          label,
          " sheet does not contain a Month header."
        )
      )
    )
  }
  issues <- list()

  for (header_index in seq_along(header_rows)) {
    block_end <- if (header_index < length(header_rows)) {
      header_rows[[header_index + 1L]] - 1L
    } else {
      nrow(raw_sheet)
    }
    months <- vapply(
      seq.int(header_rows[[header_index]] + 1L, block_end),
      function(source_row) parse_month_number(raw_sheet[[1]][source_row]),
      integer(1)
    )
    missing_months <- setdiff(seq_len(12L), months[!is.na(months)])
    duplicate_months <- unique(months[!is.na(months) & duplicated(months)])

    if (length(missing_months) > 0L) {
      issues[[length(issues) + 1L]] <- make_import_issue(
        level = "warning",
        source_file = workbook$source_file,
        sheet = sheet_name,
        message = paste0(
          label,
          " block beginning at row ",
          header_rows[[header_index]],
          " is missing month row(s): ",
          paste(month.abb[missing_months], collapse = ", "),
          "."
        )
      )
    }
    if (length(duplicate_months) > 0L) {
      issues[[length(issues) + 1L]] <- make_import_issue(
        level = "warning",
        source_file = workbook$source_file,
        sheet = sheet_name,
        message = paste0(
          label,
          " block beginning at row ",
          header_rows[[header_index]],
          " contains duplicate month row(s): ",
          paste(month.abb[duplicate_months], collapse = ", "),
          "."
        )
      )
    }
  }
  bind_import_tables(issues, empty_import_issue_table())
}


validate_business_unit_template_workbook <- function(workbook) {

  if (!inherits(workbook, "business_unit_template_workbook")) {
    return(
      make_import_issue(
        level = "error",
        source_file = NA_character_,
        message = "Object is not a Business Unit Template workbook."
      )
    )
  }
  raw_finance <- workbook$sheets$finance
  issues <- list()

  if (!is.data.frame(raw_finance) || nrow(raw_finance) == 0L) {
    issues[[length(issues) + 1L]] <- make_import_issue(
      level = "error",
      source_file = workbook$source_file,
      sheet = workbook$sheet_names$finance,
      message = "The Finance sheet is empty."
    )
  } else {
    required_sections <- c(
      finance = sheet_has_finance_sections(raw_finance),
      discounts = sheet_has_discount_section(raw_finance),
      total_revenue = sheet_has_total_revenue_section(raw_finance)
    )
    missing_sections <- names(required_sections)[!required_sections]
    if (length(missing_sections) > 0L) {
      issues[[length(issues) + 1L]] <- make_import_issue(
        level = "error",
        source_file = workbook$source_file,
        sheet = workbook$sheet_names$finance,
        message = paste0(
          "The Finance sheet is missing section(s): ",
          paste(missing_sections, collapse = ", "),
          "."
        )
      )
    }
    finance_months <- unique(
      vapply(raw_finance[[1]], parse_month_number, integer(1))
    )
    finance_months <- finance_months[!is.na(finance_months)]
    missing_months <- setdiff(seq_len(12L), finance_months)
    if (length(missing_months) > 0L) {
      issues[[length(issues) + 1L]] <- make_import_issue(
        level = "warning",
        source_file = workbook$source_file,
        sheet = workbook$sheet_names$finance,
        message = paste0(
          "The Finance sheet is missing month row(s): ",
          paste(month.abb[missing_months], collapse = ", "),
          "."
        )
      )
    }
  }

  issues <- c(
    issues,
    list(
      validate_business_unit_visit_structure(
        workbook,
        "direct_visits",
        "Direct Visits"
      ),
      validate_business_unit_visit_structure(
        workbook,
        "indirect_visits",
        "Indirect Visits"
      ),
      validate_business_unit_member_structure(
        workbook,
        "memberships",
        "Memberships"
      ),
      validate_business_unit_member_structure(
        workbook,
        "enrollments",
        "Enrollments"
      )
    )
  )
  bind_import_tables(issues, empty_import_issue_table())
}


validate_business_unit_template_collection <- function(workbooks) {

  if (length(workbooks) == 0L) {
    return(empty_import_issue_table())
  }
  keys <- vapply(
    workbooks,
    function(x) paste(x$report_year, x$report_unit, sep = " / "),
    character(1)
  )
  duplicate_keys <- unique(keys[duplicated(keys)])
  if (length(duplicate_keys) == 0L) {
    return(empty_import_issue_table())
  }
  make_import_issue(
    level = "warning",
    source_file = NA_character_,
    message = paste0(
      "Multiple Business Unit Template files were found for: ",
      paste(duplicate_keys, collapse = ", "),
      "."
    )
  )
}


make_business_unit_check <- function(
  report_year,
  dataset,
  check_type,
  report_unit,
  item,
  metric,
  value_type,
  month,
  expected_value,
  reported_value,
  validation_tolerance,
  source_file,
  source_sheet,
  source_row,
  source_column
) {

  difference <- if (
    is.na(expected_value) ||
      is.na(reported_value)
  ) {
    NA_real_
  } else {
    reported_value - expected_value
  }
  status <- if (is.na(expected_value) && is.na(reported_value)) {
    "no_data"
  } else if (is.na(expected_value)) {
    "missing_components"
  } else if (is.na(reported_value)) {
    "missing_reported_value"
  } else if (abs(difference) <= validation_tolerance) {
    "pass"
  } else {
    "fail"
  }

  # A one-row record; bound with records_to_table().
  list(
    report_year = as.integer(report_year),
    dataset = dataset,
    check_type = check_type,
    report_unit = report_unit,
    item = item,
    metric = metric,
    value_type = value_type,
    month = as.integer(month),
    expected_value = as.numeric(expected_value),
    reported_value = as.numeric(reported_value),
    difference = as.numeric(difference),
    tolerance = as.numeric(validation_tolerance),
    status = status,
    source_file = source_file,
    source_sheet = source_sheet,
    source_row = as.integer(source_row),
    source_column = as.integer(source_column)
  )
}


business_unit_check_record <- function(data, keep) {

  records <- data[keep, , drop = FALSE]
  if (nrow(records) != 1L) {
    return(
      list(
        value = NA_real_,
        source_file = if (nrow(data) > 0L) data$source_file[[1]] else NA_character_,
        source_sheet = if (nrow(data) > 0L) data$source_sheet[[1]] else NA_character_,
        source_row = NA_integer_,
        source_column = NA_integer_
      )
    )
  }
  list(
    value = records$value[[1]],
    source_file = records$source_file[[1]],
    source_sheet = records$source_sheet[[1]],
    source_row = records$source_row[[1]],
    source_column = records$source_column[[1]]
  )
}


append_business_unit_check <- function(
  checks,
  group_data,
  dataset,
  check_type,
  item,
  metric,
  value_type,
  month,
  expected_value,
  reported_record,
  validation_tolerance
) {

  checks[[length(checks) + 1L]] <- make_business_unit_check(
    report_year = group_data$report_year[[1]],
    dataset = dataset,
    check_type = check_type,
    report_unit = group_data$report_unit[[1]],
    item = item,
    metric = metric,
    value_type = value_type,
    month = month,
    expected_value = expected_value,
    reported_value = reported_record$value,
    validation_tolerance = validation_tolerance,
    source_file = reported_record$source_file,
    source_sheet = reported_record$source_sheet,
    source_row = reported_record$source_row,
    source_column = reported_record$source_column
  )
  checks
}


build_business_unit_finance_checks <- function(
  finance,
  validation_tolerance = 0.01
) {

  if (!is.data.frame(finance) || nrow(finance) == 0L) {
    return(empty_business_unit_check_table())
  }
  checks <- list()
  files <- unique(finance$source_file)

  for (source_file in files) {
    file_data <- finance[finance$source_file == source_file, , drop = FALSE]
    identity_columns <- c(
      "raw_business_unit",
      "metric",
      "period_type",
      "month"
    )
    identity_data <- unique(file_data[identity_columns])

    for (index in seq_len(nrow(identity_data))) {
      key <- identity_data[index, , drop = FALSE]
      group_data <- file_data[
        file_data$raw_business_unit == key$raw_business_unit[[1]] &
          file_data$metric == key$metric[[1]] &
          file_data$period_type == key$period_type[[1]] &
          (is.na(file_data$month) == is.na(key$month[[1]])) &
          (is.na(key$month[[1]]) | file_data$month == key$month[[1]]),
        ,
        drop = FALSE
      ]
      if (!key$metric[[1]] %in% c("Income", "Expenses")) {
        next
      }
      budget <- business_unit_check_record(
        group_data,
        group_data$value_type == "Budget"
      )
      actual <- business_unit_check_record(
        group_data,
        group_data$value_type == "Actual"
      )
      variance <- business_unit_check_record(
        group_data,
        group_data$value_type == "Variance"
      )
      available_values <- c(budget$value, actual$value, variance$value)
      expected <- if (all(is.na(available_values))) {
        NA_real_
      } else {
        ifelse(is.na(actual$value), 0, actual$value) -
          ifelse(is.na(budget$value), 0, budget$value)
      }
      checks <- append_business_unit_check(
        checks,
        group_data,
        "Finance",
        "variance_identity",
        group_data$business_unit[[1]],
        group_data$metric[[1]],
        "Variance",
        group_data$month[[1]],
        expected,
        variance,
        validation_tolerance
      )
    }

    rollup_keys <- unique(
      file_data[
        file_data$metric %in% c("Income", "Expenses"),
        c("raw_business_unit", "metric", "value_type"),
        drop = FALSE
      ]
    )
    for (index in seq_len(nrow(rollup_keys))) {
      key <- rollup_keys[index, , drop = FALSE]
      group_data <- file_data[
        file_data$raw_business_unit == key$raw_business_unit[[1]] &
          file_data$metric == key$metric[[1]] &
          file_data$value_type == key$value_type[[1]],
        ,
        drop = FALSE
      ]
      reported <- business_unit_check_record(
        group_data,
        group_data$period_type == "Reported Total"
      )
      monthly <- group_data$value[group_data$period_type == "Month"]
      expected <- if (all(is.na(c(monthly, reported$value)))) {
        NA_real_
      } else {
        sum(monthly, na.rm = TRUE)
      }
      checks <- append_business_unit_check(
        checks,
        group_data,
        "Finance",
        "monthly_to_reported_total",
        group_data$business_unit[[1]],
        group_data$metric[[1]],
        group_data$value_type[[1]],
        NA_integer_,
        expected,
        reported,
        validation_tolerance
      )
    }

    profit_keys <- unique(
      file_data[
        file_data$period_type == "Reported Total",
        c("raw_business_unit", "value_type"),
        drop = FALSE
      ]
    )
    for (index in seq_len(nrow(profit_keys))) {
      key <- profit_keys[index, , drop = FALSE]
      group_data <- file_data[
        file_data$raw_business_unit == key$raw_business_unit[[1]] &
          file_data$value_type == key$value_type[[1]] &
          file_data$period_type == "Reported Total",
        ,
        drop = FALSE
      ]
      income <- business_unit_check_record(
        group_data,
        group_data$metric == "Income"
      )
      expenses <- business_unit_check_record(
        group_data,
        group_data$metric == "Expenses"
      )
      profit <- business_unit_check_record(
        group_data,
        group_data$metric == "Profit/Loss"
      )
      expected <- if (all(is.na(c(income$value, expenses$value, profit$value)))) {
        NA_real_
      } else {
        ifelse(is.na(income$value), 0, income$value) -
          ifelse(is.na(expenses$value), 0, expenses$value)
      }
      checks <- append_business_unit_check(
        checks,
        group_data,
        "Finance",
        "profit_loss_identity",
        group_data$business_unit[[1]],
        "Profit/Loss",
        group_data$value_type[[1]],
        NA_integer_,
        expected,
        profit,
        validation_tolerance
      )
    }
  }

  result <- records_to_table(checks, empty_business_unit_check_table())
  result <- result[result$status != "no_data", , drop = FALSE]
  rownames(result) <- NULL
  result
}


build_business_unit_discount_checks <- function(
  discounts,
  validation_tolerance = 0.01
) {

  if (!is.data.frame(discounts) || nrow(discounts) == 0L) {
    return(empty_business_unit_check_table())
  }
  checks <- list()
  key_columns <- c(
    "source_file",
    "raw_business_unit",
    "discount_type"
  )
  keys <- unique(discounts[key_columns])

  for (index in seq_len(nrow(keys))) {
    key <- keys[index, , drop = FALSE]
    group_data <- discounts[
      discounts$source_file == key$source_file[[1]] &
        discounts$raw_business_unit == key$raw_business_unit[[1]] &
        discounts$discount_type == key$discount_type[[1]],
      ,
      drop = FALSE
    ]
    reported <- business_unit_check_record(
      group_data,
      group_data$period_type == "Reported Total"
    )
    monthly <- group_data$value[group_data$period_type == "Month"]
    expected <- if (all(is.na(c(monthly, reported$value)))) {
      NA_real_
    } else {
      sum(monthly, na.rm = TRUE)
    }
    checks <- append_business_unit_check(
      checks,
      group_data,
      "Discounts",
      "monthly_to_reported_total",
      group_data$business_unit[[1]],
      "Discounts",
      group_data$discount_type[[1]],
      NA_integer_,
      expected,
      reported,
      validation_tolerance
    )
  }
  result <- records_to_table(checks, empty_business_unit_check_table())
  result <- result[result$status != "no_data", , drop = FALSE]
  rownames(result) <- NULL
  result
}


business_unit_visit_record <- function(data, audience) {

  business_unit_check_record(data, data$audience == audience)
}


# Row indices of each group of `data` defined by `columns`, in the order the
# groups first appear (the order of unique(data[columns])). NA values form
# their own group. Faster than filtering the whole table once per group.
business_unit_group_rows <- function(data, columns) {

  key_parts <- lapply(
    data[columns],
    function(values) ifelse(is.na(values), "\001NA", as.character(values))
  )
  key <- do.call(paste, c(key_parts, sep = "\r"))
  unname(split(seq_len(nrow(data)), factor(key, levels = unique(key))))
}


build_business_unit_visit_checks <- function(
  visits,
  validation_tolerance = 0.01
) {

  if (!is.data.frame(visits) || nrow(visits) == 0L) {
    return(empty_business_unit_check_table())
  }
  checks <- list()
  period_keys <- unique(
    visits[
      c(
        "source_file",
        "source_sheet",
        "source_row",
        "period_type",
        "month"
      )
    ]
  )

  for (rows in business_unit_group_rows(visits, names(period_keys))) {
    group_data <- visits[rows, , drop = FALSE]
    component_values <- group_data$value[
      group_data$audience != "Total"
    ]
    reported <- business_unit_visit_record(group_data, "Total")
    expected <- if (all(is.na(component_values))) {
      NA_real_
    } else {
      sum(component_values, na.rm = TRUE)
    }
    checks <- append_business_unit_check(
      checks,
      group_data,
      "Visits",
      "audience_rollup",
      paste(group_data$visit_type[[1]], group_data$activity[[1]], sep = " / "),
      "Visits",
      "Total",
      group_data$month[[1]],
      expected,
      reported,
      validation_tolerance
    )
  }

  activity_keys <- unique(
    visits[
      c(
        "source_file",
        "source_sheet",
        "period_type",
        "month",
        "audience"
      )
    ]
  )
  for (rows in business_unit_group_rows(visits, names(activity_keys))) {
    group_data <- visits[rows, , drop = FALSE]
    component_values <- group_data$value[group_data$row_type == "Activity"]
    reported <- business_unit_check_record(
      group_data,
      group_data$row_type == "Total"
    )
    expected <- if (all(is.na(component_values))) {
      NA_real_
    } else {
      sum(component_values, na.rm = TRUE)
    }
    checks <- append_business_unit_check(
      checks,
      group_data,
      "Visits",
      "activity_rollup",
      paste(group_data$visit_type[[1]], "Total", sep = " / "),
      "Visits",
      group_data$audience[[1]],
      group_data$month[[1]],
      expected,
      reported,
      validation_tolerance
    )
  }

  ytd_keys <- unique(
    visits[
      c("source_file", "source_sheet", "source_row", "audience")
    ]
  )
  for (rows in business_unit_group_rows(visits, names(ytd_keys))) {
    group_data <- visits[rows, , drop = FALSE]
    monthly_values <- group_data$value[group_data$period_type == "Month"]
    reported <- business_unit_check_record(
      group_data,
      group_data$period_type == "YTD"
    )
    expected <- if (all(is.na(monthly_values))) {
      NA_real_
    } else {
      sum(monthly_values, na.rm = TRUE)
    }
    checks <- append_business_unit_check(
      checks,
      group_data,
      "Visits",
      "monthly_to_ytd",
      paste(group_data$visit_type[[1]], group_data$activity[[1]], sep = " / "),
      "Visits",
      group_data$audience[[1]],
      NA_integer_,
      expected,
      reported,
      validation_tolerance
    )
  }

  result <- records_to_table(checks, empty_business_unit_check_table())
  result <- result[result$status != "no_data", , drop = FALSE]
  rownames(result) <- NULL
  result
}


business_unit_member_value <- function(data, metric) {

  records <- data[data$metric == metric, , drop = FALSE]
  if (nrow(records) != 1L) {
    return(NA_real_)
  }
  records$value[[1]]
}


build_business_unit_member_checks <- function(
  data,
  validation_tolerance = 0.01
) {

  if (!is.data.frame(data) || nrow(data) == 0L) {
    return(empty_business_unit_check_table())
  }
  checks <- list()
  row_keys <- unique(data[c("source_file", "source_sheet", "source_row")])

  for (index in seq_len(nrow(row_keys))) {
    key <- row_keys[index, , drop = FALSE]
    group_data <- data[
      data$source_file == key$source_file[[1]] &
        data$source_sheet == key$source_sheet[[1]] &
        data$source_row == key$source_row[[1]],
      ,
      drop = FALSE
    ]
    dataset <- group_data$dataset_type[[1]]
    definitions <- if (dataset == "Membership") {
      list(
        list(
          check_type = "new_member_rollup",
          reported = "New Members",
          components = c(
            "New Student Members",
            "New Staff Members",
            "New Community Members"
          )
        ),
        list(
          check_type = "active_member_rollup",
          reported = "Total Active",
          components = c(
            "Active Student Members",
            "Active Staff Members",
            "Active Community Members"
          )
        )
      )
    } else {
      list(
        list(
          check_type = "enrollment_rollup",
          reported = "Total Enrollments",
          components = c("Students", "Staff", "Community")
        )
      )
    }

    for (definition in definitions) {
      if (!all(definition$components %in% group_data$metric)) {
        next
      }
      if (!definition$reported %in% group_data$metric) {
        next
      }
      component_values <- vapply(
        definition$components,
        function(metric) business_unit_member_value(group_data, metric),
        double(1)
      )
      reported <- business_unit_check_record(
        group_data,
        group_data$metric == definition$reported
      )
      expected <- if (all(is.na(c(component_values, reported$value)))) {
        NA_real_
      } else {
        sum(component_values, na.rm = TRUE)
      }
      checks <- append_business_unit_check(
        checks,
        group_data,
        paste0(dataset, "s"),
        definition$check_type,
        group_data$report_unit[[1]],
        definition$reported,
        "Count",
        group_data$month[[1]],
        expected,
        reported,
        validation_tolerance
      )
    }

    if (dataset == "Membership") {
      share_definitions <- list(
        c("Active Student Members", "Student Share"),
        c("Active Staff Members", "Staff Share"),
        c("Active Community Members", "Community Share")
      )
      total_active <- business_unit_member_value(group_data, "Total Active")
      for (definition in share_definitions) {
        if (!all(definition %in% group_data$metric)) {
          next
        }
        component <- business_unit_member_value(group_data, definition[[1]])
        expected <- if (
          is.na(component) ||
            is.na(total_active) ||
            total_active == 0
        ) {
          NA_real_
        } else {
          component / total_active
        }
        reported <- business_unit_check_record(
          group_data,
          group_data$metric == definition[[2]]
        )
        checks <- append_business_unit_check(
          checks,
          group_data,
          "Memberships",
          "membership_share_identity",
          group_data$report_unit[[1]],
          definition[[2]],
          "Rate",
          group_data$month[[1]],
          expected,
          reported,
          validation_tolerance
        )
      }
    }
  }

  result <- records_to_table(checks, empty_business_unit_check_table())
  result <- result[result$status != "no_data", , drop = FALSE]
  rownames(result) <- NULL
  result
}


validate_business_unit_check_table <- function(checks) {

  if (!is.data.frame(checks) || nrow(checks) == 0L) {
    return(empty_import_issue_table())
  }
  issues <- list()
  key <- paste(checks$source_file, checks$dataset, sep = "\r")

  for (indices in split(seq_len(nrow(checks)), key)) {
    group <- checks[indices, , drop = FALSE]
    failed <- group$status == "fail"
    incomplete <- !group$status %in% c("pass", "fail")
    if (any(failed)) {
      issues[[length(issues) + 1L]] <- make_import_issue(
        level = "warning",
        source_file = group$source_file[[1]],
        sheet = group$source_sheet[[1]],
        message = paste0(
          group$dataset[[1]],
          " validation failed for ",
          sum(failed),
          " check(s); maximum absolute difference: ",
          format(
            max(abs(group$difference[failed]), na.rm = TRUE),
            big.mark = ",",
            scientific = FALSE,
            trim = TRUE
          ),
          "."
        )
      )
    }
    if (any(incomplete)) {
      counts <- table(group$status[incomplete])
      summary <- paste0(
        names(counts),
        "=",
        as.integer(counts),
        collapse = ", "
      )
      issues[[length(issues) + 1L]] <- make_import_issue(
        level = "warning",
        source_file = group$source_file[[1]],
        sheet = group$source_sheet[[1]],
        message = paste0(
          group$dataset[[1]],
          " validation was incomplete for ",
          sum(incomplete),
          " check(s) (",
          summary,
          ")."
        )
      )
    }
  }
  bind_import_tables(issues, empty_import_issue_table())
}


validate_business_unit_total_revenue <- function(total_revenue) {

  if (!is.data.frame(total_revenue) || nrow(total_revenue) == 0L) {
    return(empty_import_issue_table())
  }
  issues <- list()

  for (source_file in unique(total_revenue$source_file)) {
    data <- total_revenue[
      total_revenue$source_file == source_file,
      ,
      drop = FALSE
    ]
    failed <- data$validation_status == "fail"
    incomplete <- !data$validation_status %in% c("pass", "fail")
    if (any(failed)) {
      issues[[length(issues) + 1L]] <- make_import_issue(
        level = "warning",
        source_file = source_file,
        sheet = data$source_sheet[[1]],
        message = paste0(
          "Total Revenue validation failed for ",
          sum(failed),
          " business unit(s); maximum absolute difference: ",
          format(
            max(abs(data$validation_difference[failed]), na.rm = TRUE),
            big.mark = ",",
            scientific = FALSE,
            trim = TRUE
          ),
          "."
        )
      )
    }
    if (any(incomplete)) {
      issues[[length(issues) + 1L]] <- make_import_issue(
        level = "warning",
        source_file = source_file,
        sheet = data$source_sheet[[1]],
        message = paste0(
          "Total Revenue validation was incomplete for ",
          sum(incomplete),
          " business unit(s)."
        )
      )
    }
  }
  bind_import_tables(issues, empty_import_issue_table())
}
