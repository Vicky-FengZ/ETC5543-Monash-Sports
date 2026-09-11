# -------------------------------------------------------------------------
# Venues Tracking Report validation
# -------------------------------------------------------------------------

empty_venues_finance_check_table <- function() {

  data.frame(
    report_year = integer(),
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    fund = character(),
    row_type = character(),
    check_type = character(),
    metric = character(),
    value_type = character(),
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


empty_venues_operational_check_table <- function() {

  data.frame(
    report_year = integer(),
    dataset = character(),
    check_type = character(),
    group = character(),
    item = character(),
    row_type = character(),
    metric = character(),
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


validate_venues_operational_sheet <- function(
  workbook,
  data_field,
  name_field,
  dataset,
  total_labels
) {

  raw_sheet <- workbook[[data_field]]
  sheet_name <- workbook[[name_field]]

  if (!is.data.frame(raw_sheet)) {
    return(
      make_import_issue(
        level = "warning",
        source_file = workbook$source_file,
        sheet = sheet_name,
        message = paste0(
          "The ",
          dataset,
          " sheet was not found; its extracted table will be empty."
        )
      )
    )
  }

  if (nrow(raw_sheet) == 0L) {
    return(
      make_import_issue(
        level = "warning",
        source_file = workbook$source_file,
        sheet = sheet_name,
        message = paste0(
          "The ",
          dataset,
          " sheet is empty; its extracted table will be empty."
        )
      )
    )
  }

  issues <- list()
  header_row <- find_venues_month_header(raw_sheet, 10L)

  if (is.na(header_row)) {
    return(
      make_import_issue(
        level = "warning",
        source_file = workbook$source_file,
        sheet = sheet_name,
        message = paste0(
          "The ",
          dataset,
          " sheet does not contain a recognisable twelve-month header."
        )
      )
    )
  }

  month_columns <- find_venues_month_columns(raw_sheet, header_row)
  duplicate_months <- sort(
    unique(month_columns$month[duplicated(month_columns$month)])
  )
  missing_months <- setdiff(seq_len(12L), month_columns$month)

  if (length(duplicate_months) > 0L) {
    issues[[length(issues) + 1L]] <- make_import_issue(
      level = "warning",
      source_file = workbook$source_file,
      sheet = sheet_name,
      message = paste0(
        "The ",
        dataset,
        " sheet contains duplicate month header(s): ",
        paste(month.abb[duplicate_months], collapse = ", "),
        "."
      )
    )
  }

  if (length(missing_months) > 0L) {
    issues[[length(issues) + 1L]] <- make_import_issue(
      level = "warning",
      source_file = workbook$source_file,
      sheet = sheet_name,
      message = paste0(
        "The ",
        dataset,
        " sheet is missing month header(s): ",
        paste(month.abb[missing_months], collapse = ", "),
        "."
      )
    )
  }

  header_labels <- normalise_excel_label(
    vapply(
      seq_len(ncol(raw_sheet)),
      function(column) cell_as_text(raw_sheet, header_row, column),
      character(1)
    )
  )

  if (!any(header_labels %in% normalise_excel_label(total_labels))) {
    issues[[length(issues) + 1L]] <- make_import_issue(
      level = "warning",
      source_file = workbook$source_file,
      sheet = sheet_name,
      message = paste0(
        "The ",
        dataset,
        " sheet has no recognised reported-total column (expected ",
        paste(total_labels, collapse = " or "),
        ")."
      )
    )
  }

  bind_import_tables(issues, empty_import_issue_table())
}


validate_venues_court_hire_sheet <- function(workbook) {

  raw_sheet <- workbook$court_hire_sheet
  sheet_name <- workbook$court_hire_sheet_name

  if (!is.data.frame(raw_sheet)) {
    return(
      make_import_issue(
        level = "warning",
        source_file = workbook$source_file,
        sheet = sheet_name,
        message = paste(
          "The Court Hire comparison sheet was not found;",
          "its extracted table will be empty."
        )
      )
    )
  }

  if (nrow(raw_sheet) == 0L) {
    return(
      make_import_issue(
        level = "warning",
        source_file = workbook$source_file,
        sheet = sheet_name,
        message = paste(
          "The Court Hire comparison sheet is empty;",
          "its extracted table will be empty."
        )
      )
    )
  }

  layout <- find_venues_court_hire_layout(raw_sheet, workbook$report_year)

  if (nrow(layout) == 0L) {
    return(
      make_import_issue(
        level = "warning",
        source_file = workbook$source_file,
        sheet = sheet_name,
        message = paste0(
          "The Court Hire sheet has no recognisable comparison block for ",
          workbook$report_year,
          "."
        )
      )
    )
  }

  issues <- list()
  court_keys <- normalise_excel_label(layout$court_type)
  duplicate_courts <- unique(layout$court_type[duplicated(court_keys)])

  if (length(duplicate_courts) > 0L) {
    issues[[length(issues) + 1L]] <- make_import_issue(
      level = "warning",
      source_file = workbook$source_file,
      sheet = sheet_name,
      message = paste0(
        "The Court Hire sheet contains duplicate comparison block(s): ",
        paste(duplicate_courts, collapse = ", "),
        "."
      )
    )
  }

  if (!"total court hire" %in% court_keys) {
    issues[[length(issues) + 1L]] <- make_import_issue(
      level = "warning",
      source_file = workbook$source_file,
      sheet = sheet_name,
      message = "The Court Hire sheet has no Total Court Hire block."
    )
  }

  bind_import_tables(issues, empty_import_issue_table())
}


validate_venues_casual_hours_sheet <- function(workbook) {

  raw_sheet <- workbook$casual_hours_sheet
  sheet_name <- workbook$casual_hours_sheet_name

  if (!is.data.frame(raw_sheet)) {
    return(
      make_import_issue(
        level = "warning",
        source_file = workbook$source_file,
        sheet = sheet_name,
        message = paste(
          "The Casual Hours sheet was not found;",
          "its extracted table will be empty."
        )
      )
    )
  }

  if (nrow(raw_sheet) == 0L) {
    return(
      make_import_issue(
        level = "warning",
        source_file = workbook$source_file,
        sheet = sheet_name,
        message = paste(
          "The Casual Hours sheet is empty;",
          "its extracted table will be empty."
        )
      )
    )
  }

  layout <- find_venues_casual_hours_summary_layout(raw_sheet)

  if (nrow(layout) == 0L) {
    return(
      make_import_issue(
        level = "warning",
        source_file = workbook$source_file,
        sheet = sheet_name,
        message = paste(
          "The Casual Hours sheet does not contain a recognisable",
          "Summary / Budgeted Hours / Hours Paid / Variance block."
        )
      )
    )
  }

  period_labels <- vapply(
    seq.int(layout$header_row[[1]] + 1L, nrow(raw_sheet)),
    function(row) {
      cell_as_text(raw_sheet, row, layout$label_column[[1]])
    },
    character(1)
  )
  months <- vapply(period_labels, venues_month_from_text, integer(1))
  issues <- list()
  duplicate_months <- sort(unique(months[!is.na(months) & duplicated(months)]))
  missing_months <- setdiff(seq_len(12L), months)

  if (length(duplicate_months) > 0L) {
    issues[[length(issues) + 1L]] <- make_import_issue(
      level = "warning",
      source_file = workbook$source_file,
      sheet = sheet_name,
      message = paste0(
        "The Casual Hours summary contains duplicate month(s): ",
        paste(month.abb[duplicate_months], collapse = ", "),
        "."
      )
    )
  }

  if (length(missing_months) > 0L) {
    issues[[length(issues) + 1L]] <- make_import_issue(
      level = "warning",
      source_file = workbook$source_file,
      sheet = sheet_name,
      message = paste0(
        "The Casual Hours summary is missing month(s): ",
        paste(month.abb[missing_months], collapse = ", "),
        "."
      )
    )
  }

  if (!any(normalise_excel_label(period_labels) == "total")) {
    issues[[length(issues) + 1L]] <- make_import_issue(
      level = "warning",
      source_file = workbook$source_file,
      sheet = sheet_name,
      message = "The Casual Hours summary has no reported Total row."
    )
  }

  bind_import_tables(issues, empty_import_issue_table())
}


validate_venues_casual_wages_sheet <- function(workbook) {

  raw_sheet <- workbook$casual_wages_sheet
  sheet_name <- workbook$casual_wages_sheet_name

  if (!is.data.frame(raw_sheet)) {
    return(
      make_import_issue(
        level = "warning",
        source_file = workbook$source_file,
        sheet = sheet_name,
        message = paste(
          "The Casual Wages sheet was not found;",
          "its extracted table will be empty."
        )
      )
    )
  }

  if (nrow(raw_sheet) == 0L) {
    return(
      make_import_issue(
        level = "warning",
        source_file = workbook$source_file,
        sheet = sheet_name,
        message = paste(
          "The Casual Wages sheet is empty;",
          "its extracted table will be empty."
        )
      )
    )
  }

  layout <- find_venues_casual_wages_layout(
    raw_sheet,
    workbook$report_year
  )

  if (nrow(layout) == 0L) {
    return(
      make_import_issue(
        level = "warning",
        source_file = workbook$source_file,
        sheet = sheet_name,
        message = paste0(
          "The Casual Wages sheet has no recognisable ",
          workbook$report_year,
          " Casual Salaries block."
        )
      )
    )
  }

  if (nrow(layout) > 1L) {
    return(
      make_import_issue(
        level = "warning",
        source_file = workbook$source_file,
        sheet = sheet_name,
        message = paste0(
          "The Casual Wages sheet contains ",
          nrow(layout),
          " recognisable ",
          workbook$report_year,
          " Casual Salaries blocks; extraction was skipped to avoid ",
          "duplicating the same reporting year."
        )
      )
    )
  }

  period_labels <- vapply(
    seq.int(layout$header_row[[1]] + 1L, nrow(raw_sheet)),
    function(row) {
      cell_as_text(raw_sheet, row, layout$month_column[[1]])
    },
    character(1)
  )
  months <- vapply(period_labels, venues_month_from_text, integer(1))
  issues <- list()
  duplicate_months <- sort(unique(months[!is.na(months) & duplicated(months)]))
  missing_months <- setdiff(seq_len(12L), months)

  if (length(duplicate_months) > 0L) {
    issues[[length(issues) + 1L]] <- make_import_issue(
      level = "warning",
      source_file = workbook$source_file,
      sheet = sheet_name,
      message = paste0(
        "The ",
        workbook$report_year,
        " Casual Salaries block contains duplicate month(s): ",
        paste(month.abb[duplicate_months], collapse = ", "),
        "."
      )
    )
  }

  if (length(missing_months) > 0L) {
    issues[[length(issues) + 1L]] <- make_import_issue(
      level = "warning",
      source_file = workbook$source_file,
      sheet = sheet_name,
      message = paste0(
        "The ",
        workbook$report_year,
        " Casual Salaries block is missing month(s): ",
        paste(month.abb[missing_months], collapse = ", "),
        "."
      )
    )
  }

  if (!any(normalise_excel_label(period_labels) == "total")) {
    issues[[length(issues) + 1L]] <- make_import_issue(
      level = "warning",
      source_file = workbook$source_file,
      sheet = sheet_name,
      message = paste0(
        "The ",
        workbook$report_year,
        " Casual Salaries block has no reported Total row."
      )
    )
  }

  bind_import_tables(issues, empty_import_issue_table())
}


validate_venues_tracking_report_workbook <- function(workbook) {

  if (!inherits(workbook, "venues_tracking_report_workbook")) {
    return(
      make_import_issue(
        level = "error",
        source_file = NA_character_,
        message = "Object is not a Venues Tracking Report workbook."
      )
    )
  }

  if (
    !is.data.frame(workbook$finance_sheet) ||
      nrow(workbook$finance_sheet) == 0L
  ) {
    return(
      make_import_issue(
        level = "error",
        source_file = workbook$source_file,
        sheet = workbook$finance_sheet_name,
        message = "The MTD - Venues Finance sheet is empty."
      )
    )
  }

  anchors <- find_venues_finance_month_anchors(
    workbook$finance_sheet
  )
  issues <- list()

  if (nrow(anchors) == 0L) {
    issues[[length(issues) + 1L]] <- make_import_issue(
      level = "error",
      source_file = workbook$source_file,
      sheet = workbook$finance_sheet_name,
      message = paste(
        "No month blocks with Income, Expenses and Profit/Loss",
        "headers were found."
      )
    )
  }

  duplicate_months <- sort(
    unique(anchors$month[duplicated(anchors$month)])
  )

  if (length(duplicate_months) > 0L) {
    issues[[length(issues) + 1L]] <- make_import_issue(
      level = "error",
      source_file = workbook$source_file,
      sheet = workbook$finance_sheet_name,
      message = paste0(
        "Duplicate month block(s) were found: ",
        paste(month.abb[duplicate_months], collapse = ", "),
        "."
      )
    )
  }

  missing_months <- setdiff(
    seq_len(12L),
    anchors$month
  )

  if (length(missing_months) > 0L) {
    issues[[length(issues) + 1L]] <- make_import_issue(
      level = "warning",
      source_file = workbook$source_file,
      sheet = workbook$finance_sheet_name,
      message = paste0(
        "The MTD - Venues Finance sheet is missing month block(s): ",
        paste(month.abb[missing_months], collapse = ", "),
        "."
      )
    )
  }

  issues <- c(
    issues,
    list(
      validate_venues_operational_sheet(
        workbook = workbook,
        data_field = "attendance_sheet",
        name_field = "attendance_sheet_name",
        dataset = "Facility Attendance",
        total_labels = c("YTD", "YTD Total")
      ),
      validate_venues_operational_sheet(
        workbook = workbook,
        data_field = "facility_hire_sheet",
        name_field = "facility_hire_sheet_name",
        dataset = "Facility Hire Tracking",
        total_labels = "YTD"
      ),
      validate_venues_operational_sheet(
        workbook = workbook,
        data_field = "membership_sheet",
        name_field = "membership_sheet_name",
        dataset = "Membership Tracking",
        total_labels = c("Total", "Reported Total")
      ),
      validate_venues_operational_sheet(
        workbook = workbook,
        data_field = "online_sales_sheet",
        name_field = "online_sales_sheet_name",
        dataset = "Online Sales",
        total_labels = c("YTD", "YTD Total")
      ),
      validate_venues_court_hire_sheet(workbook),
      validate_venues_operational_sheet(
        workbook = workbook,
        data_field = "merchandise_sheet",
        name_field = "merchandise_sheet_name",
        dataset = "Merchandise Tracking",
        total_labels = c("YTD", "YTD Total")
      ),
      validate_venues_casual_hours_sheet(workbook),
      validate_venues_casual_wages_sheet(workbook)
    )
  )

  bind_import_tables(
    issues,
    empty_import_issue_table()
  )
}


make_venues_operational_check <- function(
  group_data,
  dataset,
  check_type,
  group,
  item,
  row_type,
  metric,
  total_period_type,
  validation_tolerance
) {

  monthly <- group_data[
    group_data$period_type == "Month",
    ,
    drop = FALSE
  ]
  reported <- group_data[
    group_data$period_type == total_period_type,
    ,
    drop = FALSE
  ]
  monthly_values <- monthly$value
  has_monthly_value <- any(!is.na(monthly_values))
  monthly_numbers <- suppressWarnings(as.integer(monthly$month))
  has_complete_month_set <-
    length(monthly_numbers) == 12L &&
    !anyNA(monthly_numbers) &&
    identical(sort(monthly_numbers), seq_len(12L))
  has_reporting_flag <- "is_reported" %in% names(monthly)
  monthly_complete <- if (has_reporting_flag) {
    has_complete_month_set &&
      all(!is.na(monthly$is_reported) & monthly$is_reported)
  } else {
    has_complete_month_set
  }
  reported_value <- if (nrow(reported) == 1L) {
    reported$value[[1]]
  } else {
    NA_real_
  }
  expected_value <- if (has_monthly_value) {
    sum(monthly_values, na.rm = TRUE)
  } else {
    NA_real_
  }
  difference <- if (
    is.na(expected_value) ||
      is.na(reported_value)
  ) {
    NA_real_
  } else {
    reported_value - expected_value
  }
  status <- if (nrow(reported) > 1L) {
    "duplicate_reported_values"
  } else if (!monthly_complete) {
    "incomplete_periods"
  } else if (
    !has_monthly_value &&
      !is.na(reported_value) &&
      abs(reported_value) <= validation_tolerance
  ) {
    "pass"
  } else if (!has_monthly_value && !is.na(reported_value)) {
    "missing_monthly_values"
  } else if (has_monthly_value && is.na(reported_value)) {
    "missing_reported_value"
  } else if (abs(difference) <= validation_tolerance) {
    "pass"
  } else {
    "fail"
  }
  source_record <- if (nrow(reported) > 0L) {
    reported[1L, , drop = FALSE]
  } else {
    group_data[1L, , drop = FALSE]
  }

  data.frame(
    report_year = as.integer(group_data$report_year[[1]]),
    dataset = dataset,
    check_type = check_type,
    group = group,
    item = item,
    row_type = row_type,
    metric = metric,
    expected_value = as.numeric(expected_value),
    reported_value = as.numeric(reported_value),
    difference = as.numeric(difference),
    tolerance = as.numeric(validation_tolerance),
    status = status,
    source_file = group_data$source_file[[1]],
    source_sheet = source_record$source_sheet[[1]],
    source_row = as.integer(source_record$source_row[[1]]),
    source_column = as.integer(source_record$source_column[[1]]),
    stringsAsFactors = FALSE
  )
}


build_venues_period_total_checks <- function(
  data,
  dataset,
  group_column,
  item_column,
  id_columns,
  total_period_type,
  check_type,
  metrics = NULL,
  default_metric = "Value",
  validation_tolerance = 0.01
) {

  if (!is.data.frame(data) || nrow(data) == 0L) {
    return(empty_venues_operational_check_table())
  }

  check_data <- data[
    data$period_type %in% c("Month", total_period_type),
    ,
    drop = FALSE
  ]

  if (!is.null(metrics) && "metric" %in% names(check_data)) {
    check_data <- check_data[
      check_data$metric %in% metrics,
      ,
      drop = FALSE
    ]
  }

  if (nrow(check_data) == 0L) {
    return(empty_venues_operational_check_table())
  }

  key_columns <- unique(
    c(
      "source_file",
      "report_year",
      id_columns,
      if ("metric" %in% names(check_data)) "metric"
    )
  )
  key_parts <- lapply(
    check_data[key_columns],
    function(values) ifelse(is.na(values), "<NA>", as.character(values))
  )
  group_key <- do.call(paste, c(key_parts, sep = "\r"))
  groups <- split(seq_len(nrow(check_data)), group_key, drop = TRUE)
  checks <- list()

  for (indices in groups) {
    group_data <- check_data[indices, , drop = FALSE]
    monthly_values <- group_data$value[
      group_data$period_type == "Month"
    ]
    reported_values <- group_data$value[
      group_data$period_type == total_period_type
    ]

    if (
      !any(!is.na(monthly_values)) &&
        !any(!is.na(reported_values))
    ) {
      next
    }

    checks[[length(checks) + 1L]] <- make_venues_operational_check(
      group_data = group_data,
      dataset = dataset,
      check_type = check_type,
      group = as.character(group_data[[group_column]][[1]]),
      item = as.character(group_data[[item_column]][[1]]),
      row_type = as.character(group_data$row_type[[1]]),
      metric = if ("metric" %in% names(group_data)) {
        as.character(group_data$metric[[1]])
      } else {
        default_metric
      },
      total_period_type = total_period_type,
      validation_tolerance = validation_tolerance
    )
  }

  result <- bind_import_tables(
    checks,
    empty_venues_operational_check_table()
  )

  if (nrow(result) > 0L) {
    result <- result[
      order(
        result$report_year,
        result$dataset,
        result$group,
        result$item,
        result$row_type,
        result$metric
      ),
      ,
      drop = FALSE
    ]
    rownames(result) <- NULL
  }

  result
}


build_venues_attendance_checks <- function(
  attendance,
  validation_tolerance = 0.01
) {

  build_venues_period_total_checks(
    data = attendance,
    dataset = "Facility Attendance",
    group_column = "activity",
    item_column = "category",
    id_columns = c(
      "area",
      "activity",
      "category",
      "row_type",
      "source_row"
    ),
    total_period_type = "YTD",
    check_type = "monthly_to_ytd",
    default_metric = "Attendance",
    validation_tolerance = validation_tolerance
  )
}


build_venues_facility_hire_checks <- function(
  facility_hire,
  validation_tolerance = 0.01
) {

  build_venues_period_total_checks(
    data = facility_hire,
    dataset = "Facility Hire Tracking",
    group_column = "facility",
    item_column = "category",
    id_columns = c(
      "facility",
      "category",
      "row_type",
      "source_row"
    ),
    total_period_type = "YTD",
    check_type = "monthly_to_ytd",
    metrics = c("Revenue", "Transactions"),
    validation_tolerance = validation_tolerance
  )
}


build_venues_membership_checks <- function(
  membership,
  validation_tolerance = 0.01
) {

  build_venues_period_total_checks(
    data = membership,
    dataset = "Membership Tracking",
    group_column = "membership_group",
    item_column = "membership_type",
    id_columns = c(
      "membership_group",
      "membership_type",
      "row_type",
      "source_row"
    ),
    total_period_type = "Reported Total",
    check_type = "monthly_to_reported_total",
    metrics = c("New", "Renewed", "Total", "Expired/Cancelled"),
    validation_tolerance = validation_tolerance
  )
}


build_venues_online_sales_checks <- function(
  online_sales,
  validation_tolerance = 0.01
) {

  build_venues_period_total_checks(
    data = online_sales,
    dataset = "Online Sales",
    group_column = "site",
    item_column = "category",
    id_columns = c(
      "site_scope",
      "site",
      "category",
      "row_type",
      "source_row"
    ),
    total_period_type = "YTD",
    check_type = "monthly_to_ytd",
    default_metric = "Revenue",
    validation_tolerance = validation_tolerance
  )
}


build_venues_merchandise_checks <- function(
  merchandise,
  validation_tolerance = 0.01
) {

  build_venues_period_total_checks(
    data = merchandise,
    dataset = "Merchandise Tracking",
    group_column = "section",
    item_column = "item",
    id_columns = c(
      "section",
      "item",
      "row_type",
      "source_row"
    ),
    total_period_type = "YTD",
    check_type = "monthly_to_ytd",
    metrics = c("Revenue", "Transactions"),
    validation_tolerance = validation_tolerance
  )
}


build_venues_variance_identity_checks <- function(
  data,
  dataset,
  group_column,
  item_column,
  id_columns,
  budget_metric,
  actual_metric,
  variance_metric,
  period_types = c("Month", "Reported Total"),
  validation_tolerance = 0.01
) {

  if (!is.data.frame(data) || nrow(data) == 0L) {
    return(empty_venues_operational_check_table())
  }

  check_data <- data[
    data$period_type %in% period_types &
      data$metric %in% c(
        budget_metric,
        actual_metric,
        variance_metric
      ),
    ,
    drop = FALSE
  ]

  if (nrow(check_data) == 0L) {
    return(empty_venues_operational_check_table())
  }

  key_columns <- unique(
    c(
      "source_file",
      "report_year",
      "period_type",
      if ("month" %in% names(check_data)) "month",
      id_columns
    )
  )
  key_parts <- lapply(
    check_data[key_columns],
    function(values) ifelse(is.na(values), "<NA>", as.character(values))
  )
  group_key <- do.call(paste, c(key_parts, sep = "\r"))
  groups <- split(seq_len(nrow(check_data)), group_key, drop = TRUE)
  checks <- list()

  for (indices in groups) {
    group_data <- check_data[indices, , drop = FALSE]
    budget <- group_data[group_data$metric == budget_metric, , drop = FALSE]
    actual <- group_data[group_data$metric == actual_metric, , drop = FALSE]
    variance <- group_data[
      group_data$metric == variance_metric,
      ,
      drop = FALSE
    ]
    duplicate_values <- any(c(
      nrow(budget),
      nrow(actual),
      nrow(variance)
    ) > 1L)
    budget_value <- if (nrow(budget) == 1L) budget$value[[1]] else NA_real_
    actual_value <- if (nrow(actual) == 1L) actual$value[[1]] else NA_real_
    reported_value <- if (nrow(variance) == 1L) {
      variance$value[[1]]
    } else {
      NA_real_
    }
    expected_value <- if (is.na(budget_value) || is.na(actual_value)) {
      NA_real_
    } else {
      actual_value - budget_value
    }
    difference <- if (is.na(expected_value) || is.na(reported_value)) {
      NA_real_
    } else {
      reported_value - expected_value
    }
    status <- if (duplicate_values) {
      "duplicate_reported_values"
    } else if (is.na(budget_value) || is.na(actual_value)) {
      "missing_input_values"
    } else if (is.na(reported_value)) {
      "missing_reported_value"
    } else if (abs(difference) <= validation_tolerance) {
      "pass"
    } else {
      "fail"
    }
    source_record <- if (nrow(variance) > 0L) {
      variance[1L, , drop = FALSE]
    } else {
      group_data[1L, , drop = FALSE]
    }

    checks[[length(checks) + 1L]] <- data.frame(
      report_year = as.integer(group_data$report_year[[1]]),
      dataset = dataset,
      check_type = "variance_identity",
      group = as.character(group_data[[group_column]][[1]]),
      item = as.character(group_data[[item_column]][[1]]),
      row_type = as.character(group_data$row_type[[1]]),
      metric = variance_metric,
      expected_value = as.numeric(expected_value),
      reported_value = as.numeric(reported_value),
      difference = as.numeric(difference),
      tolerance = as.numeric(validation_tolerance),
      status = status,
      source_file = group_data$source_file[[1]],
      source_sheet = source_record$source_sheet[[1]],
      source_row = as.integer(source_record$source_row[[1]]),
      source_column = as.integer(source_record$source_column[[1]]),
      stringsAsFactors = FALSE
    )
  }

  result <- bind_import_tables(
    checks,
    empty_venues_operational_check_table()
  )

  if (nrow(result) > 0L) {
    result <- result[
      order(
        result$report_year,
        result$dataset,
        result$group,
        result$item,
        result$source_row
      ),
      ,
      drop = FALSE
    ]
    rownames(result) <- NULL
  }

  result
}


build_venues_court_hire_growth_checks <- function(
  court_hire,
  validation_tolerance = 0.01
) {

  if (!is.data.frame(court_hire) || nrow(court_hire) == 0L) {
    return(empty_venues_operational_check_table())
  }

  check_data <- court_hire[
    court_hire$comparison_role %in%
      c("Previous Year", "Report Year", "Growth Achieved"),
    ,
    drop = FALSE
  ]
  key_parts <- lapply(
    check_data[c("source_file", "court_type", "period_type", "month")],
    function(values) ifelse(is.na(values), "<NA>", as.character(values))
  )
  group_key <- do.call(paste, c(key_parts, sep = "\r"))
  groups <- split(seq_len(nrow(check_data)), group_key, drop = TRUE)
  checks <- list()

  for (indices in groups) {
    group_data <- check_data[indices, , drop = FALSE]
    previous <- group_data[group_data$comparison_role == "Previous Year", , drop = FALSE]
    current <- group_data[group_data$comparison_role == "Report Year", , drop = FALSE]
    growth <- group_data[group_data$comparison_role == "Growth Achieved", , drop = FALSE]
    duplicate_values <- any(c(nrow(previous), nrow(current), nrow(growth)) > 1L)
    required_values <- c(
      if (nrow(previous) == 1L) previous$value[[1L]] else NA_real_,
      if (nrow(current) == 1L) current$value[[1L]] else NA_real_,
      if (nrow(growth) == 1L) growth$value[[1L]] else NA_real_
    )
    expected_value <- if (
      !anyNA(required_values[1:2]) &&
        abs(required_values[[1L]]) > validation_tolerance
    ) {
      required_values[[2L]] / required_values[[1L]]
    } else {
      NA_real_
    }
    reported_value <- required_values[[3L]]
    difference <- if (is.na(expected_value) || is.na(reported_value)) {
      NA_real_
    } else {
      reported_value - expected_value
    }
    status <- if (duplicate_values) {
      "duplicate_reported_values"
    } else if (anyNA(required_values)) {
      "incomplete_periods"
    } else if (is.na(expected_value)) {
      "unavailable_comparison"
    } else if (abs(difference) <= validation_tolerance) {
      "pass"
    } else {
      "fail"
    }
    source_record <- if (nrow(growth) > 0L) {
      growth[1L, , drop = FALSE]
    } else {
      group_data[1L, , drop = FALSE]
    }

    checks[[length(checks) + 1L]] <- data.frame(
      report_year = as.integer(group_data$report_year[[1L]]),
      dataset = "Court Hire",
      check_type = "growth_identity",
      group = as.character(group_data$court_type[[1L]]),
      item = paste0(
        min(group_data$data_year, na.rm = TRUE),
        " vs ",
        max(group_data$data_year, na.rm = TRUE)
      ),
      row_type = as.character(group_data$period_type[[1L]]),
      metric = "Growth Achieved",
      expected_value = as.numeric(expected_value),
      reported_value = as.numeric(reported_value),
      difference = as.numeric(difference),
      tolerance = as.numeric(validation_tolerance),
      status = status,
      source_file = as.character(group_data$source_file[[1L]]),
      source_sheet = as.character(source_record$source_sheet[[1L]]),
      source_row = as.integer(source_record$source_row[[1L]]),
      source_column = as.integer(source_record$source_column[[1L]]),
      stringsAsFactors = FALSE
    )
  }

  bind_import_tables(checks, empty_venues_operational_check_table())
}


build_venues_court_hire_checks <- function(
  court_hire,
  validation_tolerance = 0.01
) {

  revenue_data <- court_hire[
    court_hire$metric == "Court Hire Revenue",
    ,
    drop = FALSE
  ]
  total_checks <- build_venues_period_total_checks(
    data = revenue_data,
    dataset = "Court Hire",
    group_column = "court_type",
    item_column = "data_year",
    id_columns = c("court_type", "data_year", "comparison_role"),
    total_period_type = "Reported Total",
    check_type = "monthly_to_reported_total",
    metrics = "Court Hire Revenue",
    validation_tolerance = validation_tolerance
  )
  growth_checks <- build_venues_court_hire_growth_checks(
    court_hire,
    validation_tolerance = validation_tolerance
  )

  bind_import_tables(
    list(total_checks, growth_checks),
    empty_venues_operational_check_table()
  )
}


build_venues_casual_hours_checks <- function(
  casual_hours,
  validation_tolerance = 0.01
) {

  summary_data <- casual_hours[
    casual_hours$record_group == "Monthly Summary",
    ,
    drop = FALSE
  ]
  total_checks <- build_venues_period_total_checks(
    data = summary_data,
    dataset = "Casual Hours",
    group_column = "record_group",
    item_column = "record_id",
    id_columns = c("record_group", "record_id"),
    total_period_type = "Reported Total",
    check_type = "monthly_to_reported_total",
    metrics = c("Budgeted Hours", "Paid Hours", "Variance"),
    validation_tolerance = validation_tolerance
  )
  variance_checks <- build_venues_variance_identity_checks(
    data = summary_data,
    dataset = "Casual Hours",
    group_column = "record_group",
    item_column = "record_id",
    id_columns = c("record_group", "record_id"),
    budget_metric = "Budgeted Hours",
    actual_metric = "Paid Hours",
    variance_metric = "Variance",
    validation_tolerance = validation_tolerance
  )

  bind_import_tables(
    list(total_checks, variance_checks),
    empty_venues_operational_check_table()
  )
}


build_venues_casual_wages_checks <- function(
  casual_wages,
  validation_tolerance = 0.01
) {

  total_checks <- build_venues_period_total_checks(
    data = casual_wages,
    dataset = "Casual Wages",
    group_column = "series",
    item_column = "salary_year",
    id_columns = c("series", "salary_year"),
    total_period_type = "Reported Total",
    check_type = "monthly_to_reported_total",
    metrics = c("Budget", "Actual", "Variance"),
    validation_tolerance = validation_tolerance
  )
  variance_checks <- build_venues_variance_identity_checks(
    data = casual_wages,
    dataset = "Casual Wages",
    group_column = "series",
    item_column = "salary_year",
    id_columns = c("series", "salary_year"),
    budget_metric = "Budget",
    actual_metric = "Actual",
    variance_metric = "Variance",
    validation_tolerance = validation_tolerance
  )

  bind_import_tables(
    list(total_checks, variance_checks),
    empty_venues_operational_check_table()
  )
}


validate_venues_operational_checks <- function(checks) {

  if (!is.data.frame(checks) || nrow(checks) == 0L) {
    return(empty_import_issue_table())
  }

  issues <- list()
  file_dataset_key <- paste(
    checks$source_file,
    checks$dataset,
    sep = "\r"
  )

  for (indices in split(seq_len(nrow(checks)), file_dataset_key)) {
    file_checks <- checks[indices, , drop = FALSE]
    failed <- file_checks$status == "fail"
    incomplete <- !file_checks$status %in% c("pass", "fail")

    if (any(failed)) {
      maximum_difference <- max(
        abs(file_checks$difference[failed]),
        na.rm = TRUE
      )
      issues[[length(issues) + 1L]] <- make_import_issue(
        level = "warning",
        source_file = file_checks$source_file[[1]],
        sheet = file_checks$source_sheet[[1]],
        message = paste0(
          file_checks$dataset[[1]],
          " validation failed for ",
          sum(failed),
          " monthly-to-total check(s); maximum absolute difference: ",
          format(
            round(maximum_difference, 2),
            big.mark = ",",
            scientific = FALSE,
            trim = TRUE,
            nsmall = 2
          ),
          ". Inspect the corresponding *_checks table before using totals."
        )
      )
    }

    if (any(incomplete)) {
      status_counts <- table(file_checks$status[incomplete])
      status_summary <- paste0(
        names(status_counts),
        "=",
        as.integer(status_counts),
        collapse = ", "
      )
      issues[[length(issues) + 1L]] <- make_import_issue(
        level = "warning",
        source_file = file_checks$source_file[[1]],
        sheet = file_checks$source_sheet[[1]],
        message = paste0(
          file_checks$dataset[[1]],
          " validation was incomplete for ",
          sum(incomplete),
          " check(s) (",
          status_summary,
          ")."
        )
      )
    }
  }

  bind_import_tables(issues, empty_import_issue_table())
}


validate_venues_tracking_report_collection <- function(workbooks) {

  if (length(workbooks) == 0L) {
    return(empty_import_issue_table())
  }

  issues <- list()
  years <- vapply(
    workbooks,
    function(x) x$report_year,
    integer(1)
  )

  duplicate_years <- sort(
    unique(years[duplicated(years)])
  )

  if (length(duplicate_years) > 0L) {
    issues[[length(issues) + 1L]] <- make_import_issue(
      level = "warning",
      source_file = NA_character_,
      message = paste0(
        "Multiple Venues Tracking Report files were found for year(s): ",
        paste(duplicate_years, collapse = ", "),
        ". Check for duplicate reporting periods before aggregation."
      )
    )
  }

  expected_years <- seq.int(
    min(years),
    max(years)
  )
  missing_years <- setdiff(
    expected_years,
    years
  )

  if (length(missing_years) > 0L) {
    issues[[length(issues) + 1L]] <- make_import_issue(
      level = "warning",
      source_file = NA_character_,
      message = paste0(
        "The Venues reporting series has missing year(s): ",
        paste(missing_years, collapse = ", "),
        "."
      )
    )
  }

  bind_import_tables(
    issues,
    empty_import_issue_table()
  )
}


venues_finance_record <- function(
  data,
  row_type,
  metric,
  value_type,
  fund = NULL
) {

  keep <-
    data$row_type == row_type &
    data$metric == metric &
    data$value_type == value_type

  if (!is.null(fund)) {
    keep <- keep & data$fund == fund
  }

  records <- data[keep, , drop = FALSE]

  if (nrow(records) != 1L) {
    return(
      list(
        value = NA_real_,
        source_row = NA_integer_,
        source_column = NA_integer_
      )
    )
  }

  list(
    value = records$value[[1]],
    source_row = records$source_row[[1]],
    source_column = records$source_column[[1]]
  )
}


venues_number_or_zero <- function(value) {

  if (length(value) == 0L || is.na(value)) {
    return(0)
  }

  as.numeric(value)
}


make_venues_finance_check <- function(
  month_data,
  fund,
  row_type,
  check_type,
  metric,
  value_type,
  expected_value,
  reported_record,
  validation_tolerance
) {

  reported_value <- reported_record$value
  difference <- if (is.na(reported_value)) {
    NA_real_
  } else {
    reported_value - expected_value
  }
  status <- if (is.na(reported_value)) {
    "missing_reported_value"
  } else if (abs(difference) <= validation_tolerance) {
    "pass"
  } else {
    "fail"
  }

  data.frame(
    report_year = month_data$report_year[[1]],
    date = month_data$date[[1]],
    month = month_data$month[[1]],
    month_label = month_data$month_label[[1]],
    fund = fund,
    row_type = row_type,
    check_type = check_type,
    metric = metric,
    value_type = value_type,
    expected_value = as.numeric(expected_value),
    reported_value = as.numeric(reported_value),
    difference = as.numeric(difference),
    tolerance = as.numeric(validation_tolerance),
    status = status,
    source_file = month_data$source_file[[1]],
    source_sheet = month_data$source_sheet[[1]],
    source_row = as.integer(reported_record$source_row),
    source_column = as.integer(reported_record$source_column),
    stringsAsFactors = FALSE
  )
}


build_venues_finance_checks <- function(
  finance,
  validation_tolerance = 0.01
) {

  if (!is.data.frame(finance) || nrow(finance) == 0L) {
    return(empty_venues_finance_check_table())
  }

  measures <- data.frame(
    metric = c(
      "Income",
      "Income",
      "Income",
      "Expenses",
      "Expenses",
      "Expenses",
      "Profit/Loss"
    ),
    value_type = c(
      "Budget",
      "Actual",
      "Variance",
      "Budget",
      "Actual",
      "Variance",
      "Net Variance"
    ),
    stringsAsFactors = FALSE
  )

  checks <- list()
  source_files <- unique(finance$source_file)

  for (source_file in source_files) {

    file_data <- finance[
      finance$source_file == source_file,
      ,
      drop = FALSE
    ]

    for (month in sort(unique(file_data$month))) {

      month_data <- file_data[
        file_data$month == month,
        ,
        drop = FALSE
      ]
      row_keys <- unique(
        month_data[, c("fund", "row_type"), drop = FALSE]
      )

      for (row_index in seq_len(nrow(row_keys))) {

        fund <- row_keys$fund[[row_index]]
        row_type <- row_keys$row_type[[row_index]]

        for (metric in c("Income", "Expenses")) {

          budget <- venues_finance_record(
            month_data,
            row_type = row_type,
            fund = fund,
            metric = metric,
            value_type = "Budget"
          )
          actual <- venues_finance_record(
            month_data,
            row_type = row_type,
            fund = fund,
            metric = metric,
            value_type = "Actual"
          )
          variance <- venues_finance_record(
            month_data,
            row_type = row_type,
            fund = fund,
            metric = metric,
            value_type = "Variance"
          )

          expected_variance <-
            venues_number_or_zero(actual$value) -
            venues_number_or_zero(budget$value)

          checks[[length(checks) + 1L]] <-
            make_venues_finance_check(
              month_data = month_data,
              fund = fund,
              row_type = row_type,
              check_type = "variance_identity",
              metric = metric,
              value_type = "Variance",
              expected_value = expected_variance,
              reported_record = variance,
              validation_tolerance = validation_tolerance
            )
        }

        income_variance <- venues_finance_record(
          month_data,
          row_type = row_type,
          fund = fund,
          metric = "Income",
          value_type = "Variance"
        )
        expense_variance <- venues_finance_record(
          month_data,
          row_type = row_type,
          fund = fund,
          metric = "Expenses",
          value_type = "Variance"
        )
        profit_loss <- venues_finance_record(
          month_data,
          row_type = row_type,
          fund = fund,
          metric = "Profit/Loss",
          value_type = "Net Variance"
        )

        checks[[length(checks) + 1L]] <-
          make_venues_finance_check(
            month_data = month_data,
            fund = fund,
            row_type = row_type,
            check_type = "profit_loss_identity",
            metric = "Profit/Loss",
            value_type = "Net Variance",
            expected_value =
              venues_number_or_zero(income_variance$value) -
              venues_number_or_zero(expense_variance$value),
            reported_record = profit_loss,
            validation_tolerance = validation_tolerance
          )
      }

      for (measure_index in seq_len(nrow(measures))) {

        metric <- measures$metric[[measure_index]]
        value_type <- measures$value_type[[measure_index]]
        detail_values <- month_data$value[
          month_data$row_type == "Fund" &
            month_data$metric == metric &
            month_data$value_type == value_type
        ]
        mtd_total <- venues_finance_record(
          month_data,
          row_type = "MTD Total",
          metric = metric,
          value_type = value_type
        )

        checks[[length(checks) + 1L]] <-
          make_venues_finance_check(
            month_data = month_data,
            fund = "MTD Total",
            row_type = "MTD Total",
            check_type = "mtd_rollup",
            metric = metric,
            value_type = value_type,
            expected_value = sum(detail_values, na.rm = TRUE),
            reported_record = mtd_total,
            validation_tolerance = validation_tolerance
          )

        mtd_history <- file_data$value[
          file_data$month <= month &
            file_data$row_type == "MTD Total" &
            file_data$metric == metric &
            file_data$value_type == value_type
        ]
        ytd_total <- venues_finance_record(
          month_data,
          row_type = "YTD Total",
          metric = metric,
          value_type = value_type
        )

        checks[[length(checks) + 1L]] <-
          make_venues_finance_check(
            month_data = month_data,
            fund = "YTD Total",
            row_type = "YTD Total",
            check_type = "ytd_cumulative",
            metric = metric,
            value_type = value_type,
            expected_value = sum(mtd_history, na.rm = TRUE),
            reported_record = ytd_total,
            validation_tolerance = validation_tolerance
          )
      }
    }
  }

  result <- bind_import_tables(
    checks,
    empty_venues_finance_check_table()
  )

  if (nrow(result) > 0L) {
    result <- result[
      order(
        result$date,
        result$source_file,
        result$check_type,
        result$row_type,
        result$fund,
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


validate_venues_finance_checks <- function(checks) {

  if (!is.data.frame(checks) || nrow(checks) == 0L) {
    return(empty_import_issue_table())
  }

  issues <- list()

  for (source_file in unique(checks$source_file)) {

    file_checks <- checks[
      checks$source_file == source_file,
      ,
      drop = FALSE
    ]
    failed <- file_checks$status == "fail"
    incomplete <- file_checks$status == "missing_reported_value"

    if (any(failed)) {

      failure_counts <- table(
        file_checks$check_type[failed]
      )
      failure_summary <- paste0(
        names(failure_counts),
        "=",
        as.integer(failure_counts),
        collapse = ", "
      )
      maximum_difference <- max(
        abs(file_checks$difference[failed]),
        na.rm = TRUE
      )

      issues[[length(issues) + 1L]] <- make_import_issue(
        level = "warning",
        source_file = source_file,
        sheet = file_checks$source_sheet[[1]],
        message = paste0(
          "Venues finance validation failed for ",
          sum(failed),
          " check(s) (",
          failure_summary,
          "); maximum absolute difference: ",
          format(
            round(maximum_difference, 2),
            big.mark = ",",
            scientific = FALSE,
            trim = TRUE,
            nsmall = 2
          ),
          ". Inspect reports$checks before using affected totals."
        )
      )
    }

    if (any(incomplete)) {
      issues[[length(issues) + 1L]] <- make_import_issue(
        level = "warning",
        source_file = source_file,
        sheet = file_checks$source_sheet[[1]],
        message = paste0(
          "Venues finance validation was incomplete for ",
          sum(incomplete),
          " check(s) because the reported value was blank."
        )
      )
    }
  }

  bind_import_tables(
    issues,
    empty_import_issue_table()
  )
}
