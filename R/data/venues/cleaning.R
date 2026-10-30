# -------------------------------------------------------------------------
# Venues Tracking Report cleaning
# -------------------------------------------------------------------------

empty_venues_finance_table <- function() {

  data.frame(
    report_year = integer(),
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    business_unit = character(),
    fund = character(),
    row_type = character(),
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


find_venues_finance_month_anchors <- function(raw_sheet) {

  if (
    !is.data.frame(raw_sheet) ||
      nrow(raw_sheet) == 0L ||
      ncol(raw_sheet) < 8L
  ) {
    return(
      data.frame(
        month = integer(),
        header_row = integer(),
        start_column = integer(),
        stringsAsFactors = FALSE
      )
    )
  }

  anchors <- list()

  for (header_row in seq_len(nrow(raw_sheet))) {
    for (start_column in seq_len(ncol(raw_sheet) - 7L)) {

      month <- parse_month_number(
        raw_sheet[[start_column]][header_row]
      )

      if (is.na(month)) {
        next
      }

      income_label <- normalise_excel_label(
        cell_as_text(raw_sheet, header_row, start_column + 1L)
      )
      expenses_label <- normalise_excel_label(
        cell_as_text(raw_sheet, header_row, start_column + 4L)
      )
      profit_loss_label <- normalise_excel_label(
        cell_as_text(raw_sheet, header_row, start_column + 7L)
      )
      funds_label <- normalise_excel_label(
        cell_as_text(raw_sheet, header_row + 1L, start_column)
      )

      if (
        income_label == "income" &&
          expenses_label == "expenses" &&
          profit_loss_label == "profit/loss" &&
          funds_label == "funds"
      ) {
        anchors[[length(anchors) + 1L]] <- data.frame(
          month = as.integer(month),
          header_row = as.integer(header_row),
          start_column = as.integer(start_column),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  result <- bind_import_tables(
    anchors,
    data.frame(
      month = integer(),
      header_row = integer(),
      start_column = integer(),
      stringsAsFactors = FALSE
    )
  )

  if (nrow(result) > 0L) {
    result <- result[
      order(result$month),
      ,
      drop = FALSE
    ]
    rownames(result) <- NULL
  }

  result
}


extract_venues_finance_month <- function(
  raw_sheet,
  report_year,
  source_file,
  sheet_name,
  month,
  header_row,
  start_column
) {

  measure_map <- data.frame(
    column_offset = c(1L, 2L, 3L, 4L, 5L, 6L, 7L),
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

  rows <- list()
  source_row <- header_row + 2L
  found_ytd_total <- FALSE

  while (source_row <= nrow(raw_sheet)) {

    fund <- cell_as_text(
      raw_sheet,
      source_row,
      start_column
    )
    fund_key <- normalise_excel_label(fund)

    if (!nzchar(fund_key)) {
      break
    }

    row_type <- if (fund_key == "mtd total") {
      "MTD Total"
    } else if (fund_key == "ytd total") {
      "YTD Total"
    } else {
      "Fund"
    }

    fund <- if (row_type == "Fund") {
      trimws(fund)
    } else {
      row_type
    }

    for (measure_index in seq_len(nrow(measure_map))) {

      source_column <-
        start_column + measure_map$column_offset[[measure_index]]

      rows[[length(rows) + 1L]] <- data.frame(
        report_year = as.integer(report_year),
        date = as.Date(
          sprintf("%04d-%02d-01", report_year, month)
        ),
        month = as.integer(month),
        month_label = month.abb[[month]],
        business_unit = "Venues",
        fund = fund,
        row_type = row_type,
        metric = measure_map$metric[[measure_index]],
        value_type = measure_map$value_type[[measure_index]],
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

    if (row_type == "YTD Total") {
      found_ytd_total <- TRUE
      break
    }

    source_row <- source_row + 1L
  }

  if (!found_ytd_total) {
    return(empty_venues_finance_table())
  }

  bind_import_tables(
    rows,
    empty_venues_finance_table()
  )
}


clean_venues_tracking_report_finance <- function(workbook) {

  if (!inherits(workbook, "venues_tracking_report_workbook")) {
    stop(
      "workbook must be a venues_tracking_report_workbook object.",
      call. = FALSE
    )
  }

  raw_sheet <- workbook$finance_sheet
  anchors <- find_venues_finance_month_anchors(raw_sheet)

  finance_tables <- lapply(
    seq_len(nrow(anchors)),
    function(index) {
      extract_venues_finance_month(
        raw_sheet = raw_sheet,
        report_year = workbook$report_year,
        source_file = workbook$source_file,
        sheet_name = workbook$finance_sheet_name,
        month = anchors$month[[index]],
        header_row = anchors$header_row[[index]],
        start_column = anchors$start_column[[index]]
      )
    }
  )

  result <- bind_import_tables(
    finance_tables,
    empty_venues_finance_table()
  )

  if (nrow(result) > 0L) {

    row_type_order <- match(
      result$row_type,
      c("Fund", "MTD Total", "YTD Total")
    )
    metric_order <- match(
      result$metric,
      c("Income", "Expenses", "Profit/Loss")
    )
    value_type_order <- match(
      result$value_type,
      c("Budget", "Actual", "Variance", "Net Variance")
    )

    result <- result[
      order(
        result$date,
        row_type_order,
        result$fund,
        metric_order,
        value_type_order
      ),
      ,
      drop = FALSE
    ]
    rownames(result) <- NULL
  }

  result
}


# Shared operational-sheet helpers ---------------------------------------

venues_month_from_text <- function(value) {

  key <- normalise_excel_label(value)

  if (!nzchar(key)) {
    return(NA_integer_)
  }

  key <- substr(gsub("[^a-z]", "", key), 1L, 3L)
  match(key, tolower(month.abb))
}


find_venues_month_header <- function(raw_sheet, minimum_months = 6L) {

  find_month_header_row(raw_sheet, minimum_months)
}


find_venues_month_columns <- function(raw_sheet, header_row) {

  if (
    is.na(header_row) ||
      header_row < 1L ||
      header_row > nrow(raw_sheet)
  ) {
    return(
      data.frame(
        month = integer(),
        column = integer(),
        stringsAsFactors = FALSE
      )
    )
  }

  months <- vapply(
    seq_len(ncol(raw_sheet)),
    function(column) {
      venues_month_from_text(
        cell_as_text(raw_sheet, header_row, column)
      )
    },
    integer(1)
  )

  keep <- !is.na(months)
  result <- data.frame(
    month = as.integer(months[keep]),
    column = as.integer(which(keep)),
    stringsAsFactors = FALSE
  )

  result[order(result$month), , drop = FALSE]
}


canonical_venues_operational_label <- function(value) {

  label <- cell_as_text(
    data.frame(value, stringsAsFactors = FALSE),
    1L,
    1L
  )

  if (is.na(label)) {
    return(NA_character_)
  }

  key <- normalise_excel_label(label)
  replacements <- c(
    "monash univeristy staff" = "Monash University Staff",
    "monash uni sport clubs" = "Monash Sport Clubs",
    "monash sport clubs" = "Monash Sport Clubs",
    "monash sport club" = "Monash Sport Club",
    "schools" = "School",
    "overal total" = "Overall Total",
    "basketball/soccer games hall" = "Basketball/Soccer (Games Hall)",
    "basketball/soccer (games hall)*" = "Basketball/Soccer (Games Hall)",
    "happy hour/am play" = "AM Play / Happy Hour",
    "am/happy hour" = "AM Play / Happy Hour"
  )

  if (key %in% names(replacements)) {
    return(unname(replacements[[key]]))
  }

  label
}


venues_operational_period <- function(
  report_year,
  period_type,
  month = NA_integer_
) {

  if (period_type == "Month") {
    return(
      list(
        date = as.Date(sprintf("%04d-%02d-01", report_year, month)),
        month = as.integer(month),
        month_label = month.abb[[month]]
      )
    )
  }

  list(
    date = as.Date(NA),
    month = NA_integer_,
    month_label = period_type
  )
}


# Facility Attendance ----------------------------------------------------

empty_venues_attendance_table <- function() {

  data.frame(
    report_year = integer(),
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    period_type = character(),
    area = character(),
    activity = character(),
    category = character(),
    row_type = character(),
    value = double(),
    source_file = character(),
    source_sheet = character(),
    source_row = integer(),
    source_column = integer(),
    stringsAsFactors = FALSE
  )
}


venues_attendance_area <- function(activity) {

  key <- normalise_excel_label(activity)

  if (grepl("sport club", key)) {
    return("Sport Club")
  }

  if (
    grepl("outdoor", key) ||
      key == "tennis"
  ) {
    return("Outdoor")
  }

  if (key %in% c("overall", "overall total")) {
    return("All")
  }

  "Indoor"
}


attendance_records_for_row <- function(
  raw_sheet,
  workbook,
  source_row,
  activity,
  category,
  row_type,
  month_columns,
  ytd_column
) {

  periods <- lapply(
    seq_len(nrow(month_columns)),
    function(index) {
      list(
        period_type = "Month",
        month = month_columns$month[[index]],
        column = month_columns$column[[index]]
      )
    }
  )

  if (!is.na(ytd_column)) {
    periods[[length(periods) + 1L]] <- list(
      period_type = "YTD",
      month = NA_integer_,
      column = ytd_column
    )
  }

  records <- lapply(
    periods,
    function(period) {

      period_values <- venues_operational_period(
        report_year = workbook$report_year,
        period_type = period$period_type,
        month = period$month
      )

      list(
        report_year = as.integer(workbook$report_year),
        date = period_values$date,
        month = period_values$month,
        month_label = period_values$month_label,
        period_type = period$period_type,
        area = venues_attendance_area(activity),
        activity = activity,
        category = category,
        row_type = row_type,
        value = cell_as_number(
          raw_sheet,
          source_row,
          period$column
        ),
        source_file = workbook$source_file,
        source_sheet = workbook$attendance_sheet_name,
        source_row = as.integer(source_row),
        source_column = as.integer(period$column)
      )
    }
  )

  records_to_table(records, empty_venues_attendance_table())
}


clean_venues_tracking_report_attendance <- function(workbook) {

  raw_sheet <- workbook$attendance_sheet

  if (!is.data.frame(raw_sheet) || nrow(raw_sheet) == 0L) {
    return(empty_venues_attendance_table())
  }

  header_row <- find_venues_month_header(raw_sheet, 10L)
  month_columns <- find_venues_month_columns(raw_sheet, header_row)
  header_labels <- normalise_excel_label(
    vapply(
      seq_len(ncol(raw_sheet)),
      function(column) cell_as_text(raw_sheet, header_row, column),
      character(1)
    )
  )
  ytd_candidates <- which(header_labels %in% c("ytd", "ytd total"))
  ytd_column <- if (length(ytd_candidates) == 1L) {
    as.integer(ytd_candidates[[1]])
  } else {
    NA_integer_
  }

  records <- list()
  current_activity <- NA_character_
  in_summary <- FALSE
  expect_section <- TRUE

  for (source_row in seq.int(header_row + 1L, nrow(raw_sheet))) {

    raw_label <- cell_as_text(raw_sheet, source_row, 1L)
    label_key <- normalise_excel_label(raw_label)
    month_values <- vapply(
      month_columns$column,
      function(column) cell_as_number(raw_sheet, source_row, column),
      double(1)
    )
    has_values <- any(!is.na(month_values))

    if (label_key == "summary breakdown") {
      in_summary <- TRUE
      current_activity <- NA_character_
      next
    }

    if (!nzchar(label_key)) {
      if (in_summary && has_values && !is.na(current_activity)) {
        records[[length(records) + 1L]] <- attendance_records_for_row(
          raw_sheet = raw_sheet,
          workbook = workbook,
          source_row = source_row,
          activity = current_activity,
          category = "Total",
          row_type = "Summary Total",
          month_columns = month_columns,
          ytd_column = ytd_column
        )
      } else if (!has_values && !in_summary) {
        expect_section <- TRUE
      }
      next
    }

    canonical_label <- canonical_venues_operational_label(raw_label)

    if (grepl("^overal{1,2} ", label_key)) {
      if (label_key == "overall total" || label_key == "overal total") {
        records[[length(records) + 1L]] <- attendance_records_for_row(
          raw_sheet = raw_sheet,
          workbook = workbook,
          source_row = source_row,
          activity = "Overall",
          category = "Overall Total",
          row_type = "Overall Total",
          month_columns = month_columns,
          ytd_column = ytd_column
        )
      } else {
        current_activity <- sub(
          "[[:space:]]+MTD[[:space:]]+Total$",
          "",
          canonical_label,
          ignore.case = TRUE
        )
        in_summary <- TRUE
      }
      next
    }

    if (in_summary) {
      row_type <- if (label_key == "total") {
        "Summary Total"
      } else {
        "Summary Detail"
      }
      category <- if (label_key == "total") {
        "Total"
      } else {
        canonical_label
      }
    } else if (expect_section) {
      current_activity <- canonical_label
      row_type <- "Section Value"
      category <- "Reported Section Value"
      expect_section <- FALSE
    } else if (label_key == "total") {
      row_type <- "Section Total"
      category <- "Total"
      expect_section <- TRUE
    } else {
      row_type <- "Detail"
      category <- canonical_label
    }

    if (is.na(current_activity)) {
      next
    }

    records[[length(records) + 1L]] <- attendance_records_for_row(
      raw_sheet = raw_sheet,
      workbook = workbook,
      source_row = source_row,
      activity = current_activity,
      category = category,
      row_type = row_type,
      month_columns = month_columns,
      ytd_column = ytd_column
    )
  }

  result <- bind_import_tables(
    records,
    empty_venues_attendance_table()
  )

  if (nrow(result) > 0L) {
    result <- result[
      order(
        result$report_year,
        is.na(result$month),
        result$month,
        result$area,
        result$activity,
        result$source_row
      ),
      ,
      drop = FALSE
    ]
    rownames(result) <- NULL
  }

  result
}


# Facility Hire Tracking -------------------------------------------------

empty_venues_facility_hire_table <- function() {

  data.frame(
    report_year = integer(),
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    period_type = character(),
    facility = character(),
    category = character(),
    raw_label = character(),
    row_type = character(),
    unit_cost = double(),
    metric = character(),
    value = double(),
    source_file = character(),
    source_sheet = character(),
    source_row = integer(),
    source_column = integer(),
    stringsAsFactors = FALSE
  )
}


canonical_venues_facility <- function(value) {

  label <- canonical_venues_operational_label(value)
  key <- normalise_excel_label(label)
  replacements <- c(
    "badminton" = "Badminton",
    "squash" = "Squash",
    "table tennis" = "Table Tennis",
    "basketball/soccer games hall" = "Basketball/Soccer (Games Hall)",
    "volleyball" = "Volleyball",
    "tennis" = "Tennis",
    "rec hall" = "Rec Hall",
    "stadium" = "Stadium",
    "seminar room" = "Seminar Room",
    "mp meeting rooms" = "MP Meeting Rooms",
    "np meeting rooms" = "NP Meeting Rooms",
    "group fitness studio" = "Group Fitness Studio",
    "mind & body/cycle" = "Mind & Body/Cycle",
    "martial arts" = "Martial Arts",
    "multi-purpose studio" = "Multi-Purpose Studio",
    "frearson oval" = "Frearson Oval",
    "synthetics" = "Synthetics",
    "practice pitch" = "Practice Pitch",
    "futsal pitches" = "Futsal Pitches",
    "hockey pitch" = "Hockey Pitch",
    "hv" = "HV",
    "bbq" = "BBQ"
  )

  if (key %in% names(replacements)) {
    return(unname(replacements[[key]]))
  }

  label
}


facility_hire_records_for_row <- function(
  raw_sheet,
  workbook,
  source_row,
  facility,
  category,
  raw_label,
  row_type,
  month_columns,
  ytd_column
) {

  records <- list()
  unit_cost <- cell_as_number(raw_sheet, source_row, 2L)

  for (index in seq_len(nrow(month_columns))) {

    month <- month_columns$month[[index]]
    revenue_column <- month_columns$column[[index]]
    transaction_column <- revenue_column + 1L
    period <- venues_operational_period(
      workbook$report_year,
      "Month",
      month
    )

    for (metric_index in seq_along(c("Revenue", "Transactions"))) {
      metric <- c("Revenue", "Transactions")[[metric_index]]
      source_column <- c(
        revenue_column,
        transaction_column
      )[[metric_index]]

      records[[length(records) + 1L]] <- list(
        report_year = as.integer(workbook$report_year),
        date = period$date,
        month = period$month,
        month_label = period$month_label,
        period_type = "Month",
        facility = facility,
        category = category,
        raw_label = raw_label,
        row_type = row_type,
        unit_cost = unit_cost,
        metric = metric,
        value = cell_as_number(raw_sheet, source_row, source_column),
        source_file = workbook$source_file,
        source_sheet = workbook$facility_hire_sheet_name,
        source_row = as.integer(source_row),
        source_column = as.integer(source_column)
      )
    }
  }

  if (!is.na(ytd_column)) {
    for (metric_index in seq_along(c("Revenue", "Transactions"))) {
      metric <- c("Revenue", "Transactions")[[metric_index]]
      source_column <- ytd_column + metric_index - 1L

      records[[length(records) + 1L]] <- list(
        report_year = as.integer(workbook$report_year),
        date = as.Date(NA),
        month = NA_integer_,
        month_label = "YTD",
        period_type = "YTD",
        facility = facility,
        category = category,
        raw_label = raw_label,
        row_type = row_type,
        unit_cost = unit_cost,
        metric = metric,
        value = cell_as_number(raw_sheet, source_row, source_column),
        source_file = workbook$source_file,
        source_sheet = workbook$facility_hire_sheet_name,
        source_row = as.integer(source_row),
        source_column = as.integer(source_column)
      )
    }
  }

  records_to_table(records, empty_venues_facility_hire_table())
}


clean_venues_tracking_report_facility_hire <- function(workbook) {

  raw_sheet <- workbook$facility_hire_sheet

  if (!is.data.frame(raw_sheet) || nrow(raw_sheet) == 0L) {
    return(empty_venues_facility_hire_table())
  }

  header_row <- find_venues_month_header(raw_sheet, 10L)
  month_columns <- find_venues_month_columns(raw_sheet, header_row)
  header_labels <- normalise_excel_label(
    vapply(
      seq_len(ncol(raw_sheet)),
      function(column) cell_as_text(raw_sheet, header_row, column),
      character(1)
    )
  )
  ytd_candidates <- which(header_labels == "ytd")
  ytd_column <- if (length(ytd_candidates) == 1L) {
    as.integer(ytd_candidates[[1]])
  } else {
    NA_integer_
  }

  records <- list()
  current_facility <- NA_character_

  for (source_row in seq.int(header_row + 1L, nrow(raw_sheet))) {

    raw_label <- cell_as_text(raw_sheet, source_row, 1L)
    label_key <- normalise_excel_label(raw_label)

    if (!nzchar(label_key)) {
      next
    }

    value_columns <- c(
      month_columns$column,
      month_columns$column + 1L,
      if (!is.na(ytd_column)) c(ytd_column, ytd_column + 1L)
    )
    row_values <- vapply(
      value_columns[value_columns <= ncol(raw_sheet)],
      function(column) cell_as_number(raw_sheet, source_row, column),
      double(1)
    )
    has_values <- any(!is.na(row_values))
    unit_cost <- cell_as_number(raw_sheet, source_row, 2L)

    if (label_key == "total") {
      if (is.na(current_facility)) {
        next
      }
      row_type <- "Total"
      category <- "Total"
    } else if (!has_values && is.na(unit_cost)) {
      current_facility <- canonical_venues_facility(raw_label)
      next
    } else {
      if (is.na(current_facility)) {
        current_facility <- canonical_venues_facility(raw_label)
        next
      }
      row_type <- "Detail"
      category <- canonical_venues_operational_label(raw_label)
    }

    records[[length(records) + 1L]] <- facility_hire_records_for_row(
      raw_sheet = raw_sheet,
      workbook = workbook,
      source_row = source_row,
      facility = current_facility,
      category = category,
      raw_label = raw_label,
      row_type = row_type,
      month_columns = month_columns,
      ytd_column = ytd_column
    )
  }

  result <- bind_import_tables(
    records,
    empty_venues_facility_hire_table()
  )

  if (nrow(result) > 0L) {
    result <- result[
      order(
        result$report_year,
        is.na(result$month),
        result$month,
        result$facility,
        result$row_type,
        result$category,
        result$metric
      ),
      ,
      drop = FALSE
    ]
    rownames(result) <- NULL
  }

  result
}


# Membership Tracking ----------------------------------------------------

empty_venues_membership_table <- function() {

  data.frame(
    report_year = integer(),
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    period_type = character(),
    membership_group = character(),
    membership_type = character(),
    row_type = character(),
    metric = character(),
    raw_metric = character(),
    value = double(),
    source_file = character(),
    source_sheet = character(),
    source_row = integer(),
    source_column = integer(),
    stringsAsFactors = FALSE
  )
}


canonical_venues_membership_group <- function(value) {

  label <- canonical_venues_operational_label(value)
  key <- normalise_excel_label(label)

  if (key == "fitness") {
    return("Fitness")
  }
  if (key == "fitness & aquatics") {
    return("Fitness & Aquatics")
  }
  if (grepl("^staff memberships", key)) {
    return("Staff Memberships")
  }
  if (key == "aquatics memberships") {
    return("Aquatics Memberships")
  }
  if (grepl("^free offerings", key)) {
    return("Free Offerings")
  }

  label
}


is_venues_membership_group <- function(value) {

  key <- normalise_excel_label(value)

  key %in% c(
    "fitness",
    "fitness & aquatics",
    "aquatics memberships"
  ) ||
    grepl("^staff memberships", key) ||
    grepl("^free offerings", key)
}


canonical_venues_membership_metric <- function(value) {

  key <- normalise_excel_label(value)
  replacements <- c(
    "new" = "New",
    "renewed" = "Renewed",
    "total" = "Total",
    "expired members" = "Expired/Cancelled",
    "expired/cancelled" = "Expired/Cancelled",
    "active members" = "Active",
    "active" = "Active",
    "retention" = "Retention"
  )

  if (key %in% names(replacements)) {
    return(unname(replacements[[key]]))
  }

  canonical_venues_operational_label(value)
}


membership_records_for_row <- function(
  raw_sheet,
  workbook,
  source_row,
  membership_group,
  membership_type,
  row_type,
  period_blocks,
  metric_row
) {

  records <- list()

  for (block_index in seq_len(nrow(period_blocks))) {

    block <- period_blocks[block_index, , drop = FALSE]
    period <- venues_operational_period(
      workbook$report_year,
      block$period_type[[1]],
      block$month[[1]]
    )

    for (source_column in seq.int(
      block$start_column[[1]],
      block$end_column[[1]]
    )) {

      raw_metric <- cell_as_text(
        raw_sheet,
        metric_row,
        source_column
      )

      if (is.na(raw_metric)) {
        next
      }

      records[[length(records) + 1L]] <- list(
        report_year = as.integer(workbook$report_year),
        date = period$date,
        month = period$month,
        month_label = period$month_label,
        period_type = block$period_type[[1]],
        membership_group = membership_group,
        membership_type = membership_type,
        row_type = row_type,
        metric = canonical_venues_membership_metric(raw_metric),
        raw_metric = raw_metric,
        value = cell_as_number(raw_sheet, source_row, source_column),
        source_file = workbook$source_file,
        source_sheet = workbook$membership_sheet_name,
        source_row = as.integer(source_row),
        source_column = as.integer(source_column)
      )
    }
  }

  records_to_table(records, empty_venues_membership_table())
}


clean_venues_tracking_report_membership <- function(workbook) {

  raw_sheet <- workbook$membership_sheet

  if (!is.data.frame(raw_sheet) || nrow(raw_sheet) == 0L) {
    return(empty_venues_membership_table())
  }

  header_row <- find_venues_month_header(raw_sheet, 10L)
  metric_row <- header_row + 1L
  month_columns <- find_venues_month_columns(raw_sheet, header_row)
  header_labels <- normalise_excel_label(
    vapply(
      seq_len(ncol(raw_sheet)),
      function(column) cell_as_text(raw_sheet, header_row, column),
      character(1)
    )
  )
  total_candidates <- which(header_labels == "total")
  total_column <- if (length(total_candidates) == 1L) {
    as.integer(total_candidates[[1]])
  } else {
    NA_integer_
  }

  period_blocks <- list()

  for (index in seq_len(nrow(month_columns))) {
    start_column <- month_columns$column[[index]]
    next_column <- if (index < nrow(month_columns)) {
      month_columns$column[[index + 1L]]
    } else if (!is.na(total_column)) {
      total_column
    } else {
      ncol(raw_sheet) + 1L
    }

    period_blocks[[length(period_blocks) + 1L]] <- data.frame(
      period_type = "Month",
      month = month_columns$month[[index]],
      start_column = as.integer(start_column),
      end_column = as.integer(next_column - 1L),
      stringsAsFactors = FALSE
    )
  }

  if (!is.na(total_column)) {
    period_blocks[[length(period_blocks) + 1L]] <- data.frame(
      period_type = "Reported Total",
      month = NA_integer_,
      start_column = total_column,
      end_column = ncol(raw_sheet),
      stringsAsFactors = FALSE
    )
  }

  period_blocks <- bind_import_tables(
    period_blocks,
    data.frame(
      period_type = character(),
      month = integer(),
      start_column = integer(),
      end_column = integer(),
      stringsAsFactors = FALSE
    )
  )

  records <- list()
  current_group <- NA_character_

  for (source_row in seq.int(metric_row + 1L, nrow(raw_sheet))) {

    raw_label <- cell_as_text(raw_sheet, source_row, 1L)

    if (is.na(raw_label)) {
      current_group <- NA_character_
      next
    }

    if (is_venues_membership_group(raw_label)) {
      current_group <- canonical_venues_membership_group(raw_label)
      membership_type <- current_group
      row_type <- "Section Total"
    } else if (
      is.na(current_group) ||
        current_group == "Supplementary Metrics"
    ) {
      current_group <- "Supplementary Metrics"
      membership_type <- canonical_venues_operational_label(raw_label)
      row_type <- "Supplementary"
    } else {
      membership_type <- canonical_venues_operational_label(raw_label)
      row_type <- "Detail"
    }

    records[[length(records) + 1L]] <- membership_records_for_row(
      raw_sheet = raw_sheet,
      workbook = workbook,
      source_row = source_row,
      membership_group = current_group,
      membership_type = membership_type,
      row_type = row_type,
      period_blocks = period_blocks,
      metric_row = metric_row
    )
  }

  result <- bind_import_tables(
    records,
    empty_venues_membership_table()
  )

  if (nrow(result) > 0L) {
    result <- result[
      order(
        result$report_year,
        is.na(result$month),
        result$month,
        result$membership_group,
        result$row_type,
        result$membership_type,
        result$metric
      ),
      ,
      drop = FALSE
    ]
    rownames(result) <- NULL
  }

  result
}


# Online Sales -----------------------------------------------------------

empty_venues_online_sales_table <- function() {

  data.frame(
    report_year = integer(),
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    period_type = character(),
    site = character(),
    site_scope = character(),
    category = character(),
    row_type = character(),
    value = double(),
    is_reported = logical(),
    source_file = character(),
    source_sheet = character(),
    source_row = integer(),
    source_column = integer(),
    stringsAsFactors = FALSE
  )
}


find_venues_online_sales_headers <- function(raw_sheet) {

  if (!is.data.frame(raw_sheet) || nrow(raw_sheet) == 0L) {
    return(
      data.frame(
        header_row = integer(),
        site = character(),
        ytd_column = integer(),
        stringsAsFactors = FALSE
      )
    )
  }

  headers <- list()

  for (source_row in seq_len(nrow(raw_sheet))) {
    month_columns <- find_venues_month_columns(raw_sheet, source_row)

    if (nrow(month_columns) < 10L) {
      next
    }

    site <- canonical_venues_operational_label(
      cell_as_text(raw_sheet, source_row, 1L)
    )

    if (is.na(site)) {
      next
    }

    header_labels <- normalise_excel_label(
      vapply(
        seq_len(ncol(raw_sheet)),
        function(column) cell_as_text(raw_sheet, source_row, column),
        character(1)
      )
    )
    ytd_candidates <- which(header_labels %in% c("ytd", "ytd total"))

    headers[[length(headers) + 1L]] <- data.frame(
      header_row = as.integer(source_row),
      site = site,
      ytd_column = if (length(ytd_candidates) == 1L) {
        as.integer(ytd_candidates[[1]])
      } else {
        NA_integer_
      },
      stringsAsFactors = FALSE
    )
  }

  bind_import_tables(
    headers,
    data.frame(
      header_row = integer(),
      site = character(),
      ytd_column = integer(),
      stringsAsFactors = FALSE
    )
  )
}


online_sales_records_for_row <- function(
  raw_sheet,
  workbook,
  source_row,
  site,
  category,
  row_type,
  month_columns,
  ytd_column
) {

  periods <- lapply(
    seq_len(nrow(month_columns)),
    function(index) {
      list(
        period_type = "Month",
        month = month_columns$month[[index]],
        column = month_columns$column[[index]]
      )
    }
  )

  if (!is.na(ytd_column)) {
    periods[[length(periods) + 1L]] <- list(
      period_type = "YTD",
      month = NA_integer_,
      column = ytd_column
    )
  }

  records <- lapply(
    periods,
    function(period) {
      period_values <- venues_operational_period(
        workbook$report_year,
        period$period_type,
        period$month
      )
      reported_value <- cell_as_number(
        raw_sheet,
        source_row,
        period$column
      )

      list(
        report_year = as.integer(workbook$report_year),
        date = period_values$date,
        month = period_values$month,
        month_label = period_values$month_label,
        period_type = period$period_type,
        site = site,
        site_scope = if (
          grepl("cross campus|overall", normalise_excel_label(site))
        ) {
          "Cross Campus"
        } else {
          "Site"
        },
        category = category,
        row_type = row_type,
        value = reported_value,
        is_reported = !is.na(reported_value),
        source_file = workbook$source_file,
        source_sheet = workbook$online_sales_sheet_name,
        source_row = as.integer(source_row),
        source_column = as.integer(period$column)
      )
    }
  )

  records_to_table(records, empty_venues_online_sales_table())
}


clean_venues_tracking_report_online_sales <- function(workbook) {

  raw_sheet <- workbook$online_sales_sheet

  if (!is.data.frame(raw_sheet) || nrow(raw_sheet) == 0L) {
    return(empty_venues_online_sales_table())
  }

  headers <- find_venues_online_sales_headers(raw_sheet)
  records <- list()

  for (index in seq_len(nrow(headers))) {
    header_row <- headers$header_row[[index]]
    next_header_row <- if (index < nrow(headers)) {
      headers$header_row[[index + 1L]]
    } else {
      nrow(raw_sheet) + 1L
    }
    month_columns <- find_venues_month_columns(raw_sheet, header_row)

    for (source_row in seq.int(header_row + 1L, next_header_row - 1L)) {
      raw_category <- cell_as_text(raw_sheet, source_row, 1L)

      if (is.na(raw_category)) {
        next
      }

      category <- canonical_venues_operational_label(raw_category)
      row_type <- if (normalise_excel_label(category) == "total") {
        "Total"
      } else {
        "Category"
      }

      records[[length(records) + 1L]] <- online_sales_records_for_row(
        raw_sheet = raw_sheet,
        workbook = workbook,
        source_row = source_row,
        site = headers$site[[index]],
        category = category,
        row_type = row_type,
        month_columns = month_columns,
        ytd_column = headers$ytd_column[[index]]
      )
    }
  }

  result <- bind_import_tables(records, empty_venues_online_sales_table())

  if (nrow(result) > 0L) {
    atomic_rows <-
      result$period_type == "Month" &
      result$site_scope == "Site" &
      result$row_type == "Category"
    atomic_keys <- paste(
      result$source_file[atomic_rows],
      result$report_year[atomic_rows],
      result$month[atomic_rows],
      result$site[atomic_rows],
      sep = "\r"
    )

    for (indices in split(which(atomic_rows), atomic_keys)) {
      site_month_reported <- any(result$is_reported[indices])
      result$is_reported[indices] <- site_month_reported
    }

    formula_rows <-
      result$period_type == "Month" &
      (result$row_type == "Total" | result$site_scope == "Cross Campus")

    for (row_index in which(formula_rows)) {
      comparable <-
        atomic_rows &
        result$source_file == result$source_file[[row_index]] &
        result$report_year == result$report_year[[row_index]] &
        result$month == result$month[[row_index]]

      if (result$site_scope[[row_index]] == "Site") {
        comparable <- comparable &
          result$site == result$site[[row_index]]
      }

      if (result$row_type[[row_index]] == "Category") {
        comparable <- comparable &
          result$category == result$category[[row_index]]
      }

      result$is_reported[[row_index]] <- any(
        result$is_reported[comparable]
      )
    }

    result <- result[
      order(
        result$report_year,
        is.na(result$month),
        result$month,
        result$site_scope,
        result$site,
        result$source_row
      ),
      ,
      drop = FALSE
    ]
    rownames(result) <- NULL
  }

  result
}


# Merchandise Tracking --------------------------------------------------

empty_venues_merchandise_table <- function() {

  data.frame(
    report_year = integer(),
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    period_type = character(),
    section = character(),
    item = character(),
    raw_item = character(),
    row_type = character(),
    unit_cost = double(),
    metric = character(),
    value = double(),
    source_file = character(),
    source_sheet = character(),
    source_row = integer(),
    source_column = integer(),
    stringsAsFactors = FALSE
  )
}


canonical_venues_merchandise_metric <- function(value) {

  key <- normalise_excel_label(value)

  if (key %in% c("mtd", "ytd", "revenue", "sales")) {
    return("Revenue")
  }

  if (grepl("transaction", key)) {
    return("Transactions")
  }

  canonical_venues_operational_label(value)
}


find_venues_merchandise_periods <- function(raw_sheet) {

  header_row <- find_venues_month_header(raw_sheet, 10L)

  if (is.na(header_row)) {
    return(
      list(
        header_row = NA_integer_,
        metric_row = NA_integer_,
        periods = data.frame(
          period_type = character(),
          month = integer(),
          start_column = integer(),
          end_column = integer(),
          stringsAsFactors = FALSE
        )
      )
    )
  }

  metric_row <- header_row + 1L
  month_columns <- find_venues_month_columns(raw_sheet, header_row)
  header_labels <- normalise_excel_label(
    vapply(
      seq_len(ncol(raw_sheet)),
      function(column) cell_as_text(raw_sheet, header_row, column),
      character(1)
    )
  )
  ytd_candidates <- which(header_labels %in% c("ytd", "ytd total"))
  ytd_column <- if (length(ytd_candidates) == 1L) {
    as.integer(ytd_candidates[[1]])
  } else {
    NA_integer_
  }
  periods <- list()

  for (index in seq_len(nrow(month_columns))) {
    start_column <- month_columns$column[[index]]
    next_column <- if (index < nrow(month_columns)) {
      month_columns$column[[index + 1L]]
    } else if (!is.na(ytd_column)) {
      ytd_column
    } else {
      ncol(raw_sheet) + 1L
    }

    periods[[length(periods) + 1L]] <- data.frame(
      period_type = "Month",
      month = month_columns$month[[index]],
      start_column = as.integer(start_column),
      end_column = as.integer(next_column - 1L),
      stringsAsFactors = FALSE
    )
  }

  if (!is.na(ytd_column)) {
    periods[[length(periods) + 1L]] <- data.frame(
      period_type = "YTD",
      month = NA_integer_,
      start_column = ytd_column,
      end_column = ncol(raw_sheet),
      stringsAsFactors = FALSE
    )
  }

  list(
    header_row = as.integer(header_row),
    metric_row = as.integer(metric_row),
    periods = bind_import_tables(
      periods,
      data.frame(
        period_type = character(),
        month = integer(),
        start_column = integer(),
        end_column = integer(),
        stringsAsFactors = FALSE
      )
    )
  )
}


merchandise_records_for_row <- function(
  raw_sheet,
  workbook,
  source_row,
  section,
  item,
  raw_item,
  row_type,
  unit_cost,
  layout
) {

  records <- list()

  for (period_index in seq_len(nrow(layout$periods))) {
    period_block <- layout$periods[period_index, , drop = FALSE]
    period <- venues_operational_period(
      workbook$report_year,
      period_block$period_type[[1]],
      period_block$month[[1]]
    )

    for (source_column in seq.int(
      period_block$start_column[[1]],
      period_block$end_column[[1]]
    )) {
      raw_metric <- cell_as_text(
        raw_sheet,
        layout$metric_row,
        source_column
      )
      metric <- canonical_venues_merchandise_metric(raw_metric)

      if (is.na(metric)) {
        next
      }

      reported_value <- cell_as_number(
        raw_sheet,
        source_row,
        source_column
      )

      records[[length(records) + 1L]] <- list(
        report_year = as.integer(workbook$report_year),
        date = period$date,
        month = period$month,
        month_label = period$month_label,
        period_type = period_block$period_type[[1]],
        section = section,
        item = item,
        raw_item = raw_item,
        row_type = row_type,
        unit_cost = as.numeric(unit_cost),
        metric = metric,
        value = reported_value,
        source_file = workbook$source_file,
        source_sheet = workbook$merchandise_sheet_name,
        source_row = as.integer(source_row),
        source_column = as.integer(source_column)
      )
    }
  }

  records_to_table(records, empty_venues_merchandise_table())
}


clean_venues_tracking_report_merchandise <- function(workbook) {

  raw_sheet <- workbook$merchandise_sheet

  if (!is.data.frame(raw_sheet) || nrow(raw_sheet) == 0L) {
    return(empty_venues_merchandise_table())
  }

  layout <- find_venues_merchandise_periods(raw_sheet)

  if (is.na(layout$metric_row) || nrow(layout$periods) == 0L) {
    return(empty_venues_merchandise_table())
  }

  current_section <- canonical_venues_operational_label(
    cell_as_text(raw_sheet, layout$metric_row, 1L)
  )
  records <- list()

  for (source_row in seq.int(layout$metric_row + 1L, nrow(raw_sheet))) {
    raw_item <- cell_as_text(raw_sheet, source_row, 1L)

    if (is.na(raw_item)) {
      next
    }

    item_key <- normalise_excel_label(raw_item)
    unit_cost <- cell_as_number(raw_sheet, source_row, 2L)
    value_columns <- unlist(
      lapply(
        seq_len(nrow(layout$periods)),
        function(index) seq.int(
          layout$periods$start_column[[index]],
          layout$periods$end_column[[index]]
        )
      ),
      use.names = FALSE
    )
    row_values <- vapply(
      value_columns,
      function(column) cell_as_number(raw_sheet, source_row, column),
      double(1)
    )
    has_values <- any(!is.na(row_values))

    if (item_key == "total") {
      row_type <- "Section Total"
      item <- "Total"
    } else if (is.na(unit_cost) && !has_values) {
      current_section <- canonical_venues_operational_label(raw_item)
      next
    } else {
      if (is.na(current_section)) {
        next
      }
      row_type <- "Item"
      item <- canonical_venues_operational_label(raw_item)
    }

    records[[length(records) + 1L]] <- merchandise_records_for_row(
      raw_sheet = raw_sheet,
      workbook = workbook,
      source_row = source_row,
      section = current_section,
      item = item,
      raw_item = raw_item,
      row_type = row_type,
      unit_cost = unit_cost,
      layout = layout
    )
  }

  result <- bind_import_tables(records, empty_venues_merchandise_table())

  if (nrow(result) > 0L) {
    result <- result[
      order(
        result$report_year,
        is.na(result$month),
        result$month,
        result$section,
        result$source_row,
        result$metric
      ),
      ,
      drop = FALSE
    ]
    rownames(result) <- NULL
  }

  result
}


# Court Hire -------------------------------------------------------------

empty_venues_court_hire_table <- function() {

  data.frame(
    report_year = integer(),
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    period_type = character(),
    court_type = character(),
    row_type = character(),
    data_year = integer(),
    comparison_role = character(),
    metric = character(),
    value = double(),
    is_reported = logical(),
    source_file = character(),
    source_sheet = character(),
    source_row = integer(),
    source_column = integer(),
    stringsAsFactors = FALSE
  )
}


venues_year_from_text <- function(value) {

  if (length(value) == 0L || is.na(value)) {
    return(NA_integer_)
  }

  text <- trimws(as.character(value))

  if (is.na(text) || !grepl("^(19|20)[0-9]{2}$", text)) {
    return(NA_integer_)
  }

  as.integer(text)
}


find_venues_court_hire_layout <- function(raw_sheet, report_year) {

  empty_layout <- data.frame(
    header_row = integer(),
    label_column = integer(),
    previous_column = integer(),
    current_column = integer(),
    growth_column = integer(),
    previous_year = integer(),
    current_year = integer(),
    court_type = character(),
    stringsAsFactors = FALSE
  )

  if (!is.data.frame(raw_sheet) || nrow(raw_sheet) < 2L || ncol(raw_sheet) < 4L) {
    return(empty_layout)
  }

  layouts <- list()

  for (header_row in seq.int(2L, nrow(raw_sheet))) {
    labels <- normalise_excel_label(vapply(
      seq_len(ncol(raw_sheet)),
      function(column) cell_as_text(raw_sheet, header_row, column),
      character(1L)
    ))
    label_columns <- which(labels == "month")

    for (label_column in label_columns) {
      if (label_column + 3L > ncol(raw_sheet)) {
        next
      }

      previous_column <- label_column + 1L
      current_column <- label_column + 2L
      growth_column <- label_column + 3L
      previous_year <- venues_year_from_text(
        raw_sheet[[previous_column]][header_row]
      )
      current_year <- venues_year_from_text(
        raw_sheet[[current_column]][header_row]
      )
      growth_label <- labels[[growth_column]]
      court_type <- cell_as_text(raw_sheet, header_row - 1L, label_column)

      if (is.na(previous_year) ||
          is.na(current_year) ||
          current_year != as.integer(report_year) ||
          !growth_label %in% c("growth", "growth achieved") ||
          is.na(court_type)) {
        next
      }

      layouts[[length(layouts) + 1L]] <- data.frame(
        header_row = as.integer(header_row),
        label_column = as.integer(label_column),
        previous_column = as.integer(previous_column),
        current_column = as.integer(current_column),
        growth_column = as.integer(growth_column),
        previous_year = as.integer(previous_year),
        current_year = as.integer(current_year),
        court_type = court_type,
        stringsAsFactors = FALSE
      )
    }
  }

  result <- bind_import_tables(layouts, empty_layout)

  if (nrow(result) > 0L) {
    result <- result[order(result$header_row, result$label_column), , drop = FALSE]
    rownames(result) <- NULL
  }

  result
}


clean_venues_tracking_report_court_hire <- function(workbook) {

  raw_sheet <- workbook$court_hire_sheet

  if (!is.data.frame(raw_sheet) || nrow(raw_sheet) == 0L) {
    return(empty_venues_court_hire_table())
  }

  layout <- find_venues_court_hire_layout(raw_sheet, workbook$report_year)

  if (nrow(layout) == 0L) {
    return(empty_venues_court_hire_table())
  }

  records <- list()

  for (layout_index in seq_len(nrow(layout))) {
    block <- layout[layout_index, , drop = FALSE]
    metric_columns <- c(
      "Previous Year" = block$previous_column[[1L]],
      "Report Year" = block$current_column[[1L]],
      "Growth Achieved" = block$growth_column[[1L]]
    )
    metric_years <- c(
      "Previous Year" = block$previous_year[[1L]],
      "Report Year" = block$current_year[[1L]],
      "Growth Achieved" = block$current_year[[1L]]
    )

    for (source_row in seq.int(block$header_row[[1L]] + 1L, nrow(raw_sheet))) {
      raw_period <- cell_as_text(
        raw_sheet,
        source_row,
        block$label_column[[1L]]
      )
      period_key <- normalise_excel_label(raw_period)
      month <- venues_month_from_text(raw_period)

      if (is.na(month) && period_key != "total") {
        next
      }

      period_type <- if (period_key == "total") {
        "Reported Total"
      } else {
        "Month"
      }

      for (comparison_role in names(metric_columns)) {
        source_column <- unname(metric_columns[[comparison_role]])
        data_year <- unname(metric_years[[comparison_role]])
        reported_value <- cell_as_number(raw_sheet, source_row, source_column)
        metric <- if (comparison_role == "Growth Achieved") {
          "Growth Achieved"
        } else {
          "Court Hire Revenue"
        }
        record_date <- if (period_type == "Month") {
          as.Date(sprintf("%04d-%02d-01", data_year, month))
        } else {
          as.Date(NA)
        }

        records[[length(records) + 1L]] <- data.frame(
          report_year = as.integer(workbook$report_year),
          date = record_date,
          month = as.integer(month),
          month_label = if (is.na(month)) NA_character_ else month.abb[[month]],
          period_type = period_type,
          court_type = block$court_type[[1L]],
          row_type = "Court Hire",
          data_year = as.integer(data_year),
          comparison_role = comparison_role,
          metric = metric,
          value = reported_value,
          is_reported = !is.na(reported_value),
          source_file = workbook$source_file,
          source_sheet = workbook$court_hire_sheet_name,
          source_row = as.integer(source_row),
          source_column = as.integer(source_column),
          stringsAsFactors = FALSE
        )
      }

      if (period_key == "total") {
        break
      }
    }
  }

  result <- bind_import_tables(records, empty_venues_court_hire_table())

  if (nrow(result) > 0L) {
    result <- result[
      order(
        result$report_year,
        result$court_type,
        result$period_type == "Reported Total",
        is.na(result$month),
        result$month,
        match(
          result$comparison_role,
          c("Previous Year", "Report Year", "Growth Achieved")
        )
      ),
      ,
      drop = FALSE
    ]
    rownames(result) <- NULL
  }

  result
}


# Casual Hours -----------------------------------------------------------

empty_venues_casual_hours_table <- function() {

  data.frame(
    report_year = integer(),
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    period_type = character(),
    record_group = character(),
    record_id = character(),
    row_type = character(),
    roster = character(),
    fortnight = character(),
    pay_period_start = as.Date(character()),
    pay_period_end = as.Date(character()),
    timesheet_deadline = as.Date(character()),
    pay_date = as.Date(character()),
    notes = character(),
    metric = character(),
    value = double(),
    is_reported = logical(),
    source_file = character(),
    source_sheet = character(),
    source_row = integer(),
    source_column = integer(),
    stringsAsFactors = FALSE
  )
}


venues_cell_as_date <- function(raw_sheet, row, column) {

  if (
    length(row) != 1L ||
      length(column) != 1L ||
      is.na(row) ||
      is.na(column) ||
      row < 1L ||
      column < 1L ||
      row > nrow(raw_sheet) ||
      column > ncol(raw_sheet)
  ) {
    return(as.Date(NA))
  }

  value <- raw_sheet[[column]][row]

  if (length(value) == 0L || is.na(value)) {
    return(as.Date(NA))
  }

  if (inherits(value, "Date")) {
    return(as.Date(value))
  }

  if (inherits(value, c("POSIXct", "POSIXlt"))) {
    return(as.Date(value))
  }

  if (is.numeric(value) && value > 1000) {
    return(as.Date(as.numeric(value), origin = "1899-12-30"))
  }

  text <- trimws(as.character(value))
  numeric_value <- suppressWarnings(as.numeric(text))

  if (!is.na(numeric_value) && numeric_value > 1000) {
    return(as.Date(numeric_value, origin = "1899-12-30"))
  }

  if (!grepl("^[0-9]{1,4}[-/][0-9]{1,2}[-/][0-9]{1,4}", text)) {
    return(as.Date(NA))
  }

  parsed <- suppressWarnings(
    as.Date(
      text,
      tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y")
    )
  )

  if (is.na(parsed)) as.Date(NA) else parsed
}


venues_optional_cell_as_text <- function(raw_sheet, row, column) {

  if (
    length(row) != 1L ||
      length(column) != 1L ||
      is.na(row) ||
      is.na(column)
  ) {
    return(NA_character_)
  }

  cell_as_text(raw_sheet, row, column)
}


venues_first_label_column <- function(labels, candidates) {

  matches <- which(labels %in% normalise_excel_label(candidates))

  if (length(matches) == 0L) NA_integer_ else as.integer(matches[[1]])
}


find_venues_casual_hours_summary_layout <- function(raw_sheet) {

  empty_layout <- data.frame(
    header_row = integer(),
    label_column = integer(),
    budget_column = integer(),
    paid_column = integer(),
    variance_column = integer(),
    stringsAsFactors = FALSE
  )

  if (!is.data.frame(raw_sheet) || nrow(raw_sheet) == 0L) {
    return(empty_layout)
  }

  for (header_row in seq_len(nrow(raw_sheet))) {
    labels <- normalise_excel_label(
      vapply(
        seq_len(ncol(raw_sheet)),
        function(column) cell_as_text(raw_sheet, header_row, column),
        character(1)
      )
    )
    summary_columns <- which(labels == "summary")

    for (label_column in summary_columns) {
      candidate_columns <- seq.int(label_column + 1L, ncol(raw_sheet))
      candidate_labels <- labels[candidate_columns]
      budget_match <- which(
        candidate_labels %in% c("budgeted hours", "roster/budget hours")
      )
      paid_match <- which(
        candidate_labels %in% c("hours paid", "paid hours")
      )
      variance_match <- which(candidate_labels == "variance")

      if (
        length(budget_match) > 0L &&
          length(paid_match) > 0L &&
          length(variance_match) > 0L
      ) {
        return(
          data.frame(
            header_row = as.integer(header_row),
            label_column = as.integer(label_column),
            budget_column = as.integer(
              candidate_columns[budget_match[[1]]]
            ),
            paid_column = as.integer(candidate_columns[paid_match[[1]]]),
            variance_column = as.integer(
              candidate_columns[variance_match[[1]]]
            ),
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }

  empty_layout
}


find_venues_casual_hours_detail_layout <- function(
  raw_sheet,
  summary_layout
) {

  empty_layout <- data.frame(
    header_row = integer(),
    roster_column = integer(),
    fortnight_column = integer(),
    month_column = integer(),
    pay_period_start_column = integer(),
    pay_period_end_column = integer(),
    timesheet_column = integer(),
    pay_date_column = integer(),
    budget_column = integer(),
    paid_column = integer(),
    variance_column = integer(),
    notes_column = integer(),
    stringsAsFactors = FALSE
  )

  if (!is.data.frame(raw_sheet) || nrow(raw_sheet) == 0L) {
    return(empty_layout)
  }

  summary_column <- if (nrow(summary_layout) == 1L) {
    summary_layout$label_column[[1]]
  } else {
    ncol(raw_sheet) + 1L
  }

  for (header_row in seq_len(nrow(raw_sheet))) {
    labels <- normalise_excel_label(
      vapply(
        seq_len(ncol(raw_sheet)),
        function(column) cell_as_text(raw_sheet, header_row, column),
        character(1)
      )
    )
    labels[seq_len(ncol(raw_sheet)) >= summary_column] <- ""
    fortnight_column <- venues_first_label_column(
      labels,
      c("fortnight", "p/f", "pay fortnight")
    )
    budget_column <- venues_first_label_column(
      labels,
      c("roster/budget hours", "budgeted hours", "budget hours")
    )
    paid_column <- venues_first_label_column(
      labels,
      c("paid hours", "hours paid")
    )
    variance_column <- venues_first_label_column(labels, "variance")

    if (any(is.na(c(
      fortnight_column,
      budget_column,
      paid_column,
      variance_column
    )))) {
      next
    }

    pay_period_start_column <- venues_first_label_column(
      labels,
      "pay period"
    )
    pay_period_end_column <- if (
      !is.na(pay_period_start_column) &&
        pay_period_start_column + 1L < summary_column
    ) {
      as.integer(pay_period_start_column + 1L)
    } else {
      NA_integer_
    }

    return(
      data.frame(
        header_row = as.integer(header_row),
        roster_column = venues_first_label_column(labels, "roster"),
        fortnight_column = fortnight_column,
        month_column = venues_first_label_column(labels, "month"),
        pay_period_start_column = pay_period_start_column,
        pay_period_end_column = pay_period_end_column,
        timesheet_column = venues_first_label_column(
          labels,
          c("timesheets due", "timesheet deadline")
        ),
        pay_date_column = venues_first_label_column(
          labels,
          c("pay day", "pay date")
        ),
        budget_column = budget_column,
        paid_column = paid_column,
        variance_column = variance_column,
        notes_column = venues_first_label_column(labels, "notes"),
        stringsAsFactors = FALSE
      )
    )
  }

  empty_layout
}


casual_hours_summary_records <- function(raw_sheet, workbook, layout) {

  if (nrow(layout) != 1L) {
    return(empty_venues_casual_hours_table())
  }

  metric_columns <- c(
    "Budgeted Hours" = layout$budget_column[[1]],
    "Paid Hours" = layout$paid_column[[1]],
    "Variance" = layout$variance_column[[1]]
  )
  records <- list()

  for (source_row in seq.int(layout$header_row[[1]] + 1L, nrow(raw_sheet))) {
    raw_period <- cell_as_text(
      raw_sheet,
      source_row,
      layout$label_column[[1]]
    )
    period_key <- normalise_excel_label(raw_period)
    month <- venues_month_from_text(raw_period)

    if (is.na(month) && period_key != "total") {
      next
    }

    period_type <- if (period_key == "total") {
      "Reported Total"
    } else {
      "Month"
    }
    period <- venues_operational_period(
      workbook$report_year,
      period_type,
      month
    )

    for (metric in names(metric_columns)) {
      source_column <- unname(metric_columns[[metric]])
      reported_value <- cell_as_number(raw_sheet, source_row, source_column)

      records[[length(records) + 1L]] <- data.frame(
        report_year = as.integer(workbook$report_year),
        date = period$date,
        month = period$month,
        month_label = period$month_label,
        period_type = period_type,
        record_group = "Monthly Summary",
        record_id = "Monthly Summary",
        row_type = "Monthly Summary",
        roster = NA_character_,
        fortnight = NA_character_,
        pay_period_start = as.Date(NA),
        pay_period_end = as.Date(NA),
        timesheet_deadline = as.Date(NA),
        pay_date = as.Date(NA),
        notes = NA_character_,
        metric = metric,
        value = reported_value,
        is_reported = !is.na(reported_value),
        source_file = workbook$source_file,
        source_sheet = workbook$casual_hours_sheet_name,
        source_row = as.integer(source_row),
        source_column = as.integer(source_column),
        stringsAsFactors = FALSE
      )
    }

    if (period_key == "total") {
      break
    }
  }

  bind_import_tables(records, empty_venues_casual_hours_table())
}


casual_hours_paid_reporting <- function(raw_sheet, layout) {

  empty_result <- list(
    has_detail_rows = FALSE,
    reported_months = integer()
  )

  if (nrow(layout) != 1L) {
    return(empty_result)
  }

  has_detail_rows <- FALSE
  reported_months <- integer()

  for (source_row in seq.int(layout$header_row[[1]] + 1L, nrow(raw_sheet))) {
    fortnight <- cell_as_text(
      raw_sheet,
      source_row,
      layout$fortnight_column[[1]]
    )

    if (is.na(fortnight)) {
      next
    }

    has_detail_rows <- TRUE
    raw_month <- venues_optional_cell_as_text(
      raw_sheet,
      source_row,
      layout$month_column[[1]]
    )
    month <- venues_month_from_text(raw_month)

    if (is.na(month)) {
      pay_date <- venues_cell_as_date(
        raw_sheet,
        source_row,
        layout$pay_date_column[[1]]
      )

      if (!is.na(pay_date)) {
        month <- as.integer(format(pay_date, "%m"))
      }
    }

    paid_value <- cell_as_number(
      raw_sheet,
      source_row,
      layout$paid_column[[1]]
    )

    if (!is.na(month) && !is.na(paid_value)) {
      reported_months <- c(reported_months, as.integer(month))
    }
  }

  list(
    has_detail_rows = has_detail_rows,
    reported_months = sort(unique(reported_months))
  )
}


clean_venues_tracking_report_casual_hours <- function(workbook) {

  raw_sheet <- workbook$casual_hours_sheet

  if (!is.data.frame(raw_sheet) || nrow(raw_sheet) == 0L) {
    return(empty_venues_casual_hours_table())
  }

  summary_layout <- find_venues_casual_hours_summary_layout(raw_sheet)
  detail_layout <- find_venues_casual_hours_detail_layout(
    raw_sheet,
    summary_layout
  )
  paid_reporting <- casual_hours_paid_reporting(
    raw_sheet,
    detail_layout
  )
  result <- casual_hours_summary_records(
    raw_sheet,
    workbook,
    summary_layout
  )

  if (nrow(result) > 0L) {
    summary_month_rows <-
      result$period_type == "Month" &
      result$record_group == "Monthly Summary"
    summary_indices <- which(summary_month_rows)

    for (row_index in summary_indices) {
      metric <- result$metric[[row_index]]

      if (metric == "Paid Hours" && isTRUE(paid_reporting$has_detail_rows)) {
        result$is_reported[[row_index]] <-
          result$month[[row_index]] %in% paid_reporting$reported_months
      } else if (metric == "Variance") {
        matching_summary <-
          summary_month_rows &
          result$source_file == result$source_file[[row_index]] &
          result$month == result$month[[row_index]]
        budget_reported <- any(
          result$metric[matching_summary] == "Budgeted Hours" &
            result$is_reported[matching_summary]
        )
        paid_reported <- any(
          result$metric[matching_summary] == "Paid Hours" &
            result$is_reported[matching_summary]
        )
        result$is_reported[[row_index]] <-
          result$is_reported[[row_index]] &
          budget_reported &
          paid_reported
      }
    }

    unreported_summary_values <-
      summary_month_rows &
      result$metric %in% c("Paid Hours", "Variance") &
      !result$is_reported
    result$value[unreported_summary_values] <- NA_real_

    total_rows <- result$period_type == "Reported Total"

    for (row_index in which(total_rows)) {
      matching_months <-
        summary_month_rows &
        result$source_file == result$source_file[[row_index]] &
        result$metric == result$metric[[row_index]]
      result$is_reported[[row_index]] <-
        isTRUE(result$is_reported[[row_index]]) &&
        sum(matching_months) == 12L &&
        all(result$is_reported[matching_months])
    }

    period_order <- match(
      result$period_type,
      c("Month", "Reported Total")
    )
    result <- result[
      order(
        result$report_year,
        period_order,
        is.na(result$date),
        result$date,
        result$source_row,
        result$metric
      ),
      ,
      drop = FALSE
    ]
    rownames(result) <- NULL
  }

  result
}


# Casual Wages -----------------------------------------------------------

empty_venues_casual_wages_table <- function() {

  data.frame(
    report_year = integer(),
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    period_type = character(),
    series = character(),
    salary_year = integer(),
    row_type = character(),
    metric = character(),
    raw_metric = character(),
    value = double(),
    is_reported = logical(),
    source_file = character(),
    source_sheet = character(),
    source_row = integer(),
    source_column = integer(),
    stringsAsFactors = FALSE
  )
}


find_venues_casual_wages_layout <- function(raw_sheet, report_year) {

  empty_layout <- data.frame(
    title_row = integer(),
    title_column = integer(),
    header_row = integer(),
    month_column = integer(),
    budget_column = integer(),
    actual_column = integer(),
    variance_column = integer(),
    stringsAsFactors = FALSE
  )

  if (!is.data.frame(raw_sheet) || nrow(raw_sheet) == 0L) {
    return(empty_layout)
  }

  title_pattern <- paste0(
    "^",
    as.integer(report_year),
    " casual salar(y|ies)$"
  )
  layouts <- list()

  for (title_row in seq_len(nrow(raw_sheet))) {
    for (title_column in seq_len(ncol(raw_sheet))) {
      title_key <- normalise_excel_label(
        cell_as_text(raw_sheet, title_row, title_column)
      )

      if (!grepl(title_pattern, title_key)) {
        next
      }

      header_row <- title_row + 1L

      if (header_row > nrow(raw_sheet)) {
        next
      }

      block_columns <- seq.int(
        title_column,
        min(title_column + 4L, ncol(raw_sheet))
      )
      header_labels <- normalise_excel_label(
        vapply(
          block_columns,
          function(column) cell_as_text(raw_sheet, header_row, column),
          character(1)
        )
      )
      month_match <- which(header_labels == "month")
      budget_match <- which(header_labels == "budget")
      actual_match <- which(header_labels == "actual")
      variance_match <- which(header_labels %in% c("variation", "variance"))

      if (any(lengths(list(
        month_match,
        budget_match,
        actual_match,
        variance_match
      )) == 0L)) {
        next
      }

      layouts[[length(layouts) + 1L]] <- data.frame(
        title_row = as.integer(title_row),
        title_column = as.integer(title_column),
        header_row = as.integer(header_row),
        month_column = as.integer(block_columns[month_match[[1]]]),
        budget_column = as.integer(block_columns[budget_match[[1]]]),
        actual_column = as.integer(block_columns[actual_match[[1]]]),
        variance_column = as.integer(block_columns[variance_match[[1]]]),
        stringsAsFactors = FALSE
      )
    }
  }

  bind_import_tables(layouts, empty_layout)
}


clean_venues_tracking_report_casual_wages <- function(workbook) {

  raw_sheet <- workbook$casual_wages_sheet

  if (!is.data.frame(raw_sheet) || nrow(raw_sheet) == 0L) {
    return(empty_venues_casual_wages_table())
  }

  layout <- find_venues_casual_wages_layout(
    raw_sheet,
    workbook$report_year
  )

  if (nrow(layout) != 1L) {
    return(empty_venues_casual_wages_table())
  }

  metric_columns <- c(
    "Budget" = layout$budget_column[[1]],
    "Actual" = layout$actual_column[[1]],
    "Variance" = layout$variance_column[[1]]
  )
  raw_metrics <- c(
    "Budget" = "Budget",
    "Actual" = "Actual",
    "Variance" = cell_as_text(
      raw_sheet,
      layout$header_row[[1]],
      layout$variance_column[[1]]
    )
  )
  records <- list()

  for (source_row in seq.int(layout$header_row[[1]] + 1L, nrow(raw_sheet))) {
    raw_period <- cell_as_text(
      raw_sheet,
      source_row,
      layout$month_column[[1]]
    )
    period_key <- normalise_excel_label(raw_period)
    month <- venues_month_from_text(raw_period)

    if (is.na(month) && period_key != "total") {
      next
    }

    period_type <- if (period_key == "total") {
      "Reported Total"
    } else {
      "Month"
    }
    period <- venues_operational_period(
      workbook$report_year,
      period_type,
      month
    )

    for (metric in names(metric_columns)) {
      source_column <- unname(metric_columns[[metric]])
      budget_value <- cell_as_number(
        raw_sheet,
        source_row,
        layout$budget_column[[1]]
      )
      actual_value <- cell_as_number(
        raw_sheet,
        source_row,
        layout$actual_column[[1]]
      )
      reported_value <- cell_as_number(raw_sheet, source_row, source_column)
      is_reported <- !is.na(reported_value)

      if (metric == "Variance") {
        is_reported <-
          is_reported &&
          !is.na(budget_value) &&
          !is.na(actual_value)

        if (!is_reported) {
          reported_value <- NA_real_
        }
      }

      records[[length(records) + 1L]] <- data.frame(
        report_year = as.integer(workbook$report_year),
        date = period$date,
        month = period$month,
        month_label = period$month_label,
        period_type = period_type,
        series = "Casual Salaries",
        salary_year = as.integer(workbook$report_year),
        row_type = "Salary Series",
        metric = metric,
        raw_metric = unname(raw_metrics[[metric]]),
        value = reported_value,
        is_reported = is_reported,
        source_file = workbook$source_file,
        source_sheet = workbook$casual_wages_sheet_name,
        source_row = as.integer(source_row),
        source_column = as.integer(source_column),
        stringsAsFactors = FALSE
      )
    }

    if (period_key == "total") {
      break
    }
  }

  result <- bind_import_tables(records, empty_venues_casual_wages_table())

  if (nrow(result) > 0L) {
    total_rows <- result$period_type == "Reported Total"
    month_rows <- result$period_type == "Month"

    for (row_index in which(total_rows)) {
      matching_months <-
        month_rows &
        result$source_file == result$source_file[[row_index]] &
        result$metric == result$metric[[row_index]]
      result$is_reported[[row_index]] <-
        isTRUE(result$is_reported[[row_index]]) &&
        sum(matching_months) == 12L &&
        all(result$is_reported[matching_months])
    }

    result <- result[
      order(
        result$report_year,
        is.na(result$month),
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
