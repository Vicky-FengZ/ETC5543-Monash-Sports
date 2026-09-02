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

  if (!is.data.frame(raw_sheet) || nrow(raw_sheet) == 0L) {
    return(NA_integer_)
  }

  month_counts <- vapply(
    seq_len(nrow(raw_sheet)),
    function(row) {
      sum(
        !is.na(
          vapply(
            seq_len(ncol(raw_sheet)),
            function(column) {
              venues_month_from_text(
                cell_as_text(raw_sheet, row, column)
              )
            },
            integer(1)
          )
        )
      )
    },
    integer(1)
  )

  if (max(month_counts, 0L) < minimum_months) {
    return(NA_integer_)
  }

  which.max(month_counts)
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

      data.frame(
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
        source_column = as.integer(period$column),
        stringsAsFactors = FALSE
      )
    }
  )

  bind_import_tables(records, empty_venues_attendance_table())
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

      records[[length(records) + 1L]] <- data.frame(
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
        source_column = as.integer(source_column),
        stringsAsFactors = FALSE
      )
    }
  }

  if (!is.na(ytd_column)) {
    for (metric_index in seq_along(c("Revenue", "Transactions"))) {
      metric <- c("Revenue", "Transactions")[[metric_index]]
      source_column <- ytd_column + metric_index - 1L

      records[[length(records) + 1L]] <- data.frame(
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
        source_column = as.integer(source_column),
        stringsAsFactors = FALSE
      )
    }
  }

  bind_import_tables(records, empty_venues_facility_hire_table())
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

      records[[length(records) + 1L]] <- data.frame(
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
        source_column = as.integer(source_column),
        stringsAsFactors = FALSE
      )
    }
  }

  bind_import_tables(records, empty_venues_membership_table())
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
