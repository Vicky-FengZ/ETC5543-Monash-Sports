# Extensible fields and metric definitions shared by the report importers.
# Rules describe a metric; they never change its reported numeric value.
IMPORT_METRIC_UNITS <- c("Unknown", "Count", "Currency", "Rate", "Hours", "Text")
IMPORT_METRIC_AGGREGATIONS <- c("None", "Sum", "Last", "Mean")

import_allowed_aggregations <- function(unit) {
  if (unit %in% c("Unknown", "Text")) return("None")
  if (unit == "Rate") return(c("None", "Last", "Mean"))
  IMPORT_METRIC_AGGREGATIONS
}

discount_value_columns <- function(labels) {
  keys <- normalise_excel_label(labels)
  which(seq_along(keys) > 1L & nzchar(keys) &
    !keys %in% c("discounts", "total", "subtotal", "total discounts"))
}

canonical_discount_type <- function(label) {
  aliases <- c(students = "Student", student = "Student", staff = "Staff", other = "Other")
  key <- normalise_excel_label(label)
  if (key %in% names(aliases)) unname(aliases[[key]]) else trimws(as.character(label))
}

read_import_metric_rules <- function(path = file.path("config", "metric_definitions.csv")) {
  rules <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c("family", "dataset", "metric", "unit", "aggregation")
  if (!all(required %in% names(rules)) ||
      anyNA(rules[required]) ||
      any(!rules$unit %in% IMPORT_METRIC_UNITS) ||
      any(!rules$aggregation %in% IMPORT_METRIC_AGGREGATIONS) ||
      any(!nzchar(trimws(rules$metric))) ||
      any(rules$unit %in% c("Rate", "Text", "Unknown") & rules$aggregation == "Sum") ||
      any(rules$unit %in% c("Text", "Unknown") & rules$aggregation != "None")) {
    stop("Invalid metric definitions. Check family, dataset, metric, unit and aggregation.")
  }
  keys <- paste(rules$family, rules$dataset, normalise_excel_label(rules$metric), sep = "\r")
  if (anyDuplicated(keys)) stop("Duplicate metric definitions were found.")
  rules
}

IMPORT_METRIC_RULES <- read_import_metric_rules()

import_metric_definition <- function(family, dataset, metrics, overrides = NULL) {
  rules <- IMPORT_METRIC_RULES
  if (is.data.frame(overrides) && nrow(overrides) > 0L) {
    # The last matching definition wins; session edits take precedence.
    rules <- rbind(rules, overrides[names(rules)])
  }
  rules <- rules[rules$family == family & rules$dataset == dataset, , drop = FALSE]
  rules <- rules[!duplicated(normalise_excel_label(rules$metric), fromLast = TRUE), , drop = FALSE]
  index <- match(normalise_excel_label(metrics), normalise_excel_label(rules$metric))
  data.frame(
    unit = ifelse(is.na(index), "Unknown", rules$unit[index]),
    aggregation = ifelse(is.na(index), "None", rules$aggregation[index]),
    stringsAsFactors = FALSE
  )
}

define_import_metrics <- function(data, family, dataset, overrides = NULL) {
  if (!is.data.frame(data) || !"metric" %in% names(data)) return(data)
  definitions <- import_metric_definition(family, dataset, data$metric, overrides)
  data$unit <- definitions$unit
  data$aggregation <- definitions$aggregation
  if ("metric_type" %in% names(data)) data$metric_type <- data$unit
  data
}

import_summable_metrics <- function(data, family, dataset) {
  metrics <- unique(as.character(data$metric))
  definitions <- import_metric_definition(family, dataset, metrics)
  metrics[definitions$aggregation == "Sum"]
}

import_configurable_tables <- function(family) {
  switch(family,
    site_detail = c("memberships", "enrollments"),
    venues_tracking = c("membership", "merchandise"),
    character()
  )
}

define_import_report_metrics <- function(report, family, overrides = NULL) {
  if (!is.list(report)) return(report)
  for (dataset in intersect(import_configurable_tables(family), names(report))) {
    report[[dataset]] <- define_import_metrics(report[[dataset]], family, dataset, overrides)
  }
  report
}

import_metric_catalog <- function(report, family) {
  records <- lapply(import_configurable_tables(family), function(dataset) {
    data <- report[[dataset]]
    if (!is.data.frame(data) || nrow(data) == 0L || !"metric" %in% names(data)) return(NULL)
    metrics <- unique(as.character(data$metric))
    definitions <- import_metric_definition(family, dataset, metrics)
    if (all(c("unit", "aggregation") %in% names(data))) {
      definitions <- data[match(metrics, data$metric), c("unit", "aggregation"), drop = FALSE]
    }
    data.frame(family = family, dataset = dataset, metric = metrics,
      unit = definitions$unit, aggregation = definitions$aggregation, stringsAsFactors = FALSE)
  })
  bind_import_tables(records, IMPORT_METRIC_RULES[FALSE, ])
}

empty_additional_import_data <- function() {
  data.frame(report_year = integer(), field = character(), raw_value = character(),
    value = double(), interpretation = character(), source_file = character(),
    source_sheet = character(), source_row = integer(), source_column = integer(),
    stringsAsFactors = FALSE)
}

import_raw_sheets <- function(workbook, family) {
  if (family == "monthly_site") return(workbook$sheets)
  if (family == "site_detail") {
    sheets <- workbook$sheets
    names(sheets) <- unlist(workbook$sheet_names[names(sheets)], use.names = FALSE)
  } else {
    fields <- grep("_sheet$", names(workbook), value = TRUE)
    sheets <- workbook[fields]
    names(sheets) <- vapply(fields, function(field) workbook[[paste0(field, "_name")]], character(1))
  }
  sheets[!is.na(names(sheets)) & vapply(sheets, is.data.frame, logical(1))]
}

import_unhandled_sheet_issues <- function(workbook, family, sheets) {
  # Presentation/help/budget inputs are deliberately outside the cleaned data.
  ignored <- c("dashboard", "monthly highlights", "help sheet", "from budget")
  names <- setdiff(workbook$workbook_sheets, names(sheets))
  keys <- normalise_excel_label(names)
  names <- names[!keys %in% ignored & !grepl("^chart([[:space:]_-]*[0-9]+)?$", keys)]
  bind_import_tables(lapply(names, function(sheet) {
    make_import_issue("warning", workbook$source_file,
      "This worksheet has no configured data layout and was not imported. A layout rule is needed to include it.",
      sheet)
  }), empty_import_issue_table())
}

audit_import_extensions <- function(report, workbooks, family) {
  additional <- list()
  issues <- list()
  audit_tables <- switch(family,
    monthly_site = c("finance", "discounts"),
    site_detail = c("finance", "discounts", "visits", "memberships", "enrollments"),
    venues_tracking = c("membership", "merchandise"),
    character()
  )
  for (workbook in workbooks) {
    sheets <- import_raw_sheets(workbook, family)
    issues[[length(issues) + 1L]] <- import_unhandled_sheet_issues(workbook, family, sheets)
    for (sheet_name in names(sheets)) {
      raw <- sheets[[sheet_name]]
      coordinates <- lapply(intersect(audit_tables, names(report)), function(dataset) {
        data <- report[[dataset]]
        needed <- c("source_file", "source_sheet", "source_row", "source_column")
        if (!is.data.frame(data) || !all(needed %in% names(data))) return(NULL)
        data <- data[data$source_file == workbook$source_file &
          data$source_sheet == sheet_name, , drop = FALSE]
        data[needed]
      })
      coordinates <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, coordinates)
      if (length(coordinates) == 0L) next
      coordinates <- unique(do.call(rbind, coordinates))
      covered <- paste(coordinates$source_row, coordinates$source_column, sep = ":")
      data_rows <- sort(unique(coordinates$source_row))
      section_headers <- which(first_column_labels(raw) %in%
        c("income", "expenses", "discounts", "month"))
      discounts <- report$discounts
      discount_rows <- if (is.data.frame(discounts) &&
        all(c("source_file", "source_sheet", "source_row") %in% names(discounts))) {
        discounts$source_row[discounts$source_file == workbook$source_file &
          discounts$source_sheet == sheet_name]
      } else integer()
      # Merchandise unit cost is an intentional descriptor in column B.
      first_column <- if (family == "venues_tracking" &&
        identical(sheet_name, workbook$merchandise_sheet_name)) 3L else 2L
      if (ncol(raw) < first_column) next
      # Whole-sheet lookups replace per-cell text, placeholder and coverage
      # checks; results are identical but much faster in the browser.
      text_matrix <- sheet_text_matrix(raw)
      placeholder <- normalise_excel_label(text_matrix) %in% c("-", "--", "na", "n/a")
      dim(placeholder) <- dim(text_matrix)
      covered_cells <- matrix(FALSE, nrow = nrow(raw), ncol = ncol(raw))
      in_sheet <- coordinates$source_row >= 1L & coordinates$source_row <= nrow(raw) &
        coordinates$source_column >= 1L & coordinates$source_column <= ncol(raw)
      covered_cells[cbind(coordinates$source_row[in_sheet],
        coordinates$source_column[in_sheet])] <- TRUE
      extra <- list()
      for (column in seq.int(first_column, ncol(raw))) {
        column_values <- raw[[column]]
        numeric_column <- is.numeric(column_values)
        for (row in data_rows) {
          if (row < 1L || row > nrow(raw)) next
          text <- text_matrix[row, column]
          if (is.na(text) || placeholder[row, column]) next
          number <- if (numeric_column) {
            as.numeric(column_values[row])
          } else {
            cell_as_number(raw, row, column)
          }
          is_covered <- covered_cells[row, column]
          if (is_covered && !is.na(number)) next
          field <- paste("Column", column)
          preceding <- section_headers[section_headers < row]
          if (length(preceding) > 0L) {
            heading <- cell_as_text(raw, tail(preceding, 1L), column)
            if (!is.na(heading) && nzchar(trimws(heading))) field <- heading
          }
          # Discount total columns are retained for inspection, but are not
          # new discount categories and must not be added to opportunity twice.
          reported_subtotal <- row %in% discount_rows && !is.na(number) &&
            normalise_excel_label(field) %in% c("total", "subtotal", "total discounts")
          extra[[length(extra) + 1L]] <- list(
            report_year = as.integer(workbook$report_year), field = field,
            raw_value = text, value = number,
            interpretation = if (reported_subtotal) "Reported discount total" else
              if (is_covered) "Non-numeric value" else "Unmapped field",
            source_file = workbook$source_file, source_sheet = sheet_name,
            source_row = as.integer(row), source_column = as.integer(column))
        }
      }
      cells <- records_to_table(extra, empty_additional_import_data())
      if (nrow(cells) > 0L) {
        additional[[length(additional) + 1L]] <- cells
      }
      unhandled <- cells[cells$interpretation != "Reported discount total", , drop = FALSE]
      if (nrow(unhandled) > 0L) {
        examples <- paste(head(paste0("row ", unhandled$source_row, ", column ", unhandled$source_column), 4L), collapse = "; ")
        issues[[length(issues) + 1L]] <- make_import_issue("warning", workbook$source_file,
          paste0(nrow(unhandled), " value(s) could not be interpreted in the supported data area. ",
            "Original values are retained as Additional data in the cleaned preview and download (",
            examples, "). They are not included in chart totals."), sheet_name)
      }
    }
  }
  list(data = bind_import_tables(additional, empty_additional_import_data()),
    issues = bind_import_tables(issues, empty_import_issue_table()))
}

finalise_extensible_report <- function(report, workbooks, family) {
  report <- define_import_report_metrics(report, family)
  audit <- audit_import_extensions(report, workbooks, family)
  report$additional_data <- audit$data
  report$validation <- bind_import_tables(list(report$validation, audit$issues), empty_import_issue_table())
  report
}
