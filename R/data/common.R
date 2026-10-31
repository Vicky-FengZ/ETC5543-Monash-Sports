# -------------------------------------------------------------------------
# Shared Excel import helpers
# -------------------------------------------------------------------------
# These helpers are intentionally independent of a specific report type so
# future import modules can reuse the same label, cell and issue handling.

normalise_excel_label <- function(x) {

  x <- as.character(x)
  x[is.na(x)] <- ""

  # Importers normalise the same few labels many thousands of times, one cell
  # at a time; remembering single-label results avoids repeated regex work.
  if (length(x) == 1L && nzchar(x)) {
    cached <- excel_label_cache[[x]]
    if (!is.null(cached)) {
      return(cached)
    }
    result <- tolower(trimws(gsub("[[:space:]]+", " ", x)))
    if (length(excel_label_cache) > 50000L) {
      rm(list = ls(excel_label_cache, all.names = TRUE), envir = excel_label_cache)
    }
    assign(x, result, envir = excel_label_cache)
    return(result)
  }

  x <- trimws(gsub("[[:space:]]+", " ", x))
  tolower(x)
}

excel_label_cache <- new.env(hash = TRUE, parent = emptyenv())


# Zero-row validation issue table (level, source_file, sheet, message).
empty_import_issue_table <- function() {

  data.frame(
    level = character(),
    source_file = character(),
    sheet = character(),
    message = character(),
    stringsAsFactors = FALSE
  )
}


# One validation issue as a one-row data frame; level is "error" or "warning".
make_import_issue <- function(
  level,
  source_file,
  message,
  sheet = NA_character_
) {

  data.frame(
    level = level,
    source_file = source_file,
    sheet = sheet,
    message = message,
    stringsAsFactors = FALSE
  )
}


# Stacks the non-empty data frames in `tables`, or returns `empty_table` when there are none.
bind_import_tables <- function(tables, empty_table) {

  tables <- Filter(
    function(x) is.data.frame(x) && nrow(x) > 0L,
    tables
  )

  if (length(tables) == 0L) {
    return(empty_table)
  }

  result <- do.call(rbind, tables)
  rownames(result) <- NULL
  result
}


# Build a table from one-row records stored as named lists. Equivalent to
# binding one-row data frames with bind_import_tables(), but avoids creating a
# data frame per record, which is slow for cell-by-cell extraction.
records_to_table <- function(records, empty_table) {

  records <- Filter(Negate(is.null), records)

  if (length(records) == 0L) {
    return(empty_table)
  }

  single_values <- vapply(
    records,
    function(record) all(lengths(record) == 1L),
    logical(1)
  )

  if (!all(single_values)) {
    return(bind_import_tables(
      lapply(records, as.data.frame, stringsAsFactors = FALSE),
      empty_table
    ))
  }

  columns <- names(records[[1L]])
  table <- lapply(
    columns,
    function(column) do.call(c, unname(lapply(records, `[[`, column)))
  )
  names(table) <- columns

  structure(
    table,
    class = "data.frame",
    row.names = c(NA_integer_, -length(records))
  )
}


# Worksheet names across all workbooks, in first-seen order (used to order the preview's sheet
# tabs).
workbook_source_sheet_order <- function(workbooks) {

  sheet_orders <- lapply(
    workbooks,
    function(workbook) {
      sheets <- workbook$workbook_sheets

      if (is.null(sheets)) {
        character()
      } else {
        as.character(sheets)
      }
    }
  )

  unique(unlist(sheet_orders, use.names = FALSE))
}


# Trimmed text of one cell, with inner whitespace collapsed; NA for blank or out-of-range cells.
cell_as_text <- function(raw_sheet, row, column) {

  if (
    row < 1L ||
      column < 1L ||
      row > nrow(raw_sheet) ||
      column > ncol(raw_sheet)
  ) {
    return(NA_character_)
  }

  cache <- cached_sheet_cells(raw_sheet, "cell_text")
  if (!is.null(cache)) {
    return(cache[row, column])
  }

  value <- raw_sheet[[column]][row]

  if (length(value) == 0L || is.na(value)) {
    return(NA_character_)
  }

  value <- trimws(
    gsub(
      "[[:space:]]+",
      " ",
      as.character(value)
    )
  )

  if (!nzchar(value)) NA_character_ else value
}


# Vectorised equivalent of cell_as_text() for every cell of a sheet, used by
# routines that scan whole sheets. Element [row, column] equals
# cell_as_text(raw_sheet, row, column); scanning cell by cell is much slower,
# especially when the dashboard runs in the browser (Shinylive).
sheet_text_matrix <- function(raw_sheet) {

  cache <- cached_sheet_cells(raw_sheet, "cell_text")
  if (!is.null(cache)) {
    return(cache)
  }

  rows <- nrow(raw_sheet)
  columns <- ncol(raw_sheet)
  result <- matrix(NA_character_, nrow = rows, ncol = columns)

  for (column in seq_len(columns)) {
    values <- raw_sheet[[column]]

    if (is.list(values) || inherits(values, c("POSIXt", "Date"))) {
      result[, column] <- vapply(
        seq_len(rows),
        function(row) cell_as_text(raw_sheet, row, column),
        character(1)
      )
      next
    }

    text <- as.character(values)
    text[is.na(values)] <- NA_character_
    text <- trimws(gsub("[[:space:]]+", " ", text))
    text[!is.na(text) & !nzchar(text)] <- NA_character_
    result[, column] <- text
  }

  result
}


# Vectorised equivalent of cell_as_number() for every cell of a sheet.
sheet_number_matrix <- function(raw_sheet) {

  rows <- nrow(raw_sheet)
  columns <- ncol(raw_sheet)
  result <- matrix(NA_real_, nrow = rows, ncol = columns)

  for (column in seq_len(columns)) {
    values <- raw_sheet[[column]]

    if (is.numeric(values)) {
      result[, column] <- as.numeric(values)
    } else if (is.character(values)) {
      text <- trimws(values)
      is_parenthesised <- grepl("^\\(.*\\)$", text)
      is_percentage <- grepl("%", text, fixed = TRUE)
      numbers <- suppressWarnings(
        as.numeric(gsub("[$,%()[:space:]]", "", text))
      )
      numbers[is.na(text) | !nzchar(text)] <- NA_real_
      flip <- is_parenthesised & !is.na(numbers)
      numbers[flip] <- -numbers[flip]
      scale <- is_percentage & !is.na(numbers)
      numbers[scale] <- numbers[scale] / 100
      result[, column] <- numbers
    } else if (!is.logical(values) || is.list(values)) {
      result[, column] <- vapply(
        seq_len(rows),
        function(row) cell_as_number(raw_sheet, row, column),
        numeric(1)
      )
    }
    # Logical columns (TRUE/FALSE/NA) never parse as numbers: left as NA.
  }

  result
}


# Read-once caches of every cell's text and number, attached to a sheet when
# it is read from Excel. cell_as_text() and cell_as_number() then become
# lookups, which keeps cell-by-cell parsing fast in the browser (Shinylive).
cache_sheet_cells <- function(raw_sheet) {

  attr(raw_sheet, "cell_text") <- sheet_text_matrix(raw_sheet)
  attr(raw_sheet, "cell_number") <- sheet_number_matrix(raw_sheet)
  # Row subsets keep custom attributes but change row names, so the cache is
  # only used while the sheet still has the rows it was built from.
  attr(raw_sheet, "cell_row_names") <- .row_names_info(raw_sheet, type = 0L)
  raw_sheet
}


# The text or number matrix stored on a sheet by cache_sheet_cells(), or NULL when the sheet has
# changed since (different size or rows).
cached_sheet_cells <- function(raw_sheet, which) {

  cache <- attr(raw_sheet, which, exact = TRUE)

  if (
    is.null(cache) ||
      nrow(cache) != nrow(raw_sheet) ||
      ncol(cache) != ncol(raw_sheet) ||
      !identical(
        attr(raw_sheet, "cell_row_names", exact = TRUE),
        .row_names_info(raw_sheet, type = 0L)
      )
  ) {
    return(NULL)
  }

  cache
}


# Vectorised month lookup from short or long month labels ("Jan", "January",
# "SEPT"). Returns NA for blanks and non-month labels.
month_number_from_labels <- function(values) {

  key <- normalise_excel_label(values)
  result <- match(substr(gsub("[^a-z]", "", key), 1L, 3L), tolower(month.abb))
  result[!nzchar(key)] <- NA_integer_
  result
}


# Row with the most month labels, or NA when no row has `minimum_months`.
find_month_header_row <- function(raw_sheet, minimum_months) {

  if (!is.data.frame(raw_sheet) || nrow(raw_sheet) == 0L) {
    return(NA_integer_)
  }

  months <- month_number_from_labels(sheet_text_matrix(raw_sheet))
  dim(months) <- c(nrow(raw_sheet), ncol(raw_sheet))
  counts <- rowSums(!is.na(months))

  if (max(counts, 0L) < minimum_months) {
    return(NA_integer_)
  }

  which.max(counts)
}


# Numeric value of one cell. Text such as "$1,200", "(350)" (negative) and "82.8%" (0.828) is
# converted; NA when it is not a number.
cell_as_number <- function(raw_sheet, row, column) {

  if (
    row < 1L ||
      column < 1L ||
      row > nrow(raw_sheet) ||
      column > ncol(raw_sheet)
  ) {
    return(NA_real_)
  }

  cache <- cached_sheet_cells(raw_sheet, "cell_number")
  if (!is.null(cache)) {
    return(cache[row, column])
  }

  value <- raw_sheet[[column]][row]

  if (length(value) == 0L || is.na(value)) {
    return(NA_real_)
  }

  if (is.numeric(value)) {
    return(as.numeric(value))
  }

  value <- trimws(as.character(value))

  if (!nzchar(value)) {
    return(NA_real_)
  }

  is_parenthesised <- grepl(
    "^\\(.*\\)$",
    value
  )
  is_percentage <- grepl("%", value, fixed = TRUE)

  value <- gsub(
    "[$,%()[:space:]]",
    "",
    value
  )

  result <- suppressWarnings(
    as.numeric(value)
  )

  if (is_parenthesised && !is.na(result)) {
    result <- -result
  }

  if (is_percentage && !is.na(result)) {
    result <- result / 100
  }

  result
}


# Month number (1-12) from a date, an Excel date serial, a number 1-12 or a month name such as
# "Jan" or "September"; NA otherwise.
parse_month_number <- function(value) {

  if (length(value) == 0L || is.na(value)) {
    return(NA_integer_)
  }

  if (inherits(value, c("Date", "POSIXct", "POSIXlt"))) {
    return(as.integer(format(value, "%m")))
  }

  if (is.numeric(value)) {

    if (value >= 1 && value <= 12) {
      return(as.integer(value))
    }

    # Excel stores dates as the number of days since 1899-12-30.
    if (value > 1000) {
      excel_date <- as.Date(value, origin = "1899-12-30")
      return(as.integer(format(excel_date, "%m")))
    }
  }

  text <- normalise_excel_label(value)

  if (!nzchar(text)) {
    return(NA_integer_)
  }

  month_keys <- c(
    jan = 1L,
    feb = 2L,
    mar = 3L,
    apr = 4L,
    may = 5L,
    jun = 6L,
    jul = 7L,
    aug = 8L,
    sep = 9L,
    oct = 10L,
    nov = 11L,
    dec = 12L
  )

  key <- substr(gsub("[^a-z]", "", text), 1L, 3L)

  if (key %in% names(month_keys)) {
    return(unname(month_keys[[key]]))
  }

  # Only send strings that visibly start with an ISO date to as.Date().
  # Calling as.Date("Jan") raises an error rather than a suppressible warning.
  if (
    grepl(
      "^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}",
      text
    )
  ) {

    parsed_date <- suppressWarnings(
      as.Date(
        substr(text, 1L, 10L),
        format = "%Y-%m-%d"
      )
    )

    if (!is.na(parsed_date)) {
      return(as.integer(format(parsed_date, "%m")))
    }
  }

  # A date column containing both text and Excel dates can occasionally be
  # imported as character. Preserve support for numeric Excel date strings.
  numeric_text <- suppressWarnings(
    as.numeric(text)
  )

  if (!is.na(numeric_text) && numeric_text > 1000) {
    excel_date <- as.Date(
      numeric_text,
      origin = "1899-12-30"
    )

    return(as.integer(format(excel_date, "%m")))
  }

  NA_integer_
}
