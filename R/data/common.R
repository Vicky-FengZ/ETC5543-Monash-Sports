# -------------------------------------------------------------------------
# Shared Excel import helpers
# -------------------------------------------------------------------------
# These helpers are intentionally independent of a specific report type so
# future import modules can reuse the same label, cell and issue handling.

normalise_excel_label <- function(x) {

  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- trimws(gsub("[[:space:]]+", " ", x))
  tolower(x)
}


empty_import_issue_table <- function() {

  data.frame(
    level = character(),
    source_file = character(),
    sheet = character(),
    message = character(),
    stringsAsFactors = FALSE
  )
}


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


cell_as_text <- function(raw_sheet, row, column) {

  if (
    row < 1L ||
      column < 1L ||
      row > nrow(raw_sheet) ||
      column > ncol(raw_sheet)
  ) {
    return(NA_character_)
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


cell_as_number <- function(raw_sheet, row, column) {

  if (
    row < 1L ||
      column < 1L ||
      row > nrow(raw_sheet) ||
      column > ncol(raw_sheet)
  ) {
    return(NA_real_)
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

  result
}


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
