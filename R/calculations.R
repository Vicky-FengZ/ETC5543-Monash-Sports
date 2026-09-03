# -------------------------------------------------------------------------
# Overview calculations
# -------------------------------------------------------------------------

empty_overview_annual_finance <- function() {
  data.frame(
    report_year = integer(),
    income_actual = double(),
    income_budget = double(),
    expenses_actual = double(),
    expenses_budget = double(),
    net_actual = double(),
    net_budget = double(),
    budget_variance = double(),
    aggregation_source = character(),
    stringsAsFactors = FALSE
  )
}


sum_available_finance_values <- function(values) {
  values <- suppressWarnings(as.numeric(values))

  if (length(values) == 0L || all(is.na(values))) {
    return(NA_real_)
  }

  sum(values, na.rm = TRUE)
}


overview_duplicate_report_years <- function(monthly_reports) {
  if (is.null(monthly_reports)) {
    return(integer())
  }

  files <- monthly_reports$files

  if (
    is.null(files) ||
      !is.data.frame(files) ||
      nrow(files) == 0L ||
      !all(c("report_year", "source_file") %in% names(files))
  ) {
    return(integer())
  }

  files <- unique(files[c("report_year", "source_file")])
  counts <- table(files$report_year)

  sort(as.integer(names(counts[counts > 1L])))
}


calculate_overview_annual_finance <- function(monthly_reports) {
  if (is.null(monthly_reports)) {
    return(empty_overview_annual_finance())
  }

  finance <- if (is.data.frame(monthly_reports)) {
    monthly_reports
  } else {
    monthly_reports$finance
  }

  required_columns <- c(
    "report_year",
    "site",
    "is_summary",
    "metric",
    "value_type",
    "value"
  )

  if (
    is.null(finance) ||
      !is.data.frame(finance) ||
      nrow(finance) == 0L
  ) {
    return(empty_overview_annual_finance())
  }

  missing_columns <- setdiff(required_columns, names(finance))

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Monthly Site Report finance data is missing: ",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  finance <- finance[
    finance$is_summary %in% TRUE &
      finance$metric %in% c("Income", "Expenses") &
      finance$value_type %in% c("Actual", "Budget"),
    ,
    drop = FALSE
  ]

  if (nrow(finance) == 0L) {
    return(empty_overview_annual_finance())
  }

  # The Monthly Site Report contains an organisation-wide Monash Sport
  # summary as well as Site summaries. Prefer the organisation total so the
  # same activity is not counted twice. If a future workbook omits that
  # sheet, use the Site summary rows as a documented fallback.
  years <- sort(unique(as.integer(finance$report_year)))
  years <- years[!is.na(years)]
  selected_rows <- list()
  aggregation_source <- character(length(years))

  for (index in seq_along(years)) {
    year <- years[[index]]
    year_rows <- finance[finance$report_year == year, , drop = FALSE]
    site_key <- trimws(tolower(year_rows$site))
    organisation_rows <- year_rows[
      !is.na(site_key) & site_key == "monash sport",
      ,
      drop = FALSE
    ]

    if (nrow(organisation_rows) > 0L) {
      selected_rows[[index]] <- organisation_rows
      aggregation_source[[index]] <- "Monash Sport YTD Summary"
    } else {
      selected_rows[[index]] <- year_rows[
        !is.na(site_key) & site_key != "monash sport",
        ,
        drop = FALSE
      ]
      aggregation_source[[index]] <- "Sum of Site YTD Summaries"
    }
  }

  selected <- do.call(rbind, selected_rows)
  rownames(selected) <- NULL

  value_for <- function(year, metric, value_type) {
    keep <- selected$report_year == year &
      selected$metric == metric &
      selected$value_type == value_type

    sum_available_finance_values(selected$value[keep])
  }

  result <- data.frame(
    report_year = years,
    income_actual = vapply(
      years,
      value_for,
      numeric(1),
      metric = "Income",
      value_type = "Actual"
    ),
    income_budget = vapply(
      years,
      value_for,
      numeric(1),
      metric = "Income",
      value_type = "Budget"
    ),
    expenses_actual = vapply(
      years,
      value_for,
      numeric(1),
      metric = "Expenses",
      value_type = "Actual"
    ),
    expenses_budget = vapply(
      years,
      value_for,
      numeric(1),
      metric = "Expenses",
      value_type = "Budget"
    ),
    aggregation_source = aggregation_source,
    stringsAsFactors = FALSE
  )

  result$net_actual <- result$income_actual - result$expenses_actual
  result$net_budget <- result$income_budget - result$expenses_budget
  result$budget_variance <- result$net_actual - result$net_budget

  result[
    c(
      "report_year",
      "income_actual",
      "income_budget",
      "expenses_actual",
      "expenses_budget",
      "net_actual",
      "net_budget",
      "budget_variance",
      "aggregation_source"
    )
  ]
}


overview_metric_values <- function(annual_finance, metric) {
  if (nrow(annual_finance) == 0L) {
    return(double())
  }

  switch(
    metric,
    "Income" = annual_finance$income_actual,
    "Expenses" = annual_finance$expenses_actual,
    "Net Result" = annual_finance$net_actual,
    "Budget Variance" = annual_finance$budget_variance,
    stop("Unknown Overview metric: ", metric, call. = FALSE)
  )
}
