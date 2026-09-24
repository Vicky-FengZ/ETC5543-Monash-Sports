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

# -------------------------------------------------------------------------
# Monthly Overview finance
# -------------------------------------------------------------------------

empty_overview_monthly_finance <- function() {
  data.frame(
    report_year = integer(),
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    income_actual = double(),
    income_budget = double(),
    expenses_actual = double(),
    expenses_budget = double(),
    net_actual = double(),
    net_budget = double(),
    aggregation_source = character(),
    stringsAsFactors = FALSE
  )
}


calculate_overview_monthly_finance <- function(monthly_reports) {

  if (is.null(monthly_reports)) {
    return(empty_overview_monthly_finance())
  }

  finance <- if (is.data.frame(monthly_reports)) {
    monthly_reports
  } else {
    monthly_reports$finance
  }

  required_columns <- c(
    "report_year",
    "date",
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
    return(empty_overview_monthly_finance())
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

  # Keep only monthly site-level / organisation-level summary values.
  #
  # Actual and Budget are retained here so the same prepared dataset
  # can support both the Income/Expenses trend and later Budget vs Actual
  # analysis.
  finance <- finance[
    finance$is_summary %in% TRUE &
      finance$metric %in% c("Income", "Expenses") &
      finance$value_type %in% c("Actual", "Budget") &
      !is.na(finance$date),
    ,
    drop = FALSE
  ]

  if (nrow(finance) == 0L) {
    return(empty_overview_monthly_finance())
  }

  finance$date <- as.Date(finance$date)

  years <- sort(unique(as.integer(finance$report_year)))
  years <- years[!is.na(years)]

  result_rows <- list()

  for (year in years) {

    year_rows <- finance[
      finance$report_year == year,
      ,
      drop = FALSE
    ]

    site_key <- trimws(tolower(year_rows$site))

    organisation_rows <- year_rows[
      !is.na(site_key) &
        site_key == "monash sport",
      ,
      drop = FALSE
    ]

    # Use the same anti-double-counting rule as the annual Overview:
    # prefer the Monash Sport summary when available; otherwise sum
    # the individual site summary rows.
    if (nrow(organisation_rows) > 0L) {

      selected <- organisation_rows
      aggregation_source <- "Monash Sport Monthly Summary"

    } else {

      selected <- year_rows[
        !is.na(site_key) &
          site_key != "monash sport",
        ,
        drop = FALSE
      ]

      aggregation_source <- "Sum of Site Monthly Summaries"
    }

    date_keys <- sort(unique(format(selected$date, "%Y-%m-%d")))

    for (date_key in date_keys) {

      current_date <- as.Date(date_key)

      month_rows <- selected[
        selected$date == current_date,
        ,
        drop = FALSE
      ]

      value_for <- function(metric, value_type) {

        keep <- month_rows$metric == metric &
          month_rows$value_type == value_type

        sum_available_finance_values(
          month_rows$value[keep]
        )
      }

      income_actual <- value_for(
        "Income",
        "Actual"
      )

      income_budget <- value_for(
        "Income",
        "Budget"
      )

      expenses_actual <- value_for(
        "Expenses",
        "Actual"
      )

      expenses_budget <- value_for(
        "Expenses",
        "Budget"
      )

      month_number <- as.integer(
        format(current_date, "%m")
      )

      result_rows[[length(result_rows) + 1L]] <- data.frame(
        report_year = as.integer(year),
        date = current_date,
        month = month_number,
        month_label = month.abb[[month_number]],
        income_actual = income_actual,
        income_budget = income_budget,
        expenses_actual = expenses_actual,
        expenses_budget = expenses_budget,
        net_actual = if (
          is.na(income_actual) ||
            is.na(expenses_actual)
        ) {
          NA_real_
        } else {
          income_actual - expenses_actual
        },
        net_budget = if (
          is.na(income_budget) ||
            is.na(expenses_budget)
        ) {
          NA_real_
        } else {
          income_budget - expenses_budget
        },
        aggregation_source = aggregation_source,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(result_rows) == 0L) {
    return(empty_overview_monthly_finance())
  }

  result <- do.call(
    rbind,
    result_rows
  )

  result <- result[
    order(result$date),
    ,
    drop = FALSE
  ]

  rownames(result) <- NULL

  result
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


empty_overview_annual_revenue <- function() {
  data.frame(
    report_year = integer(),
    actual_income = double(),
    discount_opportunity = double(),
    potential_revenue = double(),
    reported_total_revenue = double(),
    validation_difference = double(),
    validation_status = character(),
    capture_rate = double(),
    aggregation_source = character(),
    stringsAsFactors = FALSE
  )
}


calculate_overview_annual_revenue <- function(monthly_reports) {
  if (is.null(monthly_reports)) {
    return(empty_overview_annual_revenue())
  }

  total_revenue <- monthly_reports$total_revenue
  required_columns <- c(
    "report_year",
    "site",
    "is_summary",
    "actual_income",
    "discount_opportunity",
    "reported_total_revenue",
    "validation_status"
  )

  if (
    is.null(total_revenue) ||
      !is.data.frame(total_revenue) ||
      nrow(total_revenue) == 0L
  ) {
    return(empty_overview_annual_revenue())
  }

  missing_columns <- setdiff(required_columns, names(total_revenue))

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Monthly Site Report Total Revenue data is missing: ",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  total_revenue <- total_revenue[
    total_revenue$is_summary %in% TRUE,
    ,
    drop = FALSE
  ]

  if (nrow(total_revenue) == 0L) {
    return(empty_overview_annual_revenue())
  }

  years <- sort(unique(as.integer(total_revenue$report_year)))
  years <- years[!is.na(years)]
  rows <- vector("list", length(years))

  for (index in seq_along(years)) {
    year <- years[[index]]
    year_rows <- total_revenue[
      total_revenue$report_year == year,
      ,
      drop = FALSE
    ]
    site_key <- trimws(tolower(year_rows$site))
    organisation_rows <- year_rows[
      !is.na(site_key) & site_key == "monash sport",
      ,
      drop = FALSE
    ]

    if (nrow(organisation_rows) > 0L) {
      selected <- organisation_rows
      aggregation_source <- "Monash Sport YTD Summary"
    } else {
      selected <- year_rows[
        !is.na(site_key) & site_key != "monash sport",
        ,
        drop = FALSE
      ]
      aggregation_source <- "Sum of Site YTD Summaries"
    }

    actual_income <- sum_available_finance_values(selected$actual_income)
    discount_opportunity <- sum_available_finance_values(
      selected$discount_opportunity
    )
    potential_revenue <- if (
      is.na(actual_income) || is.na(discount_opportunity)
    ) {
      NA_real_
    } else {
      actual_income + discount_opportunity
    }
    reported_total_revenue <- sum_available_finance_values(
      selected$reported_total_revenue
    )
    validation_difference <- if (
      is.na(potential_revenue) || is.na(reported_total_revenue)
    ) {
      NA_real_
    } else {
      reported_total_revenue - potential_revenue
    }

    statuses <- unique(selected$validation_status)
    statuses <- statuses[!is.na(statuses) & nzchar(statuses)]
    validation_status <- if (
      length(statuses) > 0L && all(statuses == "pass")
    ) {
      "pass"
    } else if (any(statuses == "fail")) {
      "fail"
    } else {
      "incomplete"
    }

    capture_rate <- if (
      is.na(actual_income) ||
        is.na(potential_revenue) ||
        potential_revenue == 0
    ) {
      NA_real_
    } else {
      actual_income / potential_revenue
    }

    rows[[index]] <- data.frame(
      report_year = year,
      actual_income = actual_income,
      discount_opportunity = discount_opportunity,
      potential_revenue = potential_revenue,
      reported_total_revenue = reported_total_revenue,
      validation_difference = validation_difference,
      validation_status = validation_status,
      capture_rate = capture_rate,
      aggregation_source = aggregation_source,
      stringsAsFactors = FALSE
    )
  }

  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

