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

# -------------------------------------------------------------------------
# Site Comparison calculations
# -------------------------------------------------------------------------

empty_site_annual_finance <- function() {
  data.frame(
    report_year = integer(),
    site = character(),
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


calculate_site_annual_finance <- function(monthly_reports) {

  if (is.null(monthly_reports)) {
    return(empty_site_annual_finance())
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
    return(empty_site_annual_finance())
  }

  missing_columns <- setdiff(
    required_columns,
    names(finance)
  )

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
    return(empty_site_annual_finance())
  }

  finance$report_year <- suppressWarnings(
    as.integer(finance$report_year)
  )

  finance$site <- trimws(
    as.character(finance$site)
  )

  site_key <- tolower(finance$site)

  # Monash Sport is the organisation-wide total and must not appear as
  # another site in the comparison.
  finance <- finance[
    !is.na(finance$report_year) &
      !is.na(site_key) &
      nzchar(site_key) &
      site_key != "monash sport",
    ,
    drop = FALSE
  ]

  if (nrow(finance) == 0L) {
    return(empty_site_annual_finance())
  }

  groups <- unique(
    finance[c(
      "report_year",
      "site"
    )]
  )

  groups <- groups[
    order(
      groups$report_year,
      groups$site
    ),
    ,
    drop = FALSE
  ]

  rows <- vector(
    "list",
    nrow(groups)
  )

  for (index in seq_len(nrow(groups))) {

    year <- groups$report_year[[index]]
    site <- groups$site[[index]]

    selected <- finance[
      finance$report_year == year &
        finance$site == site,
      ,
      drop = FALSE
    ]

    value_for <- function(metric, value_type) {

      keep <- selected$metric == metric &
        selected$value_type == value_type

      sum_available_finance_values(
        selected$value[keep]
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

    net_actual <- if (
      is.na(income_actual) ||
        is.na(expenses_actual)
    ) {
      NA_real_
    } else {
      income_actual - expenses_actual
    }

    net_budget <- if (
      is.na(income_budget) ||
        is.na(expenses_budget)
    ) {
      NA_real_
    } else {
      income_budget - expenses_budget
    }

    rows[[index]] <- data.frame(
      report_year = as.integer(year),
      site = site,
      income_actual = income_actual,
      income_budget = income_budget,
      expenses_actual = expenses_actual,
      expenses_budget = expenses_budget,
      net_actual = net_actual,
      net_budget = net_budget,
      budget_variance = if (
        is.na(net_actual) ||
          is.na(net_budget)
      ) {
        NA_real_
      } else {
        net_actual - net_budget
      },
      aggregation_source = "Site YTD Summary",
      stringsAsFactors = FALSE
    )
  }

  result <- do.call(
    rbind,
    rows
  )

  rownames(result) <- NULL

  result
}

site_finance_values <- function(
    site_finance,
    metric,
    value_type = "Actual"
) {

  if (
    is.null(site_finance) ||
      !is.data.frame(site_finance) ||
      nrow(site_finance) == 0L
  ) {
    return(double())
  }

  if (!value_type %in% c("Actual", "Budget")) {
    stop(
      "value_type must be either 'Actual' or 'Budget'.",
      call. = FALSE
    )
  }

  suffix <- if (value_type == "Actual") {
    "actual"
  } else {
    "budget"
  }

  column <- switch(
    metric,
    "Income" = paste0("income_", suffix),
    "Expenses" = paste0("expenses_", suffix),
    "Net Result" = paste0("net_", suffix),
    stop(
      paste0("Unknown Site Comparison metric: ", metric),
      call. = FALSE
    )
  )

  if (!column %in% names(site_finance)) {
    stop(
      paste0(
        "Site finance data is missing: ",
        column
      ),
      call. = FALSE
    )
  }

  suppressWarnings(
    as.numeric(site_finance[[column]])
  )
}

empty_site_annual_revenue <- function() {

  data.frame(
    report_year = integer(),
    site = character(),
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


calculate_site_annual_revenue <- function(
    monthly_reports
) {

  if (is.null(monthly_reports)) {
    return(
      empty_site_annual_revenue()
    )
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
    return(
      empty_site_annual_revenue()
    )
  }

  missing_columns <- setdiff(
    required_columns,
    names(total_revenue)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Monthly Site Report Total Revenue data is missing: ",
        paste(
          missing_columns,
          collapse = ", "
        )
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
    return(
      empty_site_annual_revenue()
    )
  }

  total_revenue$report_year <- as.integer(
    total_revenue$report_year
  )

  total_revenue$site <- trimws(
    as.character(
      total_revenue$site
    )
  )

  # Monash Sport is the organisation total, not another site.
  total_revenue <- total_revenue[
    !is.na(total_revenue$site) &
      nzchar(total_revenue$site) &
      tolower(total_revenue$site) != "monash sport",
    ,
    drop = FALSE
  ]

  if (nrow(total_revenue) == 0L) {
    return(
      empty_site_annual_revenue()
    )
  }

  keys <- unique(
    total_revenue[
      c(
        "report_year",
        "site"
      )
    ]
  )

  keys <- keys[
    order(
      keys$report_year,
      keys$site
    ),
    ,
    drop = FALSE
  ]

  rows <- vector(
    "list",
    nrow(keys)
  )

  for (index in seq_len(nrow(keys))) {

    year <- keys$report_year[[index]]
    site <- keys$site[[index]]

    selected <- total_revenue[
      total_revenue$report_year == year &
        total_revenue$site == site,
      ,
      drop = FALSE
    ]

    actual_income <- sum_available_finance_values(
      selected$actual_income
    )

    discount_opportunity <- sum_available_finance_values(
      selected$discount_opportunity
    )

    potential_revenue <- if (
      is.na(actual_income) ||
        is.na(discount_opportunity)
    ) {
      NA_real_
    } else {
      actual_income +
        discount_opportunity
    }

    reported_total_revenue <- sum_available_finance_values(
      selected$reported_total_revenue
    )

    validation_difference <- if (
      is.na(potential_revenue) ||
        is.na(reported_total_revenue)
    ) {
      NA_real_
    } else {
      reported_total_revenue -
        potential_revenue
    }

    statuses <- unique(
      as.character(
        selected$validation_status
      )
    )

    statuses <- statuses[
      !is.na(statuses) &
        nzchar(statuses)
    ]

    validation_status <- if (
      length(statuses) > 0L &&
        all(statuses == "pass")
    ) {
      "pass"
    } else if (
      any(statuses == "fail")
    ) {
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
      actual_income /
        potential_revenue
    }

    rows[[index]] <- data.frame(
      report_year = year,
      site = site,
      actual_income = actual_income,
      discount_opportunity = discount_opportunity,
      potential_revenue = potential_revenue,
      reported_total_revenue = reported_total_revenue,
      validation_difference = validation_difference,
      validation_status = validation_status,
      capture_rate = capture_rate,
      aggregation_source = "Site YTD Summary",
      stringsAsFactors = FALSE
    )
  }

  result <- do.call(
    rbind,
    rows
  )

  rownames(result) <- NULL

  result
}

empty_site_annual_discounts <- function() {

  data.frame(
    report_year = integer(),
    site = character(),
    student_discount = double(),
    staff_discount = double(),
    other_discount = double(),
    total_discount = double(),
    aggregation_source = character(),
    stringsAsFactors = FALSE
  )
}


calculate_site_annual_discounts <- function(
    monthly_reports
) {

  if (is.null(monthly_reports)) {
    return(
      empty_site_annual_discounts()
    )
  }

  discounts <- monthly_reports$discounts

  required_columns <- c(
    "report_year",
    "site",
    "is_summary",
    "discount_type",
    "value"
  )

  if (
    is.null(discounts) ||
      !is.data.frame(discounts) ||
      nrow(discounts) == 0L
  ) {
    return(
      empty_site_annual_discounts()
    )
  }

  missing_columns <- setdiff(
    required_columns,
    names(discounts)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Monthly Site Report discount data is missing: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  discounts <- discounts[
    discounts$is_summary %in% TRUE &
      discounts$discount_type %in%
        c(
          "Student",
          "Staff",
          "Other"
        ),
    ,
    drop = FALSE
  ]

  if (nrow(discounts) == 0L) {
    return(
      empty_site_annual_discounts()
    )
  }

  discounts$report_year <- as.integer(
    discounts$report_year
  )

  discounts$site <- trimws(
    as.character(
      discounts$site
    )
  )

  # Monash Sport is the organisation total, not another site.
  discounts <- discounts[
    !is.na(discounts$site) &
      nzchar(discounts$site) &
      tolower(discounts$site) != "monash sport",
    ,
    drop = FALSE
  ]

  if (nrow(discounts) == 0L) {
    return(
      empty_site_annual_discounts()
    )
  }

  keys <- unique(
    discounts[
      c(
        "report_year",
        "site"
      )
    ]
  )

  keys <- keys[
    order(
      keys$report_year,
      keys$site
    ),
    ,
    drop = FALSE
  ]

  rows <- vector(
    "list",
    nrow(keys)
  )

  for (index in seq_len(nrow(keys))) {

    year <- keys$report_year[[index]]
    site <- keys$site[[index]]

    selected <- discounts[
      discounts$report_year == year &
        discounts$site == site,
      ,
      drop = FALSE
    ]

    discount_value <- function(type) {

      sum_available_finance_values(
        selected$value[
          selected$discount_type == type
        ]
      )
    }

    student_discount <- discount_value(
      "Student"
    )

    staff_discount <- discount_value(
      "Staff"
    )

    other_discount <- discount_value(
      "Other"
    )

    total_discount <- sum_available_finance_values(
      selected$value
    )

    rows[[index]] <- data.frame(
      report_year = year,
      site = site,
      student_discount = student_discount,
      staff_discount = staff_discount,
      other_discount = other_discount,
      total_discount = total_discount,
      aggregation_source = "Site YTD Summary",
      stringsAsFactors = FALSE
    )
  }

  result <- do.call(
    rbind,
    rows
  )

  rownames(result) <- NULL

  result
}

empty_facility_annual_finance <- function() {

  data.frame(
    report_year = integer(),
    report_unit = character(),
    campus = character(),
    business_unit = character(),
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


calculate_facility_annual_finance <- function(
    site_reports
) {

  if (is.null(site_reports)) {
    return(
      empty_facility_annual_finance()
    )
  }

  finance <- site_reports$finance

  required_columns <- c(
    "report_year",
    "period_type",
    "report_unit",
    "campus",
    "business_unit",
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
    return(
      empty_facility_annual_finance()
    )
  }

  missing_columns <- setdiff(
    required_columns,
    names(finance)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Detailed Site Report finance data is missing: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  # Use the workbook's annual reported totals only.
  # Summary rows are excluded because this view ranks
  # individual facilities/business units.
  finance <- finance[
    finance$period_type == "Reported Total" &
      !(finance$is_summary %in% TRUE) &
      finance$metric %in%
        c(
          "Income",
          "Expenses"
        ) &
      finance$value_type %in%
        c(
          "Actual",
          "Budget"
        ),
    ,
    drop = FALSE
  ]

  if (nrow(finance) == 0L) {
    return(
      empty_facility_annual_finance()
    )
  }

  finance$report_year <- as.integer(
    finance$report_year
  )

  finance$report_unit <- trimws(
    as.character(
      finance$report_unit
    )
  )

  finance$campus <- trimws(
    as.character(
      finance$campus
    )
  )

  finance$business_unit <- trimws(
    as.character(
      finance$business_unit
    )
  )

  finance <- finance[
    !is.na(finance$business_unit) &
      nzchar(finance$business_unit),
    ,
    drop = FALSE
  ]

  if (nrow(finance) == 0L) {
    return(
      empty_facility_annual_finance()
    )
  }

  keys <- unique(
    finance[
      c(
        "report_year",
        "report_unit",
        "campus",
        "business_unit"
      )
    ]
  )

  keys <- keys[
    order(
      keys$report_year,
      keys$report_unit,
      keys$business_unit
    ),
    ,
    drop = FALSE
  ]

  rows <- vector(
    "list",
    nrow(keys)
  )

  for (index in seq_len(nrow(keys))) {

    key <- keys[
      index,
      ,
      drop = FALSE
    ]

    selected <- finance[
      finance$report_year ==
        key$report_year[[1L]] &
        finance$report_unit ==
          key$report_unit[[1L]] &
        finance$campus ==
          key$campus[[1L]] &
        finance$business_unit ==
          key$business_unit[[1L]],
      ,
      drop = FALSE
    ]

    value_for <- function(
        metric,
        value_type
    ) {

      keep <- selected$metric == metric &
        selected$value_type == value_type

      sum_available_finance_values(
        selected$value[keep]
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

    net_actual <- if (
      is.na(income_actual) ||
        is.na(expenses_actual)
    ) {
      NA_real_
    } else {
      income_actual -
        expenses_actual
    }

    net_budget <- if (
      is.na(income_budget) ||
        is.na(expenses_budget)
    ) {
      NA_real_
    } else {
      income_budget -
        expenses_budget
    }

    budget_variance <- if (
      is.na(net_actual) ||
        is.na(net_budget)
    ) {
      NA_real_
    } else {
      net_actual -
        net_budget
    }

    rows[[index]] <- data.frame(
      report_year =
        key$report_year[[1L]],
      report_unit =
        key$report_unit[[1L]],
      campus =
        key$campus[[1L]],
      business_unit =
        key$business_unit[[1L]],
      income_actual = income_actual,
      income_budget = income_budget,
      expenses_actual = expenses_actual,
      expenses_budget = expenses_budget,
      net_actual = net_actual,
      net_budget = net_budget,
      budget_variance = budget_variance,
      aggregation_source =
        "Detailed Site Report Reported Total",
      stringsAsFactors = FALSE
    )
  }

  result <- do.call(
    rbind,
    rows
  )

  rownames(result) <- NULL

  result
}

facility_site_key <- function(site) {

  site <- as.character(site)

  switch(
    site,

    "H&F Clayton" = list(
      report_unit = "Health & Fitness",
      campus = "Clayton"
    ),

    "Venues Clayton" = list(
      report_unit = "Venues",
      campus = "Clayton"
    ),

    "Aquatics Clayton" = list(
      report_unit = "Aquatics",
      campus = "Clayton"
    ),

    "Caulfield" = list(
      report_unit = "Caulfield",
      campus = "Caulfield"
    ),

    "Peninsula" = list(
      report_unit = "Peninsula",
      campus = "Peninsula"
    ),

    "Participation Programs" = list(
      report_unit = "Participation Programs",
      campus = "Multi-campus"
    ),

    NULL
  )
}


filter_facility_finance_by_site <- function(
    facility_finance,
    site
) {

  if (
    is.null(facility_finance) ||
      !is.data.frame(facility_finance) ||
      nrow(facility_finance) == 0L
  ) {
    return(
      empty_facility_annual_finance()
    )
  }

  key <- facility_site_key(site)

  if (is.null(key)) {
    return(
      empty_facility_annual_finance()
    )
  }

  facility_finance[
    facility_finance$report_unit ==
      key$report_unit &
      facility_finance$campus ==
        key$campus,
    ,
    drop = FALSE
  ]
}

facility_finance_values <- function(
    facility_finance,
    metric,
    value_type = "Actual"
) {

  if (
    is.null(facility_finance) ||
      !is.data.frame(facility_finance) ||
      nrow(facility_finance) == 0L
  ) {
    return(double())
  }

  if (!value_type %in% c("Actual", "Budget")) {
    stop(
      "value_type must be Actual or Budget.",
      call. = FALSE
    )
  }

  suffix <- if (
    value_type == "Actual"
  ) {
    "actual"
  } else {
    "budget"
  }

  column_name <- switch(
    metric,

    "Income" = paste0(
      "income_",
      suffix
    ),

    "Expenses" = paste0(
      "expenses_",
      suffix
    ),

    "Net Result" = paste0(
      "net_",
      suffix
    ),

    stop(
      paste0(
        "Unknown facility finance metric: ",
        metric
      ),
      call. = FALSE
    )
  )

  as.numeric(
    facility_finance[[column_name]]
  )
}

# -------------------------------------------------------------------------
# Facility annual discount calculations
# -------------------------------------------------------------------------

empty_facility_annual_discounts <- function() {

  data.frame(
    report_year = integer(),
    report_unit = character(),
    campus = character(),
    business_unit = character(),
    student_discount = double(),
    staff_discount = double(),
    other_discount = double(),
    total_discount = double(),
    aggregation_source = character(),
    stringsAsFactors = FALSE
  )
}


calculate_facility_annual_discounts <- function(
    site_reports
) {

  if (is.null(site_reports)) {
    return(
      empty_facility_annual_discounts()
    )
  }

  discounts <- site_reports$discounts

  required_columns <- c(
    "report_year",
    "period_type",
    "report_unit",
    "campus",
    "business_unit",
    "is_summary",
    "discount_type",
    "value"
  )

  if (
    is.null(discounts) ||
      !is.data.frame(discounts) ||
      nrow(discounts) == 0L
  ) {
    return(
      empty_facility_annual_discounts()
    )
  }

  missing_columns <- setdiff(
    required_columns,
    names(discounts)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Detailed Site Report discount data is missing: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  # Use annual reported totals only.
  # Summary rows are excluded because this view compares
  # individual facilities/business units.
  discounts <- discounts[
    discounts$period_type == "Reported Total" &
      !(discounts$is_summary %in% TRUE) &
      discounts$discount_type %in%
        c(
          "Student",
          "Staff",
          "Other"
        ),
    ,
    drop = FALSE
  ]

  if (nrow(discounts) == 0L) {
    return(
      empty_facility_annual_discounts()
    )
  }

  discounts$report_year <- as.integer(
    discounts$report_year
  )

  discounts$report_unit <- trimws(
    as.character(
      discounts$report_unit
    )
  )

  discounts$campus <- trimws(
    as.character(
      discounts$campus
    )
  )

  discounts$business_unit <- trimws(
    as.character(
      discounts$business_unit
    )
  )

  discounts <- discounts[
    !is.na(discounts$business_unit) &
      nzchar(discounts$business_unit),
    ,
    drop = FALSE
  ]

  if (nrow(discounts) == 0L) {
    return(
      empty_facility_annual_discounts()
    )
  }

  keys <- unique(
    discounts[
      c(
        "report_year",
        "report_unit",
        "campus",
        "business_unit"
      )
    ]
  )

  keys <- keys[
    order(
      keys$report_year,
      keys$report_unit,
      keys$business_unit
    ),
    ,
    drop = FALSE
  ]

  rows <- vector(
    "list",
    nrow(keys)
  )

  for (index in seq_len(nrow(keys))) {

    key <- keys[
      index,
      ,
      drop = FALSE
    ]

    selected <- discounts[
      discounts$report_year ==
        key$report_year[[1L]] &
        discounts$report_unit ==
          key$report_unit[[1L]] &
        discounts$campus ==
          key$campus[[1L]] &
        discounts$business_unit ==
          key$business_unit[[1L]],
      ,
      drop = FALSE
    ]

    discount_value <- function(type) {

      sum_available_finance_values(
        selected$value[
          selected$discount_type == type
        ]
      )
    }

    student_discount <- discount_value(
      "Student"
    )

    staff_discount <- discount_value(
      "Staff"
    )

    other_discount <- discount_value(
      "Other"
    )

    total_discount <- sum_available_finance_values(
      selected$value
    )

    rows[[index]] <- data.frame(
      report_year =
        key$report_year[[1L]],
      report_unit =
        key$report_unit[[1L]],
      campus =
        key$campus[[1L]],
      business_unit =
        key$business_unit[[1L]],
      student_discount = student_discount,
      staff_discount = staff_discount,
      other_discount = other_discount,
      total_discount = total_discount,
      aggregation_source =
        "Detailed Site Report Reported Total",
      stringsAsFactors = FALSE
    )
  }

  result <- do.call(
    rbind,
    rows
  )

  rownames(result) <- NULL

  result
}


filter_facility_discounts_by_site <- function(
    facility_discounts,
    site
) {

  if (
    is.null(facility_discounts) ||
      !is.data.frame(facility_discounts) ||
      nrow(facility_discounts) == 0L
  ) {
    return(
      empty_facility_annual_discounts()
    )
  }

  key <- facility_site_key(site)

  if (is.null(key)) {
    return(
      empty_facility_annual_discounts()
    )
  }

  facility_discounts[
    facility_discounts$report_unit ==
      key$report_unit &
      facility_discounts$campus ==
        key$campus,
    ,
    drop = FALSE
  ]
}