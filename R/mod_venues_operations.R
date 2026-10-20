# -------------------------------------------------------------------------
# Venues Operations module
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Venues Operations data helpers
# -------------------------------------------------------------------------

venues_operations_filter_year <- function(
    data,
    year = NULL
) {

  if (
    is.null(data) ||
      !is.data.frame(data) ||
      nrow(data) == 0L
  ) {
    return(data)
  }

  if (
    is.null(year) ||
      length(year) == 0L ||
      !"report_year" %in% names(data)
  ) {
    return(data)
  }

  selected_year <- suppressWarnings(
    as.integer(year[[1L]])
  )

  if (is.na(selected_year)) {
    return(data)
  }

  data[
    !is.na(data$report_year) &
      data$report_year == selected_year,
    ,
    drop = FALSE
  ]
}


venues_operations_available_years <- function(
    venues_reports
) {

  if (
    is.null(venues_reports) ||
      !is.list(venues_reports) ||
      is.null(venues_reports$attendance) ||
      !is.data.frame(venues_reports$attendance)
  ) {
    return(integer())
  }

  years <- suppressWarnings(
    as.integer(
      venues_reports$attendance$report_year
    )
  )

  sort(
    unique(
      years[
        !is.na(years)
      ]
    )
  )
}

# -------------------------------------------------------------------------
# Workforce calculations
# -------------------------------------------------------------------------

venues_operations_workforce_hours_monthly <- function(
    casual_hours
) {

  empty_result <- data.frame(
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    budget_hours = double(),
    paid_hours = double(),
    variance_hours = double(),
    stringsAsFactors = FALSE
  )

  if (
    is.null(casual_hours) ||
      !is.data.frame(casual_hours) ||
      nrow(casual_hours) == 0L
  ) {
    return(empty_result)
  }

  required_columns <- c(
    "date",
    "month",
    "month_label",
    "period_type",
    "record_group",
    "metric",
    "value"
  )

  if (!all(
    required_columns %in%
      names(casual_hours)
  )) {
    return(empty_result)
  }

  rows <- casual_hours[
    casual_hours$period_type == "Month" &
      casual_hours$record_group ==
        "Monthly Summary" &
      casual_hours$metric %in%
        c(
          "Budgeted Hours",
          "Paid Hours",
          "Variance"
        ) &
      !is.na(casual_hours$date),
    ,
    drop = FALSE
  ]

  if (nrow(rows) == 0L) {
    return(empty_result)
  }

  rows$value <- suppressWarnings(
    as.numeric(rows$value)
  )

  if ("is_reported" %in% names(rows)) {
    rows$value[
      !(rows$is_reported %in% TRUE)
    ] <- NA_real_
  }

  dates <- sort(
    unique(
      as.Date(rows$date)
    )
  )

  result <- list()

  value_for <- function(
      month_rows,
      metric_name
  ) {

    values <- month_rows$value[
      month_rows$metric ==
        metric_name &
        is.finite(month_rows$value)
    ]

    if (length(values) == 0L) {
      return(NA_real_)
    }

    sum(
      values,
      na.rm = TRUE
    )
  }

  for (index in seq_along(dates)) {

    current_date <- as.Date(
      dates[[index]],
      origin = "1970-01-01"
    )

    month_rows <- rows[
      as.Date(rows$date) ==
        current_date,
      ,
      drop = FALSE
    ]

    result[[length(result) + 1L]] <-
      data.frame(
        date = current_date,
        month = as.integer(
          format(
            current_date,
            "%m"
          )
        ),
        month_label = format(
          current_date,
          "%b"
        ),
        budget_hours =
          value_for(
            month_rows,
            "Budgeted Hours"
          ),
        paid_hours =
          value_for(
            month_rows,
            "Paid Hours"
          ),
        variance_hours =
          value_for(
            month_rows,
            "Variance"
          ),
        stringsAsFactors = FALSE
      )
  }

  if (length(result) == 0L) {
    return(empty_result)
  }

  result <- do.call(
    rbind,
    result
  )

  keep <-
    is.finite(result$budget_hours) |
    is.finite(result$paid_hours) |
    is.finite(result$variance_hours)

  result <- result[
    keep,
    ,
    drop = FALSE
  ]

  result <- result[
    order(result$date),
    ,
    drop = FALSE
  ]

  rownames(result) <- NULL

  result
}


venues_operations_workforce_wages_monthly <- function(
    casual_wages
) {

  empty_result <- data.frame(
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    wage_budget = double(),
    wage_actual = double(),
    wage_variance = double(),
    net_budget = double(),
    net_actual = double(),
    stringsAsFactors = FALSE
  )

  if (
    is.null(casual_wages) ||
      !is.data.frame(casual_wages) ||
      nrow(casual_wages) == 0L
  ) {
    return(empty_result)
  }

  required_columns <- c(
    "date",
    "month",
    "month_label",
    "period_type",
    "metric",
    "value"
  )

  if (!all(
    required_columns %in%
      names(casual_wages)
  )) {
    return(empty_result)
  }

  rows <- casual_wages[
    casual_wages$period_type ==
      "Month" &
      casual_wages$metric %in%
        c(
          "Budget",
          "Actual",
          "Variance"
        ) &
      !is.na(casual_wages$date),
    ,
    drop = FALSE
  ]

  if (nrow(rows) == 0L) {
    return(empty_result)
  }

  rows$value <- suppressWarnings(
    as.numeric(rows$value)
  )

  if ("is_reported" %in% names(rows)) {
    rows$value[
      !(rows$is_reported %in% TRUE)
    ] <- NA_real_
  }

  dates <- sort(
    unique(
      as.Date(rows$date)
    )
  )

  result <- list()

  value_for <- function(
      month_rows,
      metric_name
  ) {

    values <- month_rows$value[
      month_rows$metric ==
        metric_name &
        is.finite(month_rows$value)
    ]

    if (length(values) == 0L) {
      return(NA_real_)
    }

    sum(
      values,
      na.rm = TRUE
    )
  }

  for (index in seq_along(dates)) {

    current_date <- as.Date(
      dates[[index]],
      origin = "1970-01-01"
    )

    month_rows <- rows[
      as.Date(rows$date) ==
        current_date,
      ,
      drop = FALSE
    ]

    budget <-
      value_for(
        month_rows,
        "Budget"
      )

    actual <-
      value_for(
        month_rows,
        "Actual"
      )

    variance <-
      value_for(
        month_rows,
        "Variance"
      )

    result[[length(result) + 1L]] <-
      data.frame(
        date = current_date,
        month = as.integer(
          format(
            current_date,
            "%m"
          )
        ),
        month_label = format(
          current_date,
          "%b"
        ),
        wage_budget = budget,
        wage_actual = actual,
        wage_variance = variance,

        # These aliases let us reuse the
        # existing Budget vs Actual finance plot.
        net_budget = budget,
        net_actual = actual,

        stringsAsFactors = FALSE
      )
  }

  if (length(result) == 0L) {
    return(empty_result)
  }

  result <- do.call(
    rbind,
    result
  )

  keep <-
    is.finite(result$wage_budget) |
    is.finite(result$wage_actual) |
    is.finite(result$wage_variance)

  result <- result[
    keep,
    ,
    drop = FALSE
  ]

  result <- result[
    order(result$date),
    ,
    drop = FALSE
  ]

  rownames(result) <- NULL

  result
}


venues_operations_workforce_annual_value <- function(
    data,
    metric,
    record_group = NULL
) {

  if (
    is.null(data) ||
      !is.data.frame(data) ||
      nrow(data) == 0L
  ) {
    return(NA_real_)
  }

  required_columns <- c(
    "period_type",
    "metric",
    "value"
  )

  if (!all(
    required_columns %in%
      names(data)
  )) {
    return(NA_real_)
  }

  rows <- data[
    data$metric == metric,
    ,
    drop = FALSE
  ]

  if (!is.null(record_group)) {

    if (!"record_group" %in% names(rows)) {
      return(NA_real_)
    }

    rows <- rows[
      rows$record_group ==
        record_group,
      ,
      drop = FALSE
    ]
  }

  if (nrow(rows) == 0L) {
    return(NA_real_)
  }

  rows$value <- suppressWarnings(
    as.numeric(rows$value)
  )

  reported <- if (
    "is_reported" %in% names(rows)
  ) {
    rows$is_reported %in% TRUE
  } else {
    is.finite(rows$value)
  }

  # Prefer the workbook's reported total
  # when it has been validated as complete.
  total_values <- rows$value[
    rows$period_type ==
      "Reported Total" &
      reported &
      is.finite(rows$value)
  ]

  if (length(total_values) > 0L) {

    return(
      sum(
        total_values,
        na.rm = TRUE
      )
    )
  }

  # Partial year fallback:
  # sum only genuinely reported months.
  monthly_values <- rows$value[
    rows$period_type == "Month" &
      reported &
      is.finite(rows$value)
  ]

  if (length(monthly_values) == 0L) {
    return(NA_real_)
  }

  sum(
    monthly_values,
    na.rm = TRUE
  )
}


venues_operations_workforce_kpis <- function(
    casual_hours,
    casual_wages
) {

  list(
    paid_hours =
      venues_operations_workforce_annual_value(
        casual_hours,
        metric = "Paid Hours",
        record_group =
          "Monthly Summary"
      ),

    actual_wages =
      venues_operations_workforce_annual_value(
        casual_wages,
        metric = "Actual"
      ),

    wage_variance =
      venues_operations_workforce_annual_value(
        casual_wages,
        metric = "Variance"
      )
  )
}

# -------------------------------------------------------------------------
# Merchandise calculations
# -------------------------------------------------------------------------

venues_operations_merchandise_section_choices <- function(
    merchandise
) {

  default_choice <- c(
    "All Sections" = "All Sections"
  )

  if (
    is.null(merchandise) ||
      !is.data.frame(merchandise) ||
      nrow(merchandise) == 0L ||
      !"section" %in% names(merchandise)
  ) {
    return(default_choice)
  }

  rows <- merchandise

  if ("source_row" %in% names(rows)) {
    rows <- rows[
      order(rows$source_row),
      ,
      drop = FALSE
    ]
  }

  sections <- unique(
    trimws(
      as.character(rows$section)
    )
  )

  sections <- sections[
    !is.na(sections) &
      nzchar(sections)
  ]

  if (length(sections) == 0L) {
    return(default_choice)
  }

  c(
    default_choice,
    stats::setNames(
      sections,
      sections
    )
  )
}


venues_operations_merchandise_item_choices <- function(
    merchandise,
    section = "All Sections"
) {

  default_choice <- c(
    "All Items" = "All Items"
  )

  if (
    is.null(merchandise) ||
      !is.data.frame(merchandise) ||
      nrow(merchandise) == 0L ||
      identical(
        section,
        "All Sections"
      )
  ) {
    return(default_choice)
  }

  rows <- merchandise[
    merchandise$row_type == "Item" &
      !is.na(merchandise$section) &
      trimws(
        as.character(
          merchandise$section
        )
      ) == section,
    ,
    drop = FALSE
  ]

  if ("source_row" %in% names(rows)) {
    rows <- rows[
      order(rows$source_row),
      ,
      drop = FALSE
    ]
  }

  items <- unique(
    trimws(
      as.character(rows$item)
    )
  )

  items <- items[
    !is.na(items) &
      nzchar(items)
  ]

  if (length(items) == 0L) {
    return(default_choice)
  }

  c(
    default_choice,
    stats::setNames(
      items,
      items
    )
  )
}


venues_operations_merchandise_scope_label <- function(
    section = "All Sections",
    item = "All Items"
) {

  if (!identical(
    item,
    "All Items"
  )) {
    return(item)
  }

  if (!identical(
    section,
    "All Sections"
  )) {
    return(section)
  }

  "All Merchandise"
}


venues_operations_merchandise_resolve <- function(
    merchandise,
    metric,
    period_type,
    section = "All Sections",
    item = "All Items",
    date = NULL
) {

  if (
    is.null(merchandise) ||
      !is.data.frame(merchandise) ||
      nrow(merchandise) == 0L
  ) {
    return(NA_real_)
  }

  required_columns <- c(
    "date",
    "period_type",
    "section",
    "item",
    "row_type",
    "metric",
    "value"
  )

  if (!all(
    required_columns %in%
      names(merchandise)
  )) {
    return(NA_real_)
  }

  rows <- merchandise[
    merchandise$metric == metric &
      merchandise$period_type ==
        period_type,
    ,
    drop = FALSE
  ]

  if (!is.null(date)) {

    selected_date <- as.Date(date)

    rows <- rows[
      !is.na(rows$date) &
        as.Date(rows$date) ==
          selected_date,
      ,
      drop = FALSE
    ]
  }

  if (nrow(rows) == 0L) {
    return(NA_real_)
  }

  rows$value <- suppressWarnings(
    as.numeric(rows$value)
  )

  # One specific item selected.
  if (!identical(
    item,
    "All Items"
  )) {

    rows <- rows[
      rows$row_type == "Item" &
        !is.na(rows$item) &
        trimws(
          as.character(rows$item)
        ) == item,
      ,
      drop = FALSE
    ]

    if (!identical(
      section,
      "All Sections"
    )) {

      rows <- rows[
        !is.na(rows$section) &
          trimws(
            as.character(
              rows$section
            )
          ) == section,
        ,
        drop = FALSE
      ]
    }

    values <- rows$value[
      is.finite(rows$value)
    ]

    if (length(values) == 0L) {
      return(NA_real_)
    }

    return(
      sum(
        values,
        na.rm = TRUE
      )
    )
  }

  # Resolve section-by-section so a reported
  # Section Total is never added to its items.
  sections <- if (
    identical(
      section,
      "All Sections"
    )
  ) {

    unique(
      trimws(
        as.character(rows$section)
      )
    )

  } else {

    section
  }

  sections <- sections[
    !is.na(sections) &
      nzchar(sections)
  ]

  if (length(sections) == 0L) {
    return(NA_real_)
  }

  resolved <- numeric()

  for (section_name in sections) {

    section_rows <- rows[
      !is.na(rows$section) &
        trimws(
          as.character(
            rows$section
          )
        ) == section_name,
      ,
      drop = FALSE
    ]

    total_values <-
      section_rows$value[
        section_rows$row_type ==
          "Section Total" &
          is.finite(section_rows$value)
      ]

    if (length(total_values) > 0L) {

      resolved <- c(
        resolved,
        sum(
          total_values,
          na.rm = TRUE
        )
      )

      next
    }

    item_values <-
      section_rows$value[
        section_rows$row_type ==
          "Item" &
          is.finite(section_rows$value)
      ]

    if (length(item_values) > 0L) {

      resolved <- c(
        resolved,
        sum(
          item_values,
          na.rm = TRUE
        )
      )
    }
  }

  if (length(resolved) == 0L) {
    return(NA_real_)
  }

  sum(
    resolved,
    na.rm = TRUE
  )
}


venues_operations_merchandise_monthly <- function(
    merchandise,
    metric = "Revenue",
    section = "All Sections",
    item = "All Items"
) {

  empty_result <- data.frame(
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    value = double(),
    stringsAsFactors = FALSE
  )

  if (
    is.null(merchandise) ||
      !is.data.frame(merchandise) ||
      nrow(merchandise) == 0L
  ) {
    return(empty_result)
  }

  dates <- sort(
    unique(
      as.Date(
        merchandise$date[
          merchandise$period_type ==
            "Month" &
            merchandise$metric ==
              metric &
            !is.na(merchandise$date)
        ]
      )
    )
  )

  if (length(dates) == 0L) {
    return(empty_result)
  }

  result <- list()

  for (index in seq_along(dates)) {

    current_date <- as.Date(
      dates[[index]],
      origin = "1970-01-01"
    )

    value <-
      venues_operations_merchandise_resolve(
        merchandise,
        metric = metric,
        period_type = "Month",
        section = section,
        item = item,
        date = current_date
      )

    if (!is.finite(value)) {
      next
    }

    result[[length(result) + 1L]] <-
      data.frame(
        date = current_date,
        month = as.integer(
          format(
            current_date,
            "%m"
          )
        ),
        month_label = format(
          current_date,
          "%b"
        ),
        value = value,
        stringsAsFactors = FALSE
      )
  }

  if (length(result) == 0L) {
    return(empty_result)
  }

  result <- do.call(
    rbind,
    result
  )

  result <- result[
    order(result$date),
    ,
    drop = FALSE
  ]

  rownames(result) <- NULL

  result
}


venues_operations_merchandise_annual_value <- function(
    merchandise,
    metric,
    section = "All Sections",
    item = "All Items"
) {

  ytd_value <-
    venues_operations_merchandise_resolve(
      merchandise,
      metric = metric,
      period_type = "YTD",
      section = section,
      item = item
    )

  if (is.finite(ytd_value)) {
    return(ytd_value)
  }

  monthly <-
    venues_operations_merchandise_monthly(
      merchandise,
      metric = metric,
      section = section,
      item = item
    )

  if (
    !is.data.frame(monthly) ||
      nrow(monthly) == 0L
  ) {
    return(NA_real_)
  }

  sum(
    monthly$value,
    na.rm = TRUE
  )
}


venues_operations_merchandise_kpis <- function(
    merchandise,
    section = "All Sections",
    item = "All Items"
) {

  revenue <-
    venues_operations_merchandise_annual_value(
      merchandise,
      metric = "Revenue",
      section = section,
      item = item
    )

  transactions <-
    venues_operations_merchandise_annual_value(
      merchandise,
      metric = "Transactions",
      section = section,
      item = item
    )

  revenue_per_transaction <- if (
    is.finite(revenue) &&
      is.finite(transactions) &&
      transactions > 0
  ) {
    revenue / transactions
  } else {
    NA_real_
  }

  list(
    revenue = revenue,
    transactions = transactions,
    revenue_per_transaction =
      revenue_per_transaction
  )
}


venues_operations_merchandise_breakdown <- function(
    merchandise,
    metric,
    section = "All Sections",
    top_n = 10L
) {

  empty_result <- data.frame(
    facility = character(),
    value = double(),
    stringsAsFactors = FALSE
  )

  if (
    is.null(merchandise) ||
      !is.data.frame(merchandise) ||
      nrow(merchandise) == 0L
  ) {
    return(empty_result)
  }

  if (identical(
    section,
    "All Sections"
  )) {

    labels <- unique(
      trimws(
        as.character(
          merchandise$section
        )
      )
    )

    labels <- labels[
      !is.na(labels) &
        nzchar(labels)
    ]

    result <- lapply(
      labels,
      function(section_name) {

        data.frame(
          facility = section_name,
          value =
            venues_operations_merchandise_annual_value(
              merchandise,
              metric = metric,
              section =
                section_name,
              item = "All Items"
            ),
          stringsAsFactors = FALSE
        )
      }
    )

  } else {

    rows <- merchandise[
      merchandise$row_type ==
        "Item" &
        !is.na(merchandise$section) &
        trimws(
          as.character(
            merchandise$section
          )
        ) == section,
      ,
      drop = FALSE
    ]

    if ("source_row" %in% names(rows)) {

      rows <- rows[
        order(rows$source_row),
        ,
        drop = FALSE
      ]
    }

    labels <- unique(
      trimws(
        as.character(rows$item)
      )
    )

    labels <- labels[
      !is.na(labels) &
        nzchar(labels)
    ]

    result <- lapply(
      labels,
      function(item_name) {

        data.frame(
          facility = item_name,
          value =
            venues_operations_merchandise_annual_value(
              merchandise,
              metric = metric,
              section = section,
              item = item_name
            ),
          stringsAsFactors = FALSE
        )
      }
    )
  }

  if (
    length(result) == 0L
  ) {
    return(empty_result)
  }

  result <- do.call(
    rbind,
    result
  )

  result <- result[
    is.finite(result$value),
    ,
    drop = FALSE
  ]

  if (nrow(result) == 0L) {
    return(empty_result)
  }

  result <- result[
    order(
      result$value,
      decreasing = TRUE
    ),
    ,
    drop = FALSE
  ]

  if (
    !identical(
      section,
      "All Sections"
    ) &&
      is.finite(top_n) &&
      top_n > 0L &&
      nrow(result) > top_n
  ) {

    result <- result[
      seq_len(top_n),
      ,
      drop = FALSE
    ]
  }

  rownames(result) <- NULL

  result
}

# -------------------------------------------------------------------------
# Online Sales calculations
# -------------------------------------------------------------------------

venues_operations_online_sales_site_choices <- function(
    online_sales
) {

  default_choice <- c(
    "All Sites" = "All Sites"
  )

  if (
    is.null(online_sales) ||
      !is.data.frame(online_sales) ||
      nrow(online_sales) == 0L ||
      !"site" %in% names(online_sales) ||
      !"site_scope" %in% names(online_sales)
  ) {
    return(default_choice)
  }

  sites <- unique(
    trimws(
      as.character(
        online_sales$site[
          online_sales$site_scope == "Site"
        ]
      )
    )
  )

  sites <- sites[
    !is.na(sites) &
      nzchar(sites)
  ]

  if (length(sites) == 0L) {
    return(default_choice)
  }

  c(
    default_choice,
    stats::setNames(
      sites,
      sites
    )
  )
}


venues_operations_online_sales_category_choices <- function(
    online_sales
) {

  default_choice <- c(
    "All Categories" = "All Categories"
  )

  if (
    is.null(online_sales) ||
      !is.data.frame(online_sales) ||
      nrow(online_sales) == 0L ||
      !"category" %in% names(online_sales)
  ) {
    return(default_choice)
  }

  categories <- unique(
    trimws(
      as.character(
        online_sales$category[
          online_sales$row_type == "Category"
        ]
      )
    )
  )

  categories <- categories[
    !is.na(categories) &
      nzchar(categories)
  ]

  if (length(categories) == 0L) {
    return(default_choice)
  }

  preferred_order <- c(
    "Facility Hire",
    "Membership/Health & Fitness",
    "DD Rejection Fees"
  )

  categories <- c(
    preferred_order[
      preferred_order %in% categories
    ],
    setdiff(
      categories,
      preferred_order
    )
  )

  c(
    default_choice,
    stats::setNames(
      categories,
      categories
    )
  )
}


venues_operations_online_sales_scope_label <- function(
    site = "All Sites",
    category = "All Categories"
) {

  site_label <- if (
    identical(site, "All Sites")
  ) {
    "All Sites"
  } else {
    site
  }

  category_label <- if (
    identical(
      category,
      "All Categories"
    )
  ) {
    "All Categories"
  } else {
    category
  }

  paste(
    site_label,
    category_label,
    sep = " · "
  )
}


venues_operations_online_sales_resolve <- function(
    online_sales,
    period_type,
    site = "All Sites",
    category = "All Categories",
    date = NULL
) {

  if (
    is.null(online_sales) ||
      !is.data.frame(online_sales) ||
      nrow(online_sales) == 0L
  ) {
    return(NA_real_)
  }

  required_columns <- c(
    "date",
    "period_type",
    "site",
    "site_scope",
    "category",
    "row_type",
    "value",
    "is_reported"
  )

  if (!all(
    required_columns %in%
      names(online_sales)
  )) {
    return(NA_real_)
  }

  rows <- online_sales[
    online_sales$period_type ==
      period_type,
    ,
    drop = FALSE
  ]

  if (!is.null(date)) {

    selected_date <- as.Date(date)

    rows <- rows[
      !is.na(rows$date) &
        as.Date(rows$date) ==
          selected_date,
      ,
      drop = FALSE
    ]
  }

  if (nrow(rows) == 0L) {
    return(NA_real_)
  }

  rows$value <- suppressWarnings(
    as.numeric(rows$value)
  )

  # For monthly values, only count months that
  # the cleaner identifies as reported.
  if (identical(
    period_type,
    "Month"
  )) {

    rows <- rows[
      rows$is_reported %in% TRUE,
      ,
      drop = FALSE
    ]
  }

  if (nrow(rows) == 0L) {
    return(NA_real_)
  }

  resolve_rows <- function(x) {

    if (
      is.null(x) ||
        !is.data.frame(x) ||
        nrow(x) == 0L
    ) {
      return(NA_real_)
    }

    if (!identical(
      category,
      "All Categories"
    )) {

      values <- x$value[
        x$row_type == "Category" &
          !is.na(x$category) &
          trimws(
            as.character(x$category)
          ) == category &
          is.finite(x$value)
      ]

      if (length(values) == 0L) {
        return(NA_real_)
      }

      return(
        sum(
          values,
          na.rm = TRUE
        )
      )
    }

    # All Categories:
    # prefer the reported Total row.
    total_values <- x$value[
      x$row_type == "Total" &
        is.finite(x$value)
    ]

    if (length(total_values) > 0L) {

      return(
        sum(
          total_values,
          na.rm = TRUE
        )
      )
    }

    # Fallback if a workbook has no Total row.
    category_values <- x$value[
      x$row_type == "Category" &
        is.finite(x$value)
    ]

    if (length(category_values) == 0L) {
      return(NA_real_)
    }

    sum(
      category_values,
      na.rm = TRUE
    )
  }

  # All Sites:
  # preferentially use Cross Campus values.
  if (identical(
    site,
    "All Sites"
  )) {

    cross_campus <- rows[
      rows$site_scope ==
        "Cross Campus",
      ,
      drop = FALSE
    ]

    cross_value <-
      resolve_rows(
        cross_campus
      )

    if (is.finite(cross_value)) {
      return(cross_value)
    }

    # Fallback:
    # resolve each actual site individually,
    # then add them.
    actual_sites <- unique(
      trimws(
        as.character(
          rows$site[
            rows$site_scope == "Site"
          ]
        )
      )
    )

    actual_sites <- actual_sites[
      !is.na(actual_sites) &
        nzchar(actual_sites)
    ]

    if (length(actual_sites) == 0L) {
      return(NA_real_)
    }

    values <- vapply(
      actual_sites,
      function(site_name) {

        site_rows <- rows[
          rows$site_scope == "Site" &
            !is.na(rows$site) &
            trimws(
              as.character(rows$site)
            ) == site_name,
          ,
          drop = FALSE
        ]

        resolve_rows(
          site_rows
        )
      },
      numeric(1)
    )

    values <- values[
      is.finite(values)
    ]

    if (length(values) == 0L) {
      return(NA_real_)
    }

    return(
      sum(
        values,
        na.rm = TRUE
      )
    )
  }

  # Specific site selected.
  site_rows <- rows[
    rows$site_scope == "Site" &
      !is.na(rows$site) &
      trimws(
        as.character(rows$site)
      ) == site,
    ,
    drop = FALSE
  ]

  resolve_rows(
    site_rows
  )
}


venues_operations_online_sales_monthly <- function(
    online_sales,
    site = "All Sites",
    category = "All Categories"
) {

  empty_result <- data.frame(
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    value = double(),
    stringsAsFactors = FALSE
  )

  if (
    is.null(online_sales) ||
      !is.data.frame(online_sales) ||
      nrow(online_sales) == 0L
  ) {
    return(empty_result)
  }

  dates <- sort(
    unique(
      as.Date(
        online_sales$date[
          online_sales$period_type ==
            "Month" &
            !is.na(online_sales$date)
        ]
      )
    )
  )

  if (length(dates) == 0L) {
    return(empty_result)
  }

  result <- list()

  for (index in seq_along(dates)) {

    current_date <- as.Date(
      dates[[index]],
      origin = "1970-01-01"
    )

    value <-
      venues_operations_online_sales_resolve(
        online_sales,
        period_type = "Month",
        site = site,
        category = category,
        date = current_date
      )

    if (!is.finite(value)) {
      next
    }

    result[[length(result) + 1L]] <-
      data.frame(
        date = current_date,
        month = as.integer(
          format(
            current_date,
            "%m"
          )
        ),
        month_label = format(
          current_date,
          "%b"
        ),
        value = value,
        stringsAsFactors = FALSE
      )
  }

  if (length(result) == 0L) {
    return(empty_result)
  }

  result <- do.call(
    rbind,
    result
  )

  result <- result[
    order(result$date),
    ,
    drop = FALSE
  ]

  rownames(result) <- NULL

  result
}


venues_operations_online_sales_annual_value <- function(
    online_sales,
    site = "All Sites",
    category = "All Categories"
) {

  ytd_value <-
    venues_operations_online_sales_resolve(
      online_sales,
      period_type = "YTD",
      site = site,
      category = category
    )

  if (is.finite(ytd_value)) {
    return(ytd_value)
  }

  monthly <-
    venues_operations_online_sales_monthly(
      online_sales,
      site = site,
      category = category
    )

  if (
    !is.data.frame(monthly) ||
      nrow(monthly) == 0L
  ) {
    return(NA_real_)
  }

  sum(
    monthly$value,
    na.rm = TRUE
  )
}


venues_operations_online_sales_kpis <- function(
    online_sales,
    site = "All Sites",
    category = "All Categories"
) {

  monthly <-
    venues_operations_online_sales_monthly(
      online_sales,
      site = site,
      category = category
    )

  total_revenue <-
    venues_operations_online_sales_annual_value(
      online_sales,
      site = site,
      category = category
    )

  average_monthly <- if (
    is.data.frame(monthly) &&
      nrow(monthly) > 0L
  ) {
    mean(
      monthly$value,
      na.rm = TRUE
    )
  } else {
    NA_real_
  }

  if (
    is.data.frame(monthly) &&
      nrow(monthly) > 0L
  ) {

    peak_index <- which.max(
      monthly$value
    )

    peak_value <-
      monthly$value[
        peak_index
      ]

    peak_month <-
      monthly$month_label[
        peak_index
      ]

  } else {

    peak_value <- NA_real_
    peak_month <- NA_character_
  }

  list(
    total_revenue =
      total_revenue,
    average_monthly =
      average_monthly,
    peak_value =
      peak_value,
    peak_month =
      peak_month,
    reporting_months =
      if (
        is.data.frame(monthly)
      ) {
        nrow(monthly)
      } else {
        0L
      }
  )
}


venues_operations_online_sales_by_site <- function(
    online_sales,
    category = "All Categories"
) {

  empty_result <- data.frame(
    facility = character(),
    value = double(),
    stringsAsFactors = FALSE
  )

  if (
    is.null(online_sales) ||
      !is.data.frame(online_sales) ||
      nrow(online_sales) == 0L
  ) {
    return(empty_result)
  }

  sites <- unique(
    trimws(
      as.character(
        online_sales$site[
          online_sales$site_scope == "Site"
        ]
      )
    )
  )

  sites <- sites[
    !is.na(sites) &
      nzchar(sites)
  ]

  if (length(sites) == 0L) {
    return(empty_result)
  }

  result <- lapply(
    sites,
    function(site_name) {

      data.frame(
        facility = site_name,
        value =
          venues_operations_online_sales_annual_value(
            online_sales,
            site = site_name,
            category = category
          ),
        stringsAsFactors = FALSE
      )
    }
  )

  result <- do.call(
    rbind,
    result
  )

  result <- result[
    is.finite(result$value),
    ,
    drop = FALSE
  ]

  result <- result[
    order(
      result$value,
      decreasing = TRUE
    ),
    ,
    drop = FALSE
  ]

  rownames(result) <- NULL

  result
}


venues_operations_online_sales_by_category <- function(
    online_sales,
    site = "All Sites"
) {

  empty_result <- data.frame(
    facility = character(),
    value = double(),
    stringsAsFactors = FALSE
  )

  if (
    is.null(online_sales) ||
      !is.data.frame(online_sales) ||
      nrow(online_sales) == 0L
  ) {
    return(empty_result)
  }

  categories <- unique(
    trimws(
      as.character(
        online_sales$category[
          online_sales$row_type ==
            "Category"
        ]
      )
    )
  )

  categories <- categories[
    !is.na(categories) &
      nzchar(categories)
  ]

  preferred_order <- c(
    "Facility Hire",
    "Membership/Health & Fitness",
    "DD Rejection Fees"
  )

  categories <- c(
    preferred_order[
      preferred_order %in%
        categories
    ],
    setdiff(
      categories,
      preferred_order
    )
  )

  if (length(categories) == 0L) {
    return(empty_result)
  }

  result <- lapply(
    categories,
    function(category_name) {

      data.frame(
        facility =
          category_name,
        value =
          venues_operations_online_sales_annual_value(
            online_sales,
            site = site,
            category =
              category_name
          ),
        stringsAsFactors = FALSE
      )
    }
  )

  result <- do.call(
    rbind,
    result
  )

  result <- result[
    is.finite(result$value),
    ,
    drop = FALSE
  ]

  result <- result[
    order(
      result$value,
      decreasing = TRUE
    ),
    ,
    drop = FALSE
  ]

  rownames(result) <- NULL

  result
}

# -------------------------------------------------------------------------
# Venue Finance calculations
# -------------------------------------------------------------------------

venues_operations_finance_fund_choices <- function(finance) {

  default_choice <- c(
    "All Funds" = "All Funds"
  )

  if (
    is.null(finance) ||
      !is.data.frame(finance) ||
      nrow(finance) == 0L ||
      !"fund" %in% names(finance)
  ) {
    return(default_choice)
  }

  funds <- unique(
    trimws(
      as.character(
        finance$fund[
          finance$row_type == "Fund"
        ]
      )
    )
  )

  funds <- funds[
    !is.na(funds) &
      nzchar(funds)
  ]

  if (length(funds) == 0L) {
    return(default_choice)
  }

  c(
    default_choice,
    stats::setNames(
      funds,
      funds
    )
  )
}


venues_operations_finance_monthly <- function(
    finance,
    fund = "All Funds"
) {

  empty_result <- data.frame(
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    income_actual = double(),
    income_budget = double(),
    expenses_actual = double(),
    expenses_budget = double(),
    net_actual = double(),
    net_budget = double(),
    stringsAsFactors = FALSE
  )

  if (
    is.null(finance) ||
      !is.data.frame(finance) ||
      nrow(finance) == 0L
  ) {
    return(empty_result)
  }

  required_columns <- c(
    "date",
    "month",
    "month_label",
    "fund",
    "row_type",
    "metric",
    "value_type",
    "value"
  )

  if (!all(required_columns %in% names(finance))) {
    return(empty_result)
  }

  if (identical(
    fund,
    "All Funds"
  )) {

    rows <- finance[
      finance$row_type == "MTD Total" &
        !is.na(finance$date),
      ,
      drop = FALSE
    ]

  } else {

    rows <- finance[
      finance$row_type == "Fund" &
        !is.na(finance$fund) &
        trimws(
          as.character(finance$fund)
        ) == fund &
        !is.na(finance$date),
      ,
      drop = FALSE
    ]
  }

  if (nrow(rows) == 0L) {
    return(empty_result)
  }

  rows$value <- suppressWarnings(
    as.numeric(rows$value)
  )

  dates <- sort(
    unique(
      as.Date(rows$date)
    )
  )

  result <- list()

  value_for <- function(
      month_rows,
      metric_name,
      value_type_name
  ) {

    values <- month_rows$value[
      month_rows$metric == metric_name &
        month_rows$value_type ==
          value_type_name &
        is.finite(month_rows$value)
    ]

    if (length(values) == 0L) {
      return(NA_real_)
    }

    sum(
      values,
      na.rm = TRUE
    )
  }

  for (current_date in dates) {

    current_date <- as.Date(
      current_date,
      origin = "1970-01-01"
    )

    month_rows <- rows[
      as.Date(rows$date) ==
        current_date,
      ,
      drop = FALSE
    ]

    income_actual <- value_for(
      month_rows,
      "Income",
      "Actual"
    )

    income_budget <- value_for(
      month_rows,
      "Income",
      "Budget"
    )

    expenses_actual <- value_for(
      month_rows,
      "Expenses",
      "Actual"
    )

    expenses_budget <- value_for(
      month_rows,
      "Expenses",
      "Budget"
    )

    net_actual <- if (
      is.finite(income_actual) &&
        is.finite(expenses_actual)
    ) {
      income_actual -
        expenses_actual
    } else {
      NA_real_
    }

    net_budget <- if (
      is.finite(income_budget) &&
        is.finite(expenses_budget)
    ) {
      income_budget -
        expenses_budget
    } else {
      NA_real_
    }

    result[[length(result) + 1L]] <-
      data.frame(
        date = as.Date(current_date),
        month = as.integer(
          format(
            current_date,
            "%m"
          )
        ),
        month_label = format(
          current_date,
          "%b"
        ),
        income_actual = income_actual,
        income_budget = income_budget,
        expenses_actual = expenses_actual,
        expenses_budget = expenses_budget,
        net_actual = net_actual,
        net_budget = net_budget,
        stringsAsFactors = FALSE
      )
  }

  if (length(result) == 0L) {
    return(empty_result)
  }

  result <- do.call(
    rbind,
    result
  )

  keep <-
    is.finite(result$income_actual) |
    is.finite(result$income_budget) |
    is.finite(result$expenses_actual) |
    is.finite(result$expenses_budget)

  result <- result[
    keep,
    ,
    drop = FALSE
  ]

  result <- result[
    order(result$date),
    ,
    drop = FALSE
  ]

  rownames(result) <- NULL

  result
}


venues_operations_finance_ytd <- function(
    finance,
    fund = "All Funds"
) {

  empty_result <- list(
    income_actual = NA_real_,
    income_budget = NA_real_,
    expenses_actual = NA_real_,
    expenses_budget = NA_real_,
    net_actual = NA_real_,
    net_budget = NA_real_
  )

  if (
    is.null(finance) ||
      !is.data.frame(finance) ||
      nrow(finance) == 0L
  ) {
    return(empty_result)
  }

  monthly <-
    venues_operations_finance_monthly(
      finance,
      fund = fund
    )

  sum_column <- function(column) {

    if (
      !is.data.frame(monthly) ||
        nrow(monthly) == 0L ||
        !column %in% names(monthly)
    ) {
      return(NA_real_)
    }

    values <- monthly[[column]]

    values <- values[
      is.finite(values)
    ]

    if (length(values) == 0L) {
      return(NA_real_)
    }

    sum(
      values,
      na.rm = TRUE
    )
  }

  # Individual funds are MTD rows, so YTD is
  # calculated by summing the monthly values.
  if (!identical(
    fund,
    "All Funds"
  )) {

    income_actual <-
      sum_column("income_actual")

    income_budget <-
      sum_column("income_budget")

    expenses_actual <-
      sum_column("expenses_actual")

    expenses_budget <-
      sum_column("expenses_budget")

  } else {

    # For All Funds, prefer the workbook's
    # latest reported YTD Total.
    rows <- finance[
      finance$row_type ==
        "YTD Total" &
        !is.na(finance$date),
      ,
      drop = FALSE
    ]

    rows$value <- suppressWarnings(
      as.numeric(rows$value)
    )

    latest_date <- if (
      nrow(rows) > 0L
    ) {
      max(
        as.Date(rows$date),
        na.rm = TRUE
      )
    } else {
      as.Date(NA)
    }

    latest <- if (
      !is.na(latest_date)
    ) {
      rows[
        as.Date(rows$date) ==
          latest_date,
        ,
        drop = FALSE
      ]
    } else {
      rows[0, , drop = FALSE]
    }

    value_for <- function(
        metric_name,
        value_type_name
    ) {

      values <- latest$value[
        latest$metric == metric_name &
          latest$value_type ==
            value_type_name &
          is.finite(latest$value)
      ]

      if (length(values) == 0L) {
        return(NA_real_)
      }

      sum(
        values,
        na.rm = TRUE
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

    # Fall back to accumulated MTD values
    # if any YTD value is unavailable.
    if (!is.finite(income_actual)) {
      income_actual <-
        sum_column("income_actual")
    }

    if (!is.finite(income_budget)) {
      income_budget <-
        sum_column("income_budget")
    }

    if (!is.finite(expenses_actual)) {
      expenses_actual <-
        sum_column("expenses_actual")
    }

    if (!is.finite(expenses_budget)) {
      expenses_budget <-
        sum_column("expenses_budget")
    }
  }

  net_actual <- if (
    is.finite(income_actual) &&
      is.finite(expenses_actual)
  ) {
    income_actual -
      expenses_actual
  } else {
    NA_real_
  }

  net_budget <- if (
    is.finite(income_budget) &&
      is.finite(expenses_budget)
  ) {
    income_budget -
      expenses_budget
  } else {
    NA_real_
  }

  list(
    income_actual = income_actual,
    income_budget = income_budget,
    expenses_actual = expenses_actual,
    expenses_budget = expenses_budget,
    net_actual = net_actual,
    net_budget = net_budget
  )
}


venues_operations_finance_kpis <- function(
    finance,
    fund = "All Funds"
) {

  values <-
    venues_operations_finance_ytd(
      finance,
      fund = fund
    )

  list(
    income = values$income_actual,
    expenses = values$expenses_actual,
    net_result = values$net_actual
  )
}


venues_operations_finance_by_fund <- function(finance) {

  empty_result <- data.frame(
    report_year = integer(),
    site = character(),
    income_actual = double(),
    income_budget = double(),
    expenses_actual = double(),
    expenses_budget = double(),
    net_actual = double(),
    net_budget = double(),
    stringsAsFactors = FALSE
  )

  if (
    is.null(finance) ||
      !is.data.frame(finance) ||
      nrow(finance) == 0L
  ) {
    return(empty_result)
  }

  funds <- unique(
    trimws(
      as.character(
        finance$fund[
          finance$row_type ==
            "Fund"
        ]
      )
    )
  )

  funds <- funds[
    !is.na(funds) &
      nzchar(funds)
  ]

  if (length(funds) == 0L) {
    return(empty_result)
  }

  years <- unique(
    suppressWarnings(
      as.integer(
        finance$report_year
      )
    )
  )

  years <- years[
    !is.na(years)
  ]

  report_year <- if (
    length(years) > 0L
  ) {
    years[[1L]]
  } else {
    NA_integer_
  }

  rows <- lapply(
    funds,
    function(fund_name) {

      values <-
        venues_operations_finance_ytd(
          finance,
          fund = fund_name
        )

      data.frame(
        report_year = report_year,
        site = fund_name,
        income_actual =
          values$income_actual,
        income_budget =
          values$income_budget,
        expenses_actual =
          values$expenses_actual,
        expenses_budget =
          values$expenses_budget,
        net_actual =
          values$net_actual,
        net_budget =
          values$net_budget,
        stringsAsFactors = FALSE
      )
    }
  )

  result <- do.call(
    rbind,
    rows
  )

  result <- result[
    is.finite(result$net_actual),
    ,
    drop = FALSE
  ]

  rownames(result) <- NULL

  result
}

# -------------------------------------------------------------------------
# Facility Hire calculations
# -------------------------------------------------------------------------

venues_operations_facility_hire_kpis <- function(facility_hire) {

  empty_result <- list(
    revenue = NA_real_,
    transactions = NA_real_,
    revenue_per_transaction = NA_real_
  )

  if (
    is.null(facility_hire) ||
      !is.data.frame(facility_hire) ||
      nrow(facility_hire) == 0L
  ) {
    return(empty_result)
  }

  required_columns <- c(
    "period_type",
    "row_type",
    "facility",
    "metric",
    "value"
  )

  if (!all(required_columns %in% names(facility_hire))) {
    return(empty_result)
  }

  rows <- facility_hire[
    facility_hire$period_type == "YTD" &
      !is.na(facility_hire$facility) &
      nzchar(trimws(facility_hire$facility)),
    ,
    drop = FALSE
  ]

  if (nrow(rows) == 0L) {
    return(empty_result)
  }

  rows$value <- suppressWarnings(
    as.numeric(rows$value)
  )

  rows <- rows[
    is.finite(rows$value),
    ,
    drop = FALSE
  ]

  if (nrow(rows) == 0L) {
    return(empty_result)
  }

  facilities <- unique(
    as.character(rows$facility)
  )

  resolved <- list()

  for (facility_name in facilities) {

    facility_rows <- rows[
      rows$facility == facility_name,
      ,
      drop = FALSE
    ]

    for (metric_name in c(
      "Revenue",
      "Transactions"
    )) {

      metric_rows <- facility_rows[
        facility_rows$metric == metric_name,
        ,
        drop = FALSE
      ]

      if (nrow(metric_rows) == 0L) {
        next
      }

      total_rows <- metric_rows[
        metric_rows$row_type == "Total",
        ,
        drop = FALSE
      ]

      value <- if (nrow(total_rows) > 0L) {

        # Prefer the reported facility total when available.
        sum(
          total_rows$value,
          na.rm = TRUE
        )

      } else {

        # Otherwise calculate the facility total from its components.
        detail_rows <- metric_rows[
          metric_rows$row_type == "Detail",
          ,
          drop = FALSE
        ]

        if (nrow(detail_rows) == 0L) {
          next
        }

        sum(
          detail_rows$value,
          na.rm = TRUE
        )
      }

      resolved[[length(resolved) + 1L]] <-
        data.frame(
          facility = facility_name,
          metric = metric_name,
          value = value,
          stringsAsFactors = FALSE
        )
    }
  }

  if (length(resolved) == 0L) {
    return(empty_result)
  }

  resolved <- do.call(
    rbind,
    resolved
  )

  revenue_values <- resolved$value[
    resolved$metric == "Revenue"
  ]

  transaction_values <- resolved$value[
    resolved$metric == "Transactions"
  ]

  revenue <- if (
    length(revenue_values) > 0L
  ) {
    sum(
      revenue_values,
      na.rm = TRUE
    )
  } else {
    NA_real_
  }

  transactions <- if (
    length(transaction_values) > 0L
  ) {
    sum(
      transaction_values,
      na.rm = TRUE
    )
  } else {
    NA_real_
  }

  revenue_per_transaction <- if (
    is.finite(revenue) &&
      is.finite(transactions) &&
      transactions > 0
  ) {
    revenue / transactions
  } else {
    NA_real_
  }

  list(
    revenue = revenue,
    transactions = transactions,
    revenue_per_transaction =
      revenue_per_transaction
  )
}

venues_operations_monthly_facility_hire <- function(
    facility_hire,
    metric = "Revenue"
) {

  empty_result <- data.frame(
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    value = double(),
    stringsAsFactors = FALSE
  )

  if (
    is.null(facility_hire) ||
      !is.data.frame(facility_hire) ||
      nrow(facility_hire) == 0L
  ) {
    return(empty_result)
  }

  required_columns <- c(
    "date",
    "month",
    "month_label",
    "period_type",
    "facility",
    "row_type",
    "metric",
    "value"
  )

  if (!all(required_columns %in% names(facility_hire))) {
    return(empty_result)
  }

  rows <- facility_hire[
    facility_hire$period_type == "Month" &
      facility_hire$metric == metric &
      !is.na(facility_hire$date) &
      !is.na(facility_hire$facility) &
      nzchar(trimws(facility_hire$facility)),
    ,
    drop = FALSE
  ]

  if (nrow(rows) == 0L) {
    return(empty_result)
  }

  rows$value <- suppressWarnings(
    as.numeric(rows$value)
  )

  keys <- unique(
    rows[
      ,
      c(
        "date",
        "month",
        "month_label",
        "facility"
      ),
      drop = FALSE
    ]
  )

  resolved <- list()

  for (index in seq_len(nrow(keys))) {

    key <- keys[index, , drop = FALSE]

    facility_rows <- rows[
      rows$date == key$date &
        rows$facility == key$facility,
      ,
      drop = FALSE
    ]

    total_values <- facility_rows$value[
      facility_rows$row_type == "Total" &
        is.finite(facility_rows$value)
    ]

    detail_values <- facility_rows$value[
      facility_rows$row_type == "Detail" &
        is.finite(facility_rows$value)
    ]

    value <- if (length(total_values) > 0L) {

      sum(total_values, na.rm = TRUE)

    } else if (length(detail_values) > 0L) {

      sum(detail_values, na.rm = TRUE)

    } else {
      next
    }

    resolved[[length(resolved) + 1L]] <-
      data.frame(
        date = as.Date(key$date),
        month = as.integer(key$month),
        month_label = as.character(key$month_label),
        value = value,
        stringsAsFactors = FALSE
      )
  }

  if (length(resolved) == 0L) {
    return(empty_result)
  }

  resolved <- do.call(
    rbind,
    resolved
  )

  monthly <- aggregate(
    value ~ date + month + month_label,
    data = resolved,
    FUN = sum
  )

  monthly <- monthly[
    order(monthly$date),
    ,
    drop = FALSE
  ]

  rownames(monthly) <- NULL

  monthly
}


venues_operations_facility_hire_by_facility <- function(
    facility_hire,
    metric = "Revenue"
) {

  empty_result <- data.frame(
    facility = character(),
    value = double(),
    stringsAsFactors = FALSE
  )

  if (
    is.null(facility_hire) ||
      !is.data.frame(facility_hire) ||
      nrow(facility_hire) == 0L
  ) {
    return(empty_result)
  }

  required_columns <- c(
    "period_type",
    "facility",
    "row_type",
    "metric",
    "value"
  )

  if (!all(required_columns %in% names(facility_hire))) {
    return(empty_result)
  }

  rows <- facility_hire[
    facility_hire$period_type == "YTD" &
      facility_hire$metric == metric &
      !is.na(facility_hire$facility) &
      nzchar(trimws(facility_hire$facility)),
    ,
    drop = FALSE
  ]

  if (nrow(rows) == 0L) {
    return(empty_result)
  }

  rows$value <- suppressWarnings(
    as.numeric(rows$value)
  )

  facilities <- unique(
    as.character(rows$facility)
  )

  resolved <- list()

  for (facility_name in facilities) {

    facility_rows <- rows[
      rows$facility == facility_name,
      ,
      drop = FALSE
    ]

    total_values <- facility_rows$value[
      facility_rows$row_type == "Total" &
        is.finite(facility_rows$value)
    ]

    detail_values <- facility_rows$value[
      facility_rows$row_type == "Detail" &
        is.finite(facility_rows$value)
    ]

    value <- if (length(total_values) > 0L) {

      sum(total_values, na.rm = TRUE)

    } else if (length(detail_values) > 0L) {

      sum(detail_values, na.rm = TRUE)

    } else {
      next
    }

    resolved[[length(resolved) + 1L]] <-
      data.frame(
        facility = facility_name,
        value = value,
        stringsAsFactors = FALSE
      )
  }

  if (length(resolved) == 0L) {
    return(empty_result)
  }

  result <- do.call(
    rbind,
    resolved
  )

  result <- result[
    order(
      result$value,
      decreasing = TRUE
    ),
    ,
    drop = FALSE
  ]

  rownames(result) <- NULL

  result
}

venues_operations_facility_hire_group <- function(facility) {

  sports_facilities <- c(
    "Badminton",
    "Basketball/Soccer (Games Hall)",
    "Squash",
    "Table Tennis",
    "Tennis",
    "Volleyball"
  )

  facility <- trimws(
    as.character(facility)
  )

  result <- rep(
    NA_character_,
    length(facility)
  )

  valid <-
    !is.na(facility) &
    nzchar(facility)

  result[
    valid &
      facility %in% sports_facilities
  ] <- "Sports Facilities"

  result[
    valid &
      !(facility %in% sports_facilities)
  ] <- "Venue & Space Hire"

  result
}


venues_operations_facility_hire_detail <- function(
    facility_hire,
    facility,
    metric = "Revenue"
) {

  empty_result <- data.frame(
    facility = character(),
    value = double(),
    stringsAsFactors = FALSE
  )

  if (
    is.null(facility_hire) ||
      !is.data.frame(facility_hire) ||
      nrow(facility_hire) == 0L ||
      is.null(facility) ||
      length(facility) == 0L
  ) {
    return(empty_result)
  }

  required_columns <- c(
    "period_type",
    "row_type",
    "facility",
    "raw_label",
    "metric",
    "value"
  )

  if (!all(required_columns %in% names(facility_hire))) {
    return(empty_result)
  }

  selected_facility <- trimws(
    as.character(facility[[1L]])
  )

  if (
    is.na(selected_facility) ||
      !nzchar(selected_facility) ||
      identical(
        selected_facility,
        "All Facilities"
      )
  ) {
    return(empty_result)
  }

  select_rows <- function(period_type) {

    rows <- facility_hire[
      facility_hire$period_type == period_type &
        facility_hire$row_type == "Detail" &
        facility_hire$metric == metric &
        !is.na(facility_hire$facility) &
        trimws(
          as.character(
            facility_hire$facility
          )
        ) == selected_facility &
        !is.na(facility_hire$raw_label) &
        nzchar(
          trimws(
            as.character(
              facility_hire$raw_label
            )
          )
        ),
      ,
      drop = FALSE
    ]

    rows$value <- suppressWarnings(
      as.numeric(rows$value)
    )

    rows <- rows[
      is.finite(rows$value),
      ,
      drop = FALSE
    ]

    rows
  }

  # Prefer the workbook's YTD category values.
  rows <- select_rows("YTD")

  # Fall back to summing monthly category values if YTD is unavailable.
  if (nrow(rows) == 0L) {
    rows <- select_rows("Month")
  }

  if (nrow(rows) == 0L) {
    return(empty_result)
  }

  rows$display_label <- trimws(
    as.character(rows$raw_label)
  )

  totals <- aggregate(
    value ~ display_label,
    data = rows,
    FUN = sum
  )

  result <- data.frame(
    facility = totals$display_label,
    value = totals$value,
    stringsAsFactors = FALSE
  )

  result <- result[
    order(
      result$value,
      decreasing = TRUE
    ),
    ,
    drop = FALSE
  ]

  rownames(result) <- NULL

  result
}

# -------------------------------------------------------------------------
# Membership calculations
# -------------------------------------------------------------------------

venues_operations_membership_group_display <- function(group) {

  display_names <- c(
    "Fitness" = "Fitness",
    "Fitness & Aquatics" = "Fitness & Aquatics",
    "Staff Memberships" =
      "Staff Memberships (Fitness & Aquatics)",
    "Aquatics Memberships" =
      "Aquatics Memberships",
    "Free Offerings" =
      "Free Offerings - (Fitness & Aquatics)"
  )

  group <- as.character(group)

  result <- group

  matched <-
    !is.na(group) &
    group %in% names(display_names)

  result[matched] <-
    unname(
      display_names[
        group[matched]
      ]
    )

  result
}


venues_operations_membership_group_choices <- function(
    membership
) {

  default_choice <- c(
    "All Memberships" =
      "All Memberships"
  )

  if (
    is.null(membership) ||
      !is.data.frame(membership) ||
      nrow(membership) == 0L ||
      !"membership_group" %in% names(membership)
  ) {
    return(default_choice)
  }

  groups <- as.character(
    membership$membership_group
  )

  groups <- unique(
    groups[
      !is.na(groups) &
        nzchar(trimws(groups)) &
        groups != "Supplementary Metrics"
    ]
  )

  preferred_order <- c(
    "Fitness",
    "Fitness & Aquatics",
    "Staff Memberships",
    "Aquatics Memberships",
    "Free Offerings"
  )

  groups <- c(
    preferred_order[
      preferred_order %in% groups
    ],
    setdiff(
      groups,
      preferred_order
    )
  )

  if (length(groups) == 0L) {
    return(default_choice)
  }

  labels <-
    venues_operations_membership_group_display(
      groups
    )

  c(
    default_choice,
    stats::setNames(
      groups,
      labels
    )
  )
}


venues_operations_membership_type_choices <- function(
    membership,
    group
) {

  default_choice <- c(
    "All Membership Types" =
      "All Membership Types"
  )

  if (
    is.null(membership) ||
      !is.data.frame(membership) ||
      nrow(membership) == 0L ||
      is.null(group) ||
      length(group) == 0L ||
      identical(
        group,
        "All Memberships"
      )
  ) {
    return(default_choice)
  }

  rows <- membership[
    !is.na(membership$membership_group) &
      membership$membership_group == group &
      membership$row_type == "Detail" &
      !is.na(membership$membership_type),
    ,
    drop = FALSE
  ]

  if (nrow(rows) == 0L) {
    return(default_choice)
  }

  if ("source_row" %in% names(rows)) {

    rows <- rows[
      order(rows$source_row),
      ,
      drop = FALSE
    ]
  }

  membership_types <- unique(
    trimws(
      as.character(
        rows$membership_type
      )
    )
  )

  membership_types <-
    membership_types[
      !is.na(membership_types) &
        nzchar(membership_types)
    ]

  if (length(membership_types) == 0L) {
    return(default_choice)
  }

  c(
    default_choice,
    stats::setNames(
      membership_types,
      membership_types
    )
  )
}

venues_operations_membership_scope <- function(
    membership,
    group = "All Memberships",
    membership_type = "All Membership Types"
) {

  if (
    is.null(membership) ||
      !is.data.frame(membership) ||
      nrow(membership) == 0L
  ) {
    return(membership)
  }

  rows <- membership[
    !is.na(membership$membership_group) &
      membership$membership_group !=
        "Supplementary Metrics",
    ,
    drop = FALSE
  ]

  # Specific membership product selected.
  # Use only the underlying Detail row.
  if (
    !identical(
      membership_type,
      "All Membership Types"
    )
  ) {

    return(
      rows[
        rows$row_type == "Detail" &
          rows$membership_group == group &
          rows$membership_type ==
            membership_type,
        ,
        drop = FALSE
      ]
    )
  }

  # Specific membership group selected.
  if (
    !identical(
      group,
      "All Memberships"
    )
  ) {

    rows <- rows[
      rows$membership_group == group,
      ,
      drop = FALSE
    ]
  }

  # Keep both levels.
  # The resolver below decides whether to use
  # Section Total or Detail for each group.
  rows[
    rows$row_type %in%
      c(
        "Section Total",
        "Detail"
      ),
    ,
    drop = FALSE
  ]
}


venues_operations_membership_resolve <- function(
    membership,
    metric,
    period_type,
    group = "All Memberships",
    membership_type = "All Membership Types",
    date = NULL
) {

  rows <-
    venues_operations_membership_scope(
      membership,
      group = group,
      membership_type = membership_type
    )

  if (
    is.null(rows) ||
      !is.data.frame(rows) ||
      nrow(rows) == 0L
  ) {
    return(NA_real_)
  }

  required_columns <- c(
    "period_type",
    "membership_group",
    "membership_type",
    "row_type",
    "metric",
    "value"
  )

  if (!all(required_columns %in% names(rows))) {
    return(NA_real_)
  }

  rows <- rows[
    rows$metric == metric &
      rows$period_type == period_type,
    ,
    drop = FALSE
  ]

  if (!is.null(date)) {

    if (!"date" %in% names(rows)) {
      return(NA_real_)
    }

    rows <- rows[
      !is.na(rows$date) &
        rows$date == as.Date(date),
      ,
      drop = FALSE
    ]
  }

  if (nrow(rows) == 0L) {
    return(NA_real_)
  }

  rows$value <- suppressWarnings(
    as.numeric(rows$value)
  )

  # If one specific membership product was selected,
  # there is no higher-level group total to prefer.
  if (
    !identical(
      membership_type,
      "All Membership Types"
    )
  ) {

    values <- rows$value[
      is.finite(rows$value)
    ]

    if (length(values) == 0L) {
      return(NA_real_)
    }

    return(
      sum(
        values,
        na.rm = TRUE
      )
    )
  }

  groups <- unique(
    as.character(
      rows$membership_group
    )
  )

  groups <- groups[
    !is.na(groups) &
      nzchar(trimws(groups)) &
      groups != "Supplementary Metrics"
  ]

  if (length(groups) == 0L) {
    return(NA_real_)
  }

  resolved <- numeric()

  for (group_name in groups) {

    group_rows <- rows[
      rows$membership_group ==
        group_name,
      ,
      drop = FALSE
    ]

    section_values <-
      group_rows$value[
        group_rows$row_type ==
          "Section Total" &
          is.finite(group_rows$value)
      ]

    # Prefer the group's reported section total
    # when that cell actually contains a value.
    if (length(section_values) > 0L) {

      resolved <- c(
        resolved,
        sum(
          section_values,
          na.rm = TRUE
        )
      )

      next
    }

    # Otherwise calculate that group's value
    # from its membership products.
    detail_values <-
      group_rows$value[
        group_rows$row_type ==
          "Detail" &
          is.finite(group_rows$value)
      ]

    if (length(detail_values) > 0L) {

      resolved <- c(
        resolved,
        sum(
          detail_values,
          na.rm = TRUE
        )
      )
    }
  }

  if (length(resolved) == 0L) {
    return(NA_real_)
  }

  sum(
    resolved,
    na.rm = TRUE
  )
}


venues_operations_membership_scope_label <- function(
    group,
    membership_type
) {

  if (
    !is.null(membership_type) &&
      length(membership_type) > 0L &&
      !identical(
        membership_type,
        "All Membership Types"
      )
  ) {
    return(
      as.character(
        membership_type[[1L]]
      )
    )
  }

  if (
    !is.null(group) &&
      length(group) > 0L &&
      !identical(
        group,
        "All Memberships"
      )
  ) {
    return(
      venues_operations_membership_group_display(
        group[[1L]]
      )
    )
  }

  "All Memberships"
}


venues_operations_membership_annual_value <- function(
    membership,
    metric,
    group = "All Memberships",
    membership_type = "All Membership Types"
) {

  # First preference:
  # use the workbook's reported Total block.
  reported_value <-
    venues_operations_membership_resolve(
      membership,
      metric = metric,
      period_type = "Reported Total",
      group = group,
      membership_type = membership_type
    )

  if (is.finite(reported_value)) {
    return(reported_value)
  }

  # Fallback:
  # add the resolved monthly flow values.
  rows <-
    venues_operations_membership_scope(
      membership,
      group = group,
      membership_type = membership_type
    )

  if (
    is.null(rows) ||
      !is.data.frame(rows) ||
      nrow(rows) == 0L ||
      !"date" %in% names(rows)
  ) {
    return(NA_real_)
  }

  dates <- sort(
    unique(
      as.Date(
        rows$date[
          rows$period_type == "Month" &
            rows$metric == metric &
            !is.na(rows$date)
        ]
      )
    )
  )

  if (length(dates) == 0L) {
    return(NA_real_)
  }

  values <- vapply(
    dates,
    function(current_date) {

      venues_operations_membership_resolve(
        membership,
        metric = metric,
        period_type = "Month",
        group = group,
        membership_type = membership_type,
        date = as.Date(
          current_date,
          origin = "1970-01-01"
        )
      )
    },
    numeric(1)
  )

  values <- values[
    is.finite(values)
  ]

  if (length(values) == 0L) {
    return(NA_real_)
  }

  sum(
    values,
    na.rm = TRUE
  )
}


venues_operations_membership_active_value <- function(
    membership,
    group = "All Memberships",
    membership_type = "All Membership Types"
) {

  rows <-
    venues_operations_membership_scope(
      membership,
      group = group,
      membership_type = membership_type
    )

  if (
    is.null(rows) ||
      !is.data.frame(rows) ||
      nrow(rows) == 0L ||
      !"date" %in% names(rows)
  ) {
    return(NA_real_)
  }

  dates <- as.Date(
    rows$date[
      rows$period_type == "Month" &
        rows$metric == "Active" &
        !is.na(rows$date)
    ]
  )

  if (length(dates) == 0L) {
    return(NA_real_)
  }

  latest_date <- max(
    dates,
    na.rm = TRUE
  )

  venues_operations_membership_resolve(
    membership,
    metric = "Active",
    period_type = "Month",
    group = group,
    membership_type = membership_type,
    date = latest_date
  )
}


venues_operations_membership_kpis <- function(
    membership,
    group = "All Memberships",
    membership_type = "All Membership Types"
) {

  list(
    new =
      venues_operations_membership_annual_value(
        membership,
        metric = "New",
        group = group,
        membership_type =
          membership_type
      ),

    expired_cancelled =
      venues_operations_membership_annual_value(
        membership,
        metric = "Expired/Cancelled",
        group = group,
        membership_type =
          membership_type
      ),

    active =
      venues_operations_membership_active_value(
        membership,
        group = group,
        membership_type =
          membership_type
      )
  )
}


venues_operations_membership_active_trend <- function(
    membership,
    group = "All Memberships",
    membership_type = "All Membership Types"
) {

  empty_result <- data.frame(
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    value = double(),
    stringsAsFactors = FALSE
  )

  rows <-
    venues_operations_membership_scope(
      membership,
      group = group,
      membership_type = membership_type
    )

  if (
    is.null(rows) ||
      !is.data.frame(rows) ||
      nrow(rows) == 0L ||
      !"date" %in% names(rows)
  ) {
    return(empty_result)
  }

  dates <- sort(
    unique(
      as.Date(
        rows$date[
          rows$period_type == "Month" &
            rows$metric == "Active" &
            !is.na(rows$date)
        ]
      )
    )
  )

  if (length(dates) == 0L) {
    return(empty_result)
  }

  values <- vapply(
    dates,
    function(current_date) {

      venues_operations_membership_resolve(
        membership,
        metric = "Active",
        period_type = "Month",
        group = group,
        membership_type = membership_type,
        date = as.Date(
          current_date,
          origin = "1970-01-01"
        )
      )
    },
    numeric(1)
  )

  keep <- is.finite(values)

  dates <- dates[keep]
  values <- values[keep]

  if (length(dates) == 0L) {
    return(empty_result)
  }

  data.frame(
    date = as.Date(dates),
    month = as.integer(
      format(
        as.Date(dates),
        "%m"
      )
    ),
    month_label = format(
      as.Date(dates),
      "%b"
    ),
    value = values,
    stringsAsFactors = FALSE
  )
}


venues_operations_membership_movement <- function(
    membership,
    group = "All Memberships",
    membership_type = "All Membership Types"
) {

  empty_result <- data.frame(
    date = as.Date(character()),
    month_label = character(),
    new_members = double(),
    expired_cancelled = double(),
    stringsAsFactors = FALSE
  )

  rows <-
    venues_operations_membership_scope(
      membership,
      group = group,
      membership_type = membership_type
    )

  if (
    is.null(rows) ||
      !is.data.frame(rows) ||
      nrow(rows) == 0L ||
      !"date" %in% names(rows)
  ) {
    return(empty_result)
  }

  dates <- sort(
    unique(
      as.Date(
        rows$date[
          rows$period_type == "Month" &
            !is.na(rows$date)
        ]
      )
    )
  )

  if (length(dates) == 0L) {
    return(empty_result)
  }

  new_values <- vapply(
    dates,
    function(current_date) {

      venues_operations_membership_resolve(
        membership,
        metric = "New",
        period_type = "Month",
        group = group,
        membership_type = membership_type,
        date = as.Date(
          current_date,
          origin = "1970-01-01"
        )
      )
    },
    numeric(1)
  )

  expired_values <- vapply(
    dates,
    function(current_date) {

      venues_operations_membership_resolve(
        membership,
        metric = "Expired/Cancelled",
        period_type = "Month",
        group = group,
        membership_type = membership_type,
        date = as.Date(
          current_date,
          origin = "1970-01-01"
        )
      )
    },
    numeric(1)
  )

  keep <-
    is.finite(new_values) |
    is.finite(expired_values)

  if (!any(keep)) {
    return(empty_result)
  }

  data.frame(
    date = as.Date(
      dates[keep]
    ),
    month_label = format(
      as.Date(
        dates[keep]
      ),
      "%b"
    ),
    new_members =
      new_values[keep],
    expired_cancelled =
      expired_values[keep],
    stringsAsFactors = FALSE
  )
}


venues_operations_membership_breakdown <- function(
    membership,
    group = "All Memberships",
    membership_type = "All Membership Types"
) {

  empty_result <- data.frame(
    facility = character(),
    value = double(),
    stringsAsFactors = FALSE
  )

  if (
    is.null(membership) ||
      !is.data.frame(membership) ||
      nrow(membership) == 0L
  ) {
    return(empty_result)
  }

  # Specific membership product:
  # show its three headline measures.
  if (
    !identical(
      membership_type,
      "All Membership Types"
    )
  ) {

    result <- data.frame(
      facility = c(
        "New Memberships",
        "Expired / Cancelled",
        "Active Members"
      ),
      value = c(
        venues_operations_membership_annual_value(
          membership,
          metric = "New",
          group = group,
          membership_type =
            membership_type
        ),
        venues_operations_membership_annual_value(
          membership,
          metric = "Expired/Cancelled",
          group = group,
          membership_type =
            membership_type
        ),
        venues_operations_membership_active_value(
          membership,
          group = group,
          membership_type =
            membership_type
        )
      ),
      stringsAsFactors = FALSE
    )

    result <- result[
      is.finite(result$value),
      ,
      drop = FALSE
    ]

    rownames(result) <- NULL

    return(result)
  }

  # All Memberships:
  # compare Active Members across the five groups.
  if (
    identical(
      group,
      "All Memberships"
    )
  ) {

    groups <- unique(
      as.character(
        membership$membership_group
      )
    )

    groups <- groups[
      !is.na(groups) &
        nzchar(trimws(groups)) &
        groups != "Supplementary Metrics"
    ]

    preferred_order <- c(
      "Fitness",
      "Fitness & Aquatics",
      "Staff Memberships",
      "Aquatics Memberships",
      "Free Offerings"
    )

    groups <- c(
      preferred_order[
        preferred_order %in% groups
      ],
      setdiff(
        groups,
        preferred_order
      )
    )

    results <- lapply(
      groups,
      function(group_name) {

        data.frame(
          facility =
            venues_operations_membership_group_display(
              group_name
            ),
          value =
            venues_operations_membership_active_value(
              membership,
              group = group_name,
              membership_type =
                "All Membership Types"
            ),
          stringsAsFactors = FALSE
        )
      }
    )

  } else {

    # Specific group:
    # compare all products within that group.
    rows <- membership[
      membership$membership_group == group &
        membership$row_type == "Detail" &
        !is.na(membership$membership_type),
      ,
      drop = FALSE
    ]

    if ("source_row" %in% names(rows)) {

      rows <- rows[
        order(rows$source_row),
        ,
        drop = FALSE
      ]
    }

    membership_types <- unique(
      as.character(
        rows$membership_type
      )
    )

    membership_types <-
      membership_types[
        !is.na(membership_types) &
          nzchar(trimws(membership_types))
      ]

    if (length(membership_types) == 0L) {
      return(empty_result)
    }

    results <- lapply(
      membership_types,
      function(type_name) {

        data.frame(
          facility = type_name,
          value =
            venues_operations_membership_active_value(
              membership,
              group = group,
              membership_type =
                type_name
            ),
          stringsAsFactors = FALSE
        )
      }
    )
  }

  if (length(results) == 0L) {
    return(empty_result)
  }

  result <- do.call(
    rbind,
    results
  )

  result <- result[
    is.finite(result$value),
    ,
    drop = FALSE
  ]

  if (nrow(result) == 0L) {
    return(empty_result)
  }

  result <- result[
    order(
      result$value,
      decreasing = TRUE
    ),
    ,
    drop = FALSE
  ]

  rownames(result) <- NULL

  result
}

# -------------------------------------------------------------------------
# Attendance calculations
# -------------------------------------------------------------------------

venues_operations_monthly_attendance <- function(attendance) {

  empty_result <- data.frame(
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    value = double(),
    stringsAsFactors = FALSE
  )

  if (
    is.null(attendance) ||
      !is.data.frame(attendance) ||
      nrow(attendance) == 0L
  ) {
    return(empty_result)
  }

  required_columns <- c(
    "date",
    "month",
    "month_label",
    "period_type",
    "row_type",
    "value"
  )

  if (!all(required_columns %in% names(attendance))) {
    return(empty_result)
  }

  rows <- attendance[
    attendance$period_type == "Month" &
      attendance$row_type == "Overall Total" &
      !is.na(attendance$date),
    ,
    drop = FALSE
  ]

  if (nrow(rows) == 0L) {
    return(empty_result)
  }

  rows$value <- suppressWarnings(
    as.numeric(rows$value)
  )

  rows <- rows[
    is.finite(rows$value),
    ,
    drop = FALSE
  ]

  if (nrow(rows) == 0L) {
    return(empty_result)
  }

  rows <- rows[
    order(rows$date),
    ,
    drop = FALSE
  ]

  data.frame(
    date = as.Date(rows$date),
    month = as.integer(rows$month),
    month_label = as.character(rows$month_label),
    value = rows$value,
    stringsAsFactors = FALSE
  )
}


venues_operations_attendance_kpis <- function(attendance) {

  monthly <- venues_operations_monthly_attendance(
    attendance
  )

  ytd_value <- NA_real_

  if (
    is.data.frame(attendance) &&
      nrow(attendance) > 0L &&
      all(
        c(
          "period_type",
          "row_type",
          "value"
        ) %in% names(attendance)
      )
  ) {

    ytd_rows <- attendance[
      attendance$period_type == "YTD" &
        attendance$row_type == "Overall Total",
      ,
      drop = FALSE
    ]

    ytd_values <- suppressWarnings(
      as.numeric(ytd_rows$value)
    )

    ytd_values <- ytd_values[
      is.finite(ytd_values)
    ]

    if (length(ytd_values) == 1L) {
      ytd_value <- ytd_values[[1L]]
    }
  }

  total_attendance <- if (
    is.finite(ytd_value)
  ) {
    ytd_value
  } else if (nrow(monthly) > 0L) {
    sum(monthly$value)
  } else {
    NA_real_
  }

  average_monthly <- if (
    nrow(monthly) > 0L
  ) {
    mean(monthly$value)
  } else {
    NA_real_
  }

  if (nrow(monthly) > 0L) {

    peak_index <- which.max(
      monthly$value
    )

    peak_value <- monthly$value[
      peak_index
    ]

    peak_month <- monthly$month_label[
      peak_index
    ]

  } else {

    peak_value <- NA_real_
    peak_month <- NA_character_
  }

  list(
    total_attendance = total_attendance,
    average_monthly = average_monthly,
    peak_value = peak_value,
    peak_month = peak_month,
    reporting_months = nrow(monthly)
  )
}

# -------------------------------------------------------------------------
# Attendance by activity
# -------------------------------------------------------------------------

venues_operations_attendance_section_totals <- function(attendance) {

  empty_result <- data.frame(
    activity = character(),
    area = character(),
    value = double(),
    stringsAsFactors = FALSE
  )

  if (
    is.null(attendance) ||
      !is.data.frame(attendance) ||
      nrow(attendance) == 0L
  ) {
    return(empty_result)
  }

  required_columns <- c(
    "period_type",
    "row_type",
    "activity",
    "area",
    "value"
  )

  if (!all(required_columns %in% names(attendance))) {
    return(empty_result)
  }

  valid_activity <-
    !is.na(attendance$activity) &
    nzchar(trimws(attendance$activity)) &
    tolower(trimws(attendance$activity)) != "overall"

  select_rows <- function(period_type) {

    rows <- attendance[
      attendance$period_type == period_type &
        attendance$row_type == "Section Total" &
        valid_activity,
      ,
      drop = FALSE
    ]

    rows$value <- suppressWarnings(
      as.numeric(rows$value)
    )

    rows[
      is.finite(rows$value),
      ,
      drop = FALSE
    ]
  }

  # Prefer reported YTD totals for each individual activity.
  rows <- select_rows("YTD")

  # If no YTD section totals exist, sum the monthly section totals.
  if (nrow(rows) == 0L) {
    rows <- select_rows("Month")
  }

  if (nrow(rows) == 0L) {
    return(empty_result)
  }

  totals <- aggregate(
    value ~ activity + area,
    data = rows,
    FUN = sum
  )

  totals <- totals[
    order(
      totals$value,
      decreasing = TRUE
    ),
    ,
    drop = FALSE
  ]

  rownames(totals) <- NULL

  totals
}

venues_operations_attendance_detail_totals <- function(
    attendance,
    activity = NULL
) {

  empty_result <- data.frame(
    category = character(),
    value = double(),
    stringsAsFactors = FALSE
  )

  if (
    is.null(attendance) ||
      !is.data.frame(attendance) ||
      nrow(attendance) == 0L ||
      is.null(activity) ||
      length(activity) == 0L
  ) {
    return(empty_result)
  }

  required_columns <- c(
    "period_type",
    "row_type",
    "activity",
    "category",
    "value"
  )

  if (!all(required_columns %in% names(attendance))) {
    return(empty_result)
  }

  selected_activity <- trimws(
    as.character(activity[[1L]])
  )

  if (
    is.na(selected_activity) ||
      !nzchar(selected_activity)
  ) {
    return(empty_result)
  }

  select_rows <- function(period_type) {

    rows <- attendance[
      attendance$period_type == period_type &
        attendance$row_type == "Detail" &
        !is.na(attendance$activity) &
        trimws(as.character(attendance$activity)) ==
          selected_activity &
        !is.na(attendance$category) &
        nzchar(trimws(as.character(attendance$category))) &
        tolower(trimws(as.character(attendance$category))) !=
          "total",
      ,
      drop = FALSE
    ]

    rows$value <- suppressWarnings(
      as.numeric(rows$value)
    )

    rows[
      is.finite(rows$value),
      ,
      drop = FALSE
    ]
  }

  # Prefer reported YTD detail values.
  rows <- select_rows("YTD")

  # Fall back to monthly detail values if YTD rows are unavailable.
  if (nrow(rows) == 0L) {
    rows <- select_rows("Month")
  }

  if (nrow(rows) == 0L) {
    return(empty_result)
  }

  totals <- aggregate(
    value ~ category,
    data = rows,
    FUN = sum
  )

  totals <- totals[
    order(
      totals$value,
      decreasing = TRUE
    ),
    ,
    drop = FALSE
  ]

  rownames(totals) <- NULL

  totals
}

venues_operations_format_count <- function(value) {

  if (
    length(value) == 0L ||
      !is.finite(value[[1L]])
  ) {
    return("—")
  }

  format(
    round(value[[1L]]),
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}

venues_operations_format_currency <- function(
    value,
    digits = 0L
) {

  if (
    length(value) == 0L ||
      !is.finite(value[[1L]])
  ) {
    return("—")
  }

  paste0(
    "$",
    format(
      round(
        value[[1L]],
        digits = digits
      ),
      big.mark = ",",
      nsmall = digits,
      scientific = FALSE,
      trim = TRUE
    )
  )
}

# -------------------------------------------------------------------------
# Attendance by area
# -------------------------------------------------------------------------

venues_operations_attendance_area_totals <- function(attendance) {

  empty_result <- data.frame(
    area = character(),
    value = double(),
    stringsAsFactors = FALSE
  )

  if (
    is.null(attendance) ||
      !is.data.frame(attendance) ||
      nrow(attendance) == 0L
  ) {
    return(empty_result)
  }

  required_columns <- c(
    "period_type",
    "row_type",
    "area",
    "value"
  )

  if (!all(required_columns %in% names(attendance))) {
    return(empty_result)
  }

  valid_area <-
    !is.na(attendance$area) &
    nzchar(trimws(attendance$area)) &
    attendance$area != "All"

  select_rows <- function(period_type, row_type) {

    rows <- attendance[
      attendance$period_type == period_type &
        attendance$row_type == row_type &
        valid_area,
      ,
      drop = FALSE
    ]

    rows$value <- suppressWarnings(
      as.numeric(rows$value)
    )

    rows[
      is.finite(rows$value),
      ,
      drop = FALSE
    ]
  }

  # Prefer reported annual/YTD activity totals.
  rows <- select_rows(
    "YTD",
    "Summary Total"
  )

  if (nrow(rows) == 0L) {
    rows <- select_rows(
      "YTD",
      "Section Total"
    )
  }

  if (nrow(rows) == 0L) {
    rows <- select_rows(
      "Month",
      "Summary Total"
    )
  }

  if (nrow(rows) == 0L) {
    rows <- select_rows(
      "Month",
      "Section Total"
    )
  }

  if (nrow(rows) == 0L) {
    return(empty_result)
  }

  totals <- aggregate(
    value ~ area,
    data = rows,
    FUN = sum
  )

  totals <- totals[
    order(
      totals$value,
      decreasing = TRUE
    ),
    ,
    drop = FALSE
  ]

  rownames(totals) <- NULL

  totals
}

# UI ----------------------------------------------------------------------
# Detailed Analysis -> Venues Operations.
# This page is separated from Site Detail because Tracking Reports contain
# much more operational and category-specific information.

mod_venues_operations_ui <- function(id) {

  ns <- shiny::NS(id)

  bslib::nav_panel(
    "Venues Operations",

    div(
      class = "p-4",

      div(
        class = "page-heading",
        h2("Venues Operations"),
        p(
          "Explore detailed operational patterns from the Venues Tracking Reports."
        )
      ),

      # Choose the Tracking Report year and the operational topic to inspect.
      div(
        class = "filter-bar",

        div(
          class = "filter-section-title",
          "Operational detail"
        ),

        layout_columns(

          selectInput(
            ns("year"),
            "Year",
            choices = character()
          ),

          selectInput(
            ns("area"),
            "Operational area",
            choices = character()
          ),

          col_widths = c(3, 5)
        ),

        uiOutput(
          ns("availability_note")
        )
      ),

      # Current operational focus and context-specific KPI summary.
      uiOutput(
        ns("focus_heading")
      ),

      uiOutput(
        ns("kpis")
      ),

      br(),

      # Main view: month-by-month trend for the selected operational area.
      card(
        full_screen = TRUE,

        card_header(
          div(
            div(
              class = "chart-header-title",
              uiOutput(
                ns("main_title"),
                inline = TRUE
              )
            ),

            uiOutput(
              ns("main_subtitle")
            )
          )
        ),

        plotOutput(
          ns("attendance_over_time"),
          height = "440px"
        )
      ),

      # Supporting views: one category breakdown plus one secondary analysis.
      layout_columns(

        card(
          full_screen = TRUE,

          card_header(
            div(
              div(
                class = "chart-header-title",
                uiOutput(
                  ns("breakdown_title"),
                  inline = TRUE
                )
              ),

              uiOutput(
                ns("breakdown_subtitle")
              )
            )
          ),

          uiOutput(
            ns("breakdown_controls")
          ),

          plotOutput(
            ns("attendance_by_activity"),
            height = "360px"
          )
        ),

        card(
          full_screen = TRUE,

          card_header(
            div(
              div(
                class = "chart-header-title",
                uiOutput(
                  ns("secondary_title"),
                  inline = TRUE
                )
              ),

              uiOutput(
                ns("secondary_subtitle")
              )
            )
          ),

          plotOutput(
            ns("attendance_by_area"),
            height = "360px"
          )
        ),

        col_widths = c(7, 5)
      )
    )
  )
}


# Server ------------------------------------------------------------------
# Changes available operational areas by year and prepares the labels/KPIs
# for the selected Venues Tracking Report topic.

mod_venues_operations_server <- function(
  id,
  data = NULL
) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      # -------------------------------------------------------------------
      # Venues Tracking Report data
      # -------------------------------------------------------------------

      venues_reports <- reactive({

        if (is.null(data)) {
          return(NULL)
        }

        dashboard_data <- if (
          shiny::is.reactive(data)
        ) {
          data()
        } else {
          data
        }

        if (is.null(dashboard_data)) {
          return(NULL)
        }

        if (
          inherits(
            dashboard_data,
            "venues_tracking_report_data"
          )
        ) {
          return(dashboard_data)
        }

        if (!is.list(dashboard_data)) {
          return(NULL)
        }

        if (
          inherits(
            dashboard_data$venues_reports,
            "venues_tracking_report_data"
          )
        ) {
          return(
            dashboard_data$venues_reports
          )
        }

        if (
          inherits(
            dashboard_data$venues,
            "venues_tracking_report_data"
          )
        ) {
          return(
            dashboard_data$venues
          )
        }

        NULL
      })


      selected_attendance <- reactive({

        reports <- venues_reports()

        if (
          is.null(reports) ||
            !is.list(reports)
        ) {
          return(NULL)
        }

        venues_operations_filter_year(
          reports$attendance,
          year = input$year
        )
      })

      selected_facility_hire <- reactive({

        reports <- venues_reports()

        if (
          is.null(reports) ||
            !is.list(reports)
        ) {
          return(NULL)
        }

        venues_operations_filter_year(
          reports$facility_hire,
          year = input$year
        )
      })

      selected_online_sales <- reactive({

        reports <- venues_reports()

        if (
          is.null(reports) ||
            !is.list(reports)
        ) {
          return(NULL)
        }

        venues_operations_filter_year(
          reports$online_sales,
          year = input$year
        )
      })


      online_sales_site_selection <- reactive({

        value <- input$online_sales_site

        if (
          is.null(value) ||
            length(value) == 0L
        ) {
          return("All Sites")
        }

        as.character(
          value[[1L]]
        )
      })


      online_sales_category_selection <- reactive({

        value <- input$online_sales_category

        if (
          is.null(value) ||
            length(value) == 0L
        ) {
          return("All Categories")
        }

        as.character(
          value[[1L]]
        )
      })


      online_sales_scope_label <- reactive({

        venues_operations_online_sales_scope_label(
          site =
            online_sales_site_selection(),
          category =
            online_sales_category_selection()
        )
      })


      online_sales_monthly <- reactive({

        venues_operations_online_sales_monthly(
          selected_online_sales(),
          site =
            online_sales_site_selection(),
          category =
            online_sales_category_selection()
        )
      })


      online_sales_kpis <- reactive({

        venues_operations_online_sales_kpis(
          selected_online_sales(),
          site =
            online_sales_site_selection(),
          category =
            online_sales_category_selection()
        )
      })


      online_sales_by_site <- reactive({

        venues_operations_online_sales_by_site(
          selected_online_sales(),
          category =
            online_sales_category_selection()
        )
      })


      online_sales_by_category <- reactive({

        venues_operations_online_sales_by_category(
          selected_online_sales(),
          site =
            online_sales_site_selection()
        )
      })

      selected_merchandise <- reactive({

        reports <- venues_reports()

        if (
          is.null(reports) ||
            !is.list(reports)
        ) {
          return(NULL)
        }

        venues_operations_filter_year(
          reports$merchandise,
          year = input$year
        )
      })


      merchandise_section_selection <- reactive({

        value <- input$merchandise_section

        if (
          is.null(value) ||
            length(value) == 0L
        ) {
          return("All Sections")
        }

        as.character(
          value[[1L]]
        )
      })


      merchandise_item_selection <- reactive({

        value <- input$merchandise_item

        if (
          is.null(value) ||
            length(value) == 0L
        ) {
          return("All Items")
        }

        as.character(
          value[[1L]]
        )
      })


      merchandise_scope_label <- reactive({

        venues_operations_merchandise_scope_label(
          section =
            merchandise_section_selection(),
          item =
            merchandise_item_selection()
        )
      })


      merchandise_monthly_revenue <- reactive({

        venues_operations_merchandise_monthly(
          selected_merchandise(),
          metric = "Revenue",
          section =
            merchandise_section_selection(),
          item =
            merchandise_item_selection()
        )
      })


      merchandise_kpis <- reactive({

        venues_operations_merchandise_kpis(
          selected_merchandise(),
          section =
            merchandise_section_selection(),
          item =
            merchandise_item_selection()
        )
      })


      merchandise_revenue_breakdown <- reactive({

        venues_operations_merchandise_breakdown(
          selected_merchandise(),
          metric = "Revenue",
          section =
            merchandise_section_selection()
        )
      })


      merchandise_transactions_breakdown <- reactive({

        venues_operations_merchandise_breakdown(
          selected_merchandise(),
          metric = "Transactions",
          section =
            merchandise_section_selection()
        )
      })

      selected_casual_hours <- reactive({

        reports <- venues_reports()

        if (
          is.null(reports) ||
            !is.list(reports)
        ) {
          return(NULL)
        }

        venues_operations_filter_year(
          reports$casual_hours,
          year = input$year
        )
      })


      selected_casual_wages <- reactive({

        reports <- venues_reports()

        if (
          is.null(reports) ||
            !is.list(reports)
        ) {
          return(NULL)
        }

        venues_operations_filter_year(
          reports$casual_wages,
          year = input$year
        )
      })


      workforce_hours_monthly <- reactive({

        venues_operations_workforce_hours_monthly(
          selected_casual_hours()
        )
      })


      workforce_wages_monthly <- reactive({

        venues_operations_workforce_wages_monthly(
          selected_casual_wages()
        )
      })


      workforce_kpis <- reactive({

        venues_operations_workforce_kpis(
          selected_casual_hours(),
          selected_casual_wages()
        )
      })

      selected_finance <- reactive({

        reports <- venues_reports()

        if (
          is.null(reports) ||
            !is.list(reports)
        ) {
          return(NULL)
        }

        venues_operations_filter_year(
          reports$finance,
          year = input$year
        )
      })


      finance_fund_selection <- reactive({

        value <- input$finance_fund

        if (
          is.null(value) ||
            length(value) == 0L
        ) {
          return("All Funds")
        }

        as.character(
          value[[1L]]
        )
      })


      finance_scope_label <- reactive({

        fund <- finance_fund_selection()

        if (identical(
          fund,
          "All Funds"
        )) {
          return("All Funds")
        }

        fund
      })


      finance_monthly <- reactive({

        venues_operations_finance_monthly(
          selected_finance(),
          fund =
            finance_fund_selection()
        )
      })


      finance_kpis <- reactive({

        venues_operations_finance_kpis(
          selected_finance(),
          fund =
            finance_fund_selection()
        )
      })


      finance_by_fund <- reactive({

        venues_operations_finance_by_fund(
          selected_finance()
        )
      })

      selected_membership <- reactive({

        reports <- venues_reports()

        if (
          is.null(reports) ||
            !is.list(reports)
        ) {
          return(NULL)
        }

        venues_operations_filter_year(
          reports$membership,
          year = input$year
        )
      })


      membership_group_selection <- reactive({

        value <- input$membership_group

        if (
          is.null(value) ||
            length(value) == 0L
        ) {
          return("All Memberships")
        }

        as.character(
          value[[1L]]
        )
      })


      membership_type_selection <- reactive({

        value <- input$membership_type

        if (
          is.null(value) ||
            length(value) == 0L
        ) {
          return("All Membership Types")
        }

        as.character(
          value[[1L]]
        )
      })


      membership_scope_label <- reactive({

        venues_operations_membership_scope_label(
          membership_group_selection(),
          membership_type_selection()
        )
      })


      membership_kpis <- reactive({

        venues_operations_membership_kpis(
          selected_membership(),
          group =
            membership_group_selection(),
          membership_type =
            membership_type_selection()
        )
      })


      membership_active_trend <- reactive({

        venues_operations_membership_active_trend(
          selected_membership(),
          group =
            membership_group_selection(),
          membership_type =
            membership_type_selection()
        )
      })


      membership_movement <- reactive({

        venues_operations_membership_movement(
          selected_membership(),
          group =
            membership_group_selection(),
          membership_type =
            membership_type_selection()
        )
      })


      membership_breakdown <- reactive({

        venues_operations_membership_breakdown(
          selected_membership(),
          group =
            membership_group_selection(),
          membership_type =
            membership_type_selection()
        )
      })

      facility_hire_kpis <- reactive({

        venues_operations_facility_hire_kpis(
          selected_facility_hire()
        )
      })

      monthly_facility_hire_revenue <- reactive({

        venues_operations_monthly_facility_hire(
          selected_facility_hire(),
          metric = "Revenue"
        )
      })


      facility_hire_revenue_by_facility <- reactive({

        venues_operations_facility_hire_by_facility(
          selected_facility_hire(),
          metric = "Revenue"
        )
      })


      facility_hire_transactions_by_facility <- reactive({

        venues_operations_facility_hire_by_facility(
          selected_facility_hire(),
          metric = "Transactions"
        )
      })

      facility_hire_revenue_breakdown <- reactive({

        selected_facility <-
          input$facility_hire_facility

        if (
          !is.null(selected_facility) &&
            length(selected_facility) > 0L &&
            !identical(
              selected_facility,
              "All Facilities"
            )
        ) {
          return(
            venues_operations_facility_hire_detail(
              selected_facility_hire(),
              facility = selected_facility,
              metric = "Revenue"
            )
          )
        }

        facility_data <-
          facility_hire_revenue_by_facility()

        selected_group <-
          input$facility_hire_group

        if (
          is.null(selected_group) ||
            length(selected_group) == 0L ||
            identical(
              selected_group,
              "All Facilities"
            )
        ) {
          return(facility_data)
        }

        groups <-
          venues_operations_facility_hire_group(
            facility_data$facility
          )

        facility_data[
          !is.na(groups) &
            groups == selected_group,
          ,
          drop = FALSE
        ]
      })


      facility_hire_transactions_breakdown <- reactive({

        selected_facility <-
          input$facility_hire_facility

        if (
          !is.null(selected_facility) &&
            length(selected_facility) > 0L &&
            !identical(
              selected_facility,
              "All Facilities"
            )
        ) {
          return(
            venues_operations_facility_hire_detail(
              selected_facility_hire(),
              facility = selected_facility,
              metric = "Transactions"
            )
          )
        }

        facility_data <-
          facility_hire_transactions_by_facility()

        selected_group <-
          input$facility_hire_group

        if (
          is.null(selected_group) ||
            length(selected_group) == 0L ||
            identical(
              selected_group,
              "All Facilities"
            )
        ) {
          return(facility_data)
        }

        groups <-
          venues_operations_facility_hire_group(
            facility_data$facility
          )

        facility_data[
          !is.na(groups) &
            groups == selected_group,
          ,
          drop = FALSE
        ]
      })

      attendance_kpis <- reactive({

        venues_operations_attendance_kpis(
          selected_attendance()
        )
      })

      monthly_attendance <- reactive({

        venues_operations_monthly_attendance(
          selected_attendance()
        )
      })

      attendance_activity <- reactive({

        venues_operations_attendance_section_totals(
          selected_attendance()
        )
      })

      attendance_activity_filtered <- reactive({

        activity_data <- attendance_activity()

        if (
          !is.data.frame(activity_data) ||
            nrow(activity_data) == 0L
        ) {
          return(activity_data)
        }

        selected_area <- input$attendance_breakdown_area

        if (
          is.null(selected_area) ||
            length(selected_area) == 0L ||
            identical(selected_area, "All Areas")
        ) {
          return(activity_data)
        }

        activity_data[
          activity_data$area == selected_area,
          ,
          drop = FALSE
        ]
      })

      observeEvent(
        attendance_activity_filtered(),
        {

          activity_data <- attendance_activity_filtered()

          activities <- if (
            is.data.frame(activity_data) &&
              nrow(activity_data) > 0L
          ) {
            as.character(activity_data$activity)
          } else {
            character()
          }

          activities <- activities[
            !is.na(activities) &
              nzchar(trimws(activities))
          ]

          choices <- c(
            "All Activities",
            activities
          )

          current_selection <-
            input$attendance_breakdown_activity

          selected <- if (
            !is.null(current_selection) &&
              current_selection %in% choices
          ) {
            current_selection
          } else {
            "All Activities"
          }

          updateSelectInput(
            session,
            "attendance_breakdown_activity",
            choices = choices,
            selected = selected
          )
        },
        ignoreInit = FALSE
      )

      attendance_breakdown_data <- reactive({

        selected_activity <-
          input$attendance_breakdown_activity

        if (
          is.null(selected_activity) ||
            length(selected_activity) == 0L ||
            identical(
              selected_activity,
              "All Activities"
            )
        ) {
          return(
            attendance_activity_filtered()
          )
        }

        detail_data <-
          venues_operations_attendance_detail_totals(
            selected_attendance(),
            activity = selected_activity
          )

        if (
          !is.data.frame(detail_data) ||
            nrow(detail_data) == 0L
        ) {
          return(detail_data)
        }

        data.frame(
          activity = detail_data$category,
          value = detail_data$value,
          stringsAsFactors = FALSE
        )
      })

      attendance_area <- reactive({

        venues_operations_attendance_area_totals(
          selected_attendance()
        )
      })

      # -------------------------------------------------------------------
      # Available years
      # -------------------------------------------------------------------

      observeEvent(
        venues_reports(),
        {

          years <- venues_operations_available_years(
            venues_reports()
          )

          selected_year <- if (
            length(years) > 0L
          ) {
            as.character(
              max(years)
            )
          } else {
            character()
          }

          updateSelectInput(
            session,
            "year",
            choices = years,
            selected = selected_year
          )
        },
        ignoreInit = FALSE
      )

      # -------------------------------------------------------------------
      # Available operational areas
      # -------------------------------------------------------------------

      observeEvent(
        list(
          venues_reports(),
          input$year
        ),
        {
          attendance <- selected_attendance()
          facility_hire <- selected_facility_hire()
          membership <- selected_membership()
          finance <- selected_finance()
          online_sales <- selected_online_sales()
          merchandise <- selected_merchandise()
          casual_hours <- selected_casual_hours()
          casual_wages <- selected_casual_wages()

            areas <- character()

            if (
              is.data.frame(attendance) &&
                nrow(attendance) > 0L
            ) {
              areas <- c(
                areas,
                "Attendance"
              )
            }

            if (
              is.data.frame(facility_hire) &&
                nrow(facility_hire) > 0L
            ) {
              areas <- c(
                areas,
                "Facility Hire"
              )
            }
          
          if (
            is.data.frame(membership) &&
              nrow(membership) > 0L
          ) {
            areas <- c(
              areas,
              "Membership"
            )
          }

          if (
            is.data.frame(finance) &&
              nrow(finance) > 0L
          ) {
            areas <- c(
              areas,
              "Venue Finances"
            )
          }

          if (
            is.data.frame(online_sales) &&
              nrow(online_sales) > 0L
          ) {
            areas <- c(
              areas,
              "Online Sales"
            )
          }

          if (
            is.data.frame(merchandise) &&
              nrow(merchandise) > 0L
          ) {
            areas <- c(
              areas,
              "Merchandise"
            )
          }

          if (
            (
              is.data.frame(casual_hours) &&
                nrow(casual_hours) > 0L
            ) ||
              (
                is.data.frame(casual_wages) &&
                  nrow(casual_wages) > 0L
              )
          ) {
            areas <- c(
              areas,
              "Workforce"
            )
          } 
          
          updateSelectInput(
            session,
            "area",
            choices = areas,
            selected = if (
              length(areas) > 0L
            ) {
              areas[[1L]]
            } else {
              character()
            }
          )
        },
        ignoreInit = FALSE
      )

      output$availability_note <- renderUI({

        reports <- venues_reports()

        if (is.null(reports)) {
          return(
            tags$small(
              class = "text-secondary",
              "Upload Venues Tracking Reports on the Data page to activate this analysis."
            )
          )
        }

        available <- character()

        if (
          is.data.frame(selected_attendance()) &&
            nrow(selected_attendance()) > 0L
        ) {
          available <- c(
            available,
            "Attendance"
          )
        }

        if (
          is.data.frame(selected_facility_hire()) &&
            nrow(selected_facility_hire()) > 0L
        ) {
          available <- c(
            available,
            "Facility Hire"
          )
        }

        if (
          is.data.frame(selected_membership()) &&
            nrow(selected_membership()) > 0L
        ) {
          available <- c(
            available,
            "Membership"
          )
        }

        if (
          is.data.frame(selected_finance()) &&
            nrow(selected_finance()) > 0L
        ) {
          available <- c(
            available,
            "Venue Finances"
          )
        }

        if (
          is.data.frame(
            selected_online_sales()
          ) &&
            nrow(
              selected_online_sales()
            ) > 0L
        ) {
          available <- c(
            available,
            "Online Sales"
          )
        }

            if (
      is.data.frame(
        selected_merchandise()
      ) &&
        nrow(
          selected_merchandise()
        ) > 0L
    ) {
      available <- c(
        available,
        "Merchandise"
      )
    }

    if (
      (
        is.data.frame(
          selected_casual_hours()
        ) &&
          nrow(
            selected_casual_hours()
          ) > 0L
      ) ||
        (
          is.data.frame(
            selected_casual_wages()
          ) &&
            nrow(
              selected_casual_wages()
            ) > 0L
        )
    ) {
      available <- c(
        available,
        "Workforce"
      )
    }

        tags$small(
          class = "text-secondary",
          paste0(
            "Available from uploaded reports: ",
            paste(
              available,
              collapse = ", "
            )
          )
        )
      })

      output$focus_heading <- renderUI({

        req(
          input$year,
          input$area
        )

        div(
          class = "snapshot-heading",

          h4(
            paste0(
              input$year,
              " · ",
              input$area
            )
          ),

          p(
            "Operational detail from the selected Venues Tracking Report."
          )
          
        )
      })

      # KPI definitions differ by operational area because each topic has
      # different business measures.
      kpi_specs <- reactive({

        req(input$area)

        if (identical(input$area, "Facility Hire")) {

          values <- facility_hire_kpis()

          return(
            list(
              list(
                title = "Total Revenue",
                value = venues_operations_format_currency(
                  values$revenue
                ),
                subtitle = "Selected year",
                icon = "currency-dollar"
              ),
              list(
                title = "Total Transactions",
                value = venues_operations_format_count(
                  values$transactions
                ),
                subtitle = "Facility hire transactions",
                icon = "receipt"
              ),
              list(
                title = "Revenue / Transaction",
                value = venues_operations_format_currency(
                  values$revenue_per_transaction,
                  digits = 2L
                ),
                subtitle = "Average transaction value",
                icon = "calculator"
              )
            )
          )
        }

        if (identical(input$area, "Venue Finances")) {

          values <- finance_kpis()

          scope <- finance_scope_label()

          return(
            list(
              list(
                title = "Actual Income",
                value =
                  venues_operations_format_currency(
                    values$income
                  ),
                subtitle =
                  paste0(
                    "YTD · ",
                    scope
                  ),
                icon = "cash-stack"
              ),
              list(
                title = "Actual Expenses",
                value =
                  venues_operations_format_currency(
                    values$expenses
                  ),
                subtitle =
                  paste0(
                    "YTD · ",
                    scope
                  ),
                icon = "receipt"
              ),
              list(
                title = "Net Result",
                value =
                  venues_operations_format_currency(
                    values$net_result
                  ),
                subtitle =
                  paste0(
                    "YTD · ",
                    scope
                  ),
                icon = "graph-up-arrow"
              )
            )
          )
        }

        if (identical(input$area, "Online Sales")) {

          values <- online_sales_kpis()

          peak_month <- if (
            is.character(
              values$peak_month
            ) &&
              length(
                values$peak_month
              ) > 0L &&
              !is.na(
                values$peak_month[[1L]]
              )
          ) {
            values$peak_month[[1L]]
          } else {
            "No reported month"
          }

          return(
            list(
              list(
                title =
                  "Total Online Revenue",
                value =
                  venues_operations_format_currency(
                    values$total_revenue
                  ),
                subtitle =
                  online_sales_scope_label(),
                icon = "currency-dollar"
              ),
              list(
                title =
                  "Average / Month",
                value =
                  venues_operations_format_currency(
                    values$average_monthly
                  ),
                subtitle =
                  paste0(
                    values$reporting_months,
                    " reporting months"
                  ),
                icon = "calendar3"
              ),
              list(
                title =
                  "Peak Month",
                value =
                  venues_operations_format_currency(
                    values$peak_value
                  ),
                subtitle =
                  peak_month,
                icon = "bar-chart-line"
              )
            )
          )
        }

        if (identical(
          input$area,
          "Workforce"
        )) {

          values <- workforce_kpis()

          return(
            list(
              list(
                title = "Paid Hours",
                value =
                  venues_operations_format_count(
                    values$paid_hours
                  ),
                subtitle =
                  "Reported casual hours",
                icon = "clock"
              ),
              list(
                title = "Actual Wages",
                value =
                  venues_operations_format_currency(
                    values$actual_wages
                  ),
                subtitle =
                  "Reported casual wages",
                icon = "cash-stack"
              ),
              list(
                title = "Wage Variance",
                value =
                  venues_operations_format_currency(
                    values$wage_variance
                  ),
                subtitle =
                  "Budget vs Actual",
                icon = "graph-up-arrow"
              )
            )
          )
        }

        if (identical(input$area, "Merchandise")) {

          values <- merchandise_kpis()

          scope <- merchandise_scope_label()

          return(
            list(
              list(
                title = "Total Revenue",
                value =
                  venues_operations_format_currency(
                    values$revenue
                  ),
                subtitle = scope,
                icon = "currency-dollar"
              ),
              list(
                title = "Total Transactions",
                value =
                  venues_operations_format_count(
                    values$transactions
                  ),
                subtitle = scope,
                icon = "receipt"
              ),
              list(
                title = "Revenue / Transaction",
                value =
                  venues_operations_format_currency(
                    values$revenue_per_transaction,
                    digits = 2L
                  ),
                subtitle = scope,
                icon = "calculator"
              )
            )
          )
        }

        if (identical(input$area, "Membership")) {

          values <- membership_kpis()

          scope <- membership_scope_label()

          return(
            list(
              list(
                title = "New Memberships",
                value =
                  venues_operations_format_count(
                    values$new
                  ),
                subtitle = scope,
                icon = "person-plus"
              ),
              list(
                title = "Expired / Cancelled",
                value =
                  venues_operations_format_count(
                    values$expired_cancelled
                  ),
                subtitle = scope,
                icon = "person-dash"
              ),
              list(
                title = "Active Members",
                value =
                  venues_operations_format_count(
                    values$active
                  ),
                subtitle = "Latest monthly snapshot",
                icon = "people"
              )
            )
          )
        }

        if (!identical(input$area, "Attendance")) {
          return(NULL)
        }

        values <- attendance_kpis()

        peak_month <- if (
          is.character(values$peak_month) &&
            length(values$peak_month) > 0L &&
            !is.na(values$peak_month[[1L]])
        ) {
          values$peak_month[[1L]]
        } else {
          "No reported month"
        }

        list(
          list(
            title = "Total Attendance",
            value = venues_operations_format_count(
              values$total_attendance
            ),
            subtitle = "Selected year",
            icon = "people"
          ),
          list(
            title = "Average / Month",
            value = venues_operations_format_count(
              values$average_monthly
            ),
            subtitle = paste0(
              values$reporting_months,
              " reporting months"
            ),
            icon = "calendar3"
          ),
          list(
            title = "Peak Month",
            value = venues_operations_format_count(
              values$peak_value
            ),
            subtitle = peak_month,
            icon = "bar-chart-line"
          )
        )
      })

      output$kpis <- renderUI({
        make_value_box_row(
          kpi_specs(),
          width = 1 / 3
        )
      })

      output$main_title <- renderUI({

        req(input$area)

        if (identical(input$area, "Attendance")) {
          return("Attendance Over Time")
        }

        if (identical(input$area, "Facility Hire")) {
          return("Facility Hire Revenue Over Time")
        }

        if (identical(input$area, "Membership")) {
          return("Active Memberships Over Time")
        }

        if (identical(input$area, "Venue Finances")) {
          return("Monthly Income vs Expenses")}
        
        if (identical( input$area, "Online Sales")) {
          return("Online Sales Revenue Over Time")}
        
        if (identical(input$area,"Merchandise")) {
          return("Merchandise Revenue Over Time")}
        
        if (identical(input$area, "Workforce")) {
          return("Budgeted vs Paid Hours Over Time")}

        "Operational Trend"
      })

      output$main_subtitle <- renderUI({

        req(
          input$year,
          input$area
        )

        if (identical(input$area, "Facility Hire")) {
          return(
            chart_subtitle(
              paste0(
                "Monthly revenue · January–December ",
                input$year
              )
            )
          )
        }

        if (identical(input$area, "Membership")) {

          return(
            chart_subtitle(
              paste0(
                membership_scope_label(),
                " · January–December ",
                input$year
              )
            )
          )
        }

        if (identical(input$area, "Venue Finances")) {

          return(
            chart_subtitle(
              paste0(
                "Actual Income vs Actual Expenses · ",
                finance_scope_label(),
                " · ",
                input$year
              )
            )
          )
        }

        if (identical(input$area, "Online Sales")) {

          return(
            chart_subtitle(
              paste0(
                online_sales_scope_label(),
                " · January–December ",
                input$year
              )
            )
          )
        }

        if (identical(input$area, "Merchandise")) {

          return(
            chart_subtitle(
              paste0(
                merchandise_scope_label(),
                " · January–December ",
                input$year
              )
            )
          )
        }

        if (identical(input$area, "Workforce")) {

          return(
            chart_subtitle(
              paste0(
                "Monthly casual hours · January–December ",
                input$year
              )
            )
          )
        }

        chart_subtitle(
          paste0(
            input$area,
            " · January–December ",
            input$year
          )
        )
      })

      output$attendance_over_time <- renderPlot(
        {

          req(input$area)

          if (identical(input$area, "Attendance")) {

            monthly <- monthly_attendance()

            validate(
              need(
                is.data.frame(monthly) &&
                  nrow(monthly) > 0L &&
                  any(is.finite(monthly$value)),
                "No monthly attendance values are available for this selection."
              )
            )

            return(
              plot_venues_attendance_over_time(
                monthly
              )
            )
          }

          if (identical(input$area, "Facility Hire")) {

            monthly <- monthly_facility_hire_revenue()

            validate(
              need(
                is.data.frame(monthly) &&
                  nrow(monthly) > 0L &&
                  any(is.finite(monthly$value)),
                "No monthly Facility Hire revenue values are available for this selection."
              )
            )

            return(
              plot_venues_facility_hire_revenue_over_time(
                monthly
              )
            )
          }

          if (identical(input$area, "Membership")) {

            membership_data <-
              membership_active_trend()

            validate(
              need(
                is.data.frame(membership_data) &&
                  nrow(membership_data) > 0L &&
                  any(
                    is.finite(
                      membership_data$value
                    )
                  ),
                "No active membership values are available for this selection."
              )
            )

            return(
              plot_venues_attendance_over_time(
                membership_data
              )
            )
          }

          if (identical(input$area, "Venue Finances")) {

            finance_data <- finance_monthly()

            validate(
              need(
                is.data.frame(finance_data) &&
                  nrow(finance_data) > 0L &&
                  any(
                    is.finite(
                      c(
                        finance_data$income_actual,
                        finance_data$expenses_actual
                      )
                    )
                  ),
                "No Venue Finance values are available for this selection."
              )
            )

            return(
              plot_monthly_income_expenses(
                finance_data
              )
            )
          }

          if (identical(input$area, "Online Sales")) {

            monthly <-
              online_sales_monthly()

            validate(
              need(
                is.data.frame(monthly) &&
                  nrow(monthly) > 0L &&
                  any(
                    is.finite(
                      monthly$value
                    )
                  ),
                "No monthly Online Sales values are available for this selection."
              )
            )

            return(
              plot_venues_facility_hire_revenue_over_time(
                monthly
              )
            )
          }

          if (identical(input$area,"Merchandise")) {

            monthly <-
              merchandise_monthly_revenue()

            validate(
              need(
                is.data.frame(monthly) &&
                  nrow(monthly) > 0L &&
                  any(
                    is.finite(
                      monthly$value
                    )
                  ),
                "No monthly Merchandise revenue values are available for this selection."
              )
            )

            return(
              plot_venues_facility_hire_revenue_over_time(
                monthly
              )
            )
          }

          if (identical(input$area, "Workforce")) {

            workforce_data <-
              workforce_hours_monthly()

            validate(
              need(
                is.data.frame(workforce_data) &&
                  nrow(workforce_data) > 0L &&
                  any(
                    is.finite(
                      c(
                        workforce_data$budget_hours,
                        workforce_data$paid_hours
                      )
                    )
                  ),
                "No monthly Casual Hours values are available."
              )
            )

            return(
              plot_workforce_hours_over_time(
                workforce_data
              )
            )
          }

        },
        res = 96,
        bg = "white"
      )

      output$breakdown_controls <- renderUI({

        req(input$area)

        if (identical(input$area, "Attendance")) {

          return(
            layout_columns(

              selectInput(
                session$ns(
                  "attendance_breakdown_area"
                ),
                "Area",
                choices = "All Areas",
                selected = "All Areas",
                width = "100%"
              ),

              selectInput(
                session$ns(
                  "attendance_breakdown_activity"
                ),
                "Activity",
                choices = "All Activities",
                selected = "All Activities",
                width = "100%"
              ),

              col_widths = c(4, 8)
            )
          )
        }

        if (identical(input$area, "Facility Hire")) {

          return(
            layout_columns(

              selectInput(
                session$ns(
                  "facility_hire_group"
                ),
                "Facility Group",
                choices = c(
                  "All Facilities",
                  "Sports Facilities",
                  "Venue & Space Hire"
                ),
                selected = "All Facilities",
                width = "100%"
              ),

              selectInput(
                session$ns(
                  "facility_hire_facility"
                ),
                "Facility",
                choices = "All Facilities",
                selected = "All Facilities",
                width = "100%"
              ),

              col_widths = c(4, 8)
            )
          )
        }

        if (identical(input$area, "Membership")) {

          return(
            layout_columns(

              selectInput(
                session$ns(
                  "membership_group"
                ),
                "Membership Group",
                choices =
                  venues_operations_membership_group_choices(
                    selected_membership()
                  ),
                selected = "All Memberships",
                width = "100%"
              ),

              selectInput(
                session$ns(
                  "membership_type"
                ),
                "Membership Type",
                choices = c(
                  "All Membership Types" =
                    "All Membership Types"
                ),
                selected =
                  "All Membership Types",
                width = "100%"
              ),

              col_widths = c(5, 7)
            )
          )
        }

        if (identical(input$area, "Merchandise")) {

          return(
            layout_columns(

              selectInput(
                session$ns(
                  "merchandise_section"
                ),
                "Section",
                choices =
                  venues_operations_merchandise_section_choices(
                    selected_merchandise()
                  ),
                selected =
                  "All Sections",
                width = "100%"
              ),

              selectInput(
                session$ns(
                  "merchandise_item"
                ),
                "Item",
                choices = c(
                  "All Items" =
                    "All Items"
                ),
                selected =
                  "All Items",
                width = "100%"
              ),

              col_widths = c(5, 7)
            )
          )
        }

        if (identical(input$area, "Online Sales")) {
          return(
            layout_columns(

              selectInput(
                session$ns(
                  "online_sales_site"
                ),
                "Site",
                choices =
                  venues_operations_online_sales_site_choices(
                    selected_online_sales()
                  ),
                selected =
                  "All Sites",
                width = "100%"
              ),

              selectInput(
                session$ns(
                  "online_sales_category"
                ),
                "Category",
                choices =
                  venues_operations_online_sales_category_choices(
                    selected_online_sales()
                  ),
                selected =
                  "All Categories",
                width = "100%"
              ),

              col_widths = c(5, 7)
            )
          )
        }

        if (identical(input$area, "Venue Finances")) {

          return(
            layout_columns(

              selectInput(
                session$ns(
                  "finance_fund"
                ),
                "Fund",
                choices =
                  venues_operations_finance_fund_choices(
                    selected_finance()
                  ),
                selected = "All Funds",
                width = "100%"
              ),

              col_widths = 6
            )
          )
        }

        NULL
      })

      observeEvent(
        list(
          input$area,
          input$facility_hire_group,
          selected_facility_hire()
        ),
        {

          if (!identical(
            input$area,
            "Facility Hire"
          )) {
            return()
          }

          facility_hire <-
            selected_facility_hire()

          if (
            !is.data.frame(facility_hire) ||
              nrow(facility_hire) == 0L
          ) {
            return()
          }

          facilities <- sort(
            unique(
              trimws(
                as.character(
                  facility_hire$facility
                )
              )
            )
          )

          facilities <- facilities[
            !is.na(facilities) &
              nzchar(facilities)
          ]

          selected_group <-
            input$facility_hire_group

          if (
            !is.null(selected_group) &&
              length(selected_group) > 0L &&
              !identical(
                selected_group,
                "All Facilities"
              )
          ) {

            groups <-
              venues_operations_facility_hire_group(
                facilities
              )

            facilities <- facilities[
              !is.na(groups) &
                groups == selected_group
            ]
          }

          choices <- c(
            "All Facilities",
            facilities
          )

          current_selection <-
            isolate(
              input$facility_hire_facility
            )

          selected <- if (
            !is.null(current_selection) &&
              length(current_selection) > 0L &&
              current_selection %in% choices
          ) {
            current_selection
          } else {
            "All Facilities"
          }

          updateSelectInput(
            session,
            "facility_hire_facility",
            choices = choices,
            selected = selected
          )
        },
        ignoreInit = FALSE
      )

      observeEvent(
        list(
          input$area,
          input$membership_group,
          selected_membership()
        ),
        {

          if (!identical(
            input$area,
            "Membership"
          )) {
            return()
          }

          group <- membership_group_selection()

          choices <-
            venues_operations_membership_type_choices(
              selected_membership(),
              group
            )

          current <- isolate(
            input$membership_type
          )

          selected <- if (
            !is.null(current) &&
              length(current) > 0L &&
              current %in% unname(choices)
          ) {
            current
          } else {
            "All Membership Types"
          }

          updateSelectInput(
            session,
            "membership_type",
            choices = choices,
            selected = selected
          )
        },
        ignoreInit = FALSE
      )

      observeEvent(
        list(
          input$area,
          input$merchandise_section,
          selected_merchandise()
        ),
        {

          if (!identical(
            input$area,
            "Merchandise"
          )) {
            return()
          }

          section <-
            merchandise_section_selection()

          choices <-
            venues_operations_merchandise_item_choices(
              selected_merchandise(),
              section = section
            )

          current <- isolate(
            input$merchandise_item
          )

          selected <- if (
            !is.null(current) &&
              length(current) > 0L &&
              current %in%
                unname(choices)
          ) {
            current
          } else {
            "All Items"
          }

          updateSelectInput(
            session,
            "merchandise_item",
            choices = choices,
            selected = selected
          )
        },
        ignoreInit = FALSE
      )

      output$breakdown_title <- renderUI({

        req(input$area)

        if (identical(input$area, "Facility Hire")) {

          selected_facility <-
            input$facility_hire_facility

          if (
            !is.null(selected_facility) &&
              length(selected_facility) > 0L &&
              !identical(
                selected_facility,
                "All Facilities"
              )
          ) {
            return("Revenue Breakdown")
          }

          return("Revenue by Facility")
        }

        if (identical(input$area, "Merchandise")) {

          if (identical(
            merchandise_section_selection(),
            "All Sections"
          )) {
            return(
              "Revenue by Section"
            )
          }

          return(
            "Revenue by Item"
          )
        }

        if (identical(input$area, "Venue Finances")) {
          return("Budget vs Actual Net Result")
        }

        if (identical(input$area, "Online Sales")) {
          return("Revenue by Site")}

        if (identical(input$area, "Membership")) {
          return("Membership Movement")
        }

        if (!identical(input$area, "Attendance")) {
          return("Operational Breakdown")
        }

        if (identical(input$area, "Workforce")) {
          return("Hours Variance by Month")}

        selected_activity <-
          input$attendance_breakdown_activity

        if (
          !is.null(selected_activity) &&
            length(selected_activity) > 0L &&
            !identical(
              selected_activity,
              "All Activities"
            )
        ) {
          return("Attendance Breakdown")
        }

        "Attendance by Activity"
      })
      
      output$secondary_title <- renderUI({

        req(input$area)

        if (identical(input$area, "Facility Hire")) {

          selected_facility <-
            input$facility_hire_facility

          if (
            !is.null(selected_facility) &&
              length(selected_facility) > 0L &&
              !identical(
                selected_facility,
                "All Facilities"
              )
          ) {
            return("Transactions Breakdown")
          }

          return("Transactions by Facility")
        }

        if (identical(input$area, "Merchandise")) {

          if (identical(
            merchandise_section_selection(),
            "All Sections"
          )) {
            return(
              "Transactions by Section"
            )
          }

          return(
            "Transactions by Item"
          )
        }

        if (identical(input$area, "Online Sales")) {
          return("Revenue by Category")}

        if (identical( input$area, "Venue Finances")) {
          return("Net Result by Fund")}

        if (identical(input$area, "Membership")) {

          if (
            !identical(
              membership_type_selection(),
              "All Membership Types"
            )
          ) {
            return("Membership Summary")
          }

          if (
            !identical(
              membership_group_selection(),
              "All Memberships"
            )
          ) {
            return(
              "Active Members by Membership Type"
            )
          }

          return("Active Members by Group")
        }

        if (identical(input$area, "Workforce")) {
          return("Budget vs Actual Casual Wages")}

        if (identical(input$area, "Attendance")) {
          return("Attendance by Area")
        }

        "Secondary Analysis"
      })

      output$breakdown_subtitle <- renderUI({

        req(
          input$year,
          input$area
        )

        if (identical(input$area, "Facility Hire")) {

          selected_group <-
            input$facility_hire_group

          selected_facility <-
            input$facility_hire_facility

          if (
            !is.null(selected_facility) &&
              length(selected_facility) > 0L &&
              !identical(
                selected_facility,
                "All Facilities"
              )
          ) {
            return(
              chart_subtitle(
                paste0(
                  selected_facility,
                  " · Original hire categories · ",
                  input$year
                )
              )
            )
          }

          group_label <- if (
            is.null(selected_group) ||
              length(selected_group) == 0L
          ) {
            "All Facilities"
          } else {
            selected_group
          }

          return(
            chart_subtitle(
              paste0(
                group_label,
                " · Annual revenue · ",
                input$year
              )
            )
          )
        }

        if (identical(input$area, "Merchandise")) {

          return(
            chart_subtitle(
              paste0(
                merchandise_section_selection(),
                " · YTD revenue · ",
                input$year
              )
            )
          )
        }

        if (identical(input$area, "Online Sales")) {

          category <-
            online_sales_category_selection()

          return(
            chart_subtitle(
              paste0(
                category,
                " · YTD revenue · ",
                input$year
              )
            )
          )
        }

        if (identical(input$area, "Venue Finances")) {

          return(
            chart_subtitle(
              paste0(
                finance_scope_label(),
                " · Monthly Net Result · ",
                input$year
              )
            )
          )
        }

        if (identical(input$area, "Membership")) {

          return(
            chart_subtitle(
              paste0(
                "New vs Expired / Cancelled · ",
                membership_scope_label(),
                " · ",
                input$year
              )
            )
          )
        }

        if (identical(input$area, "Workforce")) {

          return(
            chart_subtitle(
              paste0(
                "Budgeted vs Paid hours variance · ",
                input$year
              )
            )
          )
        }

        if (!identical(input$area, "Attendance")) {
          return(
            chart_subtitle(
              paste0(
                "Category-level view · ",
                input$area
              )
            )
          )
        }

        selected_area <-
          input$attendance_breakdown_area

        selected_activity <-
          input$attendance_breakdown_activity

        if (
          !is.null(selected_activity) &&
            length(selected_activity) > 0L &&
            !identical(
              selected_activity,
              "All Activities"
            )
        ) {

          activity_data <- attendance_activity()

          activity_row <- activity_data[
            activity_data$activity == selected_activity,
            ,
            drop = FALSE
          ]

          activity_total <- if (
            nrow(activity_row) > 0L
          ) {
            sum(
              activity_row$value,
              na.rm = TRUE
            )
          } else {
            NA_real_
          }

          activity_area <- if (
            nrow(activity_row) > 0L
          ) {
            as.character(
              activity_row$area[[1L]]
            )
          } else {
            selected_area
          }

          return(
            chart_subtitle(
              paste0(
                selected_activity,
                " · ",
                activity_area,
                " · Total Attendance ",
                venues_operations_format_count(
                  activity_total
                ),
                " · ",
                input$year
              )
            )
          )
        }

        area_label <- if (
          is.null(selected_area) ||
            length(selected_area) == 0L ||
            identical(
              selected_area,
              "All Areas"
            )
        ) {
          "All Areas"
        } else {
          selected_area
        }

        chart_subtitle(
          paste0(
            "Annual attendance by activity · ",
            area_label,
            " · ",
            input$year
          )
        )
      })

      output$secondary_subtitle <- renderUI({

        req(
          input$year,
          input$area
        )

        if (identical(input$area, "Facility Hire")) {

          selected_group <-
            input$facility_hire_group

          selected_facility <-
            input$facility_hire_facility

          if (
            !is.null(selected_facility) &&
              length(selected_facility) > 0L &&
              !identical(
                selected_facility,
                "All Facilities"
              )
          ) {
            return(
              chart_subtitle(
                paste0(
                  selected_facility,
                  " · Original hire categories · ",
                  input$year
                )
              )
            )
          }

          group_label <- if (
            is.null(selected_group) ||
              length(selected_group) == 0L
          ) {
            "All Facilities"
          } else {
            selected_group
          }

          return(
            chart_subtitle(
              paste0(
                group_label,
                " · Annual transactions · ",
                input$year
              )
            )
          )
        }

        if (identical(input$area, "Merchandise")) {

          return(
            chart_subtitle(
              paste0(
                merchandise_section_selection(),
                " · YTD transactions · ",
                input$year
              )
            )
          )
        }

        if (identical(input$area, "Online Sales")) {

          return(
            chart_subtitle(
              paste0(
                online_sales_site_selection(),
                " · YTD revenue · ",
                input$year
              )
            )
          )
        }

        if (identical(input$area, "Venue Finances")) {

          return(
            chart_subtitle(
              paste0(
                "YTD Actual Net Result · ",
                input$year
              )
            )
          )
        }

        if (identical(input$area, "Membership")) {

          return(
            chart_subtitle(
              paste0(
                membership_scope_label(),
                " · ",
                input$year
              )
            )
          )
        }

        if (identical(input$area, "Workforce")) {

          return(
            chart_subtitle(
              paste0(
                "Monthly Budget vs Actual wages · ",
                input$year
              )
            )
          )
        }

        if (identical(input$area, "Attendance")) {
          return(
            chart_subtitle(
              paste0(
                "Indoor, Outdoor and Sport Club · ",
                input$year
              )
            )
          )
        }

        chart_subtitle(
          paste0(
            "Additional analysis · ",
            input$area
          )
        )
      })

      output$attendance_by_activity <- renderPlot(
        {

          req(input$area)

          if (identical(input$area, "Attendance")) {

            activity_data <- attendance_breakdown_data()

            validate(
              need(
                is.data.frame(activity_data) &&
                  nrow(activity_data) > 0L &&
                  any(is.finite(activity_data$value)),
                "No activity attendance values are available for this selection."
              )
            )

            return(
              plot_venues_attendance_by_activity(
                activity_data
              )
            )
          }

          if (identical(input$area, "Facility Hire")) {

            facility_data <-
              facility_hire_revenue_breakdown()

            validate(
              need(
                is.data.frame(facility_data) &&
                  nrow(facility_data) > 0L &&
                  any(is.finite(facility_data$value)),
                "No Facility Hire revenue values are available for this selection."
              )
            )

            return(
              plot_venues_facility_hire_by_facility(
                facility_data,
                currency = TRUE
              )
            )
          }

          if (identical(input$area, "Workforce")) {

            workforce_data <-
              workforce_hours_monthly()

            validate(
              need(
                is.data.frame(workforce_data) &&
                  nrow(workforce_data) > 0L &&
                  any(
                    is.finite(
                      workforce_data$variance_hours
                    )
                  ),
                "No monthly Casual Hours variance values are available."
              )
            )

            return(
              plot_workforce_hours_variance(
                workforce_data
              )
            )
          }

          if (identical(input$area, "Membership")) {

            movement <- membership_movement()

            validate(
              need(
                is.data.frame(movement) &&
                  nrow(movement) > 0L,
                "No membership movement values are available for this selection."
              )
            )

            return(
              plot_site_detail_membership_movement(
                movement
              )
            )
          }

          if (identical( input$area, "Venue Finances")) {

            finance_data <- finance_monthly()

            validate(
              need(
                is.data.frame(finance_data) &&
                  nrow(finance_data) > 0L &&
                  any(
                    is.finite(
                      finance_data$net_actual
                    )
                  ) &&
                  any(
                    is.finite(
                      finance_data$net_budget
                    )
                  ),
                "No complete Budget and Actual finance values are available for this selection."
              )
            )

            return(
              plot_monthly_budget_vs_actual_net_result(
                finance_data
              )
            )
          }

          if (identical(input$area, "Online Sales")) {

            site_data <-
              online_sales_by_site()

            validate(
              need(
                is.data.frame(site_data) &&
                  nrow(site_data) > 0L &&
                  any(
                    is.finite(
                      site_data$value
                    )
                  ),
                "No site-level Online Sales values are available."
              )
            )

            return(
              plot_venues_facility_hire_by_facility(
                site_data,
                currency = TRUE
              )
            )
          }

          if (identical(input$area, "Merchandise")) {

            merchandise_data <-
              merchandise_revenue_breakdown()

            validate(
              need(
                is.data.frame(
                  merchandise_data
                ) &&
                  nrow(
                    merchandise_data
                  ) > 0L &&
                  any(
                    is.finite(
                      merchandise_data$value
                    )
                  ),
                "No Merchandise revenue breakdown is available."
              )
            )

            return(
              plot_venues_facility_hire_by_facility(
                merchandise_data,
                currency = TRUE
              )
            )
          }

      },
      res = 96,
      bg = "white"
    )

    output$attendance_by_area <- renderPlot(
  {

    req(input$area)

    if (identical(input$area, "Attendance")) {

      area_data <- attendance_area()

      validate(
        need(
          is.data.frame(area_data) &&
            nrow(area_data) > 0L &&
            any(is.finite(area_data$value)),
          "No area attendance values are available for this selection."
        )
      )

      return(
        plot_venues_attendance_by_area(
          area_data
        )
      )
    }

    if (identical(input$area, "Facility Hire")) {

      facility_data <-
       facility_hire_transactions_breakdown()

      validate(
        need(
          is.data.frame(facility_data) &&
            nrow(facility_data) > 0L &&
            any(is.finite(facility_data$value)),
          "No Facility Hire transaction values are available for this selection."
        )
      )

      return(
        plot_venues_facility_hire_by_facility(
          facility_data,
          currency = FALSE
        )
      )
    }

    if (identical(input$area, "Workforce")) {

      wage_data <-
        workforce_wages_monthly()

      validate(
        need(
          is.data.frame(wage_data) &&
            nrow(wage_data) > 0L &&
            any(
              is.finite(
                wage_data$net_budget
              )
            ) &&
            any(
              is.finite(
                wage_data$net_actual
              )
            ),
          "No complete Casual Wages Budget and Actual values are available."
        )
      )

      return(
        plot_monthly_budget_vs_actual_net_result(
          wage_data
        )
      )
    }

    if (identical(input$area, "Membership")) {

      membership_data <-
        membership_breakdown()

      validate(
        need(
          is.data.frame(membership_data) &&
            nrow(membership_data) > 0L &&
            any(
              is.finite(
                membership_data$value
              )
            ),
          "No membership breakdown values are available for this selection."
        )
      )

      return(
        plot_venues_facility_hire_by_facility(
          membership_data,
          currency = FALSE
        )
      )
    }

    if (identical(input$area, "Venue Finances")) {

      fund_data <- finance_by_fund()

      validate(
        need(
          is.data.frame(fund_data) &&
            nrow(fund_data) > 0L &&
            any(
              is.finite(
                fund_data$net_actual
              )
            ),
          "No fund-level Net Result values are available."
        )
      )

      return(
        plot_site_ranking(
          fund_data,
          "Net Result"
        )
      )
    }

    if (identical(input$area, "Online Sales")) {

      category_data <-
        online_sales_by_category()

      validate(
        need(
          is.data.frame(category_data) &&
            nrow(category_data) > 0L &&
            any(
              is.finite(
                category_data$value
              )
            ),
          "No Online Sales category values are available."
        )
      )

      return(
        plot_venues_facility_hire_by_facility(
          category_data,
          currency = TRUE
        )
      )
    }

    if (identical(input$area, "Merchandise")) {

      merchandise_data <-
        merchandise_transactions_breakdown()

      validate(
        need(
          is.data.frame(
            merchandise_data
          ) &&
            nrow(
              merchandise_data
            ) > 0L &&
            any(
              is.finite(
                merchandise_data$value
              )
            ),
          "No Merchandise transaction breakdown is available."
        )
      )

      return(
        plot_venues_facility_hire_by_facility(
          merchandise_data,
          currency = FALSE
        )
      )
    }

  },
  res = 96,
  bg = "white"
 )
      
       invisible(
        list(
          year = reactive(input$year),
          area = reactive(input$area)
        )
      )
    }
  )
}
