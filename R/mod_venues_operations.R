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

        attendance <- selected_attendance()

        if (
          !is.data.frame(attendance) ||
            nrow(attendance) == 0L
        ) {
          return(
            tags$small(
              class = "text-secondary",
              "No Attendance data is available for this year."
            )
          )
        }

        tags$small(
          class = "text-secondary",
          "Available from uploaded reports: Attendance"
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

        if (!identical(input$area, "Attendance")) {
          return("Operational Breakdown")
        }

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
