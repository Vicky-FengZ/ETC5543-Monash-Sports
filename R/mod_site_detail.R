# -------------------------------------------------------------------------
# Site Detail module
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Site Detail data helpers
# -------------------------------------------------------------------------

site_detail_filter_rows <- function(
    data,
    year = NULL,
    site = NULL
) {

  if (
    is.null(data) ||
      !is.data.frame(data) ||
      nrow(data) == 0L
  ) {
    return(data)
  }

  keep <- rep(TRUE, nrow(data))

  if (
    !is.null(year) &&
      length(year) > 0L
  ) {

    selected_year <- suppressWarnings(
      as.integer(year[[1L]])
    )

    if (
      !is.na(selected_year) &&
        "report_year" %in% names(data)
    ) {
      keep <- keep &
        data$report_year == selected_year
    }
  }

  if (
    !is.null(site) &&
      length(site) > 0L &&
      nzchar(as.character(site[[1L]]))
  ) {

    key <- facility_site_key(
      as.character(site[[1L]])
    )

    if (
      is.null(key) ||
        !all(
          c(
            "report_unit",
            "campus"
          ) %in% names(data)
        )
    ) {
      return(
        data[0, , drop = FALSE]
      )
    }

    keep <- keep &
      data$report_unit == key$report_unit &
      data$campus == key$campus
  }

  data[
    !is.na(keep) & keep,
    ,
    drop = FALSE
  ]
}


site_detail_available_years <- function(
    site_reports
) {

  if (
    is.null(site_reports) ||
      !is.list(site_reports)
  ) {
    return(integer())
  }

  tables <- lapply(
    c(
      "visits",
      "memberships",
      "enrollments"
    ),
    function(name) {
      site_reports[[name]]
    }
  )

  years <- unlist(
    lapply(
      tables,
      function(data) {

        if (
          is.null(data) ||
            !is.data.frame(data) ||
            !"report_year" %in% names(data)
        ) {
          return(integer())
        }

        as.integer(data$report_year)
      }
    ),
    use.names = FALSE
  )

  sort(
    unique(
      years[
        !is.na(years)
      ]
    )
  )
}


site_detail_available_sites <- function(
    site_reports,
    year = NULL
) {

  if (
    is.null(site_reports) ||
      !is.list(site_reports)
  ) {
    return(character())
  }

  SITE_NAMES[
    vapply(
      SITE_NAMES,
      function(site) {

        any(
          vapply(
            c(
              "visits",
              "memberships",
              "enrollments"
            ),
            function(name) {

              data <- site_detail_filter_rows(
                site_reports[[name]],
                year = year,
                site = site
              )

              is.data.frame(data) &&
                nrow(data) > 0L
            },
            logical(1)
          )
        )
      },
      logical(1)
    )
  ]
}


site_detail_available_areas <- function(
    site_reports,
    year = NULL,
    site = NULL
) {

  if (
    is.null(site_reports) ||
      !is.list(site_reports)
  ) {
    return(character())
  }

  datasets <- c(
    Visits = "visits",
    Memberships = "memberships",
    Enrolments = "enrollments"
  )

  available <- vapply(
    datasets,
    function(name) {

      data <- site_detail_filter_rows(
        site_reports[[name]],
        year = year,
        site = site
      )

      is.data.frame(data) &&
        nrow(data) > 0L
    },
    logical(1)
  )

  names(datasets)[available]
}

# -------------------------------------------------------------------------
# Site Detail visit calculations
# -------------------------------------------------------------------------

site_detail_sum_available <- function(values) {

  values <- suppressWarnings(
    as.numeric(values)
  )

  values <- values[
    is.finite(values)
  ]

  if (length(values) == 0L) {
    return(NA_real_)
  }

  sum(values)
}


site_detail_visit_value <- function(
    visits,
    visit_type,
    audience
) {

  if (
    is.null(visits) ||
      !is.data.frame(visits) ||
      nrow(visits) == 0L
  ) {
    return(NA_real_)
  }

  required_columns <- c(
    "period_type",
    "row_type",
    "visit_type",
    "audience",
    "value"
  )

  if (!all(required_columns %in% names(visits))) {
    return(NA_real_)
  }

  base_keep <-
    visits$row_type == "Total" &
    visits$visit_type == visit_type &
    visits$audience == audience

  ytd_values <- visits$value[
    base_keep &
      visits$period_type == "YTD"
  ]

  ytd_total <- site_detail_sum_available(
    ytd_values
  )

  if (is.finite(ytd_total)) {
    return(ytd_total)
  }

  monthly_values <- visits$value[
    base_keep &
      visits$period_type == "Month"
  ]

  site_detail_sum_available(
    monthly_values
  )
}


site_detail_visit_kpis <- function(visits) {

  direct_total <- site_detail_visit_value(
    visits,
    visit_type = "Direct",
    audience = "Total"
  )

  indirect_total <- site_detail_visit_value(
    visits,
    visit_type = "Indirect",
    audience = "Total"
  )

  combined_value <- function(audience) {

    direct <- site_detail_visit_value(
      visits,
      visit_type = "Direct",
      audience = audience
    )

    indirect <- site_detail_visit_value(
      visits,
      visit_type = "Indirect",
      audience = audience
    )

    if (
      !is.finite(direct) ||
        !is.finite(indirect)
    ) {
      return(NA_real_)
    }

    direct + indirect
  }

  total_visits <- if (
    is.finite(direct_total) &&
      is.finite(indirect_total)
  ) {
    direct_total + indirect_total
  } else {
    NA_real_
  }

  c(
    total = total_visits,
    direct = direct_total,
    indirect = indirect_total,
    student = combined_value("Student"),
    staff = combined_value("Staff"),
    community = combined_value("Community")
  )
}


site_detail_format_visit_count <- function(value) {

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

# -------------------------------------------------------------------------
# Monthly visit trend data
# -------------------------------------------------------------------------

site_detail_monthly_visit_totals <- function(visits) {

  empty_result <- data.frame(
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    direct = double(),
    indirect = double(),
    total = double(),
    stringsAsFactors = FALSE
  )

  if (
    is.null(visits) ||
      !is.data.frame(visits) ||
      nrow(visits) == 0L
  ) {
    return(empty_result)
  }

  required_columns <- c(
    "date",
    "period_type",
    "row_type",
    "visit_type",
    "audience",
    "value"
  )

  if (!all(required_columns %in% names(visits))) {
    return(empty_result)
  }

  keep <-
    visits$period_type == "Month" &
    visits$row_type == "Total" &
    visits$audience == "Total" &
    visits$visit_type %in% c(
      "Direct",
      "Indirect"
    ) &
    !is.na(visits$date)

  monthly <- visits[
    !is.na(keep) & keep,
    c(
      "date",
      "visit_type",
      "value"
    ),
    drop = FALSE
  ]

  if (nrow(monthly) == 0L) {
    return(empty_result)
  }

  monthly$value <- suppressWarnings(
    as.numeric(monthly$value)
  )

  totals <- aggregate(
    value ~ date + visit_type,
    data = monthly,
    FUN = site_detail_sum_available
  )

  totals <- reshape(
    totals,
    idvar = "date",
    timevar = "visit_type",
    direction = "wide"
  )

  if (!"value.Direct" %in% names(totals)) {
    totals$value.Direct <- NA_real_
  }

  if (!"value.Indirect" %in% names(totals)) {
    totals$value.Indirect <- NA_real_
  }

  dates <- as.Date(totals$date)
  order_index <- order(dates)

  direct <- totals$value.Direct[order_index]
  indirect <- totals$value.Indirect[order_index]
  dates <- dates[order_index]

  data.frame(
    date = dates,
    month = as.integer(
      format(dates, "%m")
    ),
    month_label = format(
      dates,
      "%b"
    ),
    direct = direct,
    indirect = indirect,
    total = ifelse(
      is.finite(direct) &
        is.finite(indirect),
      direct + indirect,
      NA_real_
    ),
    stringsAsFactors = FALSE
  )
}

# -------------------------------------------------------------------------
# Visit activity breakdown
# -------------------------------------------------------------------------

site_detail_activity_totals <- function(visits) {

  empty_result <- data.frame(
    activity = character(),
    value = double(),
    stringsAsFactors = FALSE
  )

  if (
    is.null(visits) ||
      !is.data.frame(visits) ||
      nrow(visits) == 0L
  ) {
    return(empty_result)
  }

  required_columns <- c(
    "period_type",
    "row_type",
    "audience",
    "activity",
    "value"
  )

  if (!all(required_columns %in% names(visits))) {
    return(empty_result)
  }

  keep <-
    visits$period_type == "Month" &
    visits$row_type == "Activity" &
    visits$audience == "Total" &
    !is.na(visits$activity) &
    nzchar(trimws(visits$activity))

  activity_rows <- visits[
    !is.na(keep) & keep,
    c(
      "activity",
      "value"
    ),
    drop = FALSE
  ]

  if (nrow(activity_rows) == 0L) {
    return(empty_result)
  }

  activity_rows$value <- suppressWarnings(
    as.numeric(activity_rows$value)
  )

  activity_rows <- activity_rows[
    is.finite(activity_rows$value),
    ,
    drop = FALSE
  ]

  if (nrow(activity_rows) == 0L) {
    return(empty_result)
  }

  totals <- aggregate(
    value ~ activity,
    data = activity_rows,
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

# -------------------------------------------------------------------------
# Membership calculations
# -------------------------------------------------------------------------

site_detail_latest_membership_value <- function(
    memberships,
    metric
) {

  if (
    is.null(memberships) ||
      !is.data.frame(memberships) ||
      nrow(memberships) == 0L ||
      !"date" %in% names(memberships) ||
      !"metric" %in% names(memberships) ||
      !"value" %in% names(memberships)
  ) {
    return(NA_real_)
  }

  rows <- memberships[
    memberships$metric == metric &
      !is.na(memberships$date),
    ,
    drop = FALSE
  ]

  if (nrow(rows) == 0L) {
    return(NA_real_)
  }

  latest_date <- max(
    rows$date,
    na.rm = TRUE
  )

  values <- suppressWarnings(
    as.numeric(
      rows$value[
        rows$date == latest_date
      ]
    )
  )

  values <- values[
    is.finite(values)
  ]

  if (length(values) == 0L) {
    return(NA_real_)
  }

  sum(values)
}


site_detail_membership_kpis <- function(memberships) {

  if (
    is.null(memberships) ||
      !is.data.frame(memberships) ||
      nrow(memberships) == 0L
  ) {
    return(
      list(
        total_active = NA_real_,
        new_members = NA_real_,
        expired_cancelled = NA_real_,
        reporting_months = 0L,
        latest_month = NA_character_
      )
    )
  }

  valid_dates <- memberships$date[
    !is.na(memberships$date)
  ]

  latest_date <- if (length(valid_dates) > 0L) {
    max(valid_dates)
  } else {
    as.Date(NA)
  }

  list(
    total_active =
      site_detail_latest_membership_value(
        memberships,
        "Total Active"
      ),

    new_members =
      site_detail_latest_membership_value(
        memberships,
        "New Members"
      ),

    expired_cancelled =
      site_detail_latest_membership_value(
        memberships,
        "Expired/Cancelled"
      ),

    reporting_months =
      length(
        unique(
          memberships$month[
            !is.na(memberships$month)
          ]
        )
      ),

    latest_month =
      if (!is.na(latest_date)) {
        format(
          latest_date,
          "%b %Y"
        )
      } else {
        NA_character_
      }
  )
}

# -------------------------------------------------------------------------
# Membership trend data
# -------------------------------------------------------------------------

site_detail_membership_trend <- function(memberships) {

  empty_result <- data.frame(
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    total_active = double(),
    stringsAsFactors = FALSE
  )

  if (
    is.null(memberships) ||
      !is.data.frame(memberships) ||
      nrow(memberships) == 0L
  ) {
    return(empty_result)
  }

  required_columns <- c(
    "date",
    "month",
    "metric",
    "value"
  )

  if (!all(required_columns %in% names(memberships))) {
    return(empty_result)
  }

  rows <- memberships[
    memberships$metric == "Total Active" &
      !is.na(memberships$date),
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
    month_label = format(
      as.Date(rows$date),
      "%b"
    ),
    total_active = rows$value,
    stringsAsFactors = FALSE
  )
}

# -------------------------------------------------------------------------
# Membership movement
# -------------------------------------------------------------------------

site_detail_membership_movement <- function(memberships) {

  empty_result <- data.frame(
    date = as.Date(character()),
    month_label = character(),
    new_members = double(),
    expired_cancelled = double(),
    stringsAsFactors = FALSE
  )

  if (
    is.null(memberships) ||
      !is.data.frame(memberships) ||
      nrow(memberships) == 0L
  ) {
    return(empty_result)
  }

  required_columns <- c(
    "date",
    "metric",
    "value"
  )

  if (!all(required_columns %in% names(memberships))) {
    return(empty_result)
  }

  rows <- memberships[
    memberships$metric %in% c(
      "New Members",
      "Expired/Cancelled"
    ) &
      !is.na(memberships$date),
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

  totals <- aggregate(
    value ~ date + metric,
    data = rows,
    FUN = sum
  )

  totals <- reshape(
    totals,
    idvar = "date",
    timevar = "metric",
    direction = "wide"
  )

  if (!"value.New Members" %in% names(totals)) {
    totals[["value.New Members"]] <- NA_real_
  }

  if (!"value.Expired/Cancelled" %in% names(totals)) {
    totals[["value.Expired/Cancelled"]] <- NA_real_
  }

  dates <- as.Date(totals$date)

  order_index <- order(dates)

  data.frame(
    date = dates[order_index],
    month_label = format(
      dates[order_index],
      "%b"
    ),
    new_members =
      totals[["value.New Members"]][order_index],
    expired_cancelled =
      totals[["value.Expired/Cancelled"]][order_index],
    stringsAsFactors = FALSE
  )
}

# -------------------------------------------------------------------------
# Active membership mix
# -------------------------------------------------------------------------

site_detail_membership_active_mix <- function(memberships) {

  empty_result <- data.frame(
    audience = character(),
    value = double(),
    share = double(),
    stringsAsFactors = FALSE
  )

  if (
    is.null(memberships) ||
      !is.data.frame(memberships) ||
      nrow(memberships) == 0L
  ) {
    return(empty_result)
  }

  required_columns <- c(
    "date",
    "metric",
    "value"
  )

  if (!all(required_columns %in% names(memberships))) {
    return(empty_result)
  }

  valid_dates <- memberships$date[
    !is.na(memberships$date)
  ]

  if (length(valid_dates) == 0L) {
    return(empty_result)
  }

  latest_date <- max(valid_dates)

  metric_map <- c(
    Student = "Active Student Members",
    Staff = "Active Staff Members",
    Community = "Active Community Members"
  )

  values <- vapply(
    metric_map,
    function(metric_name) {

      rows <- memberships[
        memberships$date == latest_date &
          memberships$metric == metric_name,
        ,
        drop = FALSE
      ]

      if (nrow(rows) == 0L) {
        return(NA_real_)
      }

      site_detail_sum_available(
        rows$value
      )
    },
    double(1)
  )

  total <- if (all(is.finite(values))) {
    sum(values)
  } else {
    NA_real_
  }

  shares <- if (
    is.finite(total) &&
      total > 0
  ) {
    values / total
  } else {
    rep(
      NA_real_,
      length(values)
    )
  }

  data.frame(
    audience = names(values),
    value = as.numeric(values),
    share = as.numeric(shares),
    stringsAsFactors = FALSE
  )
}

# -------------------------------------------------------------------------
# Enrolment calculations
# -------------------------------------------------------------------------

site_detail_latest_enrolment_metric <- function(
    enrollments,
    metric
) {

  empty_result <- list(
    value = NA_real_,
    latest_month = NA_character_
  )

  if (
    is.null(enrollments) ||
      !is.data.frame(enrollments) ||
      nrow(enrollments) == 0L ||
      !"date" %in% names(enrollments) ||
      !"metric" %in% names(enrollments) ||
      !"value" %in% names(enrollments)
  ) {
    return(empty_result)
  }

  rows <- enrollments[
    enrollments$metric == metric &
      !is.na(enrollments$date),
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

  # These enrolment templates contain zero-filled months where
  # no meaningful snapshot value has been reported yet.
  meaningful_rows <- rows[
    rows$value > 0,
    ,
    drop = FALSE
  ]

  # If the metric genuinely contains only zeros, retain zero
  # rather than treating the metric as unavailable.
  if (nrow(meaningful_rows) > 0L) {
    rows <- meaningful_rows
  }

  latest_date <- max(
    rows$date,
    na.rm = TRUE
  )

  values <- rows$value[
    rows$date == latest_date
  ]

  list(
    value = sum(values),
    latest_month = format(
      latest_date,
      "%b %Y"
    )
  )
}


site_detail_enrolment_kpis <- function(enrollments) {

  if (
    is.null(enrollments) ||
      !is.data.frame(enrollments) ||
      nrow(enrollments) == 0L
  ) {
    return(
      list(
        total_active = NA_real_,
        total_active_month = NA_character_,
        target = NA_real_,
        target_month = NA_character_,
        active_students = NA_real_,
        active_students_month = NA_character_,
        student_share = NA_real_,
        student_share_month = NA_character_
      )
    )
  }

  total_active <- site_detail_latest_enrolment_metric(
    enrollments,
    "Total Active"
  )

  target <- site_detail_latest_enrolment_metric(
    enrollments,
    "Target"
  )

  active_students <- site_detail_latest_enrolment_metric(
    enrollments,
    "Active Student Members"
  )

  student_share <- site_detail_latest_enrolment_metric(
    enrollments,
    "Student Share"
  )

  list(
    total_active = total_active$value,
    total_active_month = total_active$latest_month,

    target = target$value,
    target_month = target$latest_month,

    active_students = active_students$value,
    active_students_month = active_students$latest_month,

    student_share = student_share$value,
    student_share_month = student_share$latest_month
  )
}

# -------------------------------------------------------------------------
# Enrolment trend data
# -------------------------------------------------------------------------

site_detail_enrolment_trend <- function(enrollments) {

  empty_result <- data.frame(
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    total_active = double(),
    target = double(),
    stringsAsFactors = FALSE
  )

  if (
    is.null(enrollments) ||
      !is.data.frame(enrollments) ||
      nrow(enrollments) == 0L
  ) {
    return(empty_result)
  }

  required_columns <- c(
    "date",
    "metric",
    "value"
  )

  if (!all(required_columns %in% names(enrollments))) {
    return(empty_result)
  }

  dates <- sort(
    unique(
      as.Date(
        enrollments$date[
          !is.na(enrollments$date)
        ]
      )
    )
  )

  if (length(dates) == 0L) {
    return(empty_result)
  }

  metric_values <- function(metric_name) {

    vapply(
      seq_along(dates),
      function(index) {

        current_date <- dates[[index]]

        values <- enrollments$value[
          enrollments$date == current_date &
            enrollments$metric == metric_name
        ]

        site_detail_sum_available(
          values
        )
      },
      double(1)
    )
  }

  total_active <- metric_values(
    "Total Active"
  )

  target <- metric_values(
    "Target"
  )

  # In these enrolment templates, zero-filled snapshot values
  # represent months without a meaningful reported value.
  total_active[
    is.finite(total_active) &
      total_active == 0
  ] <- NA_real_

  target[
    is.finite(target) &
      target == 0
  ] <- NA_real_

  data.frame(
    date = dates,
    month = as.integer(
      format(dates, "%m")
    ),
    month_label = format(
      dates,
      "%b"
    ),
    total_active = total_active,
    target = target,
    stringsAsFactors = FALSE
  )
}

site_detail_enrolment_breakdown <- function(enrollments) {

  empty_result <- data.frame(
    date = as.Date(character()),
    month_label = character(),
    total_active = double(),
    active_students = double(),
    stringsAsFactors = FALSE
  )

  if (
    is.null(enrollments) ||
      !is.data.frame(enrollments) ||
      nrow(enrollments) == 0L
  ) {
    return(empty_result)
  }

  required_columns <- c(
    "date",
    "metric",
    "value"
  )

  if (!all(required_columns %in% names(enrollments))) {
    return(empty_result)
  }

  dates <- sort(
    unique(
      as.Date(
        enrollments$date[
          !is.na(enrollments$date)
        ]
      )
    )
  )

  if (length(dates) == 0L) {
    return(empty_result)
  }

  metric_values <- function(metric_name) {

    vapply(
      seq_along(dates),
      function(index) {

        values <- enrollments$value[
          enrollments$date == dates[[index]] &
            enrollments$metric == metric_name
        ]

        site_detail_sum_available(values)
      },
      double(1)
    )
  }

  total_active <- metric_values(
    "Total Active"
  )

  active_students <- metric_values(
    "Active Student Members"
  )

  total_active[
    is.finite(total_active) &
      total_active == 0
  ] <- NA_real_

  active_students[
    is.finite(active_students) &
      active_students == 0
  ] <- NA_real_

  data.frame(
    date = dates,
    month_label = format(
      dates,
      "%b"
    ),
    total_active = total_active,
    active_students = active_students,
    stringsAsFactors = FALSE
  )
}

# -------------------------------------------------------------------------
# Enrolment student share trend
# -------------------------------------------------------------------------

site_detail_enrolment_student_share <- function(enrollments) {

  empty_result <- data.frame(
    date = as.Date(character()),
    month_label = character(),
    student_share = double(),
    stringsAsFactors = FALSE
  )

  if (
    is.null(enrollments) ||
      !is.data.frame(enrollments) ||
      nrow(enrollments) == 0L ||
      !all(
        c(
          "date",
          "metric",
          "value"
        ) %in% names(enrollments)
      )
  ) {
    return(empty_result)
  }

  rows <- enrollments[
    enrollments$metric == "Student Share" &
      !is.na(enrollments$date),
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

  # Zero-filled months in these templates represent
  # months without a meaningful reported share.
  rows <- rows[
    rows$value > 0,
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
    month_label = format(
      as.Date(rows$date),
      "%b"
    ),
    student_share = rows$value,
    stringsAsFactors = FALSE
  )
}

# UI ----------------------------------------------------------------------
# Detailed Analysis -> Site Detail.
# Uses Site Report files to examine one site within one year, month by month.

mod_site_detail_ui <- function(id) {

  ns <- NS(id)

  nav_panel(
    "Site Detail",

    div(
      class = "p-4",

      div(
        class = "page-heading",
        h2("Site Detail"),
        p(
          "Explore month-by-month performance for a selected site using the detailed Site Reports."
        )
      ),

      # Site Detail controls are populated from uploaded Detailed Site Reports.
      div(
        class = "filter-bar",

        div(
          class = "filter-section-title",
          "Participation detail"
        ),

        layout_columns(
          
          selectInput(
            ns("year"),
            "Year",
            choices = character()
          ),

          selectInput(
            ns("site"),
            "Site",
            choices = character()
          ),

          col_widths = c(
            4,
            8
          )
        ),

        uiOutput(
          ns("availability_note")
        )
      ),

      div(
        class = "section-heading-custom",

        h4(
          class = "mb-1",
          "Participation Analysis"
        ),

        p(
          class = "text-secondary mb-2",
          "Choose the participation dataset to explore."
        )
      ),

      uiOutput(
        ns("area_tabs")
      ),

      # Dynamic focus heading confirms the current Year / Site / Area.
      uiOutput(
        ns("focus_heading")
      ),

      # KPI cards change according to the selected analysis area.
      uiOutput(
        ns("kpis")
      ),

      br(),

      # Main chart: the month-by-month trend for the selected analysis area.
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

        uiOutput(
          ns("main_content")
        )
      ),

      # Supporting row:
      #   left  = category/budget/membership breakdown
      #   right = qualitative context and monthly notes
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
            ns("breakdown_content")
          )
        ),

        card(
          full_screen = TRUE,

          card_header(
            div(
              div(
                class = "chart-header-title",
                uiOutput(
                  ns("audience_title"),
                  inline = TRUE
                )
              ),

              uiOutput(
                ns("audience_subtitle")
              )
            )
          ),

          uiOutput(
            ns("audience_content")
          )
        ),

        col_widths = c(
          8,
          4
        )
      ),

      uiOutput(
        ns("activity_section")
      )
    )
  )
}


# Server ------------------------------------------------------------------
# Updates available analysis areas and switches KPI/chart labels according
# to the selected Site Report content.

mod_site_detail_server <- function(
  id,
  data = NULL
) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      # -------------------------------------------------------------------
      # Detailed Site Report data
      # -------------------------------------------------------------------

      site_reports <- reactive({

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
            "business_unit_template_report_data"
          )
        ) {
          return(dashboard_data)
        }

        if (!is.list(dashboard_data)) {
          return(NULL)
        }

        if (
          inherits(
            dashboard_data$site_reports,
            "business_unit_template_report_data"
          )
        ) {
          return(
            dashboard_data$site_reports
          )
        }

        if (
          inherits(
            dashboard_data$sites,
            "business_unit_template_report_data"
          )
        ) {
          return(
            dashboard_data$sites
          )
        }

        NULL
      })

      selected_visits <- reactive({

        reports <- site_reports()

        if (
          is.null(reports) ||
            !is.list(reports)
        ) {
          return(NULL)
        }

        site_detail_filter_rows(
          reports$visits,
          year = input$year,
          site = input$site
        )
      })


      visit_kpis <- reactive({

        site_detail_visit_kpis(
          selected_visits()
        )
      })

            monthly_visits <- reactive({

        site_detail_monthly_visit_totals(
          selected_visits()
        )
      })

        visit_type_breakdown <- reactive({

        values <- visit_kpis()

        visit_values <- c(
          Direct = values[["direct"]],
          Indirect = values[["indirect"]]
        )

        total <- sum(
          visit_values[
            is.finite(visit_values)
          ]
        )

        shares <- if (
          is.finite(total) &&
            total > 0
        ) {
          visit_values / total
        } else {
          rep(
            NA_real_,
            length(visit_values)
          )
        }

        data.frame(
          visit_type = names(visit_values),
          value = as.numeric(visit_values),
          share = as.numeric(shares),
          stringsAsFactors = FALSE
        )
      })

      audience_breakdown <- reactive({

        values <- visit_kpis()

        audience_values <- c(
          Student = values[["student"]],
          Staff = values[["staff"]],
          Community = values[["community"]]
        )

        complete <- all(
          is.finite(audience_values)
        )

        total <- if (complete) {
          sum(audience_values)
        } else {
          NA_real_
        }

        shares <- if (
          is.finite(total) &&
            total > 0
        ) {
          audience_values / total
        } else {
          rep(
            NA_real_,
            length(audience_values)
          )
        }

        data.frame(
          audience = names(audience_values),
          value = as.numeric(audience_values),
          share = as.numeric(shares),
          stringsAsFactors = FALSE
        )
      })

      selected_memberships <- reactive({

        reports <- site_reports()

        if (
          is.null(reports) ||
            !is.list(reports)
        ) {
          return(NULL)
        }

        site_detail_filter_rows(
          reports$memberships,
          year = input$year,
          site = input$site
        )
      })


      membership_kpis <- reactive({

        site_detail_membership_kpis(
          selected_memberships()
        )
      })

      selected_enrollments <- reactive({

        reports <- site_reports()

        if (
          is.null(reports) ||
            !is.list(reports)
        ) {
          return(NULL)
        }

        site_detail_filter_rows(
          reports$enrollments,
          year = input$year,
          site = input$site
        )
      })

      enrolment_kpis <- reactive({

        site_detail_enrolment_kpis(
          selected_enrollments()
        )
      })

      enrolment_trend <- reactive({

        site_detail_enrolment_trend(
          selected_enrollments()
        )
      })

      enrolment_breakdown <- reactive({

        site_detail_enrolment_breakdown(
          selected_enrollments()
        )
      })

      enrolment_student_share <- reactive({

        site_detail_enrolment_student_share(
          selected_enrollments()
        )
      })

      membership_trend <- reactive({

        site_detail_membership_trend(
          selected_memberships()
        )
      })

      membership_movement <- reactive({

        site_detail_membership_movement(
          selected_memberships()
        )
      })

      membership_active_mix <- reactive({

        site_detail_membership_active_mix(
          selected_memberships()
        )
      })

      activity_breakdown <- reactive({

        site_detail_activity_totals(
          selected_visits()
        )
      })      

      # -------------------------------------------------------------------
      # Available years
      # -------------------------------------------------------------------

      observeEvent(
        site_reports(),
        {

          years <- site_detail_available_years(
            site_reports()
          )

          selected_year <- if (
            length(years) > 0L
          ) {
            as.character(max(years))
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
      # Available sites for selected year
      # -------------------------------------------------------------------

      observeEvent(
        list(
          site_reports(),
          input$year
        ),
        {

          sites <- site_detail_available_sites(
            site_reports(),
            year = input$year
          )

          selected_site <- if (
            !is.null(input$site) &&
              input$site %in% sites
          ) {
            input$site
          } else if (length(sites) > 0L) {
            sites[[1L]]
          } else {
            character()
          }

          updateSelectInput(
            session,
            "site",
            choices = sites,
            selected = selected_site
          )
        },
        ignoreInit = FALSE
      )

      # -------------------------------------------------------------------
      # Available participation views
      # -------------------------------------------------------------------

      output$area_tabs <- renderUI({

        reports <- site_reports()

        if (is.null(reports)) {
          return(NULL)
        }

        areas <- site_detail_available_areas(
          reports,
          year = input$year,
          site = input$site
        )

        if (length(areas) == 0L) {
          return(NULL)
        }

        current_area <- isolate(
          input$area
        )

        selected_area <- if (
          !is.null(current_area) &&
            current_area %in% areas
        ) {
          current_area
        } else if ("Visits" %in% areas) {
          "Visits"
        } else {
          areas[[1L]]
        }

        tabs <- lapply(
          areas,
          function(area) {

            nav_panel(
              area,
              value = area,
              div()
            )
          }
        )

        do.call(
          navset_pill,
          c(
            list(
              id = ns("area"),
              selected = selected_area
            ),
            tabs
          )
        )
      })

      output$availability_note <- renderUI({

        reports <- site_reports()

        if (is.null(reports)) {
          return(
            tags$small(
              class = "text-secondary",
              "Upload Detailed Site Reports on the Data page to activate this analysis."
            )
          )
        }

        areas <- site_detail_available_areas(
          reports,
          year = input$year,
          site = input$site
        )

        if (length(areas) == 0L) {
          return(
            tags$small(
              class = "text-secondary",
              "No participation data is available for this selection."
            )
          )
        }

        tags$small(
          class = "text-secondary",
          paste0(
            "Available from uploaded reports: ",
            paste(
              areas,
              collapse = ", "
            )
          )
        )
      })

      output$focus_heading <- renderUI({

        req(
          input$year,
          input$site,
          input$area
        )

        div(
          class = "section-heading-custom",

          h4(
            class = "mb-1",
            paste0(
              input$site,
              " · ",
              input$area
            )
          ),

          p(
            class = "mb-0 text-secondary",
            paste(
              input$year,
              "· Participation detail from the selected Detailed Site Report."
            )
          )
        )
      })

      # Defines the KPI layout for each analysis area.
      # Values are placeholders; only the structure changes at this stage.

      kpi_specs <- reactive({

        req(input$area)

        switch(
          input$area,

          "Visits" = {

            values <- visit_kpis()

            list(
              list(
                title = "Total Visits",
                value = site_detail_format_visit_count(
                  values[["total"]]
                ),
                subtitle = "Direct + indirect",
                icon = "people"
              ),
              list(
                title = "Direct Visits",
                value = site_detail_format_visit_count(
                  values[["direct"]]
                ),
                subtitle = "Selected year",
                icon = "arrow-right-circle"
              ),
              list(
                title = "Indirect Visits",
                value = site_detail_format_visit_count(
                  values[["indirect"]]
                ),
                subtitle = "Selected year",
                icon = "diagram-3"
              ),
              list(
                title = "Student Visits",
                value = site_detail_format_visit_count(
                  values[["student"]]
                ),
                subtitle = "Direct + indirect",
                icon = "mortarboard"
              ),
              list(
                title = "Staff Visits",
                value = site_detail_format_visit_count(
                  values[["staff"]]
                ),
                subtitle = "Direct + indirect",
                icon = "person-badge"
              ),
              list(
                title = "Community Visits",
                value = site_detail_format_visit_count(
                  values[["community"]]
                ),
                subtitle = "Direct + indirect",
                icon = "people-fill"
              )
            )
          },

          "Memberships" = {

            values <- membership_kpis()

            latest_month <- if (
              is.character(values$latest_month) &&
                length(values$latest_month) > 0L &&
                !is.na(values$latest_month)
            ) {
              values$latest_month
            } else {
              "Latest available month"
            }

            list(
              list(
                title = "Total Active",
                value = site_detail_format_visit_count(
                  values$total_active
                ),
                subtitle = latest_month,
                icon = "people"
              ),
              list(
                title = "New Members",
                value = site_detail_format_visit_count(
                  values$new_members
                ),
                subtitle = latest_month,
                icon = "person-plus"
              ),
              list(
                title = "Expired / Cancelled",
                value = site_detail_format_visit_count(
                  values$expired_cancelled
                ),
                subtitle = latest_month,
                icon = "person-dash"
              ),
              list(
                title = "Reporting Months",
                value = as.character(
                  values$reporting_months
                ),
                subtitle = "Available periods",
                icon = "calendar3"
              )
            )
          },

          "Enrolments" = {

            values <- enrolment_kpis()

            metric_month <- function(value) {

              if (
                is.character(value) &&
                  length(value) > 0L &&
                  !is.na(value[[1L]])
              ) {
                value[[1L]]
              } else {
                "Latest available"
              }
            }

            list(
              list(
                title = "Total Active",
                value = site_detail_format_visit_count(
                  values$total_active
                ),
                subtitle = metric_month(
                  values$total_active_month
                ),
                icon = "people"
              ),
              list(
                title = "Target",
                value = site_detail_format_visit_count(
                  values$target
                ),
                subtitle = metric_month(
                  values$target_month
                ),
                icon = "bullseye"
              ),
              list(
                title = "Active Students",
                value = site_detail_format_visit_count(
                  values$active_students
                ),
                subtitle = metric_month(
                  values$active_students_month
                ),
                icon = "mortarboard"
              ),
              list(
                title = "Student Share",
                value = if (
                  is.finite(values$student_share)
                ) {
                  paste0(
                    sprintf(
                      "%.1f",
                      100 * values$student_share
                    ),
                    "%"
                  )
                } else {
                  "—"
                },
                subtitle = metric_month(
                  values$student_share_month
                ),
                icon = "percent"
              )
            )
          },
          NULL
        )
      })

      output$kpis <- renderUI({

        boxes <- kpi_specs()

        if (is.null(boxes)) {
          return(NULL)
        }

        make_value_box_row(
          boxes,
          width = 1 / 3
        )
      })

      # Centralises chart titles/placeholders so UI text stays consistent
      # when the user switches analysis area.
      titles <- reactive({

        req(input$area)

        switch(
          input$area,

          "Visits" = list(
            main = "Visits Over Time",
            breakdown = "Direct vs Indirect Visits",
            main_placeholder =
              "Monthly Direct and Indirect Visits",
            breakdown_placeholder =
              "Annual visit-type breakdown"
          ),

          "Memberships" = list(
            main = "Memberships Over Time",
            breakdown = "Membership Movement",
            main_placeholder =
              "Monthly membership measures",
            breakdown_placeholder =
              "Membership counts and shares"
          ),

          "Enrolments" = list(
            main = "Enrolments Over Time",
            breakdown = "Enrolment Breakdown",
            main_placeholder =
              "Monthly enrolment measures",
            breakdown_placeholder =
              "Enrolment measure breakdown"
          )
        )
      })

      output$main_title <- renderUI({
        titles()$main
      })

      output$main_subtitle <- renderUI({

        req(
          input$year,
          input$site
        )

        chart_subtitle(
          paste0(
            input$site,
            " · ",
            input$year
          )
        )
      })

      output$main_content <- renderUI({

        if (identical(input$area, "Visits")) {
          return(
            plotOutput(
              ns("visits_over_time"),
              height = "440px"
            )
          )
        }

        if (identical(input$area, "Memberships")) {
          return(
            plotOutput(
              ns("memberships_over_time"),
              height = "440px"
            )
          )
        }

        if (identical(input$area, "Enrolments")) {
          return(
            plotOutput(
              ns("enrolments_over_time"),
              height = "440px"
            )
          )
        }

        placeholder_chart(
          titles()$main_placeholder
        )
      })


      output$visits_over_time <- renderPlot(
        {

          req(
            identical(
              input$area,
              "Visits"
            )
          )

          monthly <- monthly_visits()

          validate(
            need(
              is.data.frame(monthly) &&
                nrow(monthly) > 0L,
              "No monthly visit values are available for this selection."
            )
          )

          plot_site_detail_visits_over_time(
            monthly
          )
        },
        res = 96,
        bg = "white"
      )

      output$memberships_over_time <- renderPlot(
        {

          req(
            identical(
              input$area,
              "Memberships"
            )
          )

          trend <- membership_trend()

          validate(
            need(
              is.data.frame(trend) &&
                nrow(trend) > 0L,
              "No monthly Total Active membership values are available for this selection."
            )
          )

          plot_site_detail_memberships_over_time(
            trend
          )
        },
        res = 96,
        bg = "white"
      )

      output$enrolments_over_time <- renderPlot(
        {

          req(
            identical(
              input$area,
              "Enrolments"
            )
          )

          trend <- enrolment_trend()

          validate(
            need(
              is.data.frame(trend) &&
                nrow(trend) > 0L &&
                any(
                  is.finite(
                    c(
                      trend$total_active,
                      trend$target
                    )
                  )
                ),
              "No monthly Total Active or Target values are available for this selection."
            )
          )

          plot_site_detail_enrolments_over_time(
            trend
          )
        },
        res = 96,
        bg = "white"
      )

      output$breakdown_title <- renderUI({
        titles()$breakdown
      })

      output$breakdown_subtitle <- renderUI({

        req(input$area)

        chart_subtitle(
          paste0(
            "Detailed view · ",
            input$area
          )
        )
      })

      output$breakdown_content <- renderUI({

        if (identical(input$area, "Visits")) {
          return(
            plotOutput(
              ns("visit_type_breakdown"),
              height = "360px"
            )
          )
        }

        if (identical(input$area, "Memberships")) {
          return(
            plotOutput(
              ns("membership_movement"),
              height = "360px"
            )
          )
        }

        if (identical(input$area, "Enrolments")) {
          return(
            plotOutput(
              ns("enrolment_breakdown"),
              height = "360px"
            )
          )
        }

        placeholder_chart(
          titles()$breakdown_placeholder
        )
      })


      output$visit_type_breakdown <- renderPlot(
        {

          req(
            identical(
              input$area,
              "Visits"
            )
          )

          breakdown <- visit_type_breakdown()

          validate(
            need(
              is.data.frame(breakdown) &&
                nrow(breakdown) > 0L &&
                any(is.finite(breakdown$value)),
              "No visit breakdown values are available for this selection."
            )
          )

          plot_site_detail_visit_type_breakdown(
            breakdown
          )
        },
        res = 96,
        bg = "white"
      )

      output$membership_movement <- renderPlot(
        {

          req(
            identical(
              input$area,
              "Memberships"
            )
          )

          movement <- membership_movement()

          validate(
            need(
              is.data.frame(movement) &&
                nrow(movement) > 0L,
              "No monthly membership movement values are available for this selection."
            )
          )

          plot_site_detail_membership_movement(
            movement
          )
        },
        res = 96,
        bg = "white"
      )

      output$enrolment_breakdown <- renderPlot(
        {

          req(
            identical(
              input$area,
              "Enrolments"
            )
          )

          breakdown <- enrolment_breakdown()

          validate(
            need(
              is.data.frame(breakdown) &&
                nrow(breakdown) > 0L &&
                any(
                  is.finite(
                    c(
                      breakdown$total_active,
                      breakdown$active_students
                    )
                  )
                ),
              "No monthly Total Active or Active Student values are available for this selection."
            )
          )

          plot_site_detail_enrolment_breakdown(
            breakdown
          )
        },
        res = 96,
        bg = "white"
      )

      output$audience_title <- renderUI({

        req(input$area)

        if (identical(input$area, "Memberships")) {
          return("Active Member Mix")
        }

        if (identical(input$area, "Enrolments")) {
          return("Student Share Over Time")
        }

        "Audience Mix"
      })

            output$audience_subtitle <- renderUI({

        req(
          input$year,
          input$site,
          input$area
        )

        if (identical(input$area, "Memberships")) {

          values <- membership_kpis()

          latest_month <- if (
            is.character(values$latest_month) &&
              length(values$latest_month) > 0L &&
              !is.na(values$latest_month)
          ) {
            values$latest_month
          } else {
            input$year
          }

          return(
            chart_subtitle(
              paste0(
                "Student, staff and community · ",
                latest_month
              )
            )
          )
        }

        if (identical(input$area, "Enrolments")) {

          return(
            chart_subtitle(
              paste0(
                "Share of active membership · ",
                input$year
              )
            )
          )
        }

        chart_subtitle(
          paste0(
            "Student, staff and community · ",
            input$year
          )
        )
      })
      
      output$audience_content <- renderUI({

        req(input$area)

        if (identical(input$area, "Enrolments")) {
          return(
            plotOutput(
              ns("enrolment_student_share"),
              height = "360px"
            )
          )
        }

        if (
          input$area %in% c(
            "Visits",
            "Memberships"
          )
        ) {
          return(
            plotOutput(
              ns("audience_mix"),
              height = "360px"
            )
          )
        }

        placeholder_chart(
          "Composition analysis is not yet available for this dataset"
        )
      })

      output$enrolment_student_share <- renderPlot(
        {

          req(
            identical(
              input$area,
              "Enrolments"
            )
          )

          share_data <- enrolment_student_share()

          validate(
            need(
              is.data.frame(share_data) &&
                nrow(share_data) > 0L &&
                any(
                  is.finite(
                    share_data$student_share
                  )
                ),
              "No monthly Student Share values are available for this selection."
            )
          )

          plot_site_detail_enrolment_student_share(
            share_data
          )
        },
        res = 96,
        bg = "white"
      )

      output$audience_mix <- renderPlot(
        {

          req(input$area)

          req(
            input$area %in% c(
              "Visits",
              "Memberships"
            )
          )

          mix <- if (
            identical(
              input$area,
              "Memberships"
            )
          ) {
            membership_active_mix()
          } else {
            audience_breakdown()
          }

          validate(
            need(
              is.data.frame(mix) &&
                nrow(mix) == 3L &&
                all(is.finite(mix$value)) &&
                all(is.finite(mix$share)),
              "Complete Student, Staff and Community values are not available for this selection."
            )
          )

          plot_site_detail_audience_mix(
            mix
          )
        },
        res = 96,
        bg = "white"
      )

      output$activity_section <- renderUI({

        if (!identical(input$area, "Visits")) {
          return(NULL)
        }

        activities <- activity_breakdown()

        chart_height <- max(
          380,
          nrow(activities) * 42
        )

        card(
          full_screen = TRUE,

          card_header(
            div(
              div(
                class = "chart-header-title",
                "Visits by Activity"
              ),

              chart_subtitle(
                paste0(
                  "Direct + indirect · ",
                  input$site,
                  " · ",
                  input$year
                )
              )
            )
          ),

          plotOutput(
            ns("activity_breakdown"),
            height = paste0(
              chart_height,
              "px"
            )
          )
        )
      })


      output$activity_breakdown <- renderPlot(
        {

          req(
            identical(
              input$area,
              "Visits"
            )
          )

          activities <- activity_breakdown()

          validate(
            need(
              is.data.frame(activities) &&
                nrow(activities) > 0L,
              "No activity-level visit values are available for this selection."
            )
          )

          plot_site_detail_activity_breakdown(
            activities
          )
        },
        res = 96,
        bg = "white"
      )

      invisible(
        list(
          year = reactive(input$year),
          site = reactive(input$site),
          area = reactive(input$area),
          selected_visits = selected_visits,
          selected_memberships = selected_memberships,
          membership_kpis = membership_kpis,
          membership_trend = membership_trend,
          membership_movement = membership_movement,
          membership_active_mix = membership_active_mix,
          enrolment_trend = enrolment_trend,
          enrolment_breakdown = enrolment_breakdown,
          enrolment_student_share = enrolment_student_share,
          visit_kpis = visit_kpis,
          monthly_visits = monthly_visits,
          visit_type_breakdown = visit_type_breakdown,
          audience_breakdown = audience_breakdown,
          activity_breakdown = activity_breakdown,
          selected_enrollments = selected_enrollments,
          enrolment_kpis = enrolment_kpis
        )
      )
    }
  )
}
