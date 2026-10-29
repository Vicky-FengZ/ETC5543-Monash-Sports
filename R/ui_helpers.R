# -------------------------------------------------------------------------
# Shared UI helpers
# -------------------------------------------------------------------------

placeholder_chart <- function(
  text = "Chart placeholder",
  subtitle = "Real data will be connected in the next stage"
) {

  div(
    class = "chart-placeholder",
    bs_icon(
      "bar-chart-line",
      size = "32px"
    ),
    div(
      class = "placeholder-title",
      text
    ),
    div(
      class = "placeholder-subtitle",
      subtitle
    )
  )
}


prototype_note <- function(
  text = "Prototype values are placeholders for layout demonstration only"
) {
  span(
    class = "prototype-note",
    text
  )
}


chart_subtitle <- function(...) {
  div(
    class = "chart-header-subtitle",
    ...
  )
}


# Each box must be its own grid item: passing the list as one argument puts
# every box in the first column. A minimum width lets 3-6 KPIs share one row
# on wide screens and wrap on narrow ones.
make_value_box_row <- function(boxes, width = "170px") {

  boxes <- Filter(Negate(is.null), boxes)

  do.call(
    layout_column_wrap,
    c(
      list(width = width, class = "kpi-value-box-row"),
      lapply(
        boxes,
        function(x) {
          value_box(
            title = x$title,
            value = x$value,
            x$subtitle,
            showcase = bs_icon(x$icon)
          )
        }
      )
    )
  )
}


# Compact year list: c(2021, 2022, 2023, 2025) -> "2021–2023, 2025".
format_year_ranges <- function(years) {
  years <- sort(unique(as.integer(years)))
  years <- years[!is.na(years)]

  if (length(years) == 0L) {
    return("")
  }

  groups <- split(years, cumsum(c(1L, diff(years) != 1L)))

  paste(
    vapply(
      groups,
      function(group) {
        if (length(group) == 1L) {
          as.character(group)
        } else {
          paste0(group[[1L]], "\u2013", group[[length(group)]])
        }
      },
      character(1)
    ),
    collapse = ", "
  )
}

# Navbar chip summarising successfully parsed uploads. The headline uses
# Monthly Site Report years (the finance pages); the tooltip lists every
# report type, so gaps such as a missing year are visible from any page.
navbar_data_status <- function(uploads) {
  labels <- c(
    monthly_site = "Monthly Site Reports",
    site_detail = "Detailed Site Reports",
    venues_tracking = "Venues Tracking Reports"
  )

  ready <- if (is.data.frame(uploads) && nrow(uploads) > 0L) {
    uploads[uploads$status %in% "ready", , drop = FALSE]
  } else {
    NULL
  }

  if (is.null(ready) || nrow(ready) == 0L) {
    return(
      span(
        class = "navbar-data-status empty",
        bs_icon("database"),
        span(class = "navbar-data-status-text", "No data loaded")
      )
    )
  }

  years_for <- function(file_names) {
    format_year_ranges(
      regmatches(file_names, regexpr("20[0-9]{2}", file_names))
    )
  }

  details <- vapply(
    names(labels),
    function(type) {
      years <- years_for(ready$original_name[ready$report_type == type])
      paste0(labels[[type]], ": ", if (nzchar(years)) years else "none")
    },
    character(1)
  )

  headline <- years_for(ready$original_name[ready$report_type == "monthly_site"])

  if (!nzchar(headline)) {
    headline <- years_for(ready$original_name)
  }

  span(
    class = "navbar-data-status",
    title = paste(details, collapse = "\n"),
    bs_icon("database-check"),
    span(class = "navbar-data-status-text", paste("Data", headline))
  )
}
