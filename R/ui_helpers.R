# -------------------------------------------------------------------------
# Shared UI helpers
# Time-series hover labels are additional presentation detail and must
# never change the data being used for plots or KPI calculations.
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


# Subtitle line under a chart card's title.
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


# -------------------------------------------------------------------------
# Exact-value inspection for the detailed dashboard time-series charts
# -------------------------------------------------------------------------
# Base R plots are images, not interactive SVG charts. Shiny's hover object
# supplies a coordinate in plot units; match it to an *observed* period only.
# Never interpolate between years/months or invent values for missing periods.
# For multi-site lines, show each reporting category at the hovered period.
dashboard_hover_plot <- function(ns, plot_id, height = "440px") {
  shiny::tagList(
    shiny::plotOutput(
      ns(plot_id), height = height,
      hover = shiny::hoverOpts(
        id = ns(paste0(plot_id, "_hover")),
        delay = 125, delayType = "debounce"
      )
    ),
    shiny::div(
      class = "dashboard-time-series-hover-readout",
      shiny::textOutput(ns(paste0(plot_id, "_hover_value")), inline = TRUE)
    )
  )
}

# fields: named labels -> column names, e.g. c(Income = "income_actual").
# kinds: named labels -> currency, count, or percent (count is default).
# The x axis can be monthly dates, integer years, or integer bar positions.
# This helper reports the value associated with a hovered visual point;
# it never interpolates an unreported number into a data series.
dashboard_hover_text <- function(hover, data, x_col, fields,
                                 kinds = NULL, group_col = NULL,
                                 label_col = NULL, max_groups = 8L) {
  hint <- "Hover over a plotted period to inspect exact values."
  if (is.null(hover) || is.null(hover$x) || length(hover$x) != 1L) {
    return(hint)
  }
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0L ||
      !x_col %in% names(data) ||
      !all(unname(fields) %in% names(data))) {
    return(hint)
  }

  dates <- inherits(data[[x_col]], "Date")
  # Shiny typically supplies Date coordinates as day counts, but some
  # devices may return a date string. Accept either representation.
  cursor_x <- suppressWarnings(as.numeric(hover$x))
  if (dates && !is.finite(cursor_x)) {
    cursor_x <- suppressWarnings(as.numeric(as.Date(hover$x)))
  }
  if (length(cursor_x) != 1L || !is.finite(cursor_x)) return(hint)
  raw_x <- if (dates) as.numeric(as.Date(data[[x_col]])) else
    suppressWarnings(as.numeric(data[[x_col]]))
  valid <- is.finite(raw_x)
  if (!any(valid)) return(hint)

  distance <- abs(raw_x - cursor_x)
  distance[!valid] <- Inf
  nearest <- which.min(distance)
  # A bar/line must have a reported point near the mouse. In particular,
  # hovering in a year with no data must not show the nearest *other* year.
  tolerance <- if (dates) 20 else 0.45
  if (distance[[nearest]] > tolerance) return(hint)
  selected_x <- raw_x[[nearest]]
  selected <- data[valid & abs(raw_x - selected_x) < 1e-7, , drop = FALSE]
  if (nrow(selected) == 0L) return(hint)

  x_label <- if (!is.null(label_col) && label_col %in% names(selected)) {
    as.character(selected[[label_col]][[1L]])
  } else if (dates) {
    format(as.Date(selected_x, origin = "1970-01-01"), "%b %Y")
  } else {
    as.character(selected_x)
  }

  format_value <- function(value, kind) {
    value <- suppressWarnings(as.numeric(value))
    if (length(value) == 0L || !is.finite(value[[1L]])) return("Not reported")
    if (identical(kind, "currency")) {
      return(paste0(if (value[[1L]] < 0) "-" else "", "$",
                    formatC(abs(value[[1L]]), digits = 2L,
                            format = "f", big.mark = ",")))
    }
    if (identical(kind, "percent")) return(format_percentage(value[[1L]]))
    format(round(value[[1L]]), big.mark = ",", scientific = FALSE, trim = TRUE)
  }

  if (!is.null(group_col) && group_col %in% names(selected)) {
    selected <- selected[order(as.character(selected[[group_col]])), , drop = FALSE]
  }
  # Distinct sections sharing a year are not summed: preserve original
  # site/facility labels, cap the display length on narrow laptop screens.
  excess <- max(0L, nrow(selected) - max_groups)
  selected <- head(selected, max_groups)
  detail <- vapply(seq_len(nrow(selected)), function(i) {
    row <- selected[i, , drop = FALSE]
    values <- vapply(seq_along(fields), function(j) {
      name <- names(fields)[[j]]
      kind <- if (!is.null(kinds) && name %in% names(kinds)) kinds[[name]] else "count"
      paste0(name, " ", format_value(row[[unname(fields)[[j]]]], kind))
    }, character(1))
    prefix <- if (!is.null(group_col) && group_col %in% names(row)) {
      paste0(as.character(row[[group_col]][[1L]]), ": ")
    } else ""
    paste0(prefix, paste(values, collapse = " | "))
  }, character(1))
  paste(c(x_label, detail, if (excess) paste0("+", excess, " more groups")),
        collapse = "  •  ")
}
