# -------------------------------------------------------------------------
# Overview plots
# -------------------------------------------------------------------------

format_compact_currency <- function(values, digits = 1L) {
  vapply(
    values,
    function(value) {
      if (!is.finite(value)) {
        return("—")
      }

      absolute <- abs(value)
      prefix <- if (value < 0) "-$" else "$"

      if (absolute >= 1e6) {
        paste0(prefix, formatC(absolute / 1e6, digits = digits, format = "f"), "M")
      } else if (absolute >= 1e3) {
        paste0(prefix, formatC(absolute / 1e3, digits = 0, format = "f"), "K")
      } else {
        paste0(prefix, formatC(absolute, digits = 0, format = "f"))
      }
    },
    character(1)
  )
}


format_percentage <- function(values, digits = 1L) {
  vapply(
    values,
    function(value) {
      if (!is.finite(value)) {
        return("—")
      }

      paste0(
        formatC(100 * value, digits = digits, format = "f"),
        "%"
      )
    },
    character(1)
  )
}


format_percentage_change <- function(value, digits = 1L) {
  if (!is.finite(value)) {
    return("—")
  }

  prefix <- if (value > 0) "+" else if (value < 0) "−" else ""
  paste0(
    prefix,
    formatC(abs(100 * value), digits = digits, format = "f"),
    "%"
  )
}


overview_plot_limits <- function(values) {
  values <- values[is.finite(values)]

  if (length(values) == 0L) {
    return(c(-1, 1))
  }

  limits <- range(c(0, values))

  if (diff(limits) == 0) {
    padding <- max(abs(limits), 1) * 0.15
    return(limits + c(-padding, padding))
  }

  padding <- diff(limits) * 0.16
  limits + c(-padding, padding)
}

plot_monthly_income_expenses <- function(monthly_finance) {

  if (
    is.null(monthly_finance) ||
      !is.data.frame(monthly_finance) ||
      nrow(monthly_finance) == 0L
  ) {
    stop(
      "No monthly finance values are available.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "date",
    "income_actual",
    "expenses_actual"
  )

  missing_columns <- setdiff(
    required_columns,
    names(monthly_finance)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Monthly finance data is missing: ",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  monthly_finance <- monthly_finance[
    order(monthly_finance$date),
    ,
    drop = FALSE
  ]

  dates <- as.Date(monthly_finance$date)
  income <- suppressWarnings(
    as.numeric(monthly_finance$income_actual)
  )
  expenses <- suppressWarnings(
    as.numeric(monthly_finance$expenses_actual)
  )

  valid_dates <- !is.na(dates)

  if (
    !any(valid_dates) ||
      !any(is.finite(c(income, expenses)))
  ) {
    stop(
      "No finite monthly Income or Expenses values are available.",
      call. = FALSE
    )
  }

  dates <- dates[valid_dates]
  income <- income[valid_dates]
  expenses <- expenses[valid_dates]

  limits <- overview_plot_limits(
    c(income, expenses)
  )

  x_limits <- range(dates)

  if (x_limits[[1]] == x_limits[[2]]) {
    x_limits <- x_limits + c(-15, 15)
  }

  old_par <- par(
    mar = c(5.2, 5.8, 2.8, 1.2),
    xaxs = "i",
    yaxs = "i",
    family = "sans"
  )

  on.exit(
    par(old_par),
    add = TRUE
  )

  plot(
    dates,
    income,
    type = "n",
    xlim = x_limits,
    ylim = limits,
    axes = FALSE,
    xlab = "",
    ylab = ""
  )

  y_ticks <- pretty(
    limits,
    n = 5
  )

  y_ticks <- y_ticks[
    y_ticks >= limits[[1]] &
      y_ticks <= limits[[2]]
  ]

  abline(
    h = y_ticks,
    col = "#E2E8F0",
    lwd = 1
  )

  abline(
    h = 0,
    col = "#94A3B8",
    lwd = 1.1
  )

  lines(
    dates,
    income,
    col = "#2563EB",
    lwd = 2.6
  )

  points(
    dates,
    income,
    pch = 21,
    cex = 1,
    lwd = 1.8,
    col = "#2563EB",
    bg = "white"
  )

  lines(
    dates,
    expenses,
    col = "#F59E0B",
    lwd = 2.6
  )

  points(
    dates,
    expenses,
    pch = 21,
    cex = 1,
    lwd = 1.8,
    col = "#F59E0B",
    bg = "white"
  )

  x_ticks <- pretty(
    range(dates),
    n = 8
  )

  axis.Date(
    1,
    at = x_ticks,
    format = "%b\n%Y",
    tick = FALSE,
    line = -0.2,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  axis(
    2,
    at = y_ticks,
    labels = format_compact_currency(y_ticks),
    las = 1,
    tick = FALSE,
    line = -0.5,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  legend(
    "top",
    legend = c(
      "Income",
      "Expenses"
    ),
    col = c(
      "#2563EB",
      "#F59E0B"
    ),
    lty = 1,
    lwd = 2.6,
    pch = 21,
    pt.bg = "white",
    bty = "n",
    horiz = TRUE,
    cex = 0.82,
    text.col = "#475569",
    inset = c(0, -0.02),
    xpd = NA
  )

  invisible(NULL)
}

plot_annual_performance_trend <- function(
    annual_finance,
    metric
) {

  metrics <- unique(
    as.character(metric)
  )

  valid_metrics <- c(
    "Income",
    "Expenses",
    "Net Result",
    "Budget Variance"
  )

  metrics <- metrics[
    metrics %in% valid_metrics
  ]

  if (length(metrics) == 0L) {
    stop(
      "Select at least one valid performance metric.",
      call. = FALSE
    )
  }

  years <- suppressWarnings(
    as.integer(
      annual_finance$report_year
    )
  )

  series <- lapply(
    metrics,
    function(metric_name) {

      overview_metric_values(
        annual_finance,
        metric_name
      )
    }
  )

  names(series) <- metrics

  valid_year <- !is.na(years)

  years <- years[valid_year]

  series <- lapply(
    series,
    function(values) {
      values[valid_year]
    }
  )

  finite_any <- Reduce(
    `|`,
    lapply(
      series,
      is.finite
    )
  )

  years <- years[finite_any]

  series <- lapply(
    series,
    function(values) {
      values[finite_any]
    }
  )

  if (
    length(years) == 0L ||
      !any(
        is.finite(
          unlist(series)
        )
      )
  ) {
    stop(
      "No finite values are available for the selected metric.",
      call. = FALSE
    )
  }

  order_index <- order(years)

  years <- years[order_index]

  series <- lapply(
    series,
    function(values) {
      values[order_index]
    }
  )

  all_values <- unlist(
    series,
    use.names = FALSE
  )

  limits <- overview_plot_limits(
    all_values
  )

  x_limits <- range(years)

  if (diff(x_limits) == 0) {
    x_limits <-
      x_limits +
      c(-0.5, 0.5)
  } else {
    x_limits <-
      x_limits +
      c(-0.25, 0.25)
  }

  top_margin <- if (
    length(metrics) > 1L
  ) {
    4.5
  } else {
    1.2
  }

  old_par <- par(
    mar = c(
      3.8,
      5.8,
      top_margin,
      1.2
    ),
    xaxs = "i",
    yaxs = "i",
    family = "sans"
  )

  on.exit(
    par(old_par),
    add = TRUE
  )

  plot(
    years,
    rep(NA_real_, length(years)),
    type = "n",
    xlim = x_limits,
    ylim = limits,
    axes = FALSE,
    xlab = "",
    ylab = ""
  )

  ticks <- pretty(
    limits,
    n = 5
  )

  ticks <- ticks[
    ticks >= limits[[1L]] &
      ticks <= limits[[2L]]
  ]

  abline(
    h = ticks,
    col = "#E2E8F0",
    lwd = 1
  )

  abline(
    h = 0,
    col = "#94A3B8",
    lwd = 1.1
  )

  colours <- if (
    length(metrics) == 1L
  ) {

    "#2563EB"

  } else {

    grDevices::hcl.colors(
      length(metrics),
      palette = "Dark 3"
    )
  }

  if (
    length(metrics) == 1L &&
      length(years) > 1L
  ) {

    values <- series[[1L]]

    polygon(
      c(
        years,
        rev(years)
      ),
      c(
        values,
        rep(
          0,
          length(values)
        )
      ),
      border = NA,
      col =
        grDevices::adjustcolor(
          colours[[1L]],
          alpha.f = 0.08
        )
    )
  }

  for (
    index in seq_along(metrics)
  ) {

    values <- series[[index]]

    if (
      sum(
        is.finite(values)
      ) > 1L
    ) {

      lines(
        years,
        values,
        col = colours[[index]],
        lwd = 2.6
      )
    }

    points(
      years,
      values,
      pch = 21,
      cex = 1.1,
      lwd = 1.8,
      col = colours[[index]],
      bg = "white"
    )
  }

  axis(
    1,
    at = years,
    labels = years,
    tick = FALSE,
    line = -0.3,
    col.axis = "#64748B",
    cex.axis = 0.88
  )

  axis(
    2,
    at = ticks,
    labels =
      format_compact_currency(
        ticks
      ),
    las = 1,
    tick = FALSE,
    line = -0.5,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  if (length(metrics) == 1L) {

    values <- series[[1L]]

    label_offset <-
      diff(limits) * 0.045

    label_y <-
      values +
      ifelse(
        values >= 0,
        label_offset,
        -label_offset
      )

    text(
      years,
      label_y,
      labels =
        format_compact_currency(
          values
        ),
      pos =
        ifelse(
          values >= 0,
          3,
          1
        ),
      offset = 0.15,
      cex = 0.8,
      col = "#334155",
      xpd = NA
    )

  } else {

    legend(
      "top",
      legend = metrics,
      col = colours,
      lty = 1,
      lwd = 2.6,
      pch = 21,
      pt.bg = "white",
      bty = "n",
      horiz = TRUE,
      cex = 0.76,
      text.col = "#475569",
      inset = c(
        0,
        -0.03
      ),
      xpd = NA
    )
  }

  invisible(NULL)
}

plot_budget_vs_actual_net_result <- function(annual_finance) {
  values <- rbind(
    Budget = annual_finance$net_budget,
    Actual = annual_finance$net_actual
  )
  keep <- colSums(is.finite(values)) == nrow(values)
  values <- values[, keep, drop = FALSE]
  years <- annual_finance$report_year[keep]

  if (length(years) == 0L) {
    stop("No complete Budget and Actual values are available.", call. = FALSE)
  }

  limits <- overview_plot_limits(as.numeric(values))
  old_par <- par(
    mar = c(3.8, 5.8, 2.5, 1.2),
    xaxs = "i",
    yaxs = "i",
    family = "sans"
  )
  on.exit(par(old_par), add = TRUE)

  ticks <- pretty(limits, n = 5)
  ticks <- ticks[ticks >= limits[[1]] & ticks <= limits[[2]]]

  positions <- barplot(
    values,
    beside = TRUE,
    plot = FALSE,
    space = c(0.18, 0.75)
  )

  x_limits <- range(positions) + c(-0.65, 0.65)
  plot(
    NA,
    xlim = x_limits,
    ylim = limits,
    axes = FALSE,
    xlab = "",
    ylab = "",
    type = "n"
  )

  abline(h = ticks, col = "#E2E8F0", lwd = 1)
  abline(h = 0, col = "#94A3B8", lwd = 1.1)
  barplot(
    values,
    beside = TRUE,
    col = c("#CBD5E1", "#2563EB"),
    border = NA,
    axes = FALSE,
    axisnames = FALSE,
    space = c(0.18, 0.75),
    add = TRUE
  )
  axis(
    1,
    at = colMeans(positions),
    labels = years,
    tick = FALSE,
    line = -0.3,
    col.axis = "#64748B",
    cex.axis = 0.88
  )
  axis(
    2,
    at = ticks,
    labels = format_compact_currency(ticks),
    las = 1,
    tick = FALSE,
    line = -0.5,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  label_offset <- diff(limits) * 0.035
  label_y <- values + ifelse(values >= 0, label_offset, -label_offset)

  text(
    positions,
    label_y,
    labels = matrix(format_compact_currency(as.numeric(values)), nrow = 2L),
    pos = ifelse(values >= 0, 3, 1),
    offset = 0.1,
    cex = 0.66,
    col = "#334155",
    xpd = NA
  )

  legend(
    "topright",
    legend = rownames(values),
    fill = c("#CBD5E1", "#2563EB"),
    border = NA,
    bty = "n",
    horiz = TRUE,
    cex = 0.78,
    text.col = "#475569",
    inset = c(0, 0),
    xpd = FALSE
  )

  invisible(NULL)
}

plot_monthly_budget_vs_actual_net_result <- function(monthly_finance) {

  if (
    is.null(monthly_finance) ||
      !is.data.frame(monthly_finance) ||
      nrow(monthly_finance) == 0L
  ) {
    stop(
      "No monthly finance values are available.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "date",
    "net_budget",
    "net_actual"
  )

  missing_columns <- setdiff(
    required_columns,
    names(monthly_finance)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Monthly finance data is missing: ",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  monthly_finance <- monthly_finance[
    order(monthly_finance$date),
    ,
    drop = FALSE
  ]

  dates <- as.Date(monthly_finance$date)

  budget <- suppressWarnings(
    as.numeric(monthly_finance$net_budget)
  )

  actual <- suppressWarnings(
    as.numeric(monthly_finance$net_actual)
  )

  complete <- !is.na(dates) &
    is.finite(budget) &
    is.finite(actual)

  dates <- dates[complete]
  budget <- budget[complete]
  actual <- actual[complete]

  if (length(dates) == 0L) {
    stop(
      "No complete monthly Budget and Actual values are available.",
      call. = FALSE
    )
  }

  limits <- overview_plot_limits(
    c(budget, actual)
  )

  x_limits <- range(dates)

  if (x_limits[[1]] == x_limits[[2]]) {
    x_limits <- x_limits + c(-15, 15)
  }

  old_par <- par(
    mar = c(5.2, 5.8, 2.8, 1.2),
    xaxs = "i",
    yaxs = "i",
    family = "sans"
  )

  on.exit(
    par(old_par),
    add = TRUE
  )

  plot(
    dates,
    actual,
    type = "n",
    xlim = x_limits,
    ylim = limits,
    axes = FALSE,
    xlab = "",
    ylab = ""
  )

  y_ticks <- pretty(
    limits,
    n = 5
  )

  y_ticks <- y_ticks[
    y_ticks >= limits[[1]] &
      y_ticks <= limits[[2]]
  ]

  abline(
    h = y_ticks,
    col = "#E2E8F0",
    lwd = 1
  )

  abline(
    h = 0,
    col = "#94A3B8",
    lwd = 1.1
  )

  # Budget is deliberately muted so Actual remains the main focus.
  lines(
    dates,
    budget,
    col = "#94A3B8",
    lwd = 2.4,
    lty = 2
  )

  points(
    dates,
    budget,
    pch = 21,
    cex = 0.95,
    lwd = 1.6,
    col = "#94A3B8",
    bg = "white"
  )

  lines(
    dates,
    actual,
    col = "#2563EB",
    lwd = 2.8
  )

  points(
    dates,
    actual,
    pch = 21,
    cex = 1,
    lwd = 1.8,
    col = "#2563EB",
    bg = "white"
  )

  x_ticks <- pretty(
    range(dates),
    n = 8
  )

  axis.Date(
    1,
    at = x_ticks,
    format = "%b\n%Y",
    tick = FALSE,
    line = -0.2,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  axis(
    2,
    at = y_ticks,
    labels = format_compact_currency(y_ticks),
    las = 1,
    tick = FALSE,
    line = -0.5,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  legend(
    "top",
    legend = c(
      "Budget",
      "Actual"
    ),
    col = c(
      "#94A3B8",
      "#2563EB"
    ),
    lty = c(2, 1),
    lwd = c(2.4, 2.8),
    pch = 21,
    pt.bg = "white",
    bty = "n",
    horiz = TRUE,
    cex = 0.82,
    text.col = "#475569",
    inset = c(0, -0.02),
    xpd = NA
  )

  invisible(NULL)
}

plot_revenue_capture_over_time <- function(annual_revenue) {
  complete <- is.finite(annual_revenue$actual_income) &
    is.finite(annual_revenue$discount_opportunity) &
    is.finite(annual_revenue$capture_rate)
  annual_revenue <- annual_revenue[complete, , drop = FALSE]

  if (nrow(annual_revenue) == 0L) {
    stop("No complete revenue opportunity values are available.", call. = FALSE)
  }

  values <- rbind(
    "Actual Income" = annual_revenue$actual_income,
    "Discount Opportunity" = annual_revenue$discount_opportunity
  )
  years <- annual_revenue$report_year
  limits <- overview_plot_limits(colSums(values))
  rate_limits <- range(c(0, annual_revenue$capture_rate, 1), na.rm = TRUE)

  old_par <- par(
    mar = c(4, 5.8, 2.3, 5.2),
    xaxs = "i",
    yaxs = "i",
    family = "sans"
  )
  on.exit(par(old_par), add = TRUE)

  positions <- barplot(
    values,
    plot = FALSE,
    space = 0.75
  )
  x_limits <- range(positions) + c(-0.75, 0.75)
  ticks <- pretty(limits, n = 5)
  ticks <- ticks[ticks >= limits[[1]] & ticks <= limits[[2]]]

  plot(
    NA,
    xlim = x_limits,
    ylim = limits,
    axes = FALSE,
    xlab = "",
    ylab = "",
    type = "n"
  )
  abline(h = ticks, col = "#E2E8F0", lwd = 1)
  abline(h = 0, col = "#94A3B8", lwd = 1.1)
  barplot(
    values,
    col = c("#2563EB", "#F59E0B"),
    border = NA,
    axes = FALSE,
    axisnames = FALSE,
    space = 0.75,
    add = TRUE
  )
  axis(
    1,
    at = positions,
    labels = years,
    tick = FALSE,
    line = -0.3,
    col.axis = "#64748B",
    cex.axis = 0.9
  )
  axis(
    2,
    at = ticks,
    labels = format_compact_currency(ticks),
    las = 1,
    tick = FALSE,
    line = -0.5,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  potential <- colSums(values)
  label_offset <- diff(limits) * 0.035
  text(
    positions,
    potential + label_offset,
    labels = format_compact_currency(potential),
    cex = 0.76,
    col = "#334155",
    xpd = NA
  )

  par(new = TRUE)
  plot(
    positions,
    annual_revenue$capture_rate,
    type = "o",
    pch = 21,
    lwd = 2.4,
    cex = 1.15,
    col = "#0F766E",
    bg = "white",
    xlim = x_limits,
    ylim = rate_limits,
    axes = FALSE,
    xlab = "",
    ylab = ""
  )
  rate_ticks <- pretty(rate_limits, n = 5)
  rate_ticks <- rate_ticks[
    rate_ticks >= rate_limits[[1]] & rate_ticks <= rate_limits[[2]]
  ]
  axis(
    4,
    at = rate_ticks,
    labels = format_percentage(rate_ticks, digits = 0L),
    las = 1,
    tick = FALSE,
    line = -0.5,
    col.axis = "#0F766E",
    cex.axis = 0.82
  )
  text(
    positions,
    annual_revenue$capture_rate,
    labels = format_percentage(annual_revenue$capture_rate),
    pos = 1,
    offset = 0.8,
    cex = 0.72,
    col = "#0F766E",
    xpd = NA
  )

  legend(
    "top",
    legend = c("Actual Income", "Discount Opportunity", "Capture Rate"),
    fill = c("#2563EB", "#F59E0B", NA),
    border = NA,
    lty = c(NA, NA, 1),
    lwd = c(NA, NA, 2.4),
    pch = c(15, 15, 21),
    pt.bg = c("#2563EB", "#F59E0B", "white"),
    col = c("#2563EB", "#F59E0B", "#0F766E"),
    bty = "n",
    horiz = TRUE,
    cex = 0.78,
    text.col = "#475569",
    inset = c(0, -0.03),
    xpd = NA
  )

  invisible(NULL)
}

# -------------------------------------------------------------------------
# Site Comparison plots
# -------------------------------------------------------------------------

plot_site_performance_over_time <- function(
    site_finance,
    metric
) {

  if (
    is.null(site_finance) ||
      !is.data.frame(site_finance) ||
      nrow(site_finance) == 0L
  ) {
    stop(
      "No site finance values are available.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "report_year",
    "site"
  )

  missing_columns <- setdiff(
    required_columns,
    names(site_finance)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Site finance data is missing: ",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  values <- site_finance_values(
    site_finance,
    metric,
    "Actual"
  )

  years <- suppressWarnings(
    as.integer(site_finance$report_year)
  )

  sites <- trimws(
    as.character(site_finance$site)
  )

  keep <- is.finite(values) &
    !is.na(years) &
    !is.na(sites) &
    nzchar(sites)

  values <- values[keep]
  years <- years[keep]
  sites <- sites[keep]

  if (length(values) == 0L) {
    stop(
      paste0(
        "No finite Actual ",
        metric,
        " values are available."
      ),
      call. = FALSE
    )
  }

  plot_data <- data.frame(
    report_year = years,
    site = sites,
    value = values,
    stringsAsFactors = FALSE
  )

  site_names <- sort(
    unique(plot_data$site)
  )

  year_values <- sort(
    unique(plot_data$report_year)
  )

  limits <- overview_plot_limits(
    plot_data$value
  )

  x_limits <- range(
    year_values
  )

  if (length(year_values) == 1L) {
    x_limits <- x_limits + c(-0.5, 0.5)
  } else {
    x_limits <- x_limits + c(-0.25, 0.25)
  }

  colours <- grDevices::hcl.colors(
    length(site_names),
    palette = "Dark 3"
  )

  old_par <- par(
    mar = c(4.2, 5.8, 5.5, 1.2),
    xaxs = "i",
    yaxs = "i",
    family = "sans"
  )

  on.exit(
    par(old_par),
    add = TRUE
  )

  plot(
    NA,
    xlim = x_limits,
    ylim = limits,
    axes = FALSE,
    xlab = "",
    ylab = "",
    type = "n"
  )

  y_ticks <- pretty(
    limits,
    n = 5
  )

  y_ticks <- y_ticks[
    y_ticks >= limits[[1]] &
      y_ticks <= limits[[2]]
  ]

  abline(
    h = y_ticks,
    col = "#E2E8F0",
    lwd = 1
  )

  abline(
    h = 0,
    col = "#94A3B8",
    lwd = 1.1
  )

  for (index in seq_along(site_names)) {

    site_name <- site_names[[index]]

    site_rows <- plot_data[
      plot_data$site == site_name,
      ,
      drop = FALSE
    ]

    site_rows <- site_rows[
      order(site_rows$report_year),
      ,
      drop = FALSE
    ]

    if (nrow(site_rows) > 1L) {
      lines(
        site_rows$report_year,
        site_rows$value,
        col = colours[[index]],
        lwd = 2.4
      )
    }

    points(
      site_rows$report_year,
      site_rows$value,
      pch = 21,
      cex = 1.05,
      lwd = 1.8,
      col = colours[[index]],
      bg = "white"
    )
  }

  axis(
    1,
    at = year_values,
    labels = year_values,
    tick = FALSE,
    line = -0.3,
    col.axis = "#64748B",
    cex.axis = 0.88
  )

  axis(
    2,
    at = y_ticks,
    labels = format_compact_currency(y_ticks),
    las = 1,
    tick = FALSE,
    line = -0.5,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  legend(
    "top",
    legend = site_names,
    col = colours,
    lty = 1,
    lwd = 2.4,
    pch = 21,
    pt.bg = "white",
    bty = "n",
    ncol = min(3L, length(site_names)),
    cex = 0.76,
    text.col = "#475569",
    inset = c(0, -0.03),
    xpd = NA
  )

  invisible(NULL)
}

plot_site_ranking <- function(
    site_finance,
    metric
) {

  if (
    is.null(site_finance) ||
      !is.data.frame(site_finance) ||
      nrow(site_finance) == 0L
  ) {
    stop(
      "No site finance values are available.",
      call. = FALSE
    )
  }

  if (!"site" %in% names(site_finance)) {
    stop(
      "Site finance data is missing: site",
      call. = FALSE
    )
  }

  values <- site_finance_values(
    site_finance,
    metric,
    "Actual"
  )

  sites <- trimws(
    as.character(site_finance$site)
  )

  keep <- is.finite(values) &
    !is.na(sites) &
    nzchar(sites)

  values <- values[keep]
  sites <- sites[keep]

  if (length(values) == 0L) {
    stop(
      paste0(
        "No finite Actual ",
        metric,
        " values are available."
      ),
      call. = FALSE
    )
  }

  plot_data <- data.frame(
    site = sites,
    value = values,
    stringsAsFactors = FALSE
  )

  # Highest-performing site appears at the top.
  plot_data <- plot_data[
    order(
      plot_data$value,
      decreasing = FALSE
    ),
    ,
    drop = FALSE
  ]

  max_label_width <- max(
    nchar(plot_data$site),
    na.rm = TRUE
  )

  left_margin <- max(
    6.5,
    min(
      10,
      max_label_width * 0.45
    )
  )

  old_par <- par(
    mar = c(
      4,
      left_margin,
      3,
      1.5
    ),
    family = "sans"
  )

  on.exit(
    par(old_par),
    add = TRUE
  )

  value_range <- range(
    c(
      0,
      plot_data$value
    ),
    na.rm = TRUE
  )

  padding <- diff(value_range) * 0.18

  if (
    !is.finite(padding) ||
      padding == 0
  ) {
    padding <- max(
      abs(plot_data$value),
      1
    ) * 0.18
  }

  x_limits <- c(
    value_range[[1]] -
      if (value_range[[1]] < 0) padding else 0,
    value_range[[2]] + padding
  )

  bar_positions <- barplot(
    plot_data$value,
    names.arg = plot_data$site,
    horiz = TRUE,
    las = 1,
    border = NA,
    col = "#2563EB",
    xlim = x_limits,
    axes = FALSE,
    xlab = ""
  )

  x_ticks <- pretty(
    x_limits,
    n = 5
  )

  abline(
    v = x_ticks,
    col = "#E2E8F0",
    lwd = 1
  )

  abline(
    v = 0,
    col = "#94A3B8",
    lwd = 1.1
  )

  # Redraw bars so grid lines remain behind them.
  barplot(
    plot_data$value,
    names.arg = plot_data$site,
    horiz = TRUE,
    las = 1,
    border = NA,
    col = "#2563EB",
    xlim = x_limits,
    axes = FALSE,
    xlab = "",
    add = TRUE
  )

  axis(
    1,
    at = x_ticks,
    labels = format_compact_currency(
      x_ticks
    ),
    tick = FALSE,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  label_offset <- diff(x_limits) * 0.015

  for (index in seq_along(bar_positions)) {

    value <- plot_data$value[[index]]

    if (value >= 0) {

      text(
        value + label_offset,
        bar_positions[[index]],
        labels = format_compact_currency(
          value
        ),
        pos = 4,
        cex = 0.78,
        col = "#475569",
        xpd = NA
      )

    } else {

      text(
        value - label_offset,
        bar_positions[[index]],
        labels = format_compact_currency(
          value
        ),
        pos = 2,
        cex = 0.78,
        col = "#475569",
        xpd = NA
      )
    }
  }

  invisible(NULL)
}

plot_site_budget_performance <- function(
    site_finance,
    metric
) {

  if (
    is.null(site_finance) ||
      !is.data.frame(site_finance) ||
      nrow(site_finance) == 0L
  ) {
    stop(
      "No site finance values are available.",
      call. = FALSE
    )
  }

  if (!"site" %in% names(site_finance)) {
    stop(
      "Site finance data is missing: site",
      call. = FALSE
    )
  }

  actual <- site_finance_values(
    site_finance,
    metric,
    "Actual"
  )

  budget <- site_finance_values(
    site_finance,
    metric,
    "Budget"
  )

  sites <- trimws(
    as.character(site_finance$site)
  )

  # A Budget vs Actual comparison is meaningful only when both values
  # are available for the site.
  keep <- is.finite(actual) &
    is.finite(budget) &
    !is.na(sites) &
    nzchar(sites)

  actual <- actual[keep]
  budget <- budget[keep]
  sites <- sites[keep]

  if (length(actual) == 0L) {
    stop(
      paste0(
        "No complete Budget and Actual ",
        metric,
        " values are available."
      ),
      call. = FALSE
    )
  }

  plot_data <- data.frame(
    site = sites,
    budget = budget,
    actual = actual,
    stringsAsFactors = FALSE
  )

  # Sort by Actual value so the strongest result appears at the top.
  plot_data <- plot_data[
    order(
      plot_data$actual,
      decreasing = FALSE
    ),
    ,
    drop = FALSE
  ]

  values <- rbind(
    Budget = plot_data$budget,
    Actual = plot_data$actual
  )

  value_range <- range(
    c(
      0,
      values
    ),
    na.rm = TRUE
  )

  padding <- diff(value_range) * 0.16

  if (
    !is.finite(padding) ||
      padding == 0
  ) {
    padding <- max(
      abs(values),
      1
    ) * 0.16
  }

  x_limits <- c(
    value_range[[1]] -
      if (value_range[[1]] < 0) padding else 0,
    value_range[[2]] + padding
  )

  max_label_width <- max(
    nchar(plot_data$site),
    na.rm = TRUE
  )

  left_margin <- max(
    8,
    min(
      15,
      max_label_width * 0.65
    )
  )

  old_par <- par(
    mar = c(
      4.5,
      left_margin,
      4.2,
      2.5
    ),
    family = "sans"
  )

  on.exit(
    par(old_par),
    add = TRUE
  )

  positions <- barplot(
    values,
    beside = TRUE,
    horiz = TRUE,
    names.arg = plot_data$site,
    las = 1,
    border = NA,
    col = c(
      "#94A3B8",
      "#2563EB"
    ),
    axes = FALSE,
    xlim = x_limits,
    xlab = ""
  )

  x_ticks <- pretty(
    x_limits,
    n = 5
  )

  axis(
    1,
    at = x_ticks,
    labels = format_compact_currency(
      x_ticks
    ),
    tick = FALSE,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  abline(
    v = 0,
    col = "#64748B",
    lwd = 1
  )

  legend(
    "top",
    legend = c(
      "Budget",
      "Actual"
    ),
    fill = c(
      "#94A3B8",
      "#2563EB"
    ),
    border = NA,
    bty = "n",
    horiz = TRUE,
    cex = 0.82,
    text.col = "#475569",
    inset = c(0, -0.08),
    xpd = NA
  )

  label_offset <- diff(x_limits) * 0.012

  for (site_index in seq_len(ncol(values))) {

    for (series_index in seq_len(nrow(values))) {

      value <- values[
        series_index,
        site_index
      ]

      position <- positions[
        series_index,
        site_index
      ]

      if (value >= 0) {

        text(
          value + label_offset,
          position,
          labels = format_compact_currency(
            value
          ),
          pos = 4,
          cex = 0.72,
          col = "#475569",
          xpd = NA
        )

      } else {

        text(
          value - label_offset,
          position,
          labels = format_compact_currency(
            value
          ),
          pos = 2,
          cex = 0.72,
          col = "#475569",
          xpd = NA
        )
      }
    }
  }

  invisible(NULL)
}

plot_site_revenue_opportunity <- function(
    site_revenue
) {

  if (
    is.null(site_revenue) ||
      !is.data.frame(site_revenue) ||
      nrow(site_revenue) == 0L
  ) {
    stop(
      "No site revenue values are available.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "site",
    "actual_income",
    "discount_opportunity",
    "potential_revenue",
    "capture_rate"
  )

  missing_columns <- setdiff(
    required_columns,
    names(site_revenue)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Site revenue data is missing: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  actual_income <- as.numeric(
    site_revenue$actual_income
  )

  discount_opportunity <- as.numeric(
    site_revenue$discount_opportunity
  )

  potential_revenue <- as.numeric(
    site_revenue$potential_revenue
  )

  capture_rate <- as.numeric(
    site_revenue$capture_rate
  )

  sites <- trimws(
    as.character(
      site_revenue$site
    )
  )

  keep <- is.finite(actual_income) &
    is.finite(discount_opportunity) &
    is.finite(potential_revenue) &
    !is.na(sites) &
    nzchar(sites)

  actual_income <- actual_income[keep]
  discount_opportunity <- discount_opportunity[keep]
  potential_revenue <- potential_revenue[keep]
  capture_rate <- capture_rate[keep]
  sites <- sites[keep]

  if (length(actual_income) == 0L) {
    stop(
      "No complete site revenue values are available.",
      call. = FALSE
    )
  }

  opportunity_rate <- ifelse(
    potential_revenue == 0,
    NA_real_,
    discount_opportunity /
      potential_revenue
  )

  plot_data <- data.frame(
    site = sites,
    actual_income = actual_income,
    discount_opportunity = discount_opportunity,
    potential_revenue = potential_revenue,
    capture_rate = capture_rate,
    opportunity_rate = opportunity_rate,
    stringsAsFactors = FALSE
  )

  # Lowest opportunity rate is drawn first, so the largest
  # revenue opportunity appears at the top of the horizontal chart.
  plot_data <- plot_data[
    order(
      plot_data$opportunity_rate,
      na.last = TRUE
    ),
    ,
    drop = FALSE
  ]

  values <- rbind(
    "Actual Income" =
      plot_data$actual_income,
    "Discount Opportunity" =
      plot_data$discount_opportunity
  )

  max_value <- max(
    plot_data$potential_revenue,
    na.rm = TRUE
  )

  x_limit <- if (
    !is.finite(max_value) ||
      max_value <= 0
  ) {
    1
  } else {
    max_value * 1.28
  }

  max_label_width <- max(
    nchar(plot_data$site),
    na.rm = TRUE
  )

  left_margin <- max(
    8,
    min(
      15,
      max_label_width * 0.65
    )
  )

  old_par <- par(
    mar = c(
      4.5,
      left_margin,
      4.2,
      2.5
    ),
    family = "sans"
  )

  on.exit(
    par(old_par),
    add = TRUE
  )

  positions <- barplot(
    values,
    horiz = TRUE,
    beside = FALSE,
    names.arg = plot_data$site,
    las = 1,
    border = NA,
    col = c(
      "#2563EB",
      "#F59E0B"
    ),
    axes = FALSE,
    xlim = c(
      0,
      x_limit
    ),
    xlab = ""
  )

  x_ticks <- pretty(
    c(
      0,
      max_value
    ),
    n = 5
  )

  axis(
    1,
    at = x_ticks,
    labels = format_compact_currency(
      x_ticks
    ),
    tick = FALSE,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  abline(
    v = x_ticks,
    col = "#E2E8F0",
    lwd = 1
  )

  barplot(
    values,
    horiz = TRUE,
    beside = FALSE,
    names.arg = plot_data$site,
    las = 1,
    border = NA,
    col = c(
      "#2563EB",
      "#F59E0B"
    ),
    axes = FALSE,
    xlim = c(
      0,
      x_limit
    ),
    add = TRUE
  )

  legend(
    "top",
    legend = c(
      "Actual Income",
      "Discount Opportunity"
    ),
    fill = c(
      "#2563EB",
      "#F59E0B"
    ),
    border = NA,
    bty = "n",
    horiz = TRUE,
    cex = 0.82,
    text.col = "#475569",
    inset = c(
      0,
      -0.08
    ),
    xpd = NA
  )

  label_offset <- x_limit * 0.015

  for (index in seq_along(positions)) {

    opportunity_label <- if (
      is.finite(
        plot_data$opportunity_rate[[index]]
      )
    ) {
      paste0(
        " · ",
        sprintf(
          "%.1f%% opportunity",
          100 *
            plot_data$opportunity_rate[[index]]
        )
      )
    } else {
      ""
    }

    text(
      plot_data$potential_revenue[[index]] +
        label_offset,
      positions[[index]],
      labels = paste0(
        format_compact_currency(
          plot_data$potential_revenue[[index]]
        ),
        opportunity_label
      ),
      pos = 4,
      cex = 0.72,
      col = "#475569",
      xpd = NA
    )
  }

  invisible(NULL)
}

plot_site_discount_mix <- function(
    site_discounts
) {

  if (
    is.null(site_discounts) ||
      !is.data.frame(site_discounts) ||
      nrow(site_discounts) == 0L
  ) {
    stop(
      "No site discount values are available.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "site",
    "student_discount",
    "staff_discount",
    "other_discount",
    "total_discount"
  )

  missing_columns <- setdiff(
    required_columns,
    names(site_discounts)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Site discount data is missing: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  sites <- trimws(
    as.character(
      site_discounts$site
    )
  )

  student <- as.numeric(
    site_discounts$student_discount
  )

  staff <- as.numeric(
    site_discounts$staff_discount
  )

  other <- as.numeric(
    site_discounts$other_discount
  )

  total <- as.numeric(
    site_discounts$total_discount
  )

  keep <- !is.na(sites) &
    nzchar(sites) &
    is.finite(student) &
    is.finite(staff) &
    is.finite(other) &
    is.finite(total) &
    total > 0

  sites <- sites[keep]
  student <- student[keep]
  staff <- staff[keep]
  other <- other[keep]
  total <- total[keep]

  if (length(sites) == 0L) {
    stop(
      "No complete site discount mix values are available.",
      call. = FALSE
    )
  }

  proportions <- rbind(
    Student = student / total,
    Staff = staff / total,
    Other = other / total
  )

  # Order sites by total discount opportunity.
  order_index <- order(
    total,
    decreasing = FALSE
  )

  sites <- sites[order_index]
  total <- total[order_index]
  proportions <- proportions[
    ,
    order_index,
    drop = FALSE
  ]

  max_label_width <- max(
    nchar(sites),
    na.rm = TRUE
  )

  left_margin <- max(
    6.5,
    min(
      10,
      max_label_width * 0.45
    )
  )

  old_par <- par(
    mar = c(
      4,
      left_margin,
      3,
      1.5
    ),
    family = "sans"
  )

  on.exit(
    par(old_par),
    add = TRUE
  )

  positions <- barplot(
    proportions,
    horiz = TRUE,
    beside = FALSE,
    names.arg = sites,
    las = 1,
    border = NA,
    col = c(
      "#2563EB",
      "#0F766E",
      "#F59E0B"
    ),
    axes = FALSE,
    xlim = c(
      0,
      1
    )
  )

  ticks <- seq(
    0,
    1,
    by = 0.25
  )

  axis(
    1,
    at = ticks,
    labels = paste0(
      round(
        ticks * 100
      ),
      "%"
    ),
    tick = FALSE,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  abline(
    v = ticks,
    col = "#E2E8F0",
    lwd = 1
  )

  barplot(
    proportions,
    horiz = TRUE,
    beside = FALSE,
    names.arg = sites,
    las = 1,
    border = NA,
    col = c(
      "#2563EB",
      "#0F766E",
      "#F59E0B"
    ),
    axes = FALSE,
    xlim = c(
      0,
      1
    ),
    add = TRUE
  )

  legend(
    "top",
    legend = c(
      "Student",
      "Staff",
      "Other"
    ),
    fill = c(
      "#2563EB",
      "#0F766E",
      "#F59E0B"
    ),
    border = NA,
    bty = "n",
    horiz = TRUE,
    cex = 0.8,
    text.col = "#475569",
    inset = c(
      0,
      -0.08
    ),
    xpd = NA
  )

  # Add percentage labels only where the segment is large enough
  # to remain readable.
  cumulative <- apply(
    proportions,
    2,
    cumsum
  )

  starts <- rbind(
    rep(
      0,
      ncol(proportions)
    ),
    cumulative[
      -nrow(cumulative),
      ,
      drop = FALSE
    ]
  )

  centres <- starts +
    proportions / 2

  for (row_index in seq_len(nrow(proportions))) {

    for (column_index in seq_len(ncol(proportions))) {

      share <- proportions[
        row_index,
        column_index
      ]

      if (
        is.finite(share) &&
          share >= 0.03
      ) {

        text(
          centres[
            row_index,
            column_index
          ],
          positions[[column_index]],
          labels = paste0(
            round(
              share * 100
            ),
            "%"
          ),
          cex = 0.7,
          col = "white"
        )
      }
    }
  }

  invisible(NULL)
}

plot_facility_ranking <- function(
    facility_finance,
    metric
) {

  if (
    is.null(facility_finance) ||
      !is.data.frame(facility_finance) ||
      nrow(facility_finance) == 0L
  ) {
    stop(
      "No facility finance values are available.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "business_unit",
    "income_actual",
    "expenses_actual",
    "net_actual"
  )

  missing_columns <- setdiff(
    required_columns,
    names(facility_finance)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Facility finance data is missing: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  values <- facility_finance_values(
    facility_finance,
    metric,
    "Actual"
  )

  labels <- trimws(
    as.character(
      facility_finance$business_unit
    )
  )

  keep <- is.finite(values) &
    !is.na(labels) &
    nzchar(labels)

  values <- values[keep]
  labels <- labels[keep]

  if (length(values) == 0L) {
    stop(
      paste(
        "No complete facility values are available",
        "for the selected metric."
      ),
      call. = FALSE
    )
  }

  # Horizontal barplots draw the final row at the top.
  # Ascending order therefore places the highest value at the top.
  order_index <- order(
    values,
    decreasing = FALSE
  )

  values <- values[
    order_index
  ]

  labels <- labels[
    order_index
  ]

  limits <- overview_plot_limits(
    values
  )

  # Wrap long facility/business-unit names so they fit
  # comfortably inside the half-width dashboard card.
  wrapped_labels <- vapply(
    labels,
    function(label) {

      # Allow names containing "/" to wrap naturally.
      label <- gsub(
        "/",
        "/ ",
        label,
        fixed = TRUE
      )

      paste(
        strwrap(
          label,
          width = 18
        ),
        collapse = "\n"
      )
    },
    character(1)
  )

  label_lines <- unlist(
    strsplit(
      wrapped_labels,
      "\n",
      fixed = TRUE
    )
  )

  max_label_width <- max(
    nchar(label_lines),
    na.rm = TRUE
  )

  left_margin <- max(
    5.5,
    min(
      8,
      max_label_width * 0.45
    )
  )

  old_par <- par(
    mar = c(
      4,
      left_margin,
      2.5,
      1.5
    ),
    family = "sans"
  )

  on.exit(
    par(old_par),
    add = TRUE
  )

  positions <- barplot(
    values,
    horiz = TRUE,
    names.arg = wrapped_labels,
    las = 1,
    cex.names = 0.78,
    border = NA,
    col = "#2563EB",
    axes = FALSE,
    xlim = limits
  )

  ticks <- pretty(
    limits,
    n = 5
  )

  ticks <- ticks[
    ticks >= limits[[1]] &
      ticks <= limits[[2]]
  ]

  axis(
    1,
    at = ticks,
    labels = format_compact_currency(
      ticks
    ),
    tick = FALSE,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  abline(
    v = ticks,
    col = "#E2E8F0",
    lwd = 1
  )

  abline(
    v = 0,
    col = "#94A3B8",
    lwd = 1.1
  )

  # Redraw bars so grid lines remain behind them.
  barplot(
    values,
    horiz = TRUE,
    names.arg = wrapped_labels,
    las = 1,
    cex.names = 0.78,
    border = NA,
    col = "#2563EB",
    axes = FALSE,
    xlim = limits,
    add = TRUE
  )

  label_offset <- diff(
    limits
  ) * 0.02

  text(
    values +
      ifelse(
        values >= 0,
        label_offset,
        -label_offset
      ),
    positions,
    labels = format_compact_currency(
      values
    ),
    pos = ifelse(
      values >= 0,
      4,
      2
    ),
    cex = 0.72,
    col = "#334155",
    xpd = NA
  )

  invisible(NULL)
}

# -------------------------------------------------------------------------
# Facility Discount Mix
# -------------------------------------------------------------------------

plot_facility_discount_mix <- function(
    facility_discounts
) {

  if (
    is.null(facility_discounts) ||
      !is.data.frame(facility_discounts) ||
      nrow(facility_discounts) == 0L
  ) {
    stop(
      "No facility discount values are available.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "business_unit",
    "student_discount",
    "staff_discount",
    "other_discount",
    "total_discount"
  )

  missing_columns <- setdiff(
    required_columns,
    names(facility_discounts)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Facility discount data is missing: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  labels <- trimws(
    as.character(
      facility_discounts$business_unit
    )
  )

  student <- as.numeric(
    facility_discounts$student_discount
  )

  staff <- as.numeric(
    facility_discounts$staff_discount
  )

  other <- as.numeric(
    facility_discounts$other_discount
  )

  total <- as.numeric(
    facility_discounts$total_discount
  )

  keep <- !is.na(labels) &
    nzchar(labels) &
    is.finite(student) &
    is.finite(staff) &
    is.finite(other) &
    is.finite(total) &
    total > 0

  labels <- labels[keep]
  student <- student[keep]
  staff <- staff[keep]
  other <- other[keep]
  total <- total[keep]

  if (length(labels) == 0L) {
    stop(
      paste(
        "No complete facility discount mix",
        "values are available."
      ),
      call. = FALSE
    )
  }

  proportions <- rbind(
    Student = student / total,
    Staff = staff / total,
    Other = other / total
  )

  # Highest total discount opportunity appears at the top.
  order_index <- order(
    total,
    decreasing = FALSE
  )

  labels <- labels[order_index]
  total <- total[order_index]

  proportions <- proportions[
    ,
    order_index,
    drop = FALSE
  ]

  # Wrap long facility/business-unit labels so they remain
  # readable inside the half-width dashboard card.
  wrapped_labels <- vapply(
    labels,
    function(label) {

      label <- gsub(
        "/",
        "/ ",
        label,
        fixed = TRUE
      )

      paste(
        strwrap(
          label,
          width = 18
        ),
        collapse = "\n"
      )
    },
    character(1)
  )

  label_lines <- unlist(
    strsplit(
      wrapped_labels,
      "\n",
      fixed = TRUE
    )
  )

  max_label_width <- max(
    nchar(label_lines),
    na.rm = TRUE
  )

  left_margin <- max(
    5.5,
    min(
      8,
      max_label_width * 0.45
    )
  )

  old_par <- par(
    mar = c(
      4,
      left_margin,
      3,
      1.5
    ),
    family = "sans"
  )

  on.exit(
    par(old_par),
    add = TRUE
  )

  positions <- barplot(
    proportions,
    horiz = TRUE,
    beside = FALSE,
    names.arg = wrapped_labels,
    las = 1,
    cex.names = 0.78,
    border = NA,
    col = c(
      "#2563EB",
      "#0F766E",
      "#F59E0B"
    ),
    axes = FALSE,
    xlim = c(
      0,
      1
    )
  )

  ticks <- seq(
    0,
    1,
    by = 0.25
  )

  axis(
    1,
    at = ticks,
    labels = paste0(
      round(
        ticks * 100
      ),
      "%"
    ),
    tick = FALSE,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  abline(
    v = ticks,
    col = "#E2E8F0",
    lwd = 1
  )

  # Redraw bars so grid lines remain behind them.
  barplot(
    proportions,
    horiz = TRUE,
    beside = FALSE,
    names.arg = wrapped_labels,
    las = 1,
    cex.names = 0.78,
    border = NA,
    col = c(
      "#2563EB",
      "#0F766E",
      "#F59E0B"
    ),
    axes = FALSE,
    xlim = c(
      0,
      1
    ),
    add = TRUE
  )

  legend(
    "top",
    legend = c(
      "Student",
      "Staff",
      "Other"
    ),
    fill = c(
      "#2563EB",
      "#0F766E",
      "#F59E0B"
    ),
    border = NA,
    bty = "n",
    horiz = TRUE,
    cex = 0.8,
    text.col = "#475569",
    inset = c(
      0,
      -0.08
    ),
    xpd = NA
  )

  # Percentage labels are shown only when the segment
  # is large enough to remain readable.
  cumulative <- apply(
    proportions,
    2,
    cumsum
  )

  starts <- rbind(
    rep(
      0,
      ncol(proportions)
    ),
    cumulative[
      -nrow(cumulative),
      ,
      drop = FALSE
    ]
  )

  centres <- starts +
    proportions / 2

  for (row_index in seq_len(nrow(proportions))) {

    for (column_index in seq_len(ncol(proportions))) {

      share <- proportions[
        row_index,
        column_index
      ]

      if (
        is.finite(share) &&
          share >= 0.03
      ) {

        text(
          centres[
            row_index,
            column_index
          ],
          positions[[column_index]],
          labels = paste0(
            round(
              share * 100
            ),
            "%"
          ),
          cex = if (
            share < 0.08
          ) {
            0.6
          } else {
            0.7
          },
          col = "white"
        )
      }
    }
  }

  invisible(NULL)
}

# -------------------------------------------------------------------------
# Facility Discount Opportunity Trend
# -------------------------------------------------------------------------

plot_facility_discount_trend <- function(
    facility_monthly_discounts
) {

  if (
    is.null(facility_monthly_discounts) ||
      !is.data.frame(facility_monthly_discounts) ||
      nrow(facility_monthly_discounts) == 0L
  ) {
    stop(
      "No monthly facility discount values are available.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "date",
    "business_unit",
    "total_discount"
  )

  missing_columns <- setdiff(
    required_columns,
    names(facility_monthly_discounts)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Monthly facility discount data is missing: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  dates <- as.Date(
    facility_monthly_discounts$date
  )

  business_units <- trimws(
    as.character(
      facility_monthly_discounts$business_unit
    )
  )

  values <- suppressWarnings(
    as.numeric(
      facility_monthly_discounts$total_discount
    )
  )

  keep <- !is.na(dates) &
    !is.na(business_units) &
    nzchar(business_units) &
    is.finite(values)

  dates <- dates[keep]
  business_units <- business_units[keep]
  values <- values[keep]

  if (length(values) == 0L) {
    stop(
      paste(
        "No complete monthly facility discount",
        "values are available."
      ),
      call. = FALSE
    )
  }

  plot_data <- data.frame(
    date = dates,
    business_unit = business_units,
    value = values,
    stringsAsFactors = FALSE
  )

  plot_data <- plot_data[
    order(
      plot_data$date,
      plot_data$business_unit
    ),
    ,
    drop = FALSE
  ]

  facility_names <- sort(
    unique(
      plot_data$business_unit
    )
  )

  date_values <- sort(
    unique(
      plot_data$date
    )
  )

  y_limits <- overview_plot_limits(
    plot_data$value
  )

  x_limits <- range(
    date_values
  )

  if (
    x_limits[[1L]] ==
      x_limits[[2L]]
  ) {
    x_limits <- x_limits +
      c(
        -15,
        15
      )
  } else {
    x_limits <- x_limits +
      c(
        -8,
        8
      )
  }

  colours <- grDevices::hcl.colors(
    length(facility_names),
    palette = "Dark 3"
  )

  # Allow additional space above the chart for the facility legend.
  legend_rows <- ceiling(
    length(facility_names) / 2
  )

  top_margin <- max(
    4.8,
    2.8 + legend_rows * 0.8
  )

  old_par <- par(
    mar = c(
      5,
      5.8,
      top_margin,
      1.2
    ),
    xaxs = "i",
    yaxs = "i",
    family = "sans"
  )

  on.exit(
    par(old_par),
    add = TRUE
  )

  plot(
    NA,
    xlim = x_limits,
    ylim = y_limits,
    axes = FALSE,
    xlab = "",
    ylab = "",
    type = "n"
  )

  y_ticks <- pretty(
    y_limits,
    n = 5
  )

  y_ticks <- y_ticks[
    y_ticks >= y_limits[[1L]] &
      y_ticks <= y_limits[[2L]]
  ]

  abline(
    h = y_ticks,
    col = "#E2E8F0",
    lwd = 1
  )

  abline(
    h = 0,
    col = "#94A3B8",
    lwd = 1.1
  )

  for (index in seq_along(facility_names)) {

    facility_name <- facility_names[[index]]

    facility_rows <- plot_data[
      plot_data$business_unit ==
        facility_name,
      ,
      drop = FALSE
    ]

    facility_rows <- facility_rows[
      order(
        facility_rows$date
      ),
      ,
      drop = FALSE
    ]

    if (nrow(facility_rows) > 1L) {
      lines(
        facility_rows$date,
        facility_rows$value,
        col = colours[[index]],
        lwd = 2.2
      )
    }

    points(
      facility_rows$date,
      facility_rows$value,
      pch = 21,
      cex = 0.9,
      lwd = 1.5,
      col = colours[[index]],
      bg = "white"
    )
  }

  # Use the actual available monthly dates for the x-axis.
  axis.Date(
    1,
    at = date_values,
    format = "%b",
    tick = FALSE,
    line = -0.2,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  axis(
    2,
    at = y_ticks,
    labels = format_compact_currency(
      y_ticks
    ),
    las = 1,
    tick = FALSE,
    line = -0.5,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  legend(
    "top",
    legend = facility_names,
    col = colours,
    lty = 1,
    lwd = 2.2,
    pch = 21,
    pt.bg = "white",
    bty = "n",
    ncol = min(
      2L,
      length(facility_names)
    ),
    cex = 0.68,
    text.col = "#475569",
    inset = c(
      0,
      -0.04
    ),
    xpd = NA
  )

  invisible(NULL)
}

# -------------------------------------------------------------------------
# Facility Revenue Opportunity
# -------------------------------------------------------------------------

plot_facility_revenue_opportunity <- function(
    facility_revenue
) {

  if (
    is.null(facility_revenue) ||
      !is.data.frame(facility_revenue) ||
      nrow(facility_revenue) == 0L
  ) {
    stop(
      "No facility revenue opportunity values are available.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "business_unit",
    "actual_income",
    "discount_opportunity",
    "potential_revenue"
  )

  missing_columns <- setdiff(
    required_columns,
    names(facility_revenue)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Facility revenue data is missing: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  labels <- trimws(
    as.character(
      facility_revenue$business_unit
    )
  )

  actual_income <- suppressWarnings(
    as.numeric(
      facility_revenue$actual_income
    )
  )

  discount_opportunity <- suppressWarnings(
    as.numeric(
      facility_revenue$discount_opportunity
    )
  )

  potential_revenue <- suppressWarnings(
    as.numeric(
      facility_revenue$potential_revenue
    )
  )

  keep <- !is.na(labels) &
    nzchar(labels) &
    is.finite(actual_income) &
    is.finite(discount_opportunity) &
    is.finite(potential_revenue) &
    actual_income >= 0 &
    discount_opportunity >= 0 &
    potential_revenue > 0

  labels <- labels[keep]
  actual_income <- actual_income[keep]
  discount_opportunity <- discount_opportunity[keep]
  potential_revenue <- potential_revenue[keep]

  if (length(labels) == 0L) {
    stop(
      "No complete facility revenue opportunity values are available.",
      call. = FALSE
    )
  }

  opportunity_rate <-
    discount_opportunity /
      potential_revenue

  plot_data <- data.frame(
    business_unit = labels,
    actual_income = actual_income,
    discount_opportunity = discount_opportunity,
    potential_revenue = potential_revenue,
    opportunity_rate = opportunity_rate,
    stringsAsFactors = FALSE
  )

  # Horizontal bars draw the final row at the top.
  # Sort ascending so the largest relative revenue opportunity
  # appears at the top of the chart.
  order_index <- order(
    plot_data$opportunity_rate,
    plot_data$potential_revenue,
    na.last = TRUE
  )

  plot_data <- plot_data[
    order_index,
    ,
    drop = FALSE
  ]

  values <- rbind(
    "Actual Income" =
      plot_data$actual_income,
    "Discount Opportunity" =
      plot_data$discount_opportunity
  )

  wrapped_labels <- vapply(
    plot_data$business_unit,
    function(label) {

      label <- gsub(
        "/",
        "/ ",
        label,
        fixed = TRUE
      )

      paste(
        strwrap(
          label,
          width = 18
        ),
        collapse = "\n"
      )
    },
    character(1)
  )

  label_lines <- unlist(
    strsplit(
      wrapped_labels,
      "\n",
      fixed = TRUE
    )
  )

  max_label_width <- max(
    nchar(label_lines),
    na.rm = TRUE
  )

  left_margin <- max(
    5.5,
    min(
      8,
      max_label_width * 0.45
    )
  )

  max_value <- max(
    plot_data$potential_revenue,
    na.rm = TRUE
  )

  x_limit <- if (
    !is.finite(max_value) ||
      max_value <= 0
  ) {
    1
  } else {
    max_value * 1.35
  }

  old_par <- par(
    mar = c(
      4.5,
      left_margin,
      4.2,
      1.5
    ),
    family = "sans"
  )

  on.exit(
    par(old_par),
    add = TRUE
  )

  positions <- barplot(
    values,
    horiz = TRUE,
    beside = FALSE,
    names.arg = wrapped_labels,
    las = 1,
    cex.names = 0.78,
    border = NA,
    col = c(
      "#2563EB",
      "#F59E0B"
    ),
    axes = FALSE,
    xlim = c(
      0,
      x_limit
    ),
    xlab = ""
  )

  x_ticks <- pretty(
    c(
      0,
      max_value
    ),
    n = 5
  )

  axis(
    1,
    at = x_ticks,
    labels = format_compact_currency(
      x_ticks
    ),
    tick = FALSE,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  abline(
    v = x_ticks,
    col = "#E2E8F0",
    lwd = 1
  )

  # Redraw the bars so the grid stays behind them.
  barplot(
    values,
    horiz = TRUE,
    beside = FALSE,
    names.arg = wrapped_labels,
    las = 1,
    cex.names = 0.78,
    border = NA,
    col = c(
      "#2563EB",
      "#F59E0B"
    ),
    axes = FALSE,
    xlim = c(
      0,
      x_limit
    ),
    add = TRUE
  )

  legend(
    "top",
    legend = c(
      "Actual Income",
      "Discount Opportunity"
    ),
    fill = c(
      "#2563EB",
      "#F59E0B"
    ),
    border = NA,
    bty = "n",
    horiz = TRUE,
    cex = 0.78,
    text.col = "#475569",
    inset = c(
      0,
      -0.08
    ),
    xpd = NA
  )

  label_offset <- x_limit * 0.015

  for (index in seq_along(positions)) {

    opportunity_label <- paste0(
      " · ",
      sprintf(
        "%.1f%% opportunity",
        100 *
          plot_data$opportunity_rate[[index]]
      )
    )

    text(
      plot_data$potential_revenue[[index]] +
        label_offset,
      positions[[index]],
      labels = paste0(
        format_compact_currency(
          plot_data$potential_revenue[[index]]
        ),
        opportunity_label
      ),
      pos = 4,
      cex = 0.68,
      col = "#475569",
      xpd = NA
    )
  }

  invisible(NULL)
}

# -------------------------------------------------------------------------
# Facility Revenue Capture Rate
# -------------------------------------------------------------------------

plot_facility_capture_rate <- function(
    facility_revenue
) {

  if (
    is.null(facility_revenue) ||
      !is.data.frame(facility_revenue) ||
      nrow(facility_revenue) == 0L
  ) {
    stop(
      "No facility revenue capture values are available.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "business_unit",
    "capture_rate",
    "opportunity_rate"
  )

  missing_columns <- setdiff(
    required_columns,
    names(facility_revenue)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Facility revenue data is missing: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  labels <- trimws(
    as.character(
      facility_revenue$business_unit
    )
  )

  capture_rate <- suppressWarnings(
    as.numeric(
      facility_revenue$capture_rate
    )
  )

  opportunity_rate <- suppressWarnings(
    as.numeric(
      facility_revenue$opportunity_rate
    )
  )

  keep <- !is.na(labels) &
    nzchar(labels) &
    is.finite(capture_rate) &
    is.finite(opportunity_rate) &
    capture_rate >= 0 &
    opportunity_rate >= 0

  labels <- labels[keep]
  capture_rate <- capture_rate[keep]
  opportunity_rate <- opportunity_rate[keep]

  if (length(labels) == 0L) {
    stop(
      "No complete facility revenue capture values are available.",
      call. = FALSE
    )
  }

  plot_data <- data.frame(
    business_unit = labels,
    capture_rate = capture_rate,
    opportunity_rate = opportunity_rate,
    stringsAsFactors = FALSE
  )

  # Horizontal barplots draw the final row at the top,
  # so ascending order places the highest capture rate at the top.
  order_index <- order(
    plot_data$capture_rate,
    decreasing = FALSE
  )

  plot_data <- plot_data[
    order_index,
    ,
    drop = FALSE
  ]

  values <- rbind(
    "Captured Revenue" =
      plot_data$capture_rate,
    "Opportunity Gap" =
      plot_data$opportunity_rate
  )

  wrapped_labels <- vapply(
    plot_data$business_unit,
    function(label) {

      label <- gsub(
        "/",
        "/ ",
        label,
        fixed = TRUE
      )

      paste(
        strwrap(
          label,
          width = 18
        ),
        collapse = "\n"
      )
    },
    character(1)
  )

  label_lines <- unlist(
    strsplit(
      wrapped_labels,
      "\n",
      fixed = TRUE
    )
  )

  max_label_width <- max(
    nchar(label_lines),
    na.rm = TRUE
  )

  left_margin <- max(
    5.5,
    min(
      8,
      max_label_width * 0.45
    )
  )

  old_par <- par(
    mar = c(
      4.5,
      left_margin,
      4.2,
      1.5
    ),
    family = "sans"
  )

  on.exit(
    par(old_par),
    add = TRUE
  )

  positions <- barplot(
    values,
    horiz = TRUE,
    beside = FALSE,
    names.arg = wrapped_labels,
    las = 1,
    cex.names = 0.78,
    border = NA,
    col = c(
      "#2563EB",
      "#F59E0B"
    ),
    axes = FALSE,
    xlim = c(
      0,
      1
    )
  )

  ticks <- seq(
    0,
    1,
    by = 0.25
  )

  axis(
    1,
    at = ticks,
    labels = paste0(
      round(
        ticks * 100
      ),
      "%"
    ),
    tick = FALSE,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  abline(
    v = ticks,
    col = "#E2E8F0",
    lwd = 1
  )

  # Redraw bars so the grid remains behind them.
  barplot(
    values,
    horiz = TRUE,
    beside = FALSE,
    names.arg = wrapped_labels,
    las = 1,
    cex.names = 0.78,
    border = NA,
    col = c(
      "#2563EB",
      "#F59E0B"
    ),
    axes = FALSE,
    xlim = c(
      0,
      1
    ),
    add = TRUE
  )

  legend(
    "top",
    legend = c(
      "Captured Revenue",
      "Opportunity Gap"
    ),
    fill = c(
      "#2563EB",
      "#F59E0B"
    ),
    border = NA,
    bty = "n",
    horiz = TRUE,
    cex = 0.78,
    text.col = "#475569",
    inset = c(
      0,
      -0.08
    ),
    xpd = NA
  )

  for (index in seq_along(positions)) {

    rate <- plot_data$capture_rate[[index]]

    if (rate >= 0.12) {

      text(
        rate / 2,
        positions[[index]],
        labels = format_percentage(
          rate,
          digits = 1L
        ),
        cex = 0.72,
        col = "white"
      )

    } else {

      text(
        rate + 0.015,
        positions[[index]],
        labels = format_percentage(
          rate,
          digits = 1L
        ),
        pos = 4,
        cex = 0.72,
        col = "#334155",
        xpd = NA
      )
    }
  }

  invisible(NULL)
}

# -------------------------------------------------------------------------
# Facility Performance Over Time
# -------------------------------------------------------------------------

plot_facility_performance_over_time <- function(
    facility_finance,
    metric
) {

  if (
    is.null(facility_finance) ||
      !is.data.frame(facility_finance) ||
      nrow(facility_finance) == 0L
  ) {
    stop(
      "No facility finance values are available.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "report_year",
    "business_unit"
  )

  missing_columns <- setdiff(
    required_columns,
    names(facility_finance)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Facility finance data is missing: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  values <- facility_finance_values(
    facility_finance,
    metric,
    "Actual"
  )

  years <- suppressWarnings(
    as.integer(
      facility_finance$report_year
    )
  )

  business_units <- trimws(
    as.character(
      facility_finance$business_unit
    )
  )

  keep <- is.finite(values) &
    !is.na(years) &
    !is.na(business_units) &
    nzchar(business_units)

  values <- values[keep]
  years <- years[keep]
  business_units <- business_units[keep]

  if (length(values) == 0L) {
    stop(
      paste(
        "No complete facility values are available",
        "for the selected metric."
      ),
      call. = FALSE
    )
  }

  year_values <- sort(
    unique(years)
  )

  if (length(year_values) < 2L) {
    stop(
      "Multiple Detailed Site Report years are required.",
      call. = FALSE
    )
  }

  plot_data <- data.frame(
    report_year = years,
    business_unit = business_units,
    value = values,
    stringsAsFactors = FALSE
  )

  facility_names <- sort(
    unique(
      plot_data$business_unit
    )
  )

  y_limits <- overview_plot_limits(
    plot_data$value
  )

  x_limits <- range(
    year_values
  ) + c(
    -0.25,
    0.25
  )

  colours <- grDevices::hcl.colors(
    length(facility_names),
    palette = "Dark 3"
  )

  legend_rows <- ceiling(
    length(facility_names) / 2
  )

  top_margin <- max(
    4.8,
    2.8 + legend_rows * 0.8
  )

  old_par <- par(
    mar = c(
      4.2,
      5.8,
      top_margin,
      1.2
    ),
    xaxs = "i",
    yaxs = "i",
    family = "sans"
  )

  on.exit(
    par(old_par),
    add = TRUE
  )

  plot(
    NA,
    xlim = x_limits,
    ylim = y_limits,
    axes = FALSE,
    xlab = "",
    ylab = "",
    type = "n"
  )

  y_ticks <- pretty(
    y_limits,
    n = 5
  )

  y_ticks <- y_ticks[
    y_ticks >= y_limits[[1L]] &
      y_ticks <= y_limits[[2L]]
  ]

  abline(
    h = y_ticks,
    col = "#E2E8F0",
    lwd = 1
  )

  abline(
    h = 0,
    col = "#94A3B8",
    lwd = 1.1
  )

  for (index in seq_along(facility_names)) {

    facility_name <- facility_names[[index]]

    facility_rows <- plot_data[
      plot_data$business_unit ==
        facility_name,
      ,
      drop = FALSE
    ]

    facility_rows <- facility_rows[
      order(
        facility_rows$report_year
      ),
      ,
      drop = FALSE
    ]

    if (nrow(facility_rows) > 1L) {
      lines(
        facility_rows$report_year,
        facility_rows$value,
        col = colours[[index]],
        lwd = 2.4
      )
    }

    points(
      facility_rows$report_year,
      facility_rows$value,
      pch = 21,
      cex = 1,
      lwd = 1.6,
      col = colours[[index]],
      bg = "white"
    )
  }

  axis(
    1,
    at = year_values,
    labels = year_values,
    tick = FALSE,
    line = -0.3,
    col.axis = "#64748B",
    cex.axis = 0.88
  )

  axis(
    2,
    at = y_ticks,
    labels = format_compact_currency(
      y_ticks
    ),
    las = 1,
    tick = FALSE,
    line = -0.5,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  legend(
    "top",
    legend = facility_names,
    col = colours,
    lty = 1,
    lwd = 2.4,
    pch = 21,
    pt.bg = "white",
    bty = "n",
    ncol = min(
      2L,
      length(facility_names)
    ),
    cex = 0.68,
    text.col = "#475569",
    inset = c(
      0,
      -0.04
    ),
    xpd = NA
  )

  invisible(NULL)
}

# -------------------------------------------------------------------------
# Site Detail plots
# -------------------------------------------------------------------------

plot_site_detail_visits_over_time <- function(monthly_visits) {

  if (
    is.null(monthly_visits) ||
      !is.data.frame(monthly_visits) ||
      nrow(monthly_visits) == 0L
  ) {
    stop("No monthly visit values are available.", call. = FALSE)
  }

  required_columns <- c("date", "direct", "indirect")
  missing_columns <- setdiff(required_columns, names(monthly_visits))

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Monthly visit data is missing: ",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  dates <- as.Date(monthly_visits$date)
  direct <- suppressWarnings(as.numeric(monthly_visits$direct))
  indirect <- suppressWarnings(as.numeric(monthly_visits$indirect))

  keep <- !is.na(dates) &
    (is.finite(direct) | is.finite(indirect))

  dates <- dates[keep]
  direct <- direct[keep]
  indirect <- indirect[keep]

  if (length(dates) == 0L) {
    stop("No finite monthly visit values are available.", call. = FALSE)
  }

  order_index <- order(dates)
  dates <- dates[order_index]
  direct <- direct[order_index]
  indirect <- indirect[order_index]

  max_value <- max(c(direct, indirect), na.rm = TRUE)

  y_max <- if (
    !is.finite(max_value) ||
      max_value <= 0
  ) {
    1
  } else {
    max_value * 1.12
  }

  y_limits <- c(0, y_max)
  x_limits <- range(dates)

  if (x_limits[[1L]] == x_limits[[2L]]) {
    x_limits <- x_limits + c(-15, 15)
  } else {
    x_limits <- x_limits + c(-8, 8)
  }

  old_par <- par(
    mar = c(4.8, 5.8, 4.2, 1.2),
    xaxs = "i",
    yaxs = "i",
    family = "sans"
  )

  on.exit(par(old_par), add = TRUE)

  plot(
    NA,
    xlim = x_limits,
    ylim = y_limits,
    axes = FALSE,
    xlab = "",
    ylab = "",
    type = "n"
  )

  y_ticks <- pretty(y_limits, n = 5)

  y_ticks <- y_ticks[
    y_ticks >= y_limits[[1L]] &
      y_ticks <= y_limits[[2L]]
  ]

  abline(
    h = y_ticks,
    col = "#E2E8F0",
    lwd = 1
  )

  lines(
    dates,
    direct,
    col = "#2563EB",
    lwd = 2.6
  )

  points(
    dates,
    direct,
    pch = 21,
    cex = 1,
    lwd = 1.7,
    col = "#2563EB",
    bg = "white"
  )

  lines(
    dates,
    indirect,
    col = "#0F766E",
    lwd = 2.6
  )

  points(
    dates,
    indirect,
    pch = 21,
    cex = 1,
    lwd = 1.7,
    col = "#0F766E",
    bg = "white"
  )

  axis.Date(
    1,
    at = dates,
    format = "%b",
    tick = FALSE,
    line = -0.2,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  axis(
    2,
    at = y_ticks,
    labels = format(
      round(y_ticks),
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    ),
    las = 1,
    tick = FALSE,
    line = -0.5,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  legend(
    "top",
    legend = c(
      "Direct Visits",
      "Indirect Visits"
    ),
    col = c(
      "#2563EB",
      "#0F766E"
    ),
    lty = 1,
    lwd = 2.6,
    pch = 21,
    pt.bg = "white",
    bty = "n",
    horiz = TRUE,
    cex = 0.8,
    text.col = "#475569",
    inset = c(0, -0.04),
    xpd = NA
  )

  invisible(NULL)
}

# -------------------------------------------------------------------------
# Site Detail Direct vs Indirect Visits
# -------------------------------------------------------------------------

plot_site_detail_visit_type_breakdown <- function(visit_breakdown) {

  if (
    is.null(visit_breakdown) ||
      !is.data.frame(visit_breakdown) ||
      nrow(visit_breakdown) == 0L
  ) {
    stop("No visit breakdown values are available.", call. = FALSE)
  }

  labels <- as.character(visit_breakdown$visit_type)
  values <- suppressWarnings(as.numeric(visit_breakdown$value))
  shares <- suppressWarnings(as.numeric(visit_breakdown$share))

  keep <- !is.na(labels) &
    nzchar(labels) &
    is.finite(values)

  labels <- labels[keep]
  values <- values[keep]
  shares <- shares[keep]

  if (length(values) == 0L) {
    stop("No finite visit breakdown values are available.", call. = FALSE)
  }

  colours <- ifelse(
    labels == "Direct",
    "#2563EB",
    "#0F766E"
  )

  y_max <- max(values, na.rm = TRUE)

  if (!is.finite(y_max) || y_max <= 0) {
    y_max <- 1
  }

  y_limit <- y_max * 1.18

  positions <- barplot(
    values,
    plot = FALSE,
    space = 0.7
  )

  old_par <- par(
    mar = c(4.8, 5.8, 2.5, 1.2),
    xaxs = "i",
    yaxs = "i",
    family = "sans"
  )

  on.exit(par(old_par), add = TRUE)

  plot(
    NA,
    xlim = range(positions) + c(-0.7, 0.7),
    ylim = c(0, y_limit),
    axes = FALSE,
    xlab = "",
    ylab = "",
    type = "n"
  )

  y_ticks <- pretty(c(0, y_limit), n = 5)
  y_ticks <- y_ticks[y_ticks >= 0 & y_ticks <= y_limit]

  abline(
    h = y_ticks,
    col = "#E2E8F0",
    lwd = 1
  )

  barplot(
    values,
    names.arg = paste0(labels, " Visits"),
    col = colours,
    border = NA,
    axes = FALSE,
    space = 0.7,
    add = TRUE
  )

  axis(
    2,
    at = y_ticks,
    labels = format(
      round(y_ticks),
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    ),
    las = 1,
    tick = FALSE,
    line = -0.5,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  labels_above <- paste0(
    format(
      round(values),
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    ),
    ifelse(
      is.finite(shares),
      paste0(" · ", sprintf("%.1f%%", 100 * shares)),
      ""
    )
  )

  text(
    positions,
    values,
    labels = labels_above,
    pos = 3,
    offset = 0.4,
    cex = 0.78,
    col = "#475569",
    xpd = NA
  )

  invisible(NULL)
}

# -------------------------------------------------------------------------
# Site Detail Audience Mix
# -------------------------------------------------------------------------

plot_site_detail_audience_mix <- function(audience_mix) {

  if (
    is.null(audience_mix) ||
      !is.data.frame(audience_mix) ||
      nrow(audience_mix) == 0L
  ) {
    stop(
      "No audience values are available.",
      call. = FALSE
    )
  }

  labels <- as.character(
    audience_mix$audience
  )

  values <- suppressWarnings(
    as.numeric(audience_mix$value)
  )

  shares <- suppressWarnings(
    as.numeric(audience_mix$share)
  )

  keep <- !is.na(labels) &
    nzchar(labels) &
    is.finite(values) &
    is.finite(shares)

  labels <- labels[keep]
  values <- values[keep]
  shares <- shares[keep]

  if (length(values) == 0L) {
    stop(
      "No complete audience values are available.",
      call. = FALSE
    )
  }

  # Keep a consistent audience order.
  audience_order <- c(
    "Student",
    "Staff",
    "Community"
  )

  order_index <- match(
    audience_order,
    labels
  )

  order_index <- order_index[
    !is.na(order_index)
  ]

  labels <- labels[order_index]
  values <- values[order_index]
  shares <- shares[order_index]

  colours <- c(
    Student = "#2563EB",
    Staff = "#0F766E",
    Community = "#F59E0B"
  )

  bar_colours <- colours[labels]

  max_value <- max(
    values,
    na.rm = TRUE
  )

  y_limit <- if (
    !is.finite(max_value) ||
      max_value <= 0
  ) {
    1
  } else {
    max_value * 1.28
  }

  old_par <- par(
    mar = c(
      4.8,
      5.2,
      2.5,
      1.2
    ),
    xaxs = "i",
    yaxs = "i",
    family = "sans"
  )

  on.exit(
    par(old_par),
    add = TRUE
  )

  positions <- barplot(
    values,
    plot = FALSE,
    space = 0.55
  )

  x_limits <- range(positions) +
    c(-0.75, 0.75)

  plot(
    NA,
    xlim = x_limits,
    ylim = c(0, y_limit),
    axes = FALSE,
    xlab = "",
    ylab = "",
    type = "n"
  )

  y_ticks <- pretty(
    c(0, max_value),
    n = 4
  )

  y_ticks <- y_ticks[
    y_ticks >= 0 &
      y_ticks <= y_limit
  ]

  abline(
    h = y_ticks,
    col = "#E2E8F0",
    lwd = 1
  )

  barplot(
    values,
    col = bar_colours,
    border = NA,
    axes = FALSE,
    axisnames = FALSE,
    space = 0.55,
    add = TRUE
  )

  axis(
    1,
    at = positions,
    labels = labels,
    tick = FALSE,
    line = -0.3,
    col.axis = "#475569",
    cex.axis = 0.82
  )

  axis(
    2,
    at = y_ticks,
    labels = format(
      round(y_ticks),
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    ),
    las = 1,
    tick = FALSE,
    line = -0.5,
    col.axis = "#64748B",
    cex.axis = 0.78
  )

  value_labels <- paste0(
    format(
      round(values),
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    ),
    "\n",
    sprintf(
      "%.1f%%",
      100 * shares
    )
  )

  label_y <- ifelse(
    values > 0,
    values + y_limit * 0.025,
    y_limit * 0.025
  )

  text(
    positions,
    label_y,
    labels = value_labels,
    pos = 3,
    offset = 0.15,
    cex = 0.72,
    col = "#475569",
    xpd = NA
  )

  invisible(NULL)
}

# -------------------------------------------------------------------------
# Site Detail Visits by Activity
# -------------------------------------------------------------------------

plot_site_detail_activity_breakdown <- function(activity_data) {

  if (
    is.null(activity_data) ||
      !is.data.frame(activity_data) ||
      nrow(activity_data) == 0L
  ) {
    stop(
      "No activity visit values are available.",
      call. = FALSE
    )
  }

  labels <- trimws(
    as.character(
      activity_data$activity
    )
  )

  values <- suppressWarnings(
    as.numeric(
      activity_data$value
    )
  )

  keep <- !is.na(labels) &
    nzchar(labels) &
    is.finite(values)

  labels <- labels[keep]
  values <- values[keep]

  if (length(values) == 0L) {
    stop(
      "No complete activity visit values are available.",
      call. = FALSE
    )
  }

  # Horizontal barplots place the last row at the top.
  order_index <- order(
    values,
    decreasing = FALSE
  )

  labels <- labels[order_index]
  values <- values[order_index]

  wrapped_labels <- vapply(
    labels,
    function(label) {
      paste(
        strwrap(
          label,
          width = 24
        ),
        collapse = "\n"
      )
    },
    character(1)
  )

  max_value <- max(
    values,
    na.rm = TRUE
  )

  x_limit <- if (
    !is.finite(max_value) ||
      max_value <= 0
  ) {
    1
  } else {
    max_value * 1.18
  }

  old_par <- par(
    mar = c(
      4.5,
      9,
      2,
      1.5
    ),
    family = "sans"
  )

  on.exit(
    par(old_par),
    add = TRUE
  )

  positions <- barplot(
    values,
    names.arg = wrapped_labels,
    horiz = TRUE,
    las = 1,
    cex.names = 0.76,
    border = NA,
    col = "#2563EB",
    axes = FALSE,
    xlim = c(0, x_limit)
  )

  x_ticks <- pretty(
    c(0, max_value),
    n = 5
  )

  abline(
    v = x_ticks,
    col = "#E2E8F0",
    lwd = 1
  )

  barplot(
    values,
    names.arg = wrapped_labels,
    horiz = TRUE,
    las = 1,
    cex.names = 0.76,
    border = NA,
    col = "#2563EB",
    axes = FALSE,
    xlim = c(0, x_limit),
    add = TRUE
  )

  axis(
    1,
    at = x_ticks,
    labels = format(
      round(x_ticks),
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    ),
    tick = FALSE,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  label_offset <- x_limit * 0.015

  text(
    values + label_offset,
    positions,
    labels = format(
      round(values),
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    ),
    pos = 4,
    cex = 0.72,
    col = "#475569",
    xpd = NA
  )

  invisible(NULL)
}

# -------------------------------------------------------------------------
# Site Detail Memberships Over Time
# -------------------------------------------------------------------------

plot_site_detail_memberships_over_time <- function(membership_data) {

  if (
    is.null(membership_data) ||
      !is.data.frame(membership_data) ||
      nrow(membership_data) == 0L
  ) {
    stop(
      "No monthly membership values are available.",
      call. = FALSE
    )
  }

  dates <- as.Date(
    membership_data$date
  )

  values <- suppressWarnings(
    as.numeric(
      membership_data$total_active
    )
  )

  keep <- !is.na(dates) &
    is.finite(values)

  dates <- dates[keep]
  values <- values[keep]

  if (length(values) == 0L) {
    stop(
      "No finite membership values are available.",
      call. = FALSE
    )
  }

  order_index <- order(dates)

  dates <- dates[order_index]
  values <- values[order_index]

  max_value <- max(
    values,
    na.rm = TRUE
  )

  y_limit <- if (
    !is.finite(max_value) ||
      max_value <= 0
  ) {
    1
  } else {
    max_value * 1.12
  }

  x_limits <- range(dates)

  if (x_limits[[1L]] == x_limits[[2L]]) {
    x_limits <- x_limits + c(-15, 15)
  } else {
    x_limits <- x_limits + c(-8, 8)
  }

  old_par <- par(
    mar = c(4.8, 5.8, 2.8, 1.2),
    xaxs = "i",
    yaxs = "i",
    family = "sans"
  )

  on.exit(
    par(old_par),
    add = TRUE
  )

  plot(
    NA,
    xlim = x_limits,
    ylim = c(0, y_limit),
    axes = FALSE,
    xlab = "",
    ylab = "",
    type = "n"
  )

  y_ticks <- pretty(
    c(0, y_limit),
    n = 5
  )

  y_ticks <- y_ticks[
    y_ticks >= 0 &
      y_ticks <= y_limit
  ]

  abline(
    h = y_ticks,
    col = "#E2E8F0",
    lwd = 1
  )

  lines(
    dates,
    values,
    col = "#2563EB",
    lwd = 2.8
  )

  points(
    dates,
    values,
    pch = 21,
    cex = 1,
    lwd = 1.8,
    col = "#2563EB",
    bg = "white"
  )

  axis.Date(
    1,
    at = dates,
    format = "%b",
    tick = FALSE,
    line = -0.2,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  axis(
    2,
    at = y_ticks,
    labels = format(
      round(y_ticks),
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    ),
    las = 1,
    tick = FALSE,
    line = -0.5,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  invisible(NULL)
}

plot_site_detail_enrolments_over_time <- function(enrolment_data) {

  if (
    is.null(enrolment_data) ||
      !is.data.frame(enrolment_data) ||
      nrow(enrolment_data) == 0L
  ) {
    stop(
      "No monthly enrolment values are available.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "date",
    "total_active",
    "target"
  )

  missing_columns <- setdiff(
    required_columns,
    names(enrolment_data)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Enrolment trend data is missing: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  dates <- as.Date(
    enrolment_data$date
  )

  total_active <- suppressWarnings(
    as.numeric(
      enrolment_data$total_active
    )
  )

  target <- suppressWarnings(
    as.numeric(
      enrolment_data$target
    )
  )

  if (
    !any(
      is.finite(
        c(
          total_active,
          target
        )
      )
    )
  ) {
    stop(
      "No finite monthly enrolment values are available.",
      call. = FALSE
    )
  }

  order_index <- order(dates)

  dates <- dates[order_index]
  total_active <- total_active[order_index]
  target <- target[order_index]

  max_value <- max(
    c(
      total_active,
      target
    ),
    na.rm = TRUE
  )

  y_max <- if (
    !is.finite(max_value) ||
      max_value <= 0
  ) {
    1
  } else {
    max_value * 1.12
  }

  old_par <- par(
    mar = c(4.8, 5.8, 4.2, 1.2),
    xaxs = "i",
    yaxs = "i",
    family = "sans"
  )

  on.exit(
    par(old_par),
    add = TRUE
  )

  x_limits <- range(dates)

  if (x_limits[[1L]] == x_limits[[2L]]) {
    x_limits <- x_limits + c(-15, 15)
  } else {
    x_limits <- x_limits + c(-8, 8)
  }

  plot(
    NA,
    xlim = x_limits,
    ylim = c(0, y_max),
    axes = FALSE,
    xlab = "",
    ylab = "",
    type = "n"
  )

  y_ticks <- pretty(
    c(0, y_max),
    n = 5
  )

  y_ticks <- y_ticks[
    y_ticks >= 0 &
      y_ticks <= y_max
  ]

  abline(
    h = y_ticks,
    col = "#E2E8F0",
    lwd = 1
  )

  lines(
    dates,
    total_active,
    col = "#2563EB",
    lwd = 2.6
  )

  points(
    dates[is.finite(total_active)],
    total_active[is.finite(total_active)],
    pch = 21,
    cex = 1,
    lwd = 1.7,
    col = "#2563EB",
    bg = "white"
  )

  lines(
    dates,
    target,
    col = "#F59E0B",
    lwd = 2.6
  )

  points(
    dates[is.finite(target)],
    target[is.finite(target)],
    pch = 21,
    cex = 1,
    lwd = 1.7,
    col = "#F59E0B",
    bg = "white"
  )

  axis(
    1,
    at = dates,
    labels = format(
      dates,
      "%b"
    ),
    tick = FALSE,
    line = -0.3,
    col.axis = "#64748B",
    cex.axis = 0.88
  )

  axis(
    2,
    at = y_ticks,
    labels = format(
      round(y_ticks),
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    ),
    las = 1,
    tick = FALSE,
    line = -0.5,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  legend(
    "top",
    legend = c(
      "Total Active",
      "Target"
    ),
    col = c(
      "#2563EB",
      "#F59E0B"
    ),
    lty = 1,
    lwd = 2.6,
    pch = 21,
    pt.bg = "white",
    bty = "n",
    ncol = 2,
    cex = 0.8,
    text.col = "#475569",
    inset = c(0, -0.04),
    xpd = NA
  )

  invisible(NULL)
}

# -------------------------------------------------------------------------
# Enrolment breakdown
# -------------------------------------------------------------------------

plot_site_detail_enrolment_breakdown <- function(enrolment_data) {

  if (
    is.null(enrolment_data) ||
      !is.data.frame(enrolment_data) ||
      nrow(enrolment_data) == 0L
  ) {
    stop(
      "No enrolment breakdown values are available.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "date",
    "total_active",
    "active_students"
  )

  missing_columns <- setdiff(
    required_columns,
    names(enrolment_data)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Enrolment breakdown data is missing: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  dates <- as.Date(
    enrolment_data$date
  )

  total_active <- suppressWarnings(
    as.numeric(
      enrolment_data$total_active
    )
  )

  active_students <- suppressWarnings(
    as.numeric(
      enrolment_data$active_students
    )
  )

  keep <- !is.na(dates) &
    (
      is.finite(total_active) |
        is.finite(active_students)
    )

  dates <- dates[keep]
  total_active <- total_active[keep]
  active_students <- active_students[keep]

  if (length(dates) == 0L) {
    stop(
      "No finite enrolment breakdown values are available.",
      call. = FALSE
    )
  }

  order_index <- order(dates)

  dates <- dates[order_index]
  total_active <- total_active[order_index]
  active_students <- active_students[order_index]

  max_value <- max(
    c(
      total_active,
      active_students
    ),
    na.rm = TRUE
  )

  y_max <- if (
    !is.finite(max_value) ||
      max_value <= 0
  ) {
    1
  } else {
    max_value * 1.12
  }

  x_limits <- range(dates)

  if (x_limits[[1L]] == x_limits[[2L]]) {
    x_limits <- x_limits + c(-15, 15)
  } else {
    x_limits <- x_limits + c(-8, 8)
  }

  old_par <- par(
    mar = c(4.8, 5.8, 4.2, 1.2),
    xaxs = "i",
    yaxs = "i",
    family = "sans"
  )

  on.exit(
    par(old_par),
    add = TRUE
  )

  plot(
    NA,
    xlim = x_limits,
    ylim = c(0, y_max),
    axes = FALSE,
    xlab = "",
    ylab = "",
    type = "n"
  )

  y_ticks <- pretty(
    c(0, y_max),
    n = 5
  )

  y_ticks <- y_ticks[
    y_ticks >= 0 &
      y_ticks <= y_max
  ]

  abline(
    h = y_ticks,
    col = "#E2E8F0",
    lwd = 1
  )

  lines(
    dates,
    total_active,
    col = "#2563EB",
    lwd = 2.6
  )

  points(
    dates[is.finite(total_active)],
    total_active[is.finite(total_active)],
    pch = 21,
    cex = 1,
    lwd = 1.7,
    col = "#2563EB",
    bg = "white"
  )

  lines(
    dates,
    active_students,
    col = "#0F766E",
    lwd = 2.6
  )

  points(
    dates[is.finite(active_students)],
    active_students[is.finite(active_students)],
    pch = 21,
    cex = 1,
    lwd = 1.7,
    col = "#0F766E",
    bg = "white"
  )

  axis(
    1,
    at = dates,
    labels = format(
      dates,
      "%b"
    ),
    tick = FALSE,
    line = -0.3,
    col.axis = "#64748B",
    cex.axis = 0.88
  )

  axis(
    2,
    at = y_ticks,
    labels = format(
      round(y_ticks),
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    ),
    las = 1,
    tick = FALSE,
    line = -0.5,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  legend(
    "top",
    legend = c(
      "Total Active",
      "Active Students"
    ),
    col = c(
      "#2563EB",
      "#0F766E"
    ),
    lty = 1,
    lwd = 2.6,
    pch = 21,
    pt.bg = "white",
    bty = "n",
    ncol = 2,
    cex = 0.8,
    text.col = "#475569",
    inset = c(0, -0.04),
    xpd = NA
  )

  invisible(NULL)
}

# -------------------------------------------------------------------------
# Enrolment student share
# -------------------------------------------------------------------------

plot_site_detail_enrolment_student_share <- function(share_data) {

  if (
    is.null(share_data) ||
      !is.data.frame(share_data) ||
      nrow(share_data) == 0L
  ) {
    stop(
      "No student share values are available.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "date",
    "student_share"
  )

  missing_columns <- setdiff(
    required_columns,
    names(share_data)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Student share data is missing: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  dates <- as.Date(
    share_data$date
  )

  shares <- suppressWarnings(
    as.numeric(
      share_data$student_share
    )
  )

  keep <- !is.na(dates) &
    is.finite(shares)

  dates <- dates[keep]
  shares <- shares[keep]

  if (length(dates) == 0L) {
    stop(
      "No finite student share values are available.",
      call. = FALSE
    )
  }

  order_index <- order(dates)

  dates <- dates[order_index]
  shares <- shares[order_index]

  # Convert stored fractions to percentage points for plotting.
  percentages <- shares * 100

  max_value <- max(
    percentages,
    na.rm = TRUE
  )

  y_max <- if (
    !is.finite(max_value) ||
      max_value <= 0
  ) {
    100
  } else {
    min(
      100,
      max_value * 1.25
    )
  }

  x_limits <- range(dates)

  if (x_limits[[1L]] == x_limits[[2L]]) {
    x_limits <- x_limits + c(-15, 15)
  } else {
    x_limits <- x_limits + c(-8, 8)
  }

  old_par <- par(
    mar = c(4.8, 5.2, 3.8, 1.2),
    xaxs = "i",
    yaxs = "i",
    family = "sans"
  )

  on.exit(
    par(old_par),
    add = TRUE
  )

  plot(
    NA,
    xlim = x_limits,
    ylim = c(0, y_max),
    axes = FALSE,
    xlab = "",
    ylab = "",
    type = "n"
  )

  y_ticks <- pretty(
    c(0, y_max),
    n = 5
  )

  y_ticks <- y_ticks[
    y_ticks >= 0 &
      y_ticks <= y_max
  ]

  abline(
    h = y_ticks,
    col = "#E2E8F0",
    lwd = 1
  )

  if (length(dates) > 1L) {
    lines(
      dates,
      percentages,
      col = "#2563EB",
      lwd = 2.6
    )
  }

  points(
    dates,
    percentages,
    pch = 21,
    cex = 1.1,
    lwd = 1.7,
    col = "#2563EB",
    bg = "white"
  )

  text(
    dates,
    percentages,
    labels = sprintf(
      "%.1f%%",
      percentages
    ),
    pos = 3,
    offset = 0.5,
    cex = 0.78,
    col = "#475569",
    xpd = NA
  )

  axis(
    1,
    at = dates,
    labels = format(
      dates,
      "%b"
    ),
    tick = FALSE,
    line = -0.3,
    col.axis = "#64748B",
    cex.axis = 0.88
  )

  axis(
    2,
    at = y_ticks,
    labels = paste0(
      format(
        y_ticks,
        trim = TRUE,
        scientific = FALSE
      ),
      "%"
    ),
    las = 1,
    tick = FALSE,
    line = -0.5,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  invisible(NULL)
}

# -------------------------------------------------------------------------
# Site Detail Membership Movement
# -------------------------------------------------------------------------

plot_site_detail_membership_movement <- function(movement_data) {

  if (
    is.null(movement_data) ||
      !is.data.frame(movement_data) ||
      nrow(movement_data) == 0L
  ) {
    stop(
      "No membership movement values are available.",
      call. = FALSE
    )
  }

  dates <- as.Date(
    movement_data$date
  )

  new_members <- suppressWarnings(
    as.numeric(
      movement_data$new_members
    )
  )

  expired <- suppressWarnings(
    as.numeric(
      movement_data$expired_cancelled
    )
  )

  keep <- !is.na(dates) &
    (
      is.finite(new_members) |
        is.finite(expired)
    )

  dates <- dates[keep]
  new_members <- new_members[keep]
  expired <- expired[keep]

  if (length(dates) == 0L) {
    stop(
      "No finite membership movement values are available.",
      call. = FALSE
    )
  }

  order_index <- order(dates)

  dates <- dates[order_index]
  new_members <- new_members[order_index]
  expired <- expired[order_index]

  max_value <- max(
    c(
      new_members,
      expired
    ),
    na.rm = TRUE
  )

  y_limit <- if (
    !is.finite(max_value) ||
      max_value <= 0
  ) {
    1
  } else {
    max_value * 1.15
  }

  x_limits <- range(dates)

  if (x_limits[[1L]] == x_limits[[2L]]) {
    x_limits <- x_limits + c(-15, 15)
  } else {
    x_limits <- x_limits + c(-8, 8)
  }

  old_par <- par(
    mar = c(
      4.8,
      5.8,
      4.2,
      1.2
    ),
    xaxs = "i",
    yaxs = "i",
    family = "sans"
  )

  on.exit(
    par(old_par),
    add = TRUE
  )

  plot(
    NA,
    xlim = x_limits,
    ylim = c(0, y_limit),
    axes = FALSE,
    xlab = "",
    ylab = "",
    type = "n"
  )

  y_ticks <- pretty(
    c(0, y_limit),
    n = 5
  )

  y_ticks <- y_ticks[
    y_ticks >= 0 &
      y_ticks <= y_limit
  ]

  abline(
    h = y_ticks,
    col = "#E2E8F0",
    lwd = 1
  )

  lines(
    dates,
    new_members,
    col = "#2563EB",
    lwd = 2.6
  )

  points(
    dates,
    new_members,
    pch = 21,
    cex = 0.95,
    lwd = 1.7,
    col = "#2563EB",
    bg = "white"
  )

  lines(
    dates,
    expired,
    col = "#F59E0B",
    lwd = 2.6
  )

  points(
    dates,
    expired,
    pch = 21,
    cex = 0.95,
    lwd = 1.7,
    col = "#F59E0B",
    bg = "white"
  )

  axis.Date(
    1,
    at = dates,
    format = "%b",
    tick = FALSE,
    line = -0.2,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  axis(
    2,
    at = y_ticks,
    labels = format(
      round(y_ticks),
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    ),
    las = 1,
    tick = FALSE,
    line = -0.5,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  legend(
    "top",
    legend = c(
      "New Members",
      "Expired / Cancelled"
    ),
    col = c(
      "#2563EB",
      "#F59E0B"
    ),
    lty = 1,
    lwd = 2.6,
    pch = 21,
    pt.bg = "white",
    bty = "n",
    horiz = TRUE,
    cex = 0.8,
    text.col = "#475569",
    inset = c(0, -0.04),
    xpd = NA
  )

  invisible(NULL)
}