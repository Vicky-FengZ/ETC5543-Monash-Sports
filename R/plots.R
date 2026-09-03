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


plot_annual_performance_trend <- function(annual_finance, metric) {
  values <- overview_metric_values(annual_finance, metric)
  keep <- is.finite(values) & is.finite(annual_finance$report_year)
  years <- annual_finance$report_year[keep]
  values <- values[keep]

  if (length(values) == 0L) {
    stop("No finite values are available for the selected metric.", call. = FALSE)
  }

  limits <- overview_plot_limits(values)
  x_limits <- range(years)

  if (diff(x_limits) == 0) {
    x_limits <- x_limits + c(-0.5, 0.5)
  } else {
    x_limits <- x_limits + c(-0.25, 0.25)
  }

  old_par <- par(
    mar = c(3.8, 5.8, 1.2, 1.2),
    xaxs = "i",
    yaxs = "i",
    family = "sans"
  )
  on.exit(par(old_par), add = TRUE)

  plot(
    years,
    values,
    type = "n",
    xlim = x_limits,
    ylim = limits,
    axes = FALSE,
    xlab = "",
    ylab = ""
  )

  ticks <- pretty(limits, n = 5)
  ticks <- ticks[ticks >= limits[[1]] & ticks <= limits[[2]]]
  abline(h = ticks, col = "#E2E8F0", lwd = 1)
  abline(h = 0, col = "#94A3B8", lwd = 1.1)

  if (length(values) > 1L) {
    polygon(
      c(years, rev(years)),
      c(values, rep(0, length(values))),
      border = NA,
      col = grDevices::adjustcolor("#2563EB", alpha.f = 0.08)
    )
    lines(years, values, col = "#2563EB", lwd = 2.8)
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
    labels = format_compact_currency(ticks),
    las = 1,
    tick = FALSE,
    line = -0.5,
    col.axis = "#64748B",
    cex.axis = 0.82
  )

  points(
    years,
    values,
    pch = 21,
    cex = 1.25,
    lwd = 2,
    col = "#2563EB",
    bg = "white"
  )

  label_offset <- diff(limits) * 0.045
  label_y <- values + ifelse(values >= 0, label_offset, -label_offset)

  text(
    years,
    label_y,
    labels = format_compact_currency(values),
    pos = ifelse(values >= 0, 3, 1),
    offset = 0.15,
    cex = 0.8,
    col = "#334155",
    xpd = NA
  )

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
