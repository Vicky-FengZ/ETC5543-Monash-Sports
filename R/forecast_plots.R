# Visuals for monthly forecasts and the total-period pricing requirement.

# Scenario interval columns for one metric. Bands are shown only when every
# forecast month has both the 80% and 95% bounds.
forecast_scenario_band <- function(table, metric) {
  if (!is.data.frame(table)) table <- data.frame()
  column <- function(level, side) {
    values <- table[[paste0("scenario_", metric, "_", side, level)]]
    if (is.null(values)) rep(NA_real_, nrow(table)) else as.numeric(values)
  }
  band <- list(lower80 = column(80, "lower"), upper80 = column(80, "upper"),
               lower95 = column(95, "lower"), upper95 = column(95, "upper"))
  band$available <- nrow(table) > 0L &&
    all(vapply(band, function(values) all(is.finite(values)), logical(1)))
  band
}

plot_finance_forecast <- function(prepared, baseline, scenario, metric) {
  history <- prepared$history
  prediction <- baseline$forecast[baseline$forecast$metric == metric, , drop = FALSE]
  history <- tail(history, 36L)
  scenario_values <- scenario$table[[paste0("scenario_", metric)]]
  band <- forecast_scenario_band(scenario$table, metric)
  # Scenario (and its intervals) takes the metric colour; the baseline is a
  # light grey dashed reference, clearly distinct from the scenario line.
  scenario_colour <- if (metric == "income") CHART_COLOURS[["primary"]] else CHART_COLOURS[["secondary"]]
  baseline_colour <- CHART_COLOURS[["subtle"]]
  alpha95 <- 0.15
  alpha80 <- 0.30
  limits <- overview_plot_limits(c(history[[metric]], prediction$baseline, scenario_values,
                                   if (band$available) band$upper95))
  # Legends sit above the plotting region so they never cover the forecast fan.
  old <- par(mar = c(4.2, 5.3, if (band$available) 3.8 else 2.3, 1), family = "sans", xaxs = "i")
  on.exit(par(old))
  dates <- c(history$date, prediction$date)
  plot(range(dates), limits, type = "n", axes = FALSE, xlab = "", ylab = "")
  ticks <- pretty(limits, 5)
  abline(h = ticks, col = CHART_COLOURS[["grid"]], lwd = 0.8)
  axis(2, at = ticks, labels = format_compact_currency(ticks), las = 1,
       tick = FALSE, col.axis = CHART_COLOURS[["muted"]], cex.axis = 0.88)
  step <- if (length(dates) > 24L) "6 months" else "3 months"
  x_ticks <- seq(min(dates), max(dates), by = step)
  axis(1, at = x_ticks, labels = forecast_month_label(x_ticks, "\n"), tick = FALSE,
       col.axis = CHART_COLOURS[["muted"]], cex.axis = 0.86)
  if (band$available) {
    # The fan starts at the last actual month, so uncertainty widens from it.
    band_dates <- c(tail(prepared$training$date, 1L), prediction$date)
    last_value <- tail(prepared$training[[metric]], 1L)
    fan <- function(lower, upper, alpha) {
      polygon(c(band_dates, rev(band_dates)),
              c(last_value, lower, rev(c(last_value, upper))),
              col = grDevices::adjustcolor(scenario_colour, alpha.f = alpha), border = NA)
    }
    fan(band$lower95, band$upper95, alpha95)
    fan(band$lower80, band$upper80, alpha80)
  }
  # NA months break the historical line; a missing year is never connected.
  lines(history$date, history[[metric]], col = CHART_COLOURS[["muted"]], lwd = 2)
  first_forecast <- prediction$date[1L]
  abline(v = as.numeric(first_forecast) - 15, col = CHART_COLOURS[["light"]], lty = 3)
  lines(c(tail(prepared$training$date, 1L), prediction$date),
        c(tail(prepared$training[[metric]], 1L), prediction$baseline),
        col = baseline_colour, lty = 2, lwd = 2)
  lines(prediction$date, scenario_values, col = scenario_colour, lwd = 2.7)
  points(prediction$date, scenario_values, col = scenario_colour, pch = 16, cex = 0.55)
  # Legends hang from the top edge of the image, one row per legend, so they are
  # never clipped and stay above the plotting region.
  usr <- par("usr")
  figure_top <- grconvertY(1, "nfc", "user")
  line_height <- diff(grconvertY(c(0, par("csi")), "inches", "user"))
  chart_legend(usr[1], figure_top, c("Actual", "Baseline", "Your scenario"),
         col = c(CHART_COLOURS[["muted"]], baseline_colour, scenario_colour), lty = c(1, 2, 1),
         lwd = c(2, 2, 2.7), horiz = TRUE, bty = "n", cex = 0.83,
         text.col = CHART_COLOURS[["text_strong"]], xjust = 0, yjust = 1, xpd = NA)
  if (band$available) {
    # The 80% swatch shows the overlap of both layers, as it appears in the chart.
    chart_legend(usr[1], figure_top - 1.25 * line_height, c("80% interval", "95% interval"),
           fill = c(grDevices::adjustcolor(scenario_colour, alpha.f = 1 - (1 - alpha95) * (1 - alpha80)),
                    grDevices::adjustcolor(scenario_colour, alpha.f = alpha95)),
           border = NA, horiz = TRUE, bty = "n", cex = 0.83, text.col = CHART_COLOURS[["text_strong"]],
           xjust = 0, yjust = 1, xpd = NA)
  }
}

plot_forecast_price_sensitivity <- function(scenario) {
  required <- scenario$min_price_increase * 100
  upper <- max(20, scenario$chosen_price_pct * 1.3,
               if (is.finite(required)) min(required * 1.25, 500) else 20)
  prices <- seq(0, upper, length.out = 101L)
  income <- scenario$income_before_price + scenario$price_exposure * prices / 100
  limits <- overview_plot_limits(c(income, scenario$expenses))
  old <- par(mar = c(4.6, 5.5, 2.1, 1.1), family = "sans")
  on.exit(par(old))
  plot(prices, income, type = "n", axes = FALSE, ylim = limits,
       xlab = "Price increase on repriceable income (%)", ylab = "")
  ticks <- pretty(limits, 5)
  abline(h = ticks, col = CHART_COLOURS[["grid"]])
  axis(2, at = ticks, labels = format_compact_currency(ticks), las = 1,
       tick = FALSE, col.axis = CHART_COLOURS[["muted"]], cex.axis = 0.9)
  axis(1, col.axis = CHART_COLOURS[["muted"]], tick = FALSE)
  lines(prices, income, col = CHART_COLOURS[["primary"]], lwd = 2.7)
  abline(h = scenario$expenses, col = CHART_COLOURS[["secondary"]], lty = 2, lwd = 2)
  if (is.finite(required) && required <= upper) {
    abline(v = required, col = CHART_COLOURS[["highlight"]], lty = 3, lwd = 2)
  }
  points(scenario$chosen_price_pct, scenario$income, pch = 21, bg = CHART_COLOURS[["highlight"]],
         col = "white", cex = 1.5)
  chart_legend("topleft", c("Income at each price", "Scenario expenses", "Chosen price"),
         col = c(CHART_COLOURS[["primary"]], CHART_COLOURS[["secondary"]], CHART_COLOURS[["highlight"]]), lty = c(1, 2, NA),
         pch = c(NA, NA, 16), horiz = TRUE, bty = "n", cex = 0.84,
         text.col = CHART_COLOURS[["text_strong"]])
}
