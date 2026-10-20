# Visuals for monthly forecasts and the total-period pricing requirement.

plot_finance_forecast <- function(prepared, baseline, scenario, metric) {
  history <- prepared$history
  prediction <- baseline$forecast[baseline$forecast$metric == metric, , drop = FALSE]
  history <- tail(history, 36L)
  scenario_values <- scenario$table[[paste0("scenario_", metric)]]
  colour <- if (metric == "income") "#2563EB" else "#D97706"
  limits <- overview_plot_limits(c(history[[metric]], prediction$baseline, prediction$upper95, scenario_values))
  old <- par(mar = c(4.2, 5.3, 2.3, 1), family = "sans", xaxs = "i")
  on.exit(par(old))
  dates <- c(history$date, prediction$date)
  plot(range(dates), limits, type = "n", axes = FALSE, xlab = "", ylab = "")
  ticks <- pretty(limits, 5)
  abline(h = ticks, col = "#E2E8F0", lwd = 0.8)
  axis(2, at = ticks, labels = format_compact_currency(ticks), las = 1,
       tick = FALSE, col.axis = "#64748B", cex.axis = 0.88)
  step <- if (length(dates) > 24L) "6 months" else "3 months"
  x_ticks <- seq(min(dates), max(dates), by = step)
  axis(1, at = x_ticks, labels = forecast_month_label(x_ticks, "\n"), tick = FALSE,
       col.axis = "#64748B", cex.axis = 0.86)
  if (all(is.finite(prediction$lower95))) {
    polygon(c(prediction$date, rev(prediction$date)),
            c(prediction$lower95, rev(prediction$upper95)),
            col = grDevices::adjustcolor(colour, alpha.f = 0.08), border = NA)
    polygon(c(prediction$date, rev(prediction$date)),
            c(prediction$lower80, rev(prediction$upper80)),
            col = grDevices::adjustcolor(colour, alpha.f = 0.15), border = NA)
  }
  # NA months break the historical line; a missing year is never connected.
  lines(history$date, history[[metric]], col = "#64748B", lwd = 2)
  first_forecast <- prediction$date[1L]
  abline(v = as.numeric(first_forecast) - 15, col = "#CBD5E1", lty = 3)
  lines(c(tail(prepared$training$date, 1L), prediction$date),
        c(tail(prepared$training[[metric]], 1L), prediction$baseline),
        col = colour, lty = 2, lwd = 2)
  lines(prediction$date, scenario_values, col = "#0F766E", lwd = 2.7)
  points(prediction$date, scenario_values, col = "#0F766E", pch = 16, cex = 0.55)
  legend("topleft", c("Actual", "Baseline", "Your scenario"),
         col = c("#64748B", colour, "#0F766E"), lty = c(1, 2, 1),
         lwd = c(2, 2, 2.7), horiz = TRUE, bty = "n", cex = 0.83,
         text.col = "#334155")
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
  abline(h = ticks, col = "#E2E8F0")
  axis(2, at = ticks, labels = format_compact_currency(ticks), las = 1,
       tick = FALSE, col.axis = "#64748B", cex.axis = 0.9)
  axis(1, col.axis = "#64748B", tick = FALSE)
  lines(prices, income, col = "#2563EB", lwd = 2.7)
  abline(h = scenario$expenses, col = "#D97706", lty = 2, lwd = 2)
  if (is.finite(required) && required <= upper) {
    abline(v = required, col = "#0F766E", lty = 3, lwd = 2)
  }
  points(scenario$chosen_price_pct, scenario$income, pch = 21, bg = "#0F766E",
         col = "white", cex = 1.5)
  legend("topleft", c("Income at each price", "Scenario expenses", "Chosen price"),
         col = c("#2563EB", "#D97706", "#0F766E"), lty = c(1, 2, NA),
         pch = c(NA, NA, 16), horiz = TRUE, bty = "n", cex = 0.84,
         text.col = "#334155")
}
