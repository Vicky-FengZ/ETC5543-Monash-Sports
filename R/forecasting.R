# Forecast preparation, benchmark models and business scenarios.
# Uses already-cleaned reports; never reads Excel in a render.

FORECAST_MODEL_NAMES <- c(
  seasonal_naive = "Seasonal naive",
  naive = "Naive",
  holt_winters = "Holt-Winters (additive)"
)

forecast_future_dates <- function(last_date, horizon) {
  seq(as.Date(last_date), by = "month", length.out = horizon + 1L)[-1L]
}

forecast_month_label <- function(date, separator = " ") {
  paste(month.abb[as.integer(format(as.Date(date), "%m"))],
        format(as.Date(date), "%Y"), sep = separator)
}

# Show an implementable minimum, rather than a rounded-down price threshold.
forecast_required_rate <- function(value) {
  if (!is.finite(value)) return("Not achievable")
  paste0(formatC(max(0, ceiling(value * 1000 - 1e-9) / 10), format = "f", digits = 1), "%")
}

empty_forecast_history <- function() {
  data.frame(date = as.Date(character()), income = double(), expenses = double())
}

# Report only observed zeros after the last non-zero total, never missing months.
forecast_final_zero_months <- function(history, columns) {
  if (!nrow(history)) return(as.Date(character()))
  values <- as.matrix(history[columns])
  complete <- rowSums(is.finite(values)) == length(columns)
  nonzero <- which(complete & rowSums(values != 0, na.rm = TRUE) > 0L)
  last_nonzero <- if (length(nonzero)) max(nonzero) else 0L
  history$date[complete & seq_len(nrow(history)) > last_nonzero &
                 rowSums(values != 0, na.rm = TRUE) == 0L]
}

forecast_monthly_reports <- function(data) {
  if (is.null(data)) return(NULL)
  if (inherits(data, "monthly_site_report_data")) return(data)
  if (!is.list(data)) return(NULL)
  if (!is.null(data$monthly_reports)) return(data$monthly_reports)
  data$monthly
}

forecast_venues_reports <- function(data) {
  if (is.null(data) || !is.list(data)) return(NULL)
  if (inherits(data, "venues_tracking_report_data")) return(data)
  if (!is.null(data$venues_reports)) return(data$venues_reports)
  data$venues
}

prepare_forecast_history <- function(
  reports, site = "Overall", cutoff = NULL, from_year = NULL,
  include_trailing_zeros = FALSE
) {
  history <- empty_forecast_history()
  zero_months <- as.Date(character())
  fail <- function(message) {
    list(history = history, training = empty_forecast_history(), notes = character(),
         error = message, excluded_months = 0L, zero_months = zero_months)
  }
  finance <- reports$finance
  if (!is.data.frame(finance) || nrow(finance) == 0L) {
    return(fail("Upload Monthly Site Reports in Data to build a forecast."))
  }
  required <- c("date", "report_year", "site", "is_summary", "metric", "value_type", "value")
  if (!all(required %in% names(finance))) {
    return(fail("The cleaned Monthly Site Report is missing required monthly finance columns."))
  }
  x <- finance[finance$is_summary %in% TRUE &
                 finance$metric %in% c("Income", "Expenses") &
                 finance$value_type %in% "Actual" & !is.na(finance$date), , drop = FALSE]
  if (nrow(x) == 0L) return(fail("No monthly Actual Income and Expenses are available."))
  x$date <- as.Date(x$date)
  if (any(as.integer(format(x$date, "%Y")) != x$report_year, na.rm = TRUE)) {
    return(fail("A finance date does not match its report year. Review the cleaned data first."))
  }
  x$date <- as.Date(format(x$date, "%Y-%m-01"))
  if (!is.null(cutoff) && length(cutoff) == 1L && !is.na(cutoff)) {
    x <- x[x$date <= as.Date(cutoff), , drop = FALSE]
  }
  if (!is.null(from_year) && length(from_year) == 1L && is.finite(from_year)) {
    x <- x[x$report_year >= from_year, , drop = FALSE]
  }
  if (nrow(x) == 0L) return(fail("No actual data exist for this historical period."))
  notes <- character()
  annual_parts <- list()
  for (year in sort(unique(x$report_year))) {
    rows <- x[x$report_year == year, , drop = FALSE]
    organisation <- tolower(trimws(rows$site)) == "monash sport"
    organisation[is.na(organisation)] <- FALSE
    if (identical(site, "Overall")) {
      if (any(organisation)) {
        rows <- rows[organisation, , drop = FALSE]
      } else {
        rows <- rows[!organisation & !is.na(rows$site), , drop = FALSE]
        notes <- c(notes, paste(year, "uses the sum of available site summaries; confirm site coverage."))
      }
    } else {
      rows <- rows[rows$site %in% site, , drop = FALSE]
    }
    if (nrow(rows) == 0L) next
    keys <- paste(rows$date, rows$site, rows$metric, sep = "|")
    if (anyDuplicated(keys)) {
      return(fail(paste("More than one actual value exists for a site/month/metric in", year,
                        "Remove the overlapping upload before forecasting.")))
    }
    expected_sites <- unique(rows$site)
    value_for <- function(date, metric) {
      part <- rows[rows$date == date & rows$metric == metric, , drop = FALSE]
      values <- suppressWarnings(as.numeric(part$value))
      if (nrow(part) != length(expected_sites) || any(!is.finite(values))) return(NA_real_)
      sum(values)
    }
    dates <- sort(unique(rows$date))
    annual_parts[[length(annual_parts) + 1L]] <- data.frame(
      date = dates,
      income = vapply(seq_along(dates), function(i) value_for(dates[i], "Income"), numeric(1)),
      expenses = vapply(seq_along(dates), function(i) value_for(dates[i], "Expenses"), numeric(1))
    )
  }
  if (length(annual_parts) == 0L) return(fail("No summary actuals are available for this scope."))
  observed <- do.call(rbind, annual_parts)
  history <- merge(data.frame(date = seq(min(observed$date), max(observed$date), by = "month")),
                   observed, by = "date", all.x = TRUE, sort = TRUE)
  complete <- is.finite(history$income) & is.finite(history$expenses)
  if (!any(complete)) return(fail("Income and Expenses must both be present for a month."))
  last_complete <- max(which(complete))
  if (last_complete < nrow(history)) {
    notes <- c(notes, "Incomplete final months were excluded. The forecast starts after the last complete month.")
    history <- history[seq_len(last_complete), , drop = FALSE]
    complete <- complete[seq_len(last_complete)]
  }
  zero_months <- forecast_final_zero_months(history, c("income", "expenses"))
  trailing_zeros <- length(zero_months)
  last_nonzero <- which(complete & (history$income != 0 | history$expenses != 0))
  if (trailing_zeros > 0L) {
    if (!isTRUE(include_trailing_zeros)) {
      if (!length(last_nonzero)) {
        return(fail("All reported months are zero. Check the final zero months in More options before forecasting."))
      }
      history <- history[seq_len(max(last_nonzero)), , drop = FALSE]
      complete <- complete[seq_len(max(last_nonzero))]
      notes <- c(notes, paste(trailing_zeros, "final all-zero month(s) excluded until confirmed as actuals."))
    } else {
      notes <- c(notes, paste(trailing_zeros, "final all-zero month(s) included as confirmed actuals."))
    }
  }
  last_missing <- if (any(!complete)) max(which(!complete)) else 0L
  training <- history[seq.int(last_missing + 1L, nrow(history)), , drop = FALSE]
  if (last_missing > 0L) {
    notes <- c(notes, paste("A gap or missing value was found. Only the latest",
                           nrow(training), "continuous months are used for modelling; no gap is filled."))
  }
  if (any(training$income < 0 | training$expenses < 0)) {
    return(fail("Negative monthly totals were found. Review refunds or accounting adjustments before using this planning model."))
  }
  if (nrow(training) < 24L) {
    notes <- c(notes, "Less than two continuous years: use the baseline as a provisional planning reference.")
  }
  if (is.data.frame(reports$validation) && any(reports$validation$level %in% "warning")) {
    notes <- c(notes, "Uploaded reports have validation warnings. Review them in Data before making planning decisions.")
  }
  list(history = history, training = training, notes = unique(notes), error = NULL,
       excluded_months = if (include_trailing_zeros) 0L else trailing_zeros,
       zero_months = zero_months)
}

# All benchmark predictions are constrained to non-negative monetary totals.
# Intervals are monthly model intervals, conditional on historical dynamics.
forecast_model_predict <- function(y, horizon, method, intervals = TRUE) {
  n <- length(y)
  if (n < 12L || any(!is.finite(y))) stop("At least 12 continuous months are required.", call. = FALSE)
  step <- seq_len(horizon)
  lower80 <- upper80 <- lower95 <- upper95 <- rep(NA_real_, horizon)
  if (method == "holt_winters") {
    if (n < 24L) stop("Holt-Winters requires at least 24 continuous months.", call. = FALSE)
    fit <- suppressWarnings(stats::HoltWinters(stats::ts(y, frequency = 12), seasonal = "additive"))
    point <- as.numeric(stats::predict(fit, n.ahead = horizon))
    if (intervals) {
      interval80 <- stats::predict(fit, n.ahead = horizon, prediction.interval = TRUE, level = 0.8)
      interval95 <- stats::predict(fit, n.ahead = horizon, prediction.interval = TRUE, level = 0.95)
      lower80 <- interval80[, "lwr"]
      upper80 <- interval80[, "upr"]
      lower95 <- interval95[, "lwr"]
      upper95 <- interval95[, "upr"]
    }
  } else if (method %in% c("naive", "seasonal_naive")) {
    lag <- if (method == "seasonal_naive") 12L else 1L
    point <- if (lag == 12L) tail(y, 12L)[(step - 1L) %% 12L + 1L] else rep(tail(y, 1L), horizon)
    residuals <- if (n > lag) y[seq.int(lag + 1L, n)] - y[seq_len(n - lag)] else numeric()
    # A single seasonal cycle cannot estimate a seasonal residual variance.
    if (intervals && length(residuals) >= 12L) {
      sigma <- sqrt(mean(residuals^2))
      se <- sigma * if (lag == 12L) sqrt(ceiling(step / 12L)) else sqrt(step)
      lower80 <- point - stats::qnorm(0.9) * se
      upper80 <- point + stats::qnorm(0.9) * se
      lower95 <- point - stats::qnorm(0.975) * se
      upper95 <- point + stats::qnorm(0.975) * se
    }
  } else {
    stop("Unknown forecast model.", call. = FALSE)
  }
  if (any(!is.finite(point))) stop("The model did not produce finite forecasts.", call. = FALSE)
  data.frame(baseline = pmax(0, point), lower80 = pmax(0, lower80),
             upper80 = pmax(0, upper80), lower95 = pmax(0, lower95), upper95 = pmax(0, upper95))
}

backtest_forecast_models <- function(y, horizon) {
  n <- length(y)
  models <- c("seasonal_naive", "naive")
  initial <- 12L
  # Holt-Winters competes on exactly the same folds when enough history exists.
  if (n >= 30L) {
    models <- c(models, "holt_winters")
    initial <- 24L
  }
  test_horizon <- min(horizon, 12L, n - initial - 5L)
  if (test_horizon < 1L) return(data.frame())
  origins <- tail(seq.int(initial, n - test_horizon), 12L)
  results <- lapply(models, function(method) {
    errors <- lapply(origins, function(origin) {
      prediction <- tryCatch(forecast_model_predict(y[seq_len(origin)], test_horizon, method, FALSE),
                             error = function(e) NULL)
      if (is.null(prediction)) return(NULL)
      y[origin + seq_len(test_horizon)] - prediction$baseline
    })
    # Exclude a candidate rather than comparing it on fewer/easier folds.
    if (any(vapply(errors, is.null, logical(1)))) return(NULL)
    e <- unlist(errors, use.names = FALSE)
    data.frame(method = method, model = unname(FORECAST_MODEL_NAMES[method]),
               rmse = sqrt(mean(e^2)), mae = mean(abs(e)),
               origins = length(origins), horizon = test_horizon)
  })
  results <- Filter(Negate(is.null), results)
  if (!length(results)) return(data.frame())
  result <- do.call(rbind, results)
  result[order(result$rmse, match(result$method, names(FORECAST_MODEL_NAMES))), , drop = FALSE]
}

fit_finance_forecasts <- function(prepared, horizon = 12L, method = "auto") {
  fail <- function(message) list(error = message, forecast = data.frame(), scores = data.frame(),
                                 models = character(), notes = character())
  if (!is.null(prepared$error)) return(fail(prepared$error))
  if (!horizon %in% c(3L, 6L, 12L)) return(fail("Choose a 3, 6 or 12 month forecast horizon."))
  training <- prepared$training
  if (nrow(training) < 12L) return(fail("At least 12 continuous months of Income and Expenses are required."))
  if (!method %in% c("auto", names(FORECAST_MODEL_NAMES))) return(fail("Choose a supported forecast model."))
  dates <- forecast_future_dates(tail(training$date, 1L), horizon)
  scores <- list()
  tables <- list()
  selected <- character()
  notes <- character()
  for (metric in c("income", "expenses")) {
    y <- training[[metric]]
    validation <- backtest_forecast_models(y, horizon)
    chosen <- if (method == "auto") {
      if (nrow(validation)) validation$method[[1L]] else "seasonal_naive"
    } else method
    prediction <- tryCatch(forecast_model_predict(y, horizon, chosen), error = function(e) e)
    if (inherits(prediction, "error")) return(fail(conditionMessage(prediction)))
    tables[[metric]] <- data.frame(date = dates, metric = metric, prediction)
    selected[metric] <- unname(FORECAST_MODEL_NAMES[chosen])
    if (nrow(validation)) {
      validation$metric <- metric
      validation$selected <- validation$method == chosen
      scores[[metric]] <- validation
    } else {
      notes <- c(notes, "Too little continuous history for rolling-origin testing; no accuracy claim is made.")
    }
    if (all(is.na(prediction$lower80))) {
      notes <- c(notes, "Prediction intervals are unavailable for a measure with insufficient residual history.")
    }
  }
  list(error = NULL, forecast = do.call(rbind, tables),
       scores = if (length(scores)) do.call(rbind, scores) else data.frame(),
       models = selected, notes = unique(notes))
}

forecast_check_number <- function(value, name, minimum = -Inf, maximum = Inf) {
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value < minimum || value > maximum) {
    stop(paste(name, "must be a number between", minimum, "and", maximum), call. = FALSE)
  }
  as.numeric(value)
}

prepare_casual_wage_history <- function(
  reports, cutoff = NULL, from_year = NULL, include_trailing_zeros = FALSE
) {
  empty <- data.frame(date = as.Date(character()), value = double())
  zero_months <- as.Date(character())
  fail <- function(message) list(error = message, history = empty, training = empty,
                                notes = character(), sources = character(), zero_months = zero_months)
  x <- reports$casual_wages
  required <- c("date", "report_year", "period_type", "metric", "value", "is_reported")
  if (!is.data.frame(x) || !nrow(x) || !all(required %in% names(x))) {
    return(fail("Upload Venues Tracking Reports for the quick pay-rise estimate, or switch to Manual adjustment. Price-only planning still works with a zero quick pay rise."))
  }
  x <- x[x$period_type %in% "Month" & x$metric %in% "Actual" & !is.na(x$date), , drop = FALSE]
  x$date <- as.Date(x$date)
  if (any(as.integer(format(x$date, "%Y")) != x$report_year, na.rm = TRUE) ||
      ("salary_year" %in% names(x) && any(x$salary_year != x$report_year, na.rm = TRUE))) {
    return(fail("Casual wage dates or salary years do not match their report year."))
  }
  x$date <- as.Date(format(x$date, "%Y-%m-01"))
  if (!is.null(cutoff)) x <- x[x$date <= as.Date(cutoff), , drop = FALSE]
  if (!is.null(from_year) && length(from_year) == 1L && is.finite(from_year)) {
    x <- x[x$report_year >= from_year, , drop = FALSE]
  }
  if (!nrow(x)) return(fail("No casual wage actuals are available before this forecast period."))
  if (anyDuplicated(x$date)) return(fail("Overlapping casual wage months were found. Remove the duplicate Venues upload."))
  x$value <- suppressWarnings(as.numeric(x$value))
  x$value[!x$is_reported %in% TRUE | !is.finite(x$value)] <- NA_real_
  history <- merge(data.frame(date = seq(min(x$date), max(x$date), by = "month")),
                   x[c("date", "value")], by = "date", all.x = TRUE, sort = TRUE)
  complete <- is.finite(history$value)
  if (!any(complete)) return(fail("The casual wage table has no reported monthly Actual values."))
  notes <- character()
  last <- max(which(complete))
  if (last < nrow(history)) {
    notes <- c(notes, "Unreported final wage months are excluded, not treated as zero or as a complete annual total.")
    history <- history[seq_len(last), , drop = FALSE]
    complete <- complete[seq_len(last)]
  }
  zero_months <- forecast_final_zero_months(history, "value")
  if (!include_trailing_zeros && length(zero_months)) {
    nonzero <- which(complete & history$value != 0)
    if (!length(nonzero)) return(fail("All reported casual wages are zero. Confirm the final zero months in More options first."))
    last <- max(nonzero)
    notes <- c(notes, "Final zero wage months are excluded until confirmed as actuals in More options.")
    history <- history[seq_len(last), , drop = FALSE]
    complete <- complete[seq_len(last)]
  }
  last_missing <- if (any(!complete)) max(which(!complete)) else 0L
  training <- history[seq.int(last_missing + 1L, nrow(history)), , drop = FALSE]
  if (any(training$value < 0)) return(fail("Negative casual wages need review before they can be used as a cost baseline."))
  if (nrow(training) < 12L) return(fail("At least 12 continuous months of reported casual wages are needed for an automatic wage baseline."))
  if (last_missing > 0L) notes <- c(notes, "Only the latest continuous wage history is modelled; missing months are not filled.")
  if (nrow(training) < 24L) notes <- c(notes, "The casual wage baseline has less than two continuous years and is provisional.")
  sources <- if ("source_file" %in% names(x)) unique(as.character(x$source_file[x$date %in% training$date])) else character()
  list(error = NULL, history = history, training = training, notes = notes,
       sources = sources, zero_months = zero_months)
}

# Forecast only the recorded casual payroll component. Blank months are not
# actual zeros, and predictions used to align dates never become source actuals.
prepare_casual_wage_baseline <- function(
  reports, dates, cutoff = NULL, from_year = NULL,
  include_trailing_zeros = FALSE, method = "auto"
) {
  fail <- function(message) list(error = message, forecast = data.frame(), scores = data.frame(),
    history = data.frame(), training = data.frame(), notes = character(),
    model = character(), sources = character(), zero_months = as.Date(character()))
  dates <- as.Date(dates)
  if (!length(dates) || anyNA(dates) || anyDuplicated(dates) ||
      !identical(dates, seq(min(dates), by = "month", length.out = length(dates)))) {
    return(fail("The casual wage forecast needs a continuous monthly planning period."))
  }
  if (is.null(cutoff)) cutoff <- seq(dates[1], by = "-1 month", length.out = 2L)[2L]
  cutoff <- as.Date(cutoff)
  if (length(cutoff) != 1L || is.na(cutoff) || cutoff >= min(dates)) {
    return(fail("Casual wage actuals must stop before the forecast period."))
  }
  prepared <- prepare_casual_wage_history(reports, cutoff, from_year, include_trailing_zeros)
  if (!is.null(prepared$error)) return(c(prepared, list(forecast = data.frame(), scores = data.frame(), model = character())))
  history <- prepared$history
  training <- prepared$training
  notes <- prepared$notes
  prediction_dates <- seq(tail(training$date, 1L), max(dates), by = "month")[-1L]
  if (!length(prediction_dates) || length(prediction_dates) > 24L || !all(dates %in% prediction_dates)) {
    return(fail("Casual wage actuals are too old for this planning period. Upload more recent Venues reports or switch to Manual adjustment."))
  }
  if (!method %in% c("auto", names(FORECAST_MODEL_NAMES))) return(fail("Choose a supported casual wage model."))
  scores <- backtest_forecast_models(training$value, length(dates))
  chosen <- if (method == "auto") {
    if (nrow(scores)) scores$method[[1L]] else "seasonal_naive"
  } else method
  prediction <- tryCatch(forecast_model_predict(training$value, length(prediction_dates), chosen), error = function(e) e)
  if (inherits(prediction, "error")) return(fail(conditionMessage(prediction)))
  forecast <- data.frame(date = prediction_dates, prediction)
  forecast <- forecast[match(dates, forecast$date), , drop = FALSE]
  bridge <- sum(prediction_dates < min(dates))
  if (bridge > 0L) notes <- c(notes, paste(bridge, "month(s) between the last wage actual and this plan are modelled for date alignment, never relabelled as actuals."))
  if (!nrow(scores)) notes <- c(notes, "Too little wage history for rolling-origin testing; no wage accuracy claim is made.")
  if (is.data.frame(reports$validation) && any(reports$validation$level %in% "warning")) {
    notes <- c(notes, "The Venues import has validation warnings. Review them in Data.")
  }
  if (nrow(scores)) scores$selected <- scores$method == chosen
  list(error = NULL, history = history, training = training, forecast = forecast,
       scores = scores, notes = unique(notes), model = unname(FORECAST_MODEL_NAMES[chosen]),
       sources = prepared$sources, last_actual = tail(training$date, 1L), bridge_months = bridge,
       zero_months = prepared$zero_months)
}

calculate_finance_scenario <- function(
  baseline, wage_mode = "annual_amount", wage_amount = 0, wage_pct = 0,
  employee_count = 1, wage_share = NA_real_, other_cost_pct = 0,
  price_pct = 0, price_share = 100, volume_pct = 0, effective_month = 1L,
  casual_baseline = NULL, casual_hours_pct = 0
) {
  if (!is.null(baseline$error)) stop(baseline$error, call. = FALSE)
  x <- baseline$forecast
  if (!is.data.frame(x) || !nrow(x)) stop("No baseline forecast is available.", call. = FALSE)
  # Keep legacy calculation modes for existing R callers; the UI exposes only
  # a quick reported-payroll estimate and a signed monthly adjustment.
  wage_mode <- match.arg(wage_mode, c("annual_amount", "per_employee", "percentage", "casual_baseline", "monthly_amount"))
  wage_amount <- if (wage_mode == "monthly_amount") {
    if (!is.numeric(wage_amount) || length(wage_amount) != 1L || !is.finite(wage_amount)) {
      stop("Enter a finite number for Monthly payroll adjustment (AUD). Use 0 for no change.", call. = FALSE)
    }
    as.numeric(wage_amount)
  } else if (wage_mode %in% c("annual_amount", "per_employee")) {
    forecast_check_number(wage_amount, "Annual wage increase", 0)
  } else 0
  wage_pct <- if (wage_mode %in% c("percentage", "casual_baseline")) forecast_check_number(wage_pct, "Wage increase (%)", 0, 100) else 0
  casual_hours_pct <- if (wage_mode == "casual_baseline") {
    forecast_check_number(casual_hours_pct, "Casual paid-hours change (%)", -100, 200)
  } else 0
  casual_changed <- wage_mode == "casual_baseline" && (wage_pct != 0 || casual_hours_pct != 0)
  employee_count <- if (wage_mode == "per_employee") forecast_check_number(employee_count, "Employee/FTE count", 0) else 1
  other_cost_pct <- forecast_check_number(other_cost_pct, "Other cost change (%)", -100, 200)
  price_pct <- forecast_check_number(price_pct, "Price increase (%)", 0, 500)
  price_share <- forecast_check_number(price_share, "Repriceable income share (%)", 0, 100)
  volume_pct <- forecast_check_number(volume_pct, "Demand/volume change (%)", -99, 200)
  income <- x[x$metric == "income", , drop = FALSE]
  expenses <- x[x$metric == "expenses", , drop = FALSE]
  if (!identical(income$date, expenses$date)) stop("Income and Expenses dates do not match.", call. = FALSE)
  h <- nrow(income)
  effective_month <- forecast_check_number(effective_month, "Effective month", 1, h)
  if (effective_month != as.integer(effective_month)) stop("Effective month must be a whole month.", call. = FALSE)
  needs_share <- (wage_mode == "percentage" && wage_pct > 0) || other_cost_pct != 0
  if (needs_share) {
    wage_share <- forecast_check_number(wage_share, "Payroll share of Expenses (%)", 0, 100)
    if (wage_mode == "percentage" && wage_pct > 0 && wage_share == 0) {
      stop("Enter a positive payroll share before applying a percentage wage increase.", call. = FALSE)
    }
  } else if (!is.finite(wage_share)) {
    wage_share <- 0
  } else {
    wage_share <- forecast_check_number(wage_share, "Payroll share of Expenses (%)", 0, 100)
  }
  active <- seq_len(h) >= effective_month
  salary_share <- wage_share / 100
  casual_values <- rep(NA_real_, h)
  if (wage_mode == "casual_baseline") {
    casual_available <- is.list(casual_baseline) && is.null(casual_baseline$error) &&
      is.data.frame(casual_baseline$forecast) && nrow(casual_baseline$forecast) > 0L
    if (casual_available) {
      cf <- casual_baseline$forecast
      if (!identical(cf$date, expenses$date) || any(!is.finite(cf$baseline) | cf$baseline < 0)) {
        stop("Casual wage baseline dates or values do not match this plan.", call. = FALSE)
      }
      casual_values <- cf$baseline
    } else if (casual_changed) {
      message <- if (is.list(casual_baseline) && !is.null(casual_baseline$error)) casual_baseline$error else "Upload Venues Tracking Reports for the quick estimate, or switch to Manual adjustment."
      stop(message, call. = FALSE)
    }
    if (casual_changed && any(casual_values > expenses$baseline)) {
      stop("Casual wages exceed the total Expenses baseline in a month. Review scope and forecast consistency before applying pay or paid-hours changes.", call. = FALSE)
    }
  }
  wage_addition <- if (wage_mode == "casual_baseline") {
    if (!casual_changed) rep(0, h) else {
      casual_values * ((1 + casual_hours_pct / 100) * (1 + wage_pct / 100) - 1)
    }
  } else if (wage_mode == "percentage") {
    expenses$baseline * salary_share * wage_pct / 100
  } else if (wage_mode == "monthly_amount") {
    rep(wage_amount, h)
  } else {
    rep(wage_amount * if (wage_mode == "per_employee") employee_count else 1, h) / 12
  }
  wage_addition <- wage_addition * active
  other_addition <- expenses$baseline * (1 - salary_share) * other_cost_pct / 100 * active
  # Volume is an explicit user scenario, not an estimated price elasticity.
  demand_factor <- 1 + active * volume_pct / 100
  price_factor <- 1 + active * price_share / 100 * price_pct / 100
  scenario_income <- income$baseline * demand_factor * price_factor
  scenario_expenses <- expenses$baseline + wage_addition + other_addition
  if (any(!is.finite(scenario_income)) || any(!is.finite(scenario_expenses)) ||
      !is.finite(sum(scenario_income)) || !is.finite(sum(scenario_expenses))) {
    stop("The scenario amounts are too large to calculate safely. Review your assumptions.", call. = FALSE)
  }
  if (any(scenario_expenses < 0)) {
    stop("Cost changes would make monthly Expenses negative. Review your assumptions.", call. = FALSE)
  }
  before_price <- income$baseline * demand_factor
  exposed_income <- sum(before_price[active]) * price_share / 100
  additional_income <- max(0, sum(scenario_expenses) - sum(before_price))
  min_price <- if (additional_income == 0) 0 else if (exposed_income > 0) additional_income / exposed_income else Inf
  baseline_total <- sum(income$baseline)
  min_income_growth <- if (sum(scenario_expenses) <= baseline_total) 0 else if (baseline_total > 0) {
    sum(scenario_expenses) / baseline_total - 1
  } else Inf
  # Interval bounds move with the scenario exactly as the baseline does, so the
  # shaded ranges keep financial-model uncertainty only, not assumption risk.
  band <- function(frame, name) if (name %in% names(frame)) frame[[name]] else rep(NA_real_, nrow(frame))
  scenario_income_band <- function(values) values * demand_factor * price_factor
  scenario_expenses_band <- function(values) pmax(0, if (wage_mode == "percentage") {
    values * (1 + active * (salary_share * wage_pct + (1 - salary_share) * other_cost_pct) / 100)
  } else values * (1 + active * (1 - salary_share) * other_cost_pct / 100) + wage_addition)
  table <- data.frame(
    date = income$date, baseline_income = income$baseline, scenario_income = scenario_income,
    baseline_expenses = expenses$baseline, scenario_expenses = scenario_expenses,
    extra_wage_cost = wage_addition, other_cost_change = other_addition,
    baseline_casual_wages = casual_values,
    scenario_casual_wages = casual_values + wage_addition,
    required_income_to_break_even = scenario_expenses,
    income_lower80 = band(income, "lower80"), income_upper80 = band(income, "upper80"),
    income_lower95 = band(income, "lower95"), income_upper95 = band(income, "upper95"),
    expenses_lower80 = band(expenses, "lower80"), expenses_upper80 = band(expenses, "upper80"),
    expenses_lower95 = band(expenses, "lower95"), expenses_upper95 = band(expenses, "upper95"),
    scenario_income_lower80 = scenario_income_band(band(income, "lower80")),
    scenario_income_upper80 = scenario_income_band(band(income, "upper80")),
    scenario_income_lower95 = scenario_income_band(band(income, "lower95")),
    scenario_income_upper95 = scenario_income_band(band(income, "upper95")),
    scenario_expenses_lower80 = scenario_expenses_band(band(expenses, "lower80")),
    scenario_expenses_upper80 = scenario_expenses_band(band(expenses, "upper80")),
    scenario_expenses_lower95 = scenario_expenses_band(band(expenses, "lower95")),
    scenario_expenses_upper95 = scenario_expenses_band(band(expenses, "upper95"))
  )
  assumptions <- data.frame(
    assumption = c("wage_mode", "price_increase_pct", "repriceable_income_share_pct",
                   "volume_change_pct", "effective_from", "forecast_months"),
    value = as.character(c(wage_mode, price_pct, price_share, volume_pct,
                           format(income$date[effective_month], "%Y-%m"), h))
  )
  payroll_assumptions <- switch(wage_mode,
    casual_baseline = c(wage_increase_pct = wage_pct, casual_paid_hours_change_pct = casual_hours_pct),
    monthly_amount = c(monthly_payroll_adjustment = wage_amount),
    annual_amount = c(annual_wage_increase = wage_amount),
    per_employee = c(annual_wage_increase = wage_amount, employee_fte_count = employee_count),
    percentage = c(wage_increase_pct = wage_pct, payroll_share_pct = wage_share))
  assumptions <- rbind(assumptions, data.frame(assumption = names(payroll_assumptions), value = as.character(payroll_assumptions)))
  if (other_cost_pct != 0) {
    assumptions <- rbind(assumptions, data.frame(
      assumption = c("other_cost_change_pct", "payroll_share_pct"),
      value = as.character(c(other_cost_pct, wage_share))))
    assumptions <- assumptions[!duplicated(assumptions$assumption), , drop = FALSE]
  }
  if (wage_mode == "monthly_amount") {
    assumptions <- rbind(assumptions, data.frame(
      assumption = c("payroll_reference", "payroll_adjustment_timing"),
      value = c("User-entered net adjustment to Overall forecast Expenses; positive adds costs, negative reduces them; only changes not already reflected in the forecast",
                "Same monthly amount from the effective month; no compounding; no automatic payroll decomposition")))
  }
  list(table = table, income = sum(scenario_income), expenses = sum(scenario_expenses),
       baseline_income = baseline_total, baseline_expenses = sum(expenses$baseline),
       extra_wage_cost = sum(wage_addition), min_income_growth = min_income_growth,
       min_price_increase = min_price, income_before_price = sum(before_price),
       price_exposure = exposed_income, chosen_price_pct = price_pct,
       covers_costs = sum(scenario_income) + 1e-8 >= sum(scenario_expenses),
       baseline_casual_wages = if (all(is.finite(casual_values))) sum(casual_values) else NA_real_,
       scenario_casual_wages = if (all(is.finite(casual_values))) sum(casual_values + wage_addition) else NA_real_,
       assumptions = assumptions)
}
