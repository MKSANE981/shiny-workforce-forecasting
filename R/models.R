# Forecasting models: SARIMA grid search, Prophet, and naive baselines.
# Every model is fitted on the training window only and scored on the held-out test window.

fit_sarima_grid <- function(train_ts, test_y, p_max, q_max, P_max, Q_max, d, D, period,
                            on_progress = function(i, n) NULL) {
  problems <- validate_orders(p_max, q_max, P_max, Q_max, d, D, period)
  if (length(problems)) stop(paste(problems, collapse = " "))
  grid <- expand.grid(p = 0:p_max, q = 0:q_max, P = 0:P_max, Q = 0:Q_max)
  if (period <= 1) grid <- grid[grid$P == 0 & grid$Q == 0, ]
  h <- length(test_y)
  rows <- vector("list", nrow(grid))
  for (i in seq_len(nrow(grid))) {
    g <- grid[i, ]
    on_progress(i, nrow(grid))
    fit <- tryCatch(
      suppressWarnings(forecast::Arima(train_ts, order = c(g$p, d, g$q),
                                       seasonal = list(order = c(g$P, D, g$Q), period = max(period, 1)),
                                       method = "CSS-ML")),
      error = function(e) NULL)
    row <- data.frame(p = g$p, d = d, q = g$q, P = g$P, D = D, Q = g$Q,
                      AICc = NA_real_, RMSE = NA_real_, MAE = NA_real_, MAPE = NA_real_,
                      LjungBox_p = NA_real_, status = "failed", stringsAsFactors = FALSE)
    if (!is.null(fit)) {
      fc <- tryCatch(as.numeric(forecast::forecast(fit, h = h)$mean), error = function(e) NULL)
      if (!is.null(fc) && all(is.finite(fc))) {
        m <- forecast_metrics(test_y, fc)
        lb <- tryCatch(stats::Box.test(stats::residuals(fit), lag = min(10, length(train_ts) %/% 5),
                                       type = "Ljung-Box")$p.value, error = function(e) NA_real_)
        row[, c("AICc", "RMSE", "MAE", "MAPE", "LjungBox_p", "status")] <-
          list(fit$aicc, m[["RMSE"]], m[["MAE"]], m[["MAPE"]], lb, "ok")
      }
    }
    rows[[i]] <- row
  }
  res <- do.call(rbind, rows)
  res <- res[order(res$AICc, na.last = TRUE), ]
  rownames(res) <- NULL
  res
}

best_sarima <- function(results, train_ts, test_y, period, criterion = c("AICc", "RMSE")) {
  criterion <- match.arg(criterion)
  ok <- results[results$status == "ok", ]
  if (!nrow(ok)) stop("No model could be fitted with these orders. Try smaller orders or a longer series.")
  best <- ok[which.min(ok[[criterion]]), ]
  fit <- forecast::Arima(train_ts, order = c(best$p, best$d, best$q),
                         seasonal = list(order = c(best$P, best$D, best$Q), period = max(period, 1)),
                         method = "CSS-ML")
  list(spec = best, fit = fit, forecast = forecast::forecast(fit, h = length(test_y)))
}

naive_forecasts <- function(train_y, h, period) {
  out <- list(Naive = rep(utils::tail(train_y, 1), h))
  if (period > 1 && length(train_y) >= period) {
    last_cycle <- utils::tail(train_y, period)
    out[["Seasonal naive"]] <- rep(last_cycle, length.out = h)
  }
  out
}

# Prophet on the training window; test window predicted at the test dates.
fit_prophet_model <- function(train, test, freq, mode = "additive", changepoint_prior = 0.05,
                              country_holidays = FALSE) {
  if (!requireNamespace("prophet", quietly = TRUE)) stop("The 'prophet' package is not installed.")
  n_years <- as.numeric(diff(range(train$ds))) / 365
  m <- prophet::prophet(
    growth = "linear", seasonality.mode = mode, changepoint.prior.scale = changepoint_prior,
    yearly.seasonality = if (freq == "yearly") FALSE else n_years >= 2,
    weekly.seasonality = freq == "daily", daily.seasonality = FALSE, fit = FALSE)
  if (country_holidays && freq == "daily") m <- prophet::add_country_holidays(m, "FR")
  m <- suppressMessages(prophet::fit.prophet(m, train))
  test_fc <- stats::predict(m, data.frame(ds = test$ds))
  full_ds <- c(train$ds, test$ds)
  fc_all <- stats::predict(m, data.frame(ds = full_ds))
  list(model = m, test_pred = test_fc$yhat, forecast = fc_all, cv = NULL)
}

# Rolling-origin cross-validation, with windows adapted to the series length.
prophet_cv <- function(model, train, test, freq) {
  step <- PROPHET_STEP_DAYS[[freq]]
  total_days <- as.numeric(diff(range(train$ds)))
  horizon <- nrow(test) * step
  initial <- total_days - horizon * 1.5
  if (initial < 3 * step) {
    return(list(table = NULL, message = "The training window is too short for cross-validation at this test size."))
  }
  period <- max(step, horizon / 2)
  cv <- tryCatch(
    suppressMessages(prophet::cross_validation(model, initial = initial, period = period,
                                               horizon = horizon, units = "days")),
    error = function(e) NULL)
  if (is.null(cv) || !nrow(cv)) {
    return(list(table = NULL, message = "Cross-validation could not run on this series."))
  }
  list(table = cv, message = sprintf("%d cutoffs, horizon of %d periods.", length(unique(cv$cutoff)), nrow(test)))
}
