# Input validation helpers. Every function returns a character vector of problems
# (empty vector = valid), so the UI can show a message instead of the app crashing.

validate_orders <- function(p, q, P, Q, d, D, period, max_models = 150) {
  msgs <- character()
  vals <- list(p = p, q = q, P = P, Q = Q, d = d, D = D)
  for (nm in names(vals)) {
    v <- vals[[nm]]
    if (length(v) != 1 || is.na(suppressWarnings(as.numeric(v)))) {
      msgs <- c(msgs, sprintf("%s must be a number.", nm))
    } else if (as.numeric(v) != round(as.numeric(v)) || as.numeric(v) < 0) {
      msgs <- c(msgs, sprintf("%s must be a non-negative integer.", nm))
    }
  }
  if (length(msgs)) return(msgs)
  p <- as.numeric(p); q <- as.numeric(q); P <- as.numeric(P); Q <- as.numeric(Q)
  d <- as.numeric(d); D <- as.numeric(D)
  if (p > 5 || q > 5) msgs <- c(msgs, "p and q must be at most 5.")
  if (P > 2 || Q > 2) msgs <- c(msgs, "P and Q must be at most 2.")
  if (d > 2) msgs <- c(msgs, "d must be 0, 1 or 2.")
  if (D > 1) msgs <- c(msgs, "D must be 0 or 1.")
  if (period <= 1 && (P > 0 || Q > 0 || D > 0)) {
    msgs <- c(msgs, "This series has no seasonal period: set P, Q and D to 0.")
  }
  n_models <- (p + 1) * (q + 1) * (P + 1) * (Q + 1)
  if (n_models > max_models) {
    msgs <- c(msgs, sprintf("%d models would be fitted; the limit is %d. Lower the maximum orders.",
                            n_models, max_models))
  }
  msgs
}

validate_date_window <- function(start, end, min_date, max_date) {
  msgs <- character()
  if (is.null(start) || is.null(end) || is.na(start) || is.na(end)) {
    return("Choose a start and an end date.")
  }
  if (start >= end) msgs <- c(msgs, "The start date must be before the end date.")
  if (start < min_date || end > max_date) {
    msgs <- c(msgs, sprintf("Dates must lie between %s and %s.", min_date, max_date))
  }
  msgs
}

validate_series_length <- function(n, period, train_frac) {
  msgs <- character()
  n_train <- floor(n * train_frac)
  n_test <- n - n_train
  min_train <- max(10, 2 * period + 2)
  if (n_train < min_train) {
    msgs <- c(msgs, sprintf("The training window has %d points but at least %d are needed for this frequency. Use a longer period or a lower frequency.",
                            n_train, min_train))
  }
  if (n_test < 2) msgs <- c(msgs, "The test window needs at least 2 points. Lower the training share.")
  msgs
}

mape <- function(actual, predicted) {
  keep <- actual != 0
  if (!any(keep)) return(NA_real_)
  mean(abs((actual[keep] - predicted[keep]) / actual[keep])) * 100
}

forecast_metrics <- function(actual, predicted) {
  c(RMSE = sqrt(mean((actual - predicted)^2)),
    MAE = mean(abs(actual - predicted)),
    MAPE = mape(actual, predicted))
}
