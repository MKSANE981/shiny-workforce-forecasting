# Unit-root / stationarity tests, guarded so that short or constant series return a message.

stationarity_tests <- function(y, lag_adf = c(0, 1)) {
  y <- as.numeric(stats::na.omit(y))
  if (length(y) < 12) {
    return(list(table = NULL, message = "At least 12 observations are needed for the stationarity tests."))
  }
  if (stats::sd(y) == 0) {
    return(list(table = NULL, message = "The series is constant: the tests are not defined."))
  }
  run <- function(name, expr, stationary_if) {
    res <- tryCatch(suppressWarnings(expr), error = function(e) NULL)
    if (is.null(res)) {
      return(data.frame(Test = name, Statistic = NA, p.value = NA, Reading = "failed", stringsAsFactors = FALSE))
    }
    p <- as.numeric(res$p.value)
    data.frame(Test = name, Statistic = round(as.numeric(res$statistic), 3), p.value = round(p, 4),
               Reading = if (stationary_if(p)) "stationary" else "non-stationary", stringsAsFactors = FALSE)
  }
  rows <- list()
  for (k in lag_adf) {
    rows[[length(rows) + 1]] <- run(sprintf("ADF (k = %d)", k), tseries::adf.test(y, k = k), function(p) p < 0.05)
  }
  rows[[length(rows) + 1]] <- run("KPSS (level)", tseries::kpss.test(y), function(p) p >= 0.05)
  rows[[length(rows) + 1]] <- run("Phillips-Perron", tseries::pp.test(y), function(p) p < 0.05)
  tab <- do.call(rbind, rows)
  n_non <- sum(tab$Reading == "non-stationary")
  list(table = tab,
       message = sprintf("%d of %d tests read the series as non-stationary.", n_non, nrow(tab)))
}

difference_series <- function(y_ts, d = 1, D = 0, period = 1) {
  out <- y_ts
  if (D > 0 && period > 1 && length(out) > period * D + 2) out <- diff(out, lag = period, differences = D)
  if (d > 0 && length(out) > d + 2) out <- diff(out, differences = d)
  out
}
