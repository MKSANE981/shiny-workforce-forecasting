make_series <- function(n = 120, seed = 3) {
  set.seed(seed)
  data.frame(ds = seq(as.Date("2015-01-01"), by = "month", length.out = n),
             y = round(100 + 0.8 * seq_len(n) + 8 * sin(2 * pi * seq_len(n) / 12) + rnorm(n, 0, 2)))
}

test_that("stationarity tests are guarded", {
  expect_null(stationarity_tests(1:5)$table)
  expect_null(stationarity_tests(rep(3, 50))$table)
  expect_null(stationarity_tests(c(1, NA, NA))$table)
  ok <- stationarity_tests(cumsum(rnorm(80)))
  expect_equal(nrow(ok$table), 4)
})

test_that("grid search returns a table and survives failing models", {
  s <- make_series()
  sp <- split_series(s, 0.8)
  tr <- as_ts(sp$train, "monthly")
  res <- fit_sarima_grid(tr, sp$test$y, p_max = 2, q_max = 1, P_max = 1, Q_max = 0, d = 1, D = 0, period = 12)
  expect_equal(nrow(res), 3 * 2 * 2 * 1)
  expect_true(any(res$status == "ok"))
  best <- best_sarima(res, tr, sp$test$y, 12)
  expect_length(best$forecast$mean, nrow(sp$test))
})

test_that("grid search rejects invalid orders with a message", {
  s <- make_series(); sp <- split_series(s, 0.8); tr <- as_ts(sp$train, "monthly")
  expect_error(fit_sarima_grid(tr, sp$test$y, -1, 0, 0, 0, 1, 0, 12), "non-negative")
  expect_error(fit_sarima_grid(tr, sp$test$y, 1, 1, 1, 0, 1, 0, period = 1), "no seasonal period")
})

test_that("grid search handles a tiny series without crashing", {
  s <- make_series(n = 30); sp <- split_series(s, 0.8); tr <- as_ts(sp$train, "monthly")
  res <- fit_sarima_grid(tr, sp$test$y, 1, 1, 1, 1, 1, 1, 12)
  expect_true(all(res$status %in% c("ok", "failed")))
})

test_that("naive baselines have the right length", {
  b <- naive_forecasts(1:30, h = 7, period = 12)
  expect_length(b$Naive, 7)
  expect_length(b[["Seasonal naive"]], 7)
  expect_null(naive_forecasts(1:30, 7, 1)[["Seasonal naive"]])
})

test_that("prophet fits and scores a monthly series", {
  skip_if_not_installed("prophet")
  s <- make_series(n = 96); sp <- split_series(s, 0.8)
  fit <- fit_prophet_model(sp$train, sp$test, "monthly")
  expect_length(fit$test_pred, nrow(sp$test))
  cv <- prophet_cv(fit$model, sp$train, sp$test, "monthly")
  expect_true(is.character(cv$message))
})
