test_that("valid orders pass", {
  expect_length(validate_orders(2, 1, 1, 0, 1, 0, period = 12), 0)
})

test_that("bad orders are reported, not thrown", {
  expect_true(length(validate_orders(-1, 0, 0, 0, 1, 0, 12)) > 0)       # negative
  expect_true(length(validate_orders(1.5, 0, 0, 0, 1, 0, 12)) > 0)      # not an integer
  expect_true(length(validate_orders(NA, 0, 0, 0, 1, 0, 12)) > 0)       # missing
  expect_true(length(validate_orders("a", 0, 0, 0, 1, 0, 12)) > 0)      # text
  expect_true(length(validate_orders(9, 0, 0, 0, 1, 0, 12)) > 0)        # p too large
  expect_true(length(validate_orders(1, 1, 3, 0, 1, 0, 12)) > 0)        # P too large
  expect_true(length(validate_orders(1, 1, 0, 0, 3, 0, 12)) > 0)        # d too large
  expect_true(length(validate_orders(1, 1, 1, 0, 1, 0, period = 1)) > 0) # seasonal terms, no period
  expect_true(length(validate_orders(5, 5, 2, 2, 1, 0, 12)) > 0)        # too many models
})

test_that("date window is validated", {
  lo <- as.Date("2015-01-01"); hi <- as.Date("2020-01-01")
  expect_length(validate_date_window(as.Date("2016-01-01"), as.Date("2019-01-01"), lo, hi), 0)
  expect_true(length(validate_date_window(as.Date("2019-01-01"), as.Date("2016-01-01"), lo, hi)) > 0)
  expect_true(length(validate_date_window(as.Date("2010-01-01"), as.Date("2016-01-01"), lo, hi)) > 0)
  expect_true(length(validate_date_window(NA, as.Date("2016-01-01"), lo, hi)) > 0)
  expect_true(length(validate_date_window(NULL, NULL, lo, hi)) > 0)
})

test_that("series length is checked against the seasonal period", {
  expect_length(validate_series_length(100, 12, 0.8), 0)
  expect_true(length(validate_series_length(20, 12, 0.8)) > 0)
  expect_true(length(validate_series_length(11, 1, 0.99)) > 0)
})
