test_that("dates are parsed from several formats", {
  expect_equal(parse_dates("2020-03-05"), as.Date("2020-03-05"))
  expect_equal(parse_dates("05/03/2020"), as.Date("2020-03-05"))
  expect_equal(parse_dates(as.POSIXct("2020-03-05 00:00:00", tz = "UTC")), as.Date("2020-03-05"))
  expect_equal(parse_dates(43900), as.Date("2020-03-10"))
  expect_true(is.na(parse_dates("not a date")))
  expect_true(is.na(parse_dates("")))
})

test_that("cleaning drops impossible rows and reports them", {
  df <- data.frame(a = c("2020-01-01", "2020-02-01", NA, "2021-01-01", rep("2019-05-01", 10)),
                   b = c("2021-01-01", "2019-01-01", "2021-01-01", "", rep("", 10)))
  res <- prepare_workforce(df, "a", "b", today = as.Date("2023-01-01"))
  expect_equal(res$report$no_entry_date, 1)
  expect_equal(res$report$exit_before_entry, 1)
  expect_equal(res$report$kept, 12)
})

test_that("cleaning fails clearly on unusable input", {
  expect_error(prepare_workforce(data.frame(), "a", "b"), "empty")
  df <- data.frame(a = "2020-01-01", b = "2021-01-01")
  expect_error(prepare_workforce(df, "a", "b"), "Fewer than 10")
  expect_error(prepare_workforce(df, "a", "a"), "different")
  expect_error(prepare_workforce(df, "zzz", "b"), "Choose")
  junk <- data.frame(a = rep("hello", 20), b = rep("world", 20))
  expect_error(prepare_workforce(junk, "a", "b"), "Fewer than 10")
})

test_that("daily headcount matches a brute-force count", {
  w <- generate_workforce(n = 200, seed = 1, end = as.Date("2016-12-31"))
  p <- prepare_workforce(w, "Entry date", "Exit date", today = as.Date("2030-01-01"))$data
  d <- headcount_daily(p, end = as.Date("2016-12-31"))
  for (day in as.character(d$ds[c(1, 100, 700, nrow(d))])) {
    day <- as.Date(day)
    brute <- sum(p$date_in <= day & (is.na(p$date_out) | p$date_out > day))
    expect_equal(d$y[d$ds == day], brute)
  }
})

test_that("aggregation keeps the last value of each period", {
  daily <- data.frame(ds = seq(as.Date("2020-01-01"), as.Date("2020-03-31"), by = "day"), y = 1:91)
  m <- aggregate_headcount(daily, "monthly")
  expect_equal(nrow(m), 3)
  expect_equal(m$y, c(31, 60, 91))
  expect_equal(nrow(aggregate_headcount(daily, "daily")), 91)
})

test_that("window must be ordered", {
  w <- data.frame(date_in = as.Date("2020-01-01"), date_out = NA)
  expect_error(headcount_daily(w, start = as.Date("2021-01-01"), end = as.Date("2020-01-01")), "before")
})
