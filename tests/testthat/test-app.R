# Drive the real server with the inputs a user (or a careless user) can produce.
app_dir <- normalizePath(file.path(testthat::test_path(), "..", ".."))

test_that("app flow works on demo data and survives bad input", {
  old <- setwd(app_dir); on.exit(setwd(old))
  shiny::testServer(app_dir, {
    session$setInputs(source = "demo", col_in = "Entry date", col_out = "Exit date",
                      freq = "monthly", train_frac = 0.8,
                      d_view = 1, D_view = 0, lag_max = 24,
                      p = 2, q = 1, P = 1, Q = 0, d = 1, D = 0, criterion = "AICc")
    b <- bounds()
    full <- c(as.Date(b[["min"]]), as.Date(b[["max"]]))
    session$setInputs(window = full)
    expect_length(problems(), 0)
    expect_gt(nrow(series_all()), 100)

    # reversed dates: a message, not a crash
    session$setInputs(window = c(as.Date("2020-01-01"), as.Date("2015-01-01")))
    expect_error(series_all())             # silently gated by req(), never an R error shown to the user
    session$setInputs(window = full)

    # impossible orders
    session$setInputs(p = -1)
    expect_true(length(order_problems()) > 0)
    session$setInputs(p = 2.5)
    expect_true(length(order_problems()) > 0)
    session$setInputs(p = 9, q = 9, P = 9, Q = 9)
    expect_true(length(order_problems()) > 0)

    # too short a window for a monthly series
    session$setInputs(p = 2, q = 1, P = 1, Q = 0,
                      window = c(full[2] - 270, full[2]))
    expect_true(length(problems()) > 0)
  })
})

test_that("SARIMA, Prophet and the comparison run end to end on demo data", {
  skip_if_not_installed("prophet")
  old <- setwd(app_dir); on.exit(setwd(old))
  shiny::testServer(app_dir, {
    session$setInputs(source = "demo", col_in = "Entry date", col_out = "Exit date",
                      freq = "monthly", train_frac = 0.8, d_view = 1, D_view = 0, lag_max = 24,
                      p = 1, q = 1, P = 1, Q = 0, d = 1, D = 0, criterion = "AICc",
                      mode = "additive", cps = 0.05, holidays = FALSE)
    b <- bounds()
    session$setInputs(window = c(as.Date(b[["min"]]), as.Date(b[["max"]])))
    session$setInputs(run_sarima = 1)
    s <- sarima()
    expect_true(any(s$results$status == "ok"))
    expect_false(is.null(s$best))
    session$setInputs(run_prophet = 1)
    f <- prophet_res()
    expect_length(f$test_pred, nrow(win()$test))
    cmp <- comparison()
    expect_true(all(c("Naive", "Seasonal naive", "Best SARIMA", "Prophet") %in% cmp$Model))
    expect_true(all(is.finite(cmp$RMSE)))
  })
})
