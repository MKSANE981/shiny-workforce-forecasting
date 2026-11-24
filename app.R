# Workforce forecasting dashboard: headcount series from entry/exit dates,
# stationarity diagnostics, a SARIMA grid search and Prophet, compared on one held-out window.
#   shiny::runApp()

suppressPackageStartupMessages({
  library(shiny); library(shinydashboard); library(DT)
  library(ggplot2); library(forecast); library(tseries); library(lubridate)
})
for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f)
source("data/generate_synthetic.R")

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Workforce forecasting"),
  dashboardSidebar(sidebarMenu(
    id = "tabs",
    menuItem("1. Data", tabName = "data", icon = icon("database")),
    menuItem("2. Series", tabName = "series", icon = icon("chart-line")),
    menuItem("3. Stationarity", tabName = "stat", icon = icon("wave-square")),
    menuItem("4. SARIMA", tabName = "sarima", icon = icon("sliders")),
    menuItem("5. Prophet", tabName = "prophet", icon = icon("wand-magic-sparkles")),
    menuItem("6. Comparison", tabName = "compare", icon = icon("scale-balanced"))
  )),
  dashboardBody(
    tags$head(tags$style(HTML(".content-wrapper{background:#fff} .msg-warn{color:#a94442;font-weight:600}"))),
    tabItems(
      tabItem("data",
        fluidRow(
          box(title = "Source", width = 5, status = "primary",
              radioButtons("source", NULL, c("Synthetic demo data" = "demo", "Upload a file (.xlsx, .csv)" = "upload")),
              conditionalPanel("input.source == 'upload'",
                               fileInput("file", NULL, accept = c(".xlsx", ".xls", ".csv", ".txt"))),
              uiOutput("column_pickers")),
          box(title = "Data check", width = 7, status = "primary", uiOutput("data_report"))),
        fluidRow(box(title = "Preview", width = 12, DTOutput("preview")))),
      tabItem("series",
        fluidRow(box(width = 12, status = "primary",
          column(3, selectInput("freq", "Frequency", FREQUENCIES, selected = "monthly")),
          column(3, sliderInput("train_frac", "Training share", 0.5, 0.95, 0.8, step = 0.05)),
          column(6, uiOutput("window_ui")))),
        fluidRow(box(width = 12, uiOutput("series_msg"), plotOutput("series_plot", height = 320))),
        fluidRow(box(title = "Decomposition", width = 12, plotOutput("decomp_plot", height = 380)))),
      tabItem("stat",
        fluidRow(box(width = 12, status = "primary",
          column(3, selectInput("lag_max", "Maximum lag", c(12, 24, 36, 48), selected = 24)),
          column(3, numericInput("d_view", "Regular differencing d", 1, min = 0, max = 2, step = 1)),
          column(3, numericInput("D_view", "Seasonal differencing D", 0, min = 0, max = 1, step = 1)))),
        fluidRow(box(title = "Tests on the training series", width = 6, uiOutput("stat_msg"), DTOutput("stat_table")),
                 box(title = "Same tests after differencing", width = 6, uiOutput("stat_msg_diff"), DTOutput("stat_table_diff"))),
        fluidRow(box(width = 6, plotOutput("acf_plot", height = 260)), box(width = 6, plotOutput("pacf_plot", height = 260)))),
      tabItem("sarima",
        fluidRow(box(title = "Search space", width = 12, status = "primary",
          column(2, numericInput("p", "max p", 2, 0, 5, 1)), column(2, numericInput("q", "max q", 1, 0, 5, 1)),
          column(2, numericInput("P", "max P", 1, 0, 2, 1)), column(2, numericInput("Q", "max Q", 0, 0, 2, 1)),
          column(2, numericInput("d", "d", 1, 0, 2, 1)), column(2, numericInput("D", "D", 0, 0, 1, 1)),
          column(4, selectInput("criterion", "Select the best model by", c("AICc" = "AICc", "Test RMSE" = "RMSE"))),
          column(4, br(), actionButton("run_sarima", "Run grid search", icon = icon("play"), class = "btn-primary")),
          column(4, uiOutput("order_msg")))),
        fluidRow(box(title = "All fitted models", width = 12, DTOutput("sarima_table"))),
        fluidRow(box(title = "Best model", width = 12, verbatimTextOutput("sarima_summary")),
                 box(width = 6, plotOutput("sarima_forecast", height = 300)),
                 box(width = 6, plotOutput("sarima_resid", height = 300)))),
      tabItem("prophet",
        fluidRow(box(width = 12, status = "primary",
          column(3, selectInput("mode", "Seasonality mode", c("additive", "multiplicative"))),
          column(3, sliderInput("cps", "Changepoint prior scale", 0.01, 0.5, 0.05, step = 0.01)),
          column(3, checkboxInput("holidays", "French public holidays (daily series only)", FALSE)),
          column(3, br(), actionButton("run_prophet", "Fit Prophet", icon = icon("play"), class = "btn-primary")))),
        fluidRow(box(width = 12, uiOutput("prophet_msg"), plotOutput("prophet_plot", height = 320))),
        fluidRow(box(title = "Cross-validation", width = 12, uiOutput("cv_msg"), DTOutput("cv_table")))),
      tabItem("compare",
        fluidRow(box(title = "Test-window accuracy", width = 6, DTOutput("compare_table")),
                 box(width = 6, plotOutput("compare_plot", height = 300))),
        fluidRow(box(width = 12, p("All models are fitted on the same training window and scored on the same held-out test window. ",
                                   "Naive and seasonal-naive forecasts are the baselines a model has to beat."))))
    )
  )
)

server <- function(input, output, session) {

  # ---- 1. Data --------------------------------------------------------------------------
  raw <- reactive({
    if (input$source == "demo") return(generate_workforce())
    req(input$file)
    tryCatch(read_workforce_file(input$file$datapath, input$file$name),
             error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
  })

  output$column_pickers <- renderUI({
    df <- raw(); req(df)
    g <- guess_date_columns(df)
    tagList(selectInput("col_in", "Entry date column", names(df), selected = g$entry),
            selectInput("col_out", "Exit date column", names(df), selected = g$exit))
  })

  workforce <- reactive({
    df <- raw(); req(df, input$col_in, input$col_out)
    tryCatch(prepare_workforce(df, input$col_in, input$col_out), error = function(e) {
      list(error = conditionMessage(e))
    })
  })

  output$data_report <- renderUI({
    w <- workforce()
    if (!is.null(w$error)) return(p(class = "msg-warn", w$error))
    r <- w$report
    tags$ul(
      tags$li(sprintf("%d rows read, %d kept after cleaning.", r$rows, r$kept)),
      tags$li(sprintf("Removed: %d without an entry date, %d with an exit before the entry, %d entering in the future.",
                      r$no_entry_date, r$exit_before_entry, r$entry_in_future)),
      tags$li(sprintf("%d people have no exit date and are counted as still employed.", r$still_active)),
      tags$li(sprintf("Entry dates range from %s to %s.", min(w$data$date_in), max(w$data$date_in))))
  })

  output$preview <- renderDT({ df <- raw(); req(df); datatable(utils::head(df, 200), options = list(pageLength = 8, scrollX = TRUE)) })

  # ---- 2. Series ------------------------------------------------------------------------
  bounds <- reactive({
    w <- workforce(); req(is.null(w$error))
    c(min = min(w$data$date_in), max = max(c(w$data$date_in, w$data$date_out), na.rm = TRUE))
  })

  output$window_ui <- renderUI({
    b <- bounds()
    dateRangeInput("window", "Analysis window", start = as.Date(b[["min"]]), end = as.Date(b[["max"]]),
                   min = as.Date(b[["min"]]), max = as.Date(b[["max"]]))
  })

  problems <- reactive({
    b <- bounds(); req(input$window, input$freq)
    msgs <- validate_date_window(input$window[1], input$window[2], as.Date(b[["min"]]), as.Date(b[["max"]]))
    if (length(msgs)) return(msgs)
    s <- series_all()
    validate_series_length(nrow(s), SEASONAL_PERIOD[[input$freq]], input$train_frac)
  })

  series_all <- reactive({
    w <- workforce(); req(is.null(w$error), input$window, input$freq)
    ok <- length(validate_date_window(input$window[1], input$window[2], as.Date(bounds()[["min"]]),
                                      as.Date(bounds()[["max"]]))) == 0
    req(ok)
    aggregate_headcount(headcount_daily(w$data, start = input$window[1], end = input$window[2]), input$freq)
  })

  output$series_msg <- renderUI({
    msgs <- tryCatch(problems(), error = function(e) character())
    if (length(msgs)) tags$ul(lapply(msgs, function(m) tags$li(class = "msg-warn", m)))
  })

  win <- reactive({
    req(length(problems()) == 0)
    split_series(series_all(), input$train_frac)
  })
  period <- reactive(SEASONAL_PERIOD[[input$freq]])
  train_ts <- reactive(as_ts(win()$train, input$freq))

  output$series_plot <- renderPlot({
    req(length(problems()) == 0)
    plot_series(series_all(), split_date = min(win()$test$ds), title = sprintf("Headcount (%s)", input$freq))
  })
  output$decomp_plot <- renderPlot({
    req(length(problems()) == 0)
    p <- plot_decomposition(train_ts())
    validate(need(!is.null(p), "Not enough seasonal cycles in the training window to decompose."))
    p
  })

  # ---- 3. Stationarity --------------------------------------------------------------------
  differenced <- reactive({
    req(length(problems()) == 0, input$d_view, input$D_view)
    difference_series(train_ts(), input$d_view, input$D_view, period())
  })
  show_tests <- function(res) datatable(res$table, rownames = FALSE, options = list(dom = "t"))
  output$stat_msg <- renderUI({ r <- stationarity_tests(win()$train$y); p(r$message) })
  output$stat_table <- renderDT({ r <- stationarity_tests(win()$train$y); req(r$table); show_tests(r) })
  output$stat_msg_diff <- renderUI({ r <- stationarity_tests(differenced()); p(r$message) })
  output$stat_table_diff <- renderDT({ r <- stationarity_tests(differenced()); req(r$table); show_tests(r) })
  output$acf_plot <- renderPlot(plot_acf_pacf(differenced(), as.numeric(input$lag_max)))
  output$pacf_plot <- renderPlot(plot_acf_pacf(differenced(), as.numeric(input$lag_max), pacf = TRUE))

  # ---- 4. SARIMA --------------------------------------------------------------------------
  order_problems <- reactive({
    validate_orders(input$p, input$q, input$P, input$Q, input$d, input$D, period())
  })
  output$order_msg <- renderUI({
    msgs <- tryCatch(order_problems(), error = function(e) "Enter valid orders.")
    if (length(msgs)) tags$ul(lapply(msgs, function(m) tags$li(class = "msg-warn", m)))
  })

  sarima <- eventReactive(input$run_sarima, {
    req(length(problems()) == 0)
    msgs <- order_problems()
    if (length(msgs)) { showNotification(paste(msgs, collapse = " "), type = "error"); return(NULL) }
    sp <- win(); tr <- train_ts()
    withProgress(message = "Fitting SARIMA models", value = 0, {
      res <- tryCatch(
        fit_sarima_grid(tr, sp$test$y, input$p, input$q, input$P, input$Q, input$d, input$D, period(),
                        on_progress = function(i, n) setProgress(i / n)),
        error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    })
    if (is.null(res)) return(NULL)
    best <- tryCatch(best_sarima(res, tr, sp$test$y, period(), input$criterion),
                     error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    list(results = res, best = best)
  })

  output$sarima_table <- renderDT({
    s <- sarima(); req(s)
    datatable(s$results, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) |>
      formatRound(c("AICc", "RMSE", "MAE", "MAPE", "LjungBox_p"), 3)
  })
  output$sarima_summary <- renderPrint({ s <- sarima(); req(s, s$best); print(s$best$fit) })
  output$sarima_forecast <- renderPlot({
    s <- sarima(); req(s, s$best); sp <- win()
    f <- s$best$forecast
    plot_forecast_vs_actual(sp$train, sp$test, as.numeric(f$mean), as.numeric(f$lower[, 2]),
                            as.numeric(f$upper[, 2]), "Best SARIMA on the test window (95% interval)")
  })
  output$sarima_resid <- renderPlot({ s <- sarima(); req(s, s$best); gridExtra::grid.arrange(plot_residual_diagnostics(s$best$fit)) })

  # ---- 5. Prophet -------------------------------------------------------------------------
  prophet_res <- eventReactive(input$run_prophet, {
    req(length(problems()) == 0)
    sp <- win()
    withProgress(message = "Fitting Prophet", value = 0.3, {
      fit <- tryCatch(fit_prophet_model(sp$train, sp$test, input$freq, input$mode, input$cps, input$holidays),
                      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
      if (is.null(fit)) return(NULL)
      setProgress(0.7, message = "Cross-validation")
      fit$cv <- prophet_cv(fit$model, sp$train, sp$test, input$freq)
      fit
    })
  })
  output$prophet_msg <- renderUI({ if (is.null(prophet_res())) p("Press 'Fit Prophet'.") })
  output$prophet_plot <- renderPlot({
    f <- prophet_res(); req(f); sp <- win()
    plot_forecast_vs_actual(sp$train, sp$test, f$test_pred, title = "Prophet on the test window")
  })
  output$cv_msg <- renderUI({ f <- prophet_res(); req(f); p(f$cv$message) })
  output$cv_table <- renderDT({
    f <- prophet_res(); req(f, f$cv$table)
    pm <- prophet::performance_metrics(f$cv$table)
    datatable(utils::head(pm, 50), rownames = FALSE, options = list(pageLength = 6, scrollX = TRUE)) |>
      formatRound(setdiff(names(pm), "horizon"), 3)
  })

  # ---- 6. Comparison ---------------------------------------------------------------------
  comparison <- reactive({
    req(length(problems()) == 0)
    sp <- win(); rows <- list()
    add <- function(name, pred) rows[[length(rows) + 1]] <<-
      data.frame(Model = name, t(round(forecast_metrics(sp$test$y, pred), 2)), check.names = FALSE)
    for (nm in names(naive_forecasts(sp$train$y, nrow(sp$test), period()))) {
      add(nm, naive_forecasts(sp$train$y, nrow(sp$test), period())[[nm]])
    }
    s <- tryCatch(sarima(), error = function(e) NULL)
    if (!is.null(s) && !is.null(s$best)) add("Best SARIMA", as.numeric(s$best$forecast$mean))
    f <- tryCatch(prophet_res(), error = function(e) NULL)
    if (!is.null(f)) add("Prophet", f$test_pred)
    do.call(rbind, rows)
  })
  output$compare_table <- renderDT(datatable(comparison(), rownames = FALSE, options = list(dom = "t")))
  output$compare_plot <- renderPlot(plot_metric_comparison(comparison()))
}

shinyApp(ui, server)
