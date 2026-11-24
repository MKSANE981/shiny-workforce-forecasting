# ggplot2 figures. Each returns a ggplot object (never prints), and handles empty input.

theme_app <- function() ggplot2::theme_minimal(base_size = 13)

plot_series <- function(series, split_date = NULL, title = "Headcount") {
  p <- ggplot2::ggplot(series, ggplot2::aes(ds, y)) +
    ggplot2::geom_line(color = "#1f4e79", linewidth = 0.9) +
    ggplot2::labs(title = title, x = NULL, y = "Employees") + theme_app()
  if (!is.null(split_date)) {
    p <- p + ggplot2::geom_vline(xintercept = as.numeric(split_date), linetype = "dashed", color = "grey40") +
      ggplot2::annotate("text", x = split_date, y = max(series$y), label = " test window", hjust = 0, size = 3.5)
  }
  p
}

plot_forecast_vs_actual <- function(train, test, predicted, lower = NULL, upper = NULL, title = "Forecast") {
  pred <- data.frame(ds = test$ds, y = predicted)
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(data = train, ggplot2::aes(ds, y), color = "grey50") +
    ggplot2::geom_line(data = test, ggplot2::aes(ds, y, color = "Actual"), linewidth = 1) +
    ggplot2::geom_line(data = pred, ggplot2::aes(ds, y, color = "Forecast"), linewidth = 1) +
    ggplot2::scale_color_manual(values = c(Actual = "#1f4e79", Forecast = "#d95f02"), name = NULL) +
    ggplot2::labs(title = title, x = NULL, y = "Employees") + theme_app()
  if (!is.null(lower) && !is.null(upper)) {
    band <- data.frame(ds = test$ds, lo = lower, hi = upper)
    p <- p + ggplot2::geom_ribbon(data = band, ggplot2::aes(ds, ymin = lo, ymax = hi),
                                  fill = "#d95f02", alpha = 0.15)
  }
  p
}

plot_acf_pacf <- function(y_ts, lag_max, pacf = FALSE) {
  lag_max <- min(lag_max, max(2, length(y_ts) - 2))
  p <- if (pacf) forecast::ggPacf(y_ts, lag.max = lag_max) else forecast::ggAcf(y_ts, lag.max = lag_max)
  p + ggplot2::labs(title = if (pacf) "Partial ACF" else "ACF") + theme_app()
}

plot_residual_diagnostics <- function(fit) {
  r <- as.numeric(stats::residuals(fit))
  df <- data.frame(i = seq_along(r), r = r)
  a <- ggplot2::ggplot(df, ggplot2::aes(i, r)) + ggplot2::geom_line(color = "#1f4e79") +
    ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    ggplot2::labs(title = "Residuals", x = "Index", y = NULL) + theme_app()
  b <- ggplot2::ggplot(df, ggplot2::aes(sample = r)) + ggplot2::stat_qq(color = "#1f4e79") +
    ggplot2::stat_qq_line(color = "red") + ggplot2::labs(title = "Normal Q-Q", x = NULL, y = NULL) + theme_app()
  gridExtra::arrangeGrob(a, b, ncol = 2)
}

plot_decomposition <- function(y_ts) {
  if (stats::frequency(y_ts) < 2 || length(y_ts) < 2 * stats::frequency(y_ts) + 1) return(NULL)
  ggplot2::autoplot(stats::decompose(y_ts)) + theme_app() + ggplot2::labs(title = "Classical decomposition")
}

plot_metric_comparison <- function(tab) {
  ggplot2::ggplot(tab, ggplot2::aes(stats::reorder(Model, RMSE), RMSE)) +
    ggplot2::geom_col(fill = "#1f4e79") + ggplot2::coord_flip() +
    ggplot2::labs(title = "Test-window RMSE (lower is better)", x = NULL, y = "RMSE") + theme_app()
}
