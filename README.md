# Workforce forecasting dashboard (R Shiny)

An interactive app that turns employee records (entry date, optional exit date) into a headcount
time series and compares forecasting models on a held-out window: a SARIMA grid search, Prophet,
and naive baselines.

It started as an ENSAI final-year group project with a data consulting firm. This version is a
**rewrite for general use**: modular code, input validation, tests, and **simulated data only**.
No client data, name or logo is included.

## What it does

1. **Data**: upload `.xlsx` or `.csv`, or use the built-in synthetic data. The entry and exit columns are
   guessed from their names and can be changed. Rows with no entry date, with an exit before the entry, or
   entering in the future are removed and counted in a report.
2. **Series**: headcount at the end of each year, month, week or day, over a window you choose, split into
   a training and a test window. Classical decomposition when there are enough seasonal cycles.
3. **Stationarity**: ADF, KPSS and Phillips-Perron tests, ACF and PACF, before and after differencing.
4. **SARIMA**: grid search over `p, q, P, Q` for chosen `d, D`. Each model is scored on AICc, a Ljung-Box
   test of the residuals and RMSE, MAE and MAPE on the test window. The best model is shown with its
   forecast and residual diagnostics.
5. **Prophet**: additive or multiplicative seasonality, adjustable changepoint prior, optional French
   public holidays for daily series, and rolling-origin cross-validation whose windows adapt to the series.
6. **Comparison**: SARIMA, Prophet, naive and seasonal-naive forecasts on the same test window.

## Run it

```r
source("install_packages.R")
shiny::runApp()
```

Run the tests with `Rscript -e 'testthat::test_dir("tests/testthat")'`.

## Layout

```
app.R                     user interface and server
R/utils.R                 validation of orders, dates and series length; accuracy metrics
R/data_prep.R             parsing, cleaning, headcount series, aggregation, train/test split
R/stationarity.R          guarded stationarity tests, differencing
R/models.R                SARIMA grid search, Prophet and its cross-validation, baselines
R/plots.R                 figures
data/generate_synthetic.R seeded generator of the demo data (data/synthetic_workforce.csv)
tests/testthat/           unit tests and tests that drive the real server
```

## Robustness: what the tests check

The first version of this app broke when users typed unrealistic values. The rewrite handles them:

- **Orders `p, q, P, Q, d, D`**: negative, fractional, missing, text or too large values, seasonal
  terms on a series with no seasonal period, and grids that would fit too many models are refused with a
  message that says what to change.
- **Dates**: a start after the end, dates outside the data, or a window too short for the chosen frequency
  give a message instead of an error.
- **Fitting**: every model is fitted inside `tryCatch`; a model that does not converge is listed as
  `failed` and the search continues.
- **Series**: unparseable dates, empty files, constant or very short series are rejected or reported.
- **Accuracy**: errors are computed on the original scale of the series.

## Reading the demo results

On the synthetic data (monthly headcount, 120 months of training and 30 of testing):

| Model | RMSE | MAE | MAPE |
|---|---|---|---|
| Naive | 38.39 | 34.80 | 9.92 % |
| Seasonal naive | 56.07 | 52.77 | 15.17 % |
| Best SARIMA (2,1,1)(0,0,0)[12] | 6.01 | 4.43 | 1.28 % |
| Prophet | 9.86 | 7.62 | 2.17 % |

Headcount is a cumulative quantity, so the series is smooth and easy to forecast, and these figures
belong to this simulated dataset: they do not predict performance on a real organisation. The grid
search selects the best model by AICc, and its test error is then reported on the same window; for a
stricter estimate, use a second held-out window.
