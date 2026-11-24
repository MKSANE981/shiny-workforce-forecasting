# Packages used by the app and the tests.
pkgs <- c("shiny", "shinydashboard", "DT", "ggplot2", "gridExtra", "forecast", "tseries",
          "lubridate", "readxl", "readr", "prophet", "testthat")
missing <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing)) install.packages(missing)
