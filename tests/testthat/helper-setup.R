suppressPackageStartupMessages({library(forecast); library(tseries); library(lubridate)})
root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
for (f in list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE)) source(f)
source(file.path(root, "data", "generate_synthetic.R"))
