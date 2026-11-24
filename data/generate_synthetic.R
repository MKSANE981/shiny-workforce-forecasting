# Synthetic employee records: ID, gender, entry date, exit date (empty = still employed).
# Everything is simulated; no real organisation or person is represented.
#   Rscript data/generate_synthetic.R

generate_workforce <- function(n = 1000, seed = 42, start = as.Date("2013-01-01"), end = as.Date("2025-06-30")) {
  set.seed(seed)
  days <- seq(start, end, by = "day")
  month <- as.integer(format(days, "%m"))
  year <- as.integer(format(days, "%Y"))
  # Hiring intensity: growth over time, peaks in September and January, a dip in 2020, no weekend hiring.
  w <- (1 + 0.12 * (year - 2013)) *
    (1 + 0.6 * (month == 9) + 0.3 * (month == 1)) *
    ifelse(year == 2020 & month %in% 3:9, 0.4, 1) *
    ifelse(format(days, "%u") %in% c("6", "7"), 0, 1)
  date_in <- sort(sample(days, n, replace = TRUE, prob = w))
  tenure_days <- round(stats::rweibull(n, shape = 1.3, scale = 4.2 * 365))   # mean around 4 years
  date_out <- date_in + tenure_days
  date_out[date_out > end] <- NA
  data.frame(`Employee ID` = seq_len(n),
             Gender = sample(c("F", "M"), n, replace = TRUE, prob = c(0.45, 0.55)),
             `Entry date` = format(date_in, "%Y-%m-%d"),
             `Exit date` = ifelse(is.na(date_out), "", format(date_out, "%Y-%m-%d")),
             check.names = FALSE, stringsAsFactors = FALSE)
}

if (sys.nframe() == 0) {
  out <- generate_workforce()
  utils::write.table(out, "data/synthetic_workforce.csv", sep = ";", row.names = FALSE, quote = FALSE,
                     fileEncoding = "UTF-8")
  cat(sprintf("%d rows written to data/synthetic_workforce.csv (%d still employed)\n",
              nrow(out), sum(out$`Exit date` == "")))
}
