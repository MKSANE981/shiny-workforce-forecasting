# From employee records (entry date, optional exit date) to headcount time series.

FREQUENCIES <- c("Yearly" = "yearly", "Monthly" = "monthly", "Weekly" = "weekly", "Daily" = "daily")
SEASONAL_PERIOD <- c(yearly = 1, monthly = 12, weekly = 52, daily = 7)
FLOOR_UNIT <- c(yearly = "year", monthly = "month", weekly = "week", daily = "day")
PROPHET_STEP_DAYS <- c(yearly = 365, monthly = 30, weekly = 7, daily = 1)

read_workforce_file <- function(path, filename = path) {
  ext <- tolower(tools::file_ext(filename))
  if (ext %in% c("xlsx", "xls")) {
    as.data.frame(readxl::read_excel(path))
  } else if (ext %in% c("csv", "txt")) {
    first <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")
    delim <- if (grepl(";", first, fixed = TRUE)) ";" else if (grepl("\t", first, fixed = TRUE)) "\t" else ","
    as.data.frame(readr::read_delim(path, delim = delim, show_col_types = FALSE,
                                    locale = readr::locale(encoding = "UTF-8")))
  } else {
    stop("Unsupported file type: use .xlsx, .xls or .csv")
  }
}

# Parse a column of dates in whatever shape it arrives (Date, POSIXct, text, Excel serial).
parse_dates <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))   # Excel serial numbers
  x <- trimws(as.character(x))
  x[x == ""] <- NA
  out <- suppressWarnings(lubridate::parse_date_time(x, orders = c("ymd HMS", "ymd", "dmy", "mdy"),
                                                     quiet = TRUE))
  as.Date(out)
}

guess_date_columns <- function(df) {
  nm <- names(df)
  entry <- grep("entr|start|hire|embauch|debut|begin|join", nm, ignore.case = TRUE)
  exit <- grep("sort|exit|end|fin|leave|depart|termin", nm, ignore.case = TRUE)
  list(entry = if (length(entry)) nm[entry[1]] else nm[1],
       exit = if (length(exit)) nm[exit[1]] else nm[min(2, length(nm))])
}

# Returns list(data = data.frame(date_in, date_out), report = list(...)) or stops with a clear message.
prepare_workforce <- function(df, col_in, col_out, today = Sys.Date()) {
  if (is.null(df) || !nrow(df)) stop("The file is empty.")
  if (!all(c(col_in, col_out) %in% names(df))) stop("Choose the entry and exit columns from the file.")
  if (identical(col_in, col_out)) stop("The entry and exit columns must be different.")
  date_in <- parse_dates(df[[col_in]])
  date_out <- parse_dates(df[[col_out]])
  n_input <- length(date_in)
  report <- list(rows = n_input, no_entry_date = sum(is.na(date_in)))
  keep <- !is.na(date_in)
  report$exit_before_entry <- sum(keep & !is.na(date_out) & date_out < date_in)
  report$entry_in_future <- sum(keep & date_in > today)
  keep <- keep & !(!is.na(date_out) & date_out < date_in) & date_in <= today
  out <- data.frame(date_in = date_in[keep], date_out = date_out[keep])
  out$date_out[!is.na(out$date_out) & out$date_out > today] <- NA   # exit dates in the future: still employed
  report$kept <- nrow(out)
  report$still_active <- sum(is.na(out$date_out))
  if (nrow(out) < 10) stop("Fewer than 10 usable rows after cleaning. Check the selected date columns.")
  list(data = out, report = report)
}

# Headcount on each calendar day between start and end: entries so far minus exits so far.
headcount_daily <- function(w, start = min(w$date_in), end = NULL) {
  if (is.null(end)) end <- max(c(w$date_in, w$date_out), na.rm = TRUE)
  if (start >= end) stop("The start date must be before the end date.")
  days <- seq(start, end, by = "day")
  entries <- sort(as.numeric(w$date_in))
  exits <- sort(as.numeric(w$date_out[!is.na(w$date_out)]))
  data.frame(ds = days,
             y = findInterval(as.numeric(days), entries) - findInterval(as.numeric(days), exits))
}

# Headcount at the end of each period (last day of the week/month/year inside the window).
aggregate_headcount <- function(daily, freq) {
  freq <- match.arg(freq, unname(FREQUENCIES))
  if (freq == "daily") return(daily)
  period_start <- lubridate::floor_date(daily$ds, FLOOR_UNIT[[freq]], week_start = 1)
  agg <- stats::aggregate(y ~ period_start, data = transform(daily, period_start = period_start),
                          FUN = function(v) v[length(v)])
  names(agg) <- c("ds", "y")
  agg
}

as_ts <- function(series, freq) {
  period <- SEASONAL_PERIOD[[freq]]
  first <- series$ds[1]
  start <- switch(freq,
                  yearly = lubridate::year(first),
                  monthly = c(lubridate::year(first), lubridate::month(first)),
                  weekly = c(lubridate::year(first), lubridate::isoweek(first)),
                  daily = 1)
  stats::ts(series$y, start = start, frequency = period)
}

split_series <- function(series, train_frac) {
  n_train <- floor(nrow(series) * train_frac)
  list(train = series[seq_len(n_train), ], test = series[-seq_len(n_train), ])
}
