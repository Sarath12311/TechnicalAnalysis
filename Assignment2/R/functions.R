# functions.R
# BDA400 Assignment 2: Technical Analysis using R, Preliminary Stage
# Author: Sarathchandra Reddy Panakanapalli
#
# Utility functions to read the portfolio, import stock data with quantmod,
# and compute basic statistics for each stock.

library(quantmod)  # getSymbols(): downloads stock prices (Yahoo Finance)
library(TTR)       # SMA(): simple moving averages

# -----------------------------------------------------------------------------
# read_portfolio(): read stock symbols from a text file, one per line.
# Blank lines and lines starting with "#" are ignored; symbols are trimmed,
# converted to upper case and de-duplicated.
# -----------------------------------------------------------------------------
read_portfolio <- function(path = "portfolio.txt") {
  if (!file.exists(path)) {
    stop("Portfolio file not found: ", path)
  }
  symbols <- trimws(readLines(path, warn = FALSE))
  symbols <- symbols[symbols != "" & !startsWith(symbols, "#")]
  unique(toupper(symbols))
}

# -----------------------------------------------------------------------------
# load_stock_data(): import price data for every symbol in the portfolio.
# Returns a named list with one data frame per symbol, with the columns
# Date, Open, High, Low, Close, Volume, Adjusted.
#   from, to : date range to download (default: the last year)
#   src      : data source for quantmod::getSymbols() (default "yahoo")
#   ...      : passed to getSymbols(), e.g. dir = "data" when src = "csv"
# A symbol that fails to download is reported and skipped, so one bad
# symbol does not stop the others from loading.
# -----------------------------------------------------------------------------
load_stock_data <- function(portfolio_file = "portfolio.txt",
                            from = Sys.Date() - 365,
                            to = Sys.Date(),
                            src = "yahoo",
                            ...) {
  symbols <- read_portfolio(portfolio_file)
  stock_data <- list()

  for (symbol in symbols) {
    prices <- tryCatch(
      getSymbols(symbol, src = src, from = from, to = to,
                 auto.assign = FALSE, ...),
      error = function(e) {
        message("Could not load ", symbol, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(prices)) next

    # Convert the xts object to a data frame with a Date column and
    # standard column names ("AAPL.Close" becomes "Close")
    df <- data.frame(Date = index(prices), coredata(prices),
                     row.names = NULL)
    names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume",
                   "Adjusted")
    stock_data[[symbol]] <- df
    message("Loaded ", symbol, ": ", nrow(df), " trading days")
  }

  stock_data
}

# -----------------------------------------------------------------------------
# stat_mode(): most frequent value in a vector, and how often it occurs.
# Stock prices are continuous, so exact repeats are rare: at the cent level
# many prices tie at two occurrences and the "mode" is arbitrary. Prices are
# therefore rounded to `digits` decimal places first (0 = whole dollars),
# which gives the most common price level. If values tie, the smallest is
# returned. The count is returned as the attribute "count".
# -----------------------------------------------------------------------------
stat_mode <- function(x, digits = 0) {
  x <- round(x[!is.na(x)], digits)
  if (length(x) == 0) return(NA_real_)
  counts <- table(x)
  mode <- as.numeric(names(counts)[which.max(counts)])
  attr(mode, "count") <- max(counts)
  mode
}

# -----------------------------------------------------------------------------
# calculate_statistics(): basic statistics for one stock's data frame.
#   price_col  : column to analyse (default "Close")
#   ma_windows : moving-average lengths in trading days (default 20 and 50)
#   mode_digits: rounding used for the mode (default 0 = whole dollars)
# Returns a list with:
#   $summary         one-row data frame of statistics
#   $moving_averages data frame of Date, price and each moving average
# -----------------------------------------------------------------------------
calculate_statistics <- function(df, symbol = "", price_col = "Close",
                                 ma_windows = c(20, 50), mode_digits = 0) {
  price <- df[[price_col]]
  if (is.null(price)) stop("Column not found: ", price_col)

  # Moving averages (the first n - 1 values are NA by definition)
  ma <- data.frame(Date = df$Date, Price = price)
  for (n in ma_windows) {
    ma[[paste0("SMA_", n)]] <- if (length(price) >= n) SMA(price, n = n) else NA
  }

  latest <- nrow(df)
  mode <- stat_mode(price, digits = mode_digits)
  summary <- data.frame(
    Symbol       = symbol,
    Start        = min(df$Date),
    End          = max(df$Date),
    Trading_Days = latest,
    Mean         = mean(price, na.rm = TRUE),
    Median       = median(price, na.rm = TRUE),
    Mode         = as.numeric(mode),
    Mode_Count   = attr(mode, "count"),
    SD           = sd(price, na.rm = TRUE),
    Min          = min(price, na.rm = TRUE),
    Max          = max(price, na.rm = TRUE),
    Latest       = price[latest]
  )
  for (n in ma_windows) {
    summary[[paste0("SMA_", n, "_Latest")]] <- ma[[paste0("SMA_", n)]][latest]
  }

  list(summary = summary, moving_averages = ma)
}

# -----------------------------------------------------------------------------
# calculate_all_statistics(): run calculate_statistics() for every stock.
# Returns a list with:
#   $by_stock  the full result for each symbol
#   $summary   one data frame with a row per symbol
# -----------------------------------------------------------------------------
calculate_all_statistics <- function(stock_data, ...) {
  by_stock <- lapply(names(stock_data), function(symbol) {
    calculate_statistics(stock_data[[symbol]], symbol = symbol, ...)
  })
  names(by_stock) <- names(stock_data)
  summary <- do.call(rbind, lapply(by_stock, `[[`, "summary"))
  rownames(summary) <- NULL
  list(by_stock = by_stock, summary = summary)
}
