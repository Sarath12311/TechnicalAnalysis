# display.R
# BDA400 Assignment 2: Technical Analysis using R, Preliminary Stage
# Author: Sarathchandra Reddy Panakanapalli
#
# Utilities to display the imported stock data and the calculated
# statistics, as tables in the console and as charts.

library(quantmod)
library(ggplot2)

# White background, so saved charts look the same in every viewer
theme_report <- function() {
  theme_minimal() +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          legend.position = "top")
}

# Format numbers with thousands separators and fixed decimals
fmt <- function(x, digits = 2) {
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

# -----------------------------------------------------------------------------
# display_stock_data(): print the first and last rows of each stock's
# data frame, with its size and date range.
# -----------------------------------------------------------------------------
display_stock_data <- function(stock_data, n = 5) {
  for (symbol in names(stock_data)) {
    df <- stock_data[[symbol]]
    cat("\n==============================================================\n")
    cat(" ", symbol, ": ", nrow(df), " trading days, ",
        format(min(df$Date)), " to ", format(max(df$Date)), "\n", sep = "")
    cat("==============================================================\n")
    cat("First", n, "rows:\n")
    print(head(df, n), row.names = FALSE)
    cat("\nLast", n, "rows:\n")
    print(tail(df, n), row.names = FALSE)
  }
  invisible(stock_data)
}

# -----------------------------------------------------------------------------
# display_quote_summary(): a quote panel laid out like the Yahoo Finance
# summary page (previous close, open, day's range, 52-week range, volume and
# average volume), using the most recent trading day in the data.
# -----------------------------------------------------------------------------
display_quote_summary <- function(df, symbol) {
  last <- nrow(df)
  prev_close <- if (last > 1) df$Close[last - 1] else NA
  change <- df$Close[last] - prev_close
  pct <- 100 * change / prev_close
  year <- df[df$Date > max(df$Date) - 365, ]

  cat("\n", symbol, "  ", fmt(df$Close[last]), "  ",
      ifelse(change >= 0, "+", ""), fmt(change), " (",
      ifelse(pct >= 0, "+", ""), fmt(pct), "%)   as of ",
      format(df$Date[last]), "\n", sep = "")
  rows <- data.frame(
    Item = c("Previous Close", "Open", "Day's Range", "52-Week Range",
             "Volume", "Avg. Volume (3 months)"),
    Value = c(fmt(prev_close),
              fmt(df$Open[last]),
              paste(fmt(df$Low[last]), "-", fmt(df$High[last])),
              paste(fmt(min(year$Low)), "-", fmt(max(year$High))),
              fmt(df$Volume[last], 0),
              fmt(mean(tail(df$Volume, 63)), 0))
  )
  print(rows, row.names = FALSE, right = FALSE)
  invisible(rows)
}

# -----------------------------------------------------------------------------
# display_statistics(): print the statistics summary as a formatted table.
# -----------------------------------------------------------------------------
display_statistics <- function(stats_summary) {
  out <- stats_summary
  num <- vapply(out, is.numeric, logical(1)) &
    !names(out) %in% c("Trading_Days", "Mode_Count")
  out[num] <- lapply(out[num], fmt)
  cat("\nBasic statistics of closing prices (in each stock's own currency)\n")
  cat("Mode = most common price rounded to the whole dollar;",
      "Mode_Count = days at that level\n\n")
  print(out, row.names = FALSE)
  invisible(stats_summary)
}

# -----------------------------------------------------------------------------
# plot_candlestick(): candlestick chart with volume and the 20- and 50-day
# moving averages, drawn with quantmod::chartSeries(). Saved to `file` if
# given, otherwise drawn on screen.
# -----------------------------------------------------------------------------
plot_candlestick <- function(df, symbol, file = NULL) {
  prices <- xts(df[, c("Open", "High", "Low", "Close", "Volume")],
                order.by = df$Date)
  if (!is.null(file)) png(file, width = 1100, height = 700, res = 110)
  chartSeries(prices, name = paste(symbol, "daily prices"), type = "candlesticks",
              theme = chartTheme("white"), TA = "addVo()")
  print(addSMA(n = 20, col = "blue"))
  print(addSMA(n = 50, col = "red"))
  if (!is.null(file)) dev.off()
  invisible(NULL)
}

# -----------------------------------------------------------------------------
# plot_price_ma(): closing price with its moving averages (ggplot2).
# -----------------------------------------------------------------------------
plot_price_ma <- function(ma_df, symbol) {
  long <- stack(ma_df[, setdiff(names(ma_df), "Date")])
  long$Date <- rep(ma_df$Date, times = ncol(ma_df) - 1)
  long$ind <- factor(long$ind, levels = setdiff(names(ma_df), "Date"),
                     labels = c("Close", sub("SMA_", "SMA ", names(ma_df)[-(1:2)])))
  ggplot(na.omit(long), aes(Date, values, colour = ind)) +
    geom_line(linewidth = 0.6) +
    scale_colour_manual(values = c("grey35", "#1F77B4", "#D62728")) +
    labs(title = paste(symbol, "closing price and moving averages"),
         x = NULL, y = "Price", colour = NULL) +
    theme_report()
}

# -----------------------------------------------------------------------------
# plot_performance(): compare all stocks by indexing each closing price to
# 100 on the first day, so stocks with different prices and currencies can
# be shown on one chart.
# -----------------------------------------------------------------------------
plot_performance <- function(stock_data) {
  perf <- do.call(rbind, lapply(names(stock_data), function(symbol) {
    df <- stock_data[[symbol]]
    data.frame(Date = df$Date, Symbol = symbol,
               Index = 100 * df$Close / df$Close[1])
  }))
  ggplot(perf, aes(Date, Index, colour = Symbol)) +
    geom_hline(yintercept = 100, colour = "grey60", linetype = "dashed") +
    geom_line(linewidth = 0.6) +
    labs(title = "Portfolio performance (first day = 100)",
         x = NULL, y = "Indexed closing price", colour = NULL) +
    theme_report()
}

# -----------------------------------------------------------------------------
# plot_volatility(): standard deviation as a percentage of the mean
# (coefficient of variation), which compares price variability across stocks
# with different price levels.
# -----------------------------------------------------------------------------
plot_volatility <- function(stats_summary) {
  cv <- data.frame(Symbol = stats_summary$Symbol,
                   CV = 100 * stats_summary$SD / stats_summary$Mean)
  ggplot(cv, aes(reorder(Symbol, CV), CV)) +
    geom_col(fill = "#2C6E63", width = 0.6) +
    geom_text(aes(label = sprintf("%.1f%%", CV)), hjust = -0.15, size = 3.5) +
    coord_flip() +
    expand_limits(y = max(cv$CV) * 1.15) +
    labs(title = "Price variability: standard deviation as % of mean",
         x = NULL, y = "Coefficient of variation (%)") +
    theme_report()
}
