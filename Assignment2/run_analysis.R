# run_analysis.R
# BDA400 Assignment 2: Technical Analysis using R, Preliminary Stage
# Author: Sarathchandra Reddy Panakanapalli
#
# Main script: loads the portfolio, imports one year of daily prices from
# Yahoo Finance, computes basic statistics and displays the results.
# Run from the Assignment2 folder:  source("run_analysis.R")

source("R/functions.R")
source("R/display.R")

# 1. Read the portfolio -------------------------------------------------------
symbols <- read_portfolio("portfolio.txt")
cat("Portfolio:", paste(symbols, collapse = ", "), "\n")

# 2. Import the stock data (one data frame per symbol) ------------------------
stock_data <- load_stock_data("portfolio.txt",
                              from = Sys.Date() - 365, to = Sys.Date())

# Also store each stock as its own data frame in the workspace,
# e.g. AAPL, MSFT, RY_TO (the "." in "RY.TO" is replaced by "_")
for (symbol in names(stock_data)) {
  assign(make.names(gsub("\\.", "_", symbol)), stock_data[[symbol]])
}

# 3. Compute the statistics ---------------------------------------------------
stats <- calculate_all_statistics(stock_data)

# 4. Display the data and statistics ------------------------------------------
display_stock_data(stock_data)
for (symbol in names(stock_data)) {
  display_quote_summary(stock_data[[symbol]], symbol)
}
display_statistics(stats$summary)

# 5. Charts (saved to output/ and drawn on screen) ----------------------------
dir.create("output", showWarnings = FALSE)
for (symbol in names(stock_data)) {
  safe <- gsub("\\.", "_", symbol)
  plot_candlestick(stock_data[[symbol]], symbol,
                   file = file.path("output", paste0(safe, "_candlestick.png")))
  p <- plot_price_ma(stats$by_stock[[symbol]]$moving_averages, symbol)
  ggsave(file.path("output", paste0(safe, "_moving_averages.png")), p,
         width = 8, height = 4.5, dpi = 120)
  print(p)
}
p_perf <- plot_performance(stock_data)
ggsave("output/portfolio_performance.png", p_perf, width = 8, height = 4.5,
       dpi = 120)
print(p_perf)
p_vol <- plot_volatility(stats$summary)
ggsave("output/price_variability.png", p_vol, width = 7, height = 4, dpi = 120)
print(p_vol)

# 6. Save the statistics table -------------------------------------------------
write.csv(stats$summary, "output/statistics_summary.csv", row.names = FALSE)
cat("\nDone. Charts and statistics saved to the output/ folder.\n")
