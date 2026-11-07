# Assignment 2: Technical Analysis using R, Preliminary Stage

Reads a portfolio of stock symbols, downloads one year of daily prices from
Yahoo Finance with **quantmod**, computes basic statistics for each stock and
displays the results as tables and charts.

## Files

| File | Purpose |
| --- | --- |
| `portfolio.txt` | Stock symbols in the portfolio, one per line |
| `R/functions.R` | `read_portfolio()`, `load_stock_data()`, `stat_mode()`, `calculate_statistics()`, `calculate_all_statistics()` |
| `R/display.R` | Display utilities: data tables, a Yahoo-style quote summary, a statistics table and four chart types |
| `run_analysis.R` | Main script that runs every step |
| `SarathchandraPanakanapalli_BDA400_A02.docx` | Assignment report with step-by-step screenshots |

## Requirements

- R 4.3 or later, and RStudio
- Packages: `install.packages(c("quantmod", "TTR", "ggplot2"))`
- An internet connection (prices are downloaded from Yahoo Finance)

## How to run

Open RStudio, set the working directory to this folder, then run:

```r
source("run_analysis.R")
```

Charts and `statistics_summary.csv` are saved to `output/`.

## Statistics calculated

For each stock's closing price: mean, median, mode, standard deviation,
minimum, maximum, latest price, and the 20- and 50-day simple moving
averages. Because prices are continuous, the mode is the most common price
rounded to the whole dollar, reported with the number of days at that level.
