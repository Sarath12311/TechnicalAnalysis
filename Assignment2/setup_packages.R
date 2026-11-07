# setup_packages.R
# Installs the R packages needed for the technical analysis project,
# then loads them and prints their versions to confirm the installation.

install.packages(c("quantmod", "TTR", "ggplot2"),
                 repos = "https://cloud.r-project.org")

library(quantmod)
library(TTR)
library(ggplot2)

cat("\nInstalled package versions:\n")
cat("quantmod:", as.character(packageVersion("quantmod")), "\n")
cat("TTR:     ", as.character(packageVersion("TTR")), "\n")
cat("ggplot2: ", as.character(packageVersion("ggplot2")), "\n")
cat(R.version.string, "\n")
