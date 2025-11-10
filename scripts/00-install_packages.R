#### Preamble ####
# Purpose: Install required packages to conduct analysis
# Author: Amanda Ng
# Date: 9 November 2025
# Contact: waiyuamanda.ng@mail.utoronto.ca

# -----------------------------------------------------------
# 1. List of packages to install
# -----------------------------------------------------------
packages <- c(
  "tidyr",
  "dplyr",
  "broom",
  "car",
  "stringr",
  "knitr",
  "ggplot2",
  "here",
  "kableExtra",
  "arrow",
  "pROC"
)

# -----------------------------------------------------------
# 2. Install packages if they are not already installed
# -----------------------------------------------------------
for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
}