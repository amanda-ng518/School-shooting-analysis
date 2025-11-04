# List of packages to install
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

# Install packages if they are not already installed
for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
}