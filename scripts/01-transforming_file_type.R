#### Preamble ####
# Purpose: Loads and saves the data in Parquet form
# Author: Amanda Ng
# Date: 9 November 2025
# Contact: waiyuamanda.ng@mail.utoronto.ca

# -----------------------------------------------------------
# 1. Load libraries
# -----------------------------------------------------------
library(arrow)
library(here)

# -----------------------------------------------------------
# 2. Load Data
# -----------------------------------------------------------
df <- read.csv(here("data/00-raw_data/school-shootings-data.csv"))

# -----------------------------------------------------------
# 3. Save Data in Parquest file
# -----------------------------------------------------------
write_parquet(df, here("data/00-raw_data/school-shootings-data.parquet"))
