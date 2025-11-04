library(arrow)
library(here)

df <- read.csv(here("data/00-raw_data/school-shootings-data.csv"))

write_parquet(df, here("data/00-raw_data/school-shootings-data.parquet"))
