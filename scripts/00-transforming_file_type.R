library(arrow)

df <- read.csv("data/00-raw_data/school-shootings-data.csv")

write_parquet(df, "data/00-raw_data/school-shootings-data.parquet")