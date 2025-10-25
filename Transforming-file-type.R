# install.packages("arrow")

library(arrow)

df <- read.csv("school-shootings-data.csv")

write_parquet(df, "school-shootings-data.parquet")