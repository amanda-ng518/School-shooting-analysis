#### Preamble ####
# Purpose: Tests cleaned data
# Author: Amanda Ng
# Date: 9 November 2025
# Contact: waiyuamanda.ng@mail.utoronto.ca
# Pre-requisites: 02-data_cleaning.R

# -----------------------------------------------------------
# 1. Load libraries
# -----------------------------------------------------------
library(tidyr)
library(dplyr)
library(arrow)
library(knitr)
library(stringr)
library(here)
# -----------------------------------------------------------
# 2. Read in the data 
# -----------------------------------------------------------
data = read_parquet(here("data/01-cleaned_data/shootings_cleaned.parquet"))

# -----------------------------------------------------------
# 2. Test data 
# -----------------------------------------------------------
# Test if the data was successfully loaded
if (exists("data")) {
  message("Test Passed: The dataset was successfully loaded.")
} else {
  stop("Test Failed: The dataset could not be loaded.")
}

# Check if the dataset has 424 rows
if (nrow(data) == 424) {
  message("Test Passed: The dataset has 424 rows.")
} else {
  stop("Test Failed: The dataset does not have 424 rows.")
}

# Check if the dataset has 9 columns
if (ncol(data) == 9) {
  message("Test Passed: The dataset has 9 columns.")
} else {
  stop("Test Failed: The dataset does not have 50 columns.")
}

# Check if there are any missing values in the dataset
if (all(!is.na(data))) {
  message("Test Passed: The dataset contains no missing values.")
} else {
  stop("Test Failed: The dataset contains missing values.")
}

# Check if the 'Shooting type' column contains only valid shooting type names
valid_types <- c("Indiscriminate","Targeted","Accidental","Suicide","Unclear")

if (all(data$shooting_type %in% valid_types)) {
  message("Test Passed: The 'shooting_type' column contains only valid shooting type names.")
} else {
  stop("Test Failed: The 'shooting_type' column contains invalid shooting type names.")
}

# Check if the 'shooter gender' column contains only valid gender names
valid_genders <- c("Male", "Female")

if (all(data$gender_shooter1 %in% valid_genders)) {
  message("Test Passed: The 'gender_shooter1' column contains only valid gender names.")
} else {
  stop("Test Failed: The 'gender_shooter1' column contains invalid gender names.")
}

# Check if the 'Shooting type' column contains only valid school type names
valid_school_types <- c("Private", "Public")

if (all(data$school_type %in% valid_school_types)) {
  message("Test Passed: The 'school_type' column contains only valid school type names.")
} else {
  stop("Test Failed: The 'school_type' column contains invalid school type names.")
}

# Check if the 'Shooter relationship' column contains only valid school type names
valid_relationship <- c("Current Student", "Other, Unknown or No Connection"
                        ,"Family/Guardian of Student","Non-Security Staff"             
                        ,"Police/Security","Former Student" )

if (all(data$shooter_relationship1 %in% valid_relationship)) {
  message("Test Passed: The 'shooter_relationship1' column contains only valid school type names.")
} else {
  stop("Test Failed: The 'shooter_relationship1' column contains invalid school type names.")
}

# Check numerical variables type
if (class(data$age_shooter1) == "numeric") {
  message("Test Passed: Correct variable type for shooter age variable.")
} else {
  stop("Test Failed: Incorrect variable type for shooter age variable.")
}

# Check number of students eligible for subsidized lunch is not greater than total enrollment
if (all(data$lunch_prop <= 1)) {
  message("Test Passed: All schools have valid entry for number of students eligible for subsidized lunch.")
} else {
  stop("Test Failed: Some schools have more students eligible for subsidized lunch than its total enrollment.")
}

# Check number of non-white students is not greater than total enrollment
if (all(data$non_white_prop <= 1)) {
  message("Test Passed: All schools have valid entry for number of non-white students.")
} else {
  stop("Test Failed: Some schools have more non-white students than its total enrollment.")
}
