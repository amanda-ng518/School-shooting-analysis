# -----------------------------------------------------------
# 1. Load libraries
# -----------------------------------------------------------
library(tidyr)
library(dplyr)
library(arrow)
library(knitr)
library(stringr)

# -----------------------------------------------------------
# 2. Read in the data 
# -----------------------------------------------------------
data = read_parquet("data/00-raw_data/school-shootings-data.parquet")

# -----------------------------------------------------------
# 2. Test data 
# -----------------------------------------------------------

# Test if the data was successfully loaded
if (exists("data")) {
  message("Test Passed: The dataset was successfully loaded.")
} else {
  stop("Test Failed: The dataset could not be loaded.")
}

# Check if the dataset has 428 rows
if (nrow(data) == 428) {
  message("Test Passed: The dataset has 428 rows.")
} else {
  stop("Test Failed: The dataset does not have 428 rows.")
}

# Check if the dataset has 50 columns
if (ncol(data) == 50) {
  message("Test Passed: The dataset has 50 columns.")
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
valid_types <- c("accidental", "hostage suicide", "indiscriminate", 
                   "public suicide", "targeted", "unclear")

if (all(data$shooting_type %in% valid_types)) {
  message("Test Passed: The 'shooting_type' column contains only valid shooting type names.")
} else {
  stop("Test Failed: The 'shooting_type' column contains invalid shooting type names.")
}

# Check if the 'shooter gender' column contains only valid gender names
valid_genders <- c("f", "m")

if (all(data$gender_shooter1 %in% valid_genders)) {
  message("Test Passed: The 'gender_shooter1' column contains only valid gender names.")
} else {
  stop("Test Failed: The 'gender_shooter1' column contains invalid gender names.")
}
if (all(data$gender_shooter2 %in% valid_genders)) {
  message("Test Passed: The 'gender_shooter2' column contains only valid gender names.")
} else {
  stop("Test Failed: The 'gender_shooter2' column contains invalid gender names.")
}

# Check if the 'shooter race' column contains only valid race names
valid_race <- c("a", "ai", "b","h","w")

if (all(data$race_ethnicity_shooter1 %in% valid_race)) {
  message("Test Passed: The 'race_ethnicity_shooter1' column contains only valid race names.")
} else {
  stop("Test Failed: The 'race_ethnicity_shooter1' column contains invalid race names.")
}
if (all(data$race_ethnicity_shooter2 %in% valid_race)) {
  message("Test Passed: The 'race_ethnicity_shooter2' column contains only valid race names.")
} else {
  stop("Test Failed: The 'race_ethnicity_shooter2' column contains invalid race names.")
}

# Check if the 'Shooting type' column contains only valid school type names
valid_school_types <- c("private", "public")

if (all(data$school_type %in% valid_school_types)) {
  message("Test Passed: The 'school_type' column contains only valid school type names.")
} else {
  stop("Test Failed: The 'school_type' column contains invalid school type names.")
}

# Check numerical variables type
if (class(data$white) == "integer") {
  message("Test Passed: Correct variable type for number of white students variable.")
} else {
  stop("Test Failed: Incorrect variable type for number of white students variable.")
}

if (class(data$enrollment) == "integer") {
  message("Test Passed: Correct variable type for enrollment variable.")
} else {
  stop("Test Failed: Incorrect variable type for enrollment variable.")
}

if (class(data$lunch) == "integer") {
  message("Test Passed: Correct variable type for number of students eligible for subsizied lunch variable.")
} else {
  stop("Test Failed: Incorrect variable type for number of students eligible for subsizied lunch variable.")
}

# Check number of students eligible for subsidized lunch is not greater than total enrollment
if (all(as.numeric(data$lunch) <= as.numeric(data$enrollment), na.rm = TRUE)) {
  message("Test Passed: All schools have valid entry for number of students eligible for subsidized lunch.")
} else {
  stop("Test Failed: Some schools have more students eligible for subsidized lunch than its total enrollment.")
}

# Check number of white students is not greater than total enrollment
if (all(as.numeric(data$white) <= as.numeric(data$enrollment), na.rm = TRUE)) {
  message("Test Passed: All schools have valid entry for number of white students.")
} else {
  stop("Test Failed: Some schools have more white students than its total enrollment.")
}
