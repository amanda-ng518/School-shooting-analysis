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
data = read_parquet("school-shootings-data.parquet")

# Number of observation
nrow(data)

# Number of variables
ncol(data)

# -----------------------------------------------------------
# 3. Data Cleaning
# -----------------------------------------------------------
# Change all blank string into NA
data <- data %>%
  mutate(across(where(is.character), ~ na_if(., "")))

# Missing values in interested variables
# Exclude id, geographical and temporal variables
# Exclude low grade, high grade of the school offers 
# Exclude staffing

interested_var = data %>%
  select(killed, injured, school_type, shooting_type, 
         age_shooter1,gender_shooter1, age_shooter2,gender_shooter2,
         enrollment, white, lunch, 
         resource_officer, weapon, weapon_source)

missing_table <- data.frame(
  Missing_Count = colSums(is.na(interested_var)),
  `Missing_Proportion` = round(colSums(is.na(interested_var)) / nrow(interested_var)*100,2)
)
missing_table <- subset(missing_table, Missing_Proportion > 0)
kable(missing_table, caption = "Proportion of missing values in each variable")

# Only 3 observations have a second shooter
# Almost 45% observations have a missing resource_officer information -> hard to impute
# More than 40% observations have missing weapon information -> hard to impute and too specific
# More than 75% observations have missing weapon source information -> hard to impute and too specific
# 130 obs with missing shooter age

# Shooting type
unique(data$shooting_type)
obs1 = data%>%filter(shooting_type == "suicide") # only one observation with 0 killed
obs2 = data%>%filter(shooting_type == "accidental or targeted") # 2 observations
obs3 = data%>%filter(shooting_type == "targeted and indiscriminate") # 6 observations, keep this category

data <- data %>%
  mutate(
    shooting_type = shooting_type %>%
      str_trim() %>%                              # remove leading/trailing spaces
      str_to_lower() %>%                          # make lowercase
      str_replace_all("uclear", "unclear") %>%    # fix typo
      str_replace_all("public suicide \\(attempted\\)", "public suicide") %>%
      str_replace_all("^suicide$", "public suicide"),              # exact match only
    shooting_type = if_else(is.na(shooting_type), "unclear", shooting_type), # NA → unclear
    shooting_type = str_to_title(shooting_type)   # pretty capitalization
  )
unique(data$shooting_type)

# Shooter age by mean of shooting type


# Shooter gender (probability) by mean of shooting type




# -----------------------------------------------------------
# 3. Summary statistics
# -----------------------------------------------------------
# Shooter Age by Shooting Type
shooter_age_summary_table <- data %>%
  group_by(as.factor(shooting_type)) %>%
  summarise(
    n = sum(!is.na(age_shooter1)),
    mean = mean(age_shooter1, na.rm = TRUE),
    median = median(age_shooter1, na.rm = TRUE),
    sd = sd(age_shooter1, na.rm = TRUE),
    min = min(age_shooter1, na.rm = TRUE),
    max = max(age_shooter1, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))
kable(shooter_age_summary_table, caption = "Summary Statistics of Shooter Age by Shooting Type")


# -----------------------------------------------------------
# 4. Extract relevant variables
# -----------------------------------------------------------
shootings <- data%>%
  select(killed,school_type, shooting_type, shooter_age, 
         age_shooter1,gender_shooter1, age_shooter2,gender_shooter2,
         enrollment, white, lunch, resource_officer)

# Binary outcome: 1 if at least one fatality
shootings <- data %>%
  mutate(fatal = if_else(killed > 0, 1, 0),
         # Simplify some categorical predictors
         shooter_age = as.numeric(age_shooter1),
         shooter_sex = factor(gender_shooter1),
         school_type = fct_lump_n(factor(school_type), 4)
  )