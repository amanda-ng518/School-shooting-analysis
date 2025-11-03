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

# Clean string representations
# State
data <- data %>%
  mutate(state = state %>%str_trim())  # remove leading/trailing spaces

# Shooting type
unique(data$shooting_type)
nrow(data%>%filter(shooting_type == "suicide")) # single obs with 0 killed
nrow(data%>%filter(shooting_type == "accidental or targeted")) # 2 
nrow(data%>%filter(shooting_type == "targeted and indiscriminate")) # 6 
nrow(data%>%filter(shooting_type == "hostage suicide")) # 2
nrow(data%>%filter(shooting_type == "public suicide")) # 8

data <- data %>%
  mutate(
    shooting_type = shooting_type %>%
      str_trim() %>%                              # remove leading/trailing spaces
      str_to_lower() %>%                          # make lowercase
      str_replace_all("uclear", "unclear") %>%    # fix typo
      str_replace_all("public suicide \\(attempted\\)", "suicide") %>%
      str_replace_all("targeted and indiscriminate", "targeted") %>%
      str_replace_all("public suicide", "suicide") %>%
      str_replace_all("accidental or targeted", "accidental") %>%
      str_replace_all("hostage suicide", "suicide"),              
    shooting_type = if_else(is.na(shooting_type), "unclear", shooting_type), 
    shooting_type = str_to_title(shooting_type)   # pretty capitalization
  )
unique(data$shooting_type)

# Shooter 1 relationship
unique(data$shooter_relationship1)
data <- data %>%
  mutate(shooter_relationship1 = case_when(
    str_detect(shooter_relationship1, regex("^student\\b|student \\(|student in summer school", ignore_case = TRUE)) ~ "Current Student",
    str_detect(shooter_relationship1, regex("former student|suspended student", ignore_case = TRUE)) ~ "Former Student",
    str_detect(shooter_relationship1, regex("parent|father|mother|brother|girlfriend", ignore_case = TRUE)) ~ "Family/Guardian of Student",
    str_detect(shooter_relationship1, regex("teacher|janitor|staff|employee|substitute", ignore_case = TRUE)) ~ "Non-Security Staff",
    str_detect(shooter_relationship1, regex("spouse|husband|ex-husband", ignore_case = TRUE)) ~ "Family of Staff",
    str_detect(shooter_relationship1, regex("police|sheriff|SWAT|resource officer|security", ignore_case = TRUE)) ~ "Police/Security",
    is.na(shooter_relationship1) ~ "Other, Unknown or No Connection", # 145
    TRUE ~ "Other, Unknown or No Connection" # 19
  ))
unique(data$shooter_relationship1)

# Gender
data <- data %>% filter(gender_shooter1 == "m"|gender_shooter1 == "f"| is.na(gender_shooter1)) # exclude single observation with "h"

# Rename categorical variables value
data <- data%>%mutate(
  gender_shooter1 = case_when(gender_shooter1 == "m" ~ "Male",
                              gender_shooter1 == "f" ~ "Female"),
  school_type = case_when(school_type == "public" ~ "Public",
                          school_type == "private" ~ "Private")
)


# Transform numerical variables back to integer representation
data$lunch <- as.integer(gsub(",", "", data$lunch))
data$enrollment <- as.integer(gsub(",", "", data$enrollment))
data$white <- as.integer(gsub(",", "", data$white))

# Lunch
data <- data %>% filter(lunch <= enrollment| is.na(lunch)) # exclude 3 obs with lunch > enrollment

#--------------Missing values------------------#
# Examine relevant variables
# Exclude id, geographical and temporal variables
# Exclude low grade, high grade of the school offers, staffing 
interested_var = data %>%
  select(killed, injured, school_type, shooting_type, 
         age_shooter1,gender_shooter1, age_shooter2,gender_shooter2,
         enrollment, white, lunch, 
         resource_officer, weapon, weapon_source,state,
         shooter_relationship1, shooter_relationship2)

new_names <- c(
  killed = "Number of killed (excluding shooter)",
  injured = "Number of injured (excluding shooter)",
  school_type = "School type",
  shooting_type = "Shooting type",
  age_shooter1 = "Age of shooter 1",
  gender_shooter1 = "Gender of shooter 1",
  age_shooter2 = "Age of shooter 2",
  gender_shooter2 = "Gender of shooter 2",
  enrollment = "Number of student enrollment",
  white = "Number of white students",
  lunch = "Number of students eligible for free/reduced-price lunch",
  resource_officer = "Resource officer present",
  weapon = "Weapon used",
  weapon_source = "Weapon source",
  state = "State",
  shooter_relationship1 = "Shooter 1 relationship with school community",
  shooter_relationship2 = "Shooter 2 relationship with school community"
)

missing_table <- data.frame(
  Variable = names(interested_var),  
  "Count" = colSums(is.na(interested_var)),
  "Proportion" = round(colSums(is.na(interested_var)) / nrow(interested_var)*100,2),
  row.names = NULL
)

# Replace variable names in the table
missing_table$Variable <- new_names[missing_table$Variable]

missing_table <- subset(missing_table, Count > 0)

write_parquet(missing_table, "data/02-analysis_data/missing_table.parquet")

kable(missing_table, col.names = c("Variable", "Missing Count", "Missing Proportion (%)"), 
      align = c("l", "r", "r"),
      caption = "Count and Proportion of Missing Values in Interested Variables")

# Only 3% observations have a second shooter
# Almost 45% observations have a missing resource_officer information -> hard to impute
# More than 40% observations have missing weapon information -> hard to impute and too specific
# More than 75% observations have missing weapon source information -> hard to impute and too specific
# 130 obs with missing shooter age

#------------------Impute data--------------------#

# --- 1. Impute age_shooter1 ---
data <- data %>%
  # Compute group mean by shooting_type × state
  group_by(shooting_type, state) %>%
  mutate(group_mean_age = mean(age_shooter1, na.rm = TRUE)) %>%
  ungroup() %>%
  # Compute state mean as fallback
  group_by(state) %>%
  mutate(state_mean_age = mean(age_shooter1, na.rm = TRUE)) %>% # 5 states still missing
  ungroup() %>%
  # Compute overall mean as fallback
  mutate(overall_age = mean(age_shooter1, na.rm = TRUE)) %>%
  # Impute: use group mean if available, else state mean, else overall mean
  mutate(
    age_shooter1 = coalesce(age_shooter1, group_mean_age, state_mean_age, overall_age)
  ) %>%
  select(-group_mean_age, -state_mean_age,-overall_age)  # remove helper columns

# --- 2. Impute gender_shooter1 probabilistically ---
# Compute state-level male proportion
state_prop <- data %>%
  group_by(state) %>%
  summarise(state_prop_m = mean(gender_shooter1 == "Male", na.rm = TRUE), .groups = "drop")

# Compute overall proportion as fallback
overall_prop_m <- mean(data$gender_shooter1 == "Male", na.rm = TRUE)

# Join state proportions
data <- data %>%
  left_join(state_prop, by = "state") %>%
  rowwise() %>%
  mutate(
    gender_shooter1 = if_else(
      is.na(gender_shooter1),
      {prob_m <- ifelse(is.na(state_prop_m), overall_prop_m, state_prop_m)
        sample(c("Male", "Female"), size = 1, replace = TRUE, prob = c(prob_m, 1 - prob_m))
      },
      gender_shooter1
    )
  ) %>%
  ungroup() %>%
  select(-state_prop_m)

# --- 3. Impute lunch ---
# Impute only for public schools, set 0 for private school
data <- data %>%
  mutate(lunch_prop = lunch / enrollment) %>%
  # Impute by state 
  group_by(state) %>%
  mutate(mean_lunch_prop_state = mean(lunch_prop, na.rm = TRUE)) %>%
  ungroup() %>%
  # Overall as fallback 
  mutate(mean_lunch_prop_overall = mean(lunch_prop, na.rm = TRUE)) %>%
  mutate(
    lunch = case_when(
      school_type == "Private" ~ 0,
      school_type == "Public"~ coalesce(lunch, round(mean_lunch_prop_state * enrollment), round(mean_lunch_prop_overall * enrollment) )
    )
  ) %>%
  select(-lunch_prop, -mean_lunch_prop_state)

# --- 3. Impute number of white ---
data <- data %>%
  mutate(white_prop = white/enrollment) %>%
  # Impute by shooting type x state x school_type
  group_by(shooting_type, state, school_type) %>%
  mutate(mean_white_prop_shooting = mean(white_prop, na.rm = TRUE)) %>%
  ungroup() %>%
  # Impute by state x school_type
  group_by(state, school_type) %>%
  mutate(mean_white_prop_state = mean(white_prop, na.rm = TRUE)) %>%
  ungroup() %>%
  # Impute by school_type
  group_by(school_type) %>%
  mutate(mean_white_prop_type = mean(white/enrollment, na.rm = TRUE)) %>%
  ungroup() %>%
  # Impute: use group mean if available, else school type mean
  mutate(
    white = coalesce(white, round(mean_white_prop_shooting*enrollment), 
                     round(mean_white_prop_state*enrollment), 
                     round(mean_white_prop_type)*enrollment)
  ) %>%
  select(-mean_white_prop_shooting, -mean_white_prop_state, -mean_white_prop_type)

#--------------Create new variables-------------------#
data <- data %>%mutate(
    killing_indicator = as.factor(if_else(killed > 0, 1L, 0L)),
    injured_indicator = as.factor(if_else(injured > 0, 1L, 0L)),
    lunch_prop = lunch / enrollment,
    non_white_prop = 1 - white / enrollment
  )

#------------Dataset for analysis-----------#
shootings <- data%>%select(killing_indicator, injured_indicator, school_type, shooting_type, 
                           age_shooter1,gender_shooter1, shooter_relationship1,
                           non_white_prop, lunch_prop)

# Save dataset
write_parquet(shootings, "data/01-cleaned_data/shootings_cleaned.parquet")
