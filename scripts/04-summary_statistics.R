#### Preamble ####
# Purpose: Conduct EDA to obtain summary statistics and visualizations
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
library(ggplot2)
library(here)

# -----------------------------------------------------------
# 2. Read in the data 
# -----------------------------------------------------------
shootings = read_parquet(here("data/01-cleaned_data/shootings_cleaned.parquet"))

nrow(shootings)
ncol(shootings)

# -----------------------------------------------------------
# 3. Subset data by killing indicator
# -----------------------------------------------------------
killing_yes_data = shootings%>%filter(killing_indicator == 1)
n_yes = nrow(killing_yes_data) # 99
killing_no_data = shootings%>%filter(killing_indicator == 0)
n_no = nrow(killing_no_data) # 328

# -----------------------------------------------------------
# 4. Numeric variables summary
# -----------------------------------------------------------

# Numeric variables by killing indicator summary table
numeric_vars <- c("age_shooter1", "non_white_prop", "lunch_prop")

# Function to summarize numerical variables
make_summary <- function(df) {
  df %>%
    summarise(across(all_of(numeric_vars),
                     list("Mean" = ~mean(., na.rm = TRUE),
                          "Standard Deviation" = ~sd(., na.rm = TRUE),
                          "Median" = ~median(., na.rm = TRUE),
                          "Min" = ~min(., na.rm = TRUE),
                          "Max" = ~max(., na.rm = TRUE)),
                     .names = "{.fn}_{.col}")) %>%
    pivot_longer(
      everything(),
      names_to = c("Measure", "Variable"),
      names_pattern = "^(.*?)_(.*)$"
    ) %>%
    pivot_wider(names_from = Variable, values_from = value) %>%
    mutate(Measure = factor(Measure, levels = c("Mean", "Standard Deviation", "Median", "Min", "Max")))
}

table_names <- c(
  "Measure" = "Measure",
  "Shooter Age" = "age_shooter1",
  "Proportion of Non-white Students" = "non_white_prop",
  "Proportion of Students with Subsidized Lunch" = "lunch_prop"
)

# Produce summaries for each group
num_summary_yes <- make_summary(filter(shootings, killing_indicator == 1)) %>% 
  rename(!!!table_names)
num_summary_no <- make_summary(filter(shootings, killing_indicator == 0)) %>% 
  rename(!!!table_names)

# Generate tables
kable(num_summary_yes, caption = "Shootings with Killings", digits = 2)
kable(num_summary_no, caption = "Shootings without Killings", digits = 2)

write_parquet(num_summary_yes, here("data/02-analysis_data/num_summary_yes.parquet"))
write_parquet(num_summary_no, here("data/02-analysis_data/num_summary_no.parquet"))

# Shooter age histogram
ggplot(shootings, aes(x = age_shooter1, fill = factor(killing_indicator))) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.5, color = "black") +
  labs(
    title = "Distribution of Shooter Age by Killing Occurrence",
    x = "Shooter age",
    y = "Frequency"
  ) +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"),
                    labels = c("No", "Yes")) +
  guides(fill = guide_legend(title = "Killing Occured")) +
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 7),
        axis.title.x =element_text(size = 8),
        axis.title.y =element_text(size = 8),
        plot.title = element_text(size = 12),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8))

# Proportion of Non-white student histogram
ggplot(shootings, aes(x = non_white_prop, fill = factor(killing_indicator))) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.5, color = "black") +
  labs(
    title = "Distribution of Proportion of Non-white student by Killing Occurrence",
    x = "Proportion of Non-white student",
    y = "Frequency"
  ) +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"),
                    labels = c("No", "Yes")) +
  guides(fill = guide_legend(title = "Killing Occured")) +
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 7),
        axis.title.x =element_text(size = 8),
        axis.title.y =element_text(size = 8),
        plot.title = element_text(size = 12),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8))

# Proportion of lunch distribution histogram
ggplot(shootings, aes(x = lunch_prop, fill = factor(killing_indicator))) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.5, color = "black") +
  labs(
    title = "Distribution of Proportion of Students Eligible for Subsidized Lunch by Killing Occurrence",
    x = "Proportion of Students Eligible for Subsidized Lunch",
    y = "Frequency"
  ) +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"),
                    labels = c("No", "Yes")) +
  guides(fill = guide_legend(title = "Killing Occured")) +
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 7),
        axis.title.x =element_text(size = 8),
        axis.title.y =element_text(size = 8),
        plot.title = element_text(size = 12),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8))

# -----------------------------------------------------------
# 5. Categorical variables summary
# -----------------------------------------------------------

# Categorical vs killing indicator (proportion bar plots)
categorical_vars <- c("injured_indicator", "school_type", "shooting_type",
                      "gender_shooter1", "shooter_relationship1")
# Shooting Type
# Summarize percentages for each categories
shooting_type_yes <- killing_yes_data %>%
  group_by(shooting_type) %>%
  summarise(shooting_type_percent = round(n()/n_yes*100, 2)) %>%
  mutate("Killing Occured" = "Yes")%>%
  # Add the missing categories and set 0
  bind_rows(tibble(
    shooting_type = c("Suicide"),
    shooting_type_percent = 0,
    `Killing Occured` = "Yes"
  ))

shooting_type_no <- killing_no_data %>%
  group_by(shooting_type) %>%
  summarise(shooting_type_percent = round(n() / n_no * 100, 2), .groups = "drop") %>%
  mutate(`Killing Occured` = "No") 

shooting_type_combined_data <- bind_rows(shooting_type_yes, shooting_type_no)

# Barplot
ggplot(shooting_type_combined_data, aes(x = shooting_type, y = shooting_type_percent, fill = `Killing Occured`)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = shooting_type_percent), position = position_dodge(width = 0.9), vjust = -0.5, size = 2) +
  labs(
    title = "Percentage of Shooting cases by Shooting type (Killing occured vs. did not occured)",
    x = "Shooting type",
    y = "Percentage (%)"
  ) +
  scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) +
  theme_minimal()+ 
  theme(axis.text.x = element_text(size = 7),
        axis.title.x =element_text(size = 8),
        axis.title.y =element_text(size = 8),
        plot.title = element_text(size = 12),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8))

# School Type
# Summarize percentages for each categories
school_type_yes <- killing_yes_data %>%
  group_by(school_type) %>%
  summarise(school_type_percent = round(n()/n_yes*100, 2)) %>%
  mutate("Killing Occured" = "Yes")

school_type_no <- killing_no_data %>%
  group_by(school_type) %>%
  summarise(school_type_percent = round(n()/n_no*100, 2)) %>%
  mutate("Killing Occured" = "No")

school_type_combined_data <- bind_rows(school_type_yes, school_type_no)

# Barplot
ggplot(school_type_combined_data, aes(x = school_type, y = school_type_percent, fill = `Killing Occured`)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = school_type_percent), position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +
  labs(
    title = "Percentage of Shooting cases by School type (Killing occured vs. did not occured)",
    x = "School type",
    y = "Percentage(%)"
  ) +
  scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) +
  theme_minimal()+ 
  theme(axis.text.x = element_text(size = 7),
        axis.title.x =element_text(size = 8),
        axis.title.y =element_text(size = 8),
        plot.title = element_text(size = 12),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8))

# Shooter gender
# Summarize percentages for each categories
gender_yes <- killing_yes_data %>%
  group_by(gender_shooter1) %>%
  summarise(gender_percent = round(n()/n_yes*100, 2)) %>%
  mutate("Killing Occured" = "Yes")

gender_no <- killing_no_data %>%
  group_by(gender_shooter1) %>%
  summarise(gender_percent = round(n()/n_no*100, 2)) %>%
  mutate("Killing Occured" = "No")

gender_combined_data <- bind_rows(gender_yes, gender_no)

# Barplot
ggplot(gender_combined_data, aes(x = gender_shooter1, y = gender_percent, fill = `Killing Occured`)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = gender_percent), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(
    title = "Percentage of Shooting cases by Shooter Gender (Killing occured vs. did not occured)",
    x = "Shooter Gender",
    y = "Percentage(%)"
  ) +
  scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) +
  theme_minimal()+ 
  theme(axis.text.x = element_text(size = 7),
        axis.title.x =element_text(size = 8),
        axis.title.y =element_text(size = 8),
        plot.title = element_text(size = 12),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8))

# shooter_relationship1
# Summarize percentages for each categories
shooter_relationship_yes <- killing_yes_data %>%
  group_by(shooter_relationship1) %>%
  summarise(shooter_relationship_percent = round(n()/n_yes*100, 2)) %>%
  mutate("Killing Occured" = "Yes")

shooter_relationship_no <- killing_no_data %>%
  group_by(shooter_relationship1) %>%
  summarise(shooter_relationship_percent = round(n() / n_no * 100, 2), .groups = "drop") %>%
  mutate(`Killing Occured` = "No")

shooter_relationship_combined_data <- bind_rows(shooter_relationship_yes, shooter_relationship_no)

# Barplot
ggplot(shooter_relationship_combined_data, aes(x = shooter_relationship1, y = shooter_relationship_percent, fill = `Killing Occured`)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = shooter_relationship_percent), position = position_dodge(width = 0.9), vjust = -0.5, size = 2) +
  labs(
    title = "Percentage of Shooting cases by Shooter Relationship (Killing occured vs. did not occured)",
    x = "Shooter Relationship",
    y = "Percentage (%)"
  ) +
  scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) +
  theme_minimal()+ 
  theme(axis.text.x = element_text(size = 7,angle = 45, hjust = 1),
        axis.title.x =element_text(size = 8),
        axis.title.y =element_text(size = 8),
        plot.title = element_text(size = 12),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8))

# injured_indicator
# Summarize percentages for each categories
injured_yes <- killing_yes_data %>%
  group_by(injured_indicator) %>%
  summarise(injured_percent = round(n()/n_yes*100, 2)) %>%
  mutate("Killing Occured" = "Yes")

injured_no <- killing_no_data %>%
  group_by(injured_indicator) %>%
  summarise(injured_percent = round(n()/n_no*100, 2)) %>%
  mutate("Killing Occured" = "No")

injured_combined_data <- bind_rows(injured_yes, injured_no)

injured_combined_data <- injured_combined_data %>%
  mutate(injured_indicator = factor(injured_indicator,
                                    levels = c(0, 1),
                                    labels = c("No injured", "At least one injured")))


# Barplot
ggplot(injured_combined_data, aes(x = injured_indicator, y = injured_percent, fill = `Killing Occured`)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = injured_percent), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(
    title = "Percentage of Shooting cases by Injured Indicator (Killing occured vs. did not occured)",
    x = "Injured Indicator",
    y = "Percentage(%)"
  ) +
  scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) +
  theme_minimal()+ 
  theme(axis.text.x = element_text(size = 7),
        axis.title.x =element_text(size = 8),
        axis.title.y =element_text(size = 8),
        plot.title = element_text(size = 12),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8))

