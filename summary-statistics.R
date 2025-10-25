# -----------------------------------------------------------
# 1. Load libraries
# -----------------------------------------------------------
library(tidyr)
library(dplyr)
library(arrow)
library(knitr)
library(stringr)
library(ggplot2)

# -----------------------------------------------------------
# 2. Read in the data 
# -----------------------------------------------------------
shootings = read_parquet("shootings_cleaned.parquet")

nrow(shootings)
ncol(shootings)

# -----------------------------------------------------------
# 3. Subset data by killing indicator
# -----------------------------------------------------------
killing_yes_data = shootings%>%filter(killing_indicator == 0)
n_yes = nrow(killing_yes_data) # 246
killing_no_data = shootings%>%filter(killing_indicator == 1)
n_no = nrow(killing_no_data) # 85


# -----------------------------------------------------------
# 4. Numeric variables summary
# -----------------------------------------------------------

# Numeric variables by killing indicator summary table
numeric_vars <- c("age_shooter1", "non_white_prop", "lunch_prop")

make_summary <- function(df) {
  df %>%
    summarise(across(all_of(numeric_vars),
                     list(mean = ~mean(., na.rm = TRUE),
                          sd = ~sd(., na.rm = TRUE),
                          median = ~median(., na.rm = TRUE),
                          min = ~min(., na.rm = TRUE),
                          max = ~max(., na.rm = TRUE)),
                     .names = "{.fn}_{.col}")) %>%
    pivot_longer(
      everything(),
      names_to = c("Measure", "Variable"),
      names_pattern = "^(.*?)_(.*)$"
    ) %>%
    pivot_wider(names_from = Variable, values_from = value) %>%
    mutate(Measure = factor(Measure, levels = c("mean", "sd", "median", "min", "max")))
}

table_names <- c(
  "Measure" = "Measure",
  "Shooter age" = "age_shooter1",
  "Proportion of Non-white Students" = "non_white_prop",
  "Proportion of Lunch" = "lunch_prop"
)

num_summary_yes <- make_summary(filter(shootings, killing_indicator == 1)) %>% 
  rename(!!!table_names)
num_summary_no <- make_summary(filter(shootings, killing_indicator == 0)) %>% 
  rename(!!!table_names)

kable(num_summary_yes, caption = "Shootings with Killings", digits = 2)
kable(num_summary_no, caption = "Shootings without Killings", digits = 2)

# Shooter age
ggplot(shootings, aes(x = age_shooter1, fill = factor(killing_indicator))) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.5, color = "black") +
  labs(
    title = "Distribution of Shooter Age by Killing Indicator",
    x = "Shooter age",
    y = "Frequency"
  ) +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"),
                    labels = c("No", "Yes")) +
  guides(fill = guide_legend(title = "Killing Occured")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10),
        axis.title.x =element_text(size = 12),
        axis.title.y =element_text(size = 12),
        plot.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

ggplot(shootings, aes(x = killing_indicator, y = age_shooter1)) +
  geom_boxplot(alpha = 0.7, width = 0.6,
               outlier.shape = 21, outlier.fill = "white", outlier.color = "black") +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  labs(
    title = "Distribution of Shooter Age by Killing Indicator",
    x = "Killing occurred",
    y = "Shooter age"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.text = element_text(size = 12)
  )


# Proportion of Non-white student
ggplot(shootings, aes(x = non_white_prop, fill = factor(killing_indicator))) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.5, color = "black") +
  labs(
    title = "Distribution of Proportion of Non-white student by Killing Indicator",
    x = "Proportion of Non-white student",
    y = "Frequency"
  ) +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"),
                    labels = c("No", "Yes")) +
  guides(fill = guide_legend(title = "Killing Occured")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10),
        axis.title.x =element_text(size = 12),
        axis.title.y =element_text(size = 12),
        plot.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

ggplot(shootings, aes(x = killing_indicator, y = non_white_prop)) +
  geom_boxplot(alpha = 0.7, width = 0.6,
               outlier.shape = 21, outlier.fill = "white", outlier.color = "black") +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  labs(
    title = "Distribution of Proportion of Non-white student by Killing Indicator",
    x = "Killing occurred",
    y = "Proportion of Non-white student"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.text = element_text(size = 12)
  )

# Proportion of lunch distribution
ggplot(shootings, aes(x = lunch_prop, fill = factor(killing_indicator))) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.5, color = "black") +
  labs(
    title = "Distribution of Proportion of lunch by Killing Indicator",
    x = "Proportion of Lunch",
    y = "Frequency"
  ) +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"),
                    labels = c("No", "Yes")) +
  guides(fill = guide_legend(title = "Killing Occured")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10),
        axis.title.x =element_text(size = 12),
        axis.title.y =element_text(size = 12),
        plot.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

ggplot(shootings, aes(x = killing_indicator, y = lunch_prop)) +
  geom_boxplot(alpha = 0.7, width = 0.6,
               outlier.shape = 21, outlier.fill = "white", outlier.color = "black") +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  labs(
    title = "Distribution of Proportion of lunch by Killing Indicator",
    x = "Killing occurred",
    y = "Proportion of lunch"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.text = element_text(size = 12)
  )


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
  mutate("Killing Occured" = "Yes")

shooting_type_no <- killing_no_data %>%
  group_by(shooting_type) %>%
  summarise(shooting_type_percent = round(n()/n_no*100, 2)) %>%
  mutate("Killing Occured" = "No")

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
  theme(axis.text.x = element_text(size = 8,angle = 45, hjust = 1),
        axis.title.x =element_text(size = 12),
        axis.title.y =element_text(size = 12),
        plot.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

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
  theme(axis.text.x = element_text(size = 10),
        axis.title.x =element_text(size = 12),
        axis.title.y =element_text(size = 12),
        plot.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

