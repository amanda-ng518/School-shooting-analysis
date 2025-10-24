# -----------------------------------------------------------
# 1. Load libraries
# -----------------------------------------------------------
library(tidyverse)
library(broom)
library(janitor)
library(car)         # for VIF
library(performance) # for diagnostics
library(ggplot2)

# -----------------------------------------------------------
# 2. Read in the data 
# -----------------------------------------------------------
data = read.csv("school-shootings-data.csv")


# Binary outcome: 1 if at least one fatality
shootings <- data %>%
  mutate(fatal = if_else(killed > 0, 1, 0),
         # Simplify some categorical predictors
         weapon_type = case_when(
           str_detect(weapon, "handgun|pistol|revolver") ~ "handgun",
           str_detect(weapon, "rifle|shotgun") ~ "long_gun",
           TRUE ~ "other"
         ),
         shooter_age = as.numeric(age_shooter1),
         shooter_sex = factor(gender_shooter1),
         school_type = fct_lump_n(factor(school_type), 4)
  )

# Remove rows with missing key values
shootings <- shootings %>%
  filter(!is.na(fatal), !is.na(weapon_type), !is.na(shooter_sex), !is.na(school_type))

# -----------------------------------------------------------
# 5. Exploratory summaries
# -----------------------------------------------------------
shootings %>%
  count(fatal) %>%
  mutate(prop = n / sum(n))

ggplot(shootings, aes(x = fatal, fill = weapon_type)) +
  geom_bar(position = "fill") +
  labs(title = "Weapon Type vs Fatal Outcome", y = "Proportion", x = "Fatal (1=Yes)")

# -----------------------------------------------------------
# 6. Fit logistic regression model
# -----------------------------------------------------------
model <- glm(fatal ~ weapon_type + shooter_sex + shooter_age + 
               school_type + injured,
             data = shootings, family = binomial)

summary(model)

# -----------------------------------------------------------
# 7. Model diagnostics and multicollinearity (VIF below 5 is fine)
# -----------------------------------------------------------
vif(model)
check_model(model)

# -----------------------------------------------------------
# 8. Tidy output and interpretation
# -----------------------------------------------------------
tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
  arrange(desc(estimate)) %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  rename(OddsRatio = estimate,
         Lower95CI = conf.low,
         Upper95CI = conf.high)