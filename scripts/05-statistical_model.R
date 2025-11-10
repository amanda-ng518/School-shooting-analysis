#### Preamble ####
# Purpose: Run statistical models, conduct model diagnostics and evaluations
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
library(car)         
library(broom)
library(pROC)
library(ggplot2)
# -----------------------------------------------------------
# 2. Read in the data 
# -----------------------------------------------------------
shootings = read_parquet(here("data/01-cleaned_data/shootings_cleaned.parquet"))

# -----------------------------------------------------------
# 3. Separate train and test datasets
# -----------------------------------------------------------
set.seed(123)  
index <- sample(1:nrow(shootings), size = 0.85 * nrow(shootings))  # 85% train, 15% test
train_data <- shootings[index, ]
test_data <- shootings[-index, ]

# Relevel categorical variables
train_data$shooting_type <- as.factor(train_data$shooting_type)
train_data$shooting_type <- relevel(train_data$shooting_type, ref = "Unclear")

train_data$school_type <- as.factor(train_data$school_type)
train_data$school_type <- relevel(train_data$school_type, ref = "Private")

train_data$gender_shooter1 <- as.factor(train_data$gender_shooter1)
train_data$gender_shooter1 <- relevel(train_data$gender_shooter1, ref = "Female")

train_data$shooter_relationship1 <- as.factor(train_data$shooter_relationship1)
train_data$shooter_relationship1 <- relevel(train_data$shooter_relationship1, ref = "Other, Unknown or No Connection")

# -----------------------------------------------------------
# 3. Fit logistic regression model on train dataset
# -----------------------------------------------------------
# Fit model
model <- glm(killing_indicator ~ .,
             data = train_data, family = binomial)

# Save model outputs
saveRDS(model, here("models/logistic_regression_model.rds"))
# -----------------------------------------------------------
# 4. Model assumptions
# -----------------------------------------------------------
# 4.1 Binary outcome

# 4.2 Independence of Observations

# 4.3 No Multicollinearity
vif(model) 
# all below 5, pass
detach("package:car", unload = TRUE)

# 4.4 Linearity of Independent Variables and Log Odds
plot(model$fitted.values, residuals(model, type = "deviance"), 
     xlab = "Fitted values", ylab = "Deviance residuals", 
     main = "Deviance Residuals vs Fitted Values")
abline(h = 0, col = "red")
# No systematic non-linear patterns, pass

# 4.5 No Overdispersion
# Pearson chi-squared statistics
sum(residuals(model, type = "pearson")^2)/df.residual(model) 
# 1.036029, close to 1, pass

# -----------------------------------------------------------
# 5. Model Estimates and Hypothesis test
# -----------------------------------------------------------
model_summary <- tidy(model, exponentiate = FALSE, conf.int = TRUE) %>%
  select(term, estimate, std.error, conf.low, conf.high, p.value) %>%
  rename(
    "Term" = term,
    "Coefficient Estimate" = estimate,
    #"Odds Ratio" = estimate,
    "Standard Error" = std.error,
    "p-value" = p.value
  ) %>%
  mutate(
    # Combine lower and upper CI into one formatted string
    `95% CI` = paste0("[", round(conf.low, 4), ", ", round(conf.high, 4), "]"),
    # Round numeric columns
    across(c(`Coefficient Estimate`, `Standard Error`, `p-value`), ~ round(., 4)),
    # Format p-values: show <0.0001 when smaller than threshold
    `p-value` = if_else(`p-value` < 0.0001, "< 0.0001", as.character(round(`p-value`, 4)))
  ) %>%
  # Drop separate lower/upper CI columns 
  select(Term, `Coefficient Estimate`, `Standard Error`, `95% CI`, `p-value`)

model_summary <- model_summary %>%
  mutate(Term = recode(Term,
                       "(Intercept)" = "Intercept",
                       "injured_indicator1" = "Injured",
                       "school_typePublic" = "Public",
                       "shooting_typeAccidental" = "Accidental",
                       "shooting_typeIndiscriminate" = "Indiscriminate",
                       "shooting_typeSuicide" = "Suicide",
                       "shooting_typeTargeted" = "Targeted",
                       "gender_shooter1Male" = "Male",
                       "age_shooter1"= "Shooter Age",
                       "non_white_prop" = "Proportion of Non-White Students",
                       "lunch_prop" = "Proportion of Students Eligible for Subsidized Lunch",
                       "shooter_relationship1Current Student" = "Current Student",
                       "shooter_relationship1Family/Guardian of Student" = "Family/Guardian of Student",
                       "shooter_relationship1Former Student" = "Former Student",
                       "shooter_relationship1Non-Security Staff" = "Non-Security Staff",
                       "shooter_relationship1Police/Security" = "Police/Security"
  ))
# Save model summary
write_parquet(model_summary, here("data/02-analysis_data/modelsummary.parquet"))

# LRT
null_model <- glm(killing_indicator ~ 1, family = binomial, data = train_data)
anova(null_model, model, test = "Chisq") # p = 5.078e-07, significant

# -----------------------------------------------------------
# 6. Model diagnostics
# -----------------------------------------------------------
# ROC Curve
pred_probs <- predict(model, newdata = test_data,type = "response")
roc_obj <- roc(test_data$killing_indicator, pred_probs)
roc_data <- data.frame(
  FalsePositiveRate = 1 - roc_obj$specificities,
  TruePositiveRate = roc_obj$sensitivities,
  Threshold = roc_obj$thresholds
)
# AUC
roc_data$AUC <- as.numeric(auc(roc_obj)) 
# Save ROC and AUC
write_parquet(roc_data, here("data/02-analysis_data/roc_data.parquet"))

# Sensitivity, Specificity, and Misclassification rates 
thresholds <- seq(0, 1, 0.01)
perf_data <- sapply(thresholds, function(t) {
  pred <- ifelse(pred_probs > t, 1, 0)
  cm <- table(factor(pred, levels = 0:1), factor(test_data$killing_indicator, levels = 0:1))
  TP <- cm["1","1"]; TN <- cm["0","0"]; FP <- cm["1","0"]; FN <- cm["0","1"]
  c(Sensitivity = TP/(TP+FN), Specificity = TN/(TN+FP), Misclassification = (FP+FN)/sum(cm))
})
perf_data <- as.data.frame(t(perf_data))
perf_data$threshold <- thresholds
perf_data_long <- perf_data %>% pivot_longer(-threshold, names_to = "metric", values_to = "value")
# Save Metric Values for each Threshold
write_parquet(perf_data_long, here("data/02-analysis_data/perf_data_long.parquet"))
# Performance metric lineplot
ggplot(perf_data_long, aes(threshold, value, color = metric)) +
  geom_line(size = 0.5) +
  labs(x = "Threshold", y = "Metric value", color = "Metric",
       title = "Performance Metrics vs. Threshold") +
  theme_minimal()

# Optimal threshold metric values
optim_threshold = 0.23
pred_labels <- ifelse(pred_probs > optim_threshold, 1, 0) 
conf_matrix <- table(Predicted = pred_labels, Actual = test_data$killing_indicator) 
conf_matrix[2,2] / (conf_matrix[2,2] + conf_matrix[1,2]) # Sensitivity
conf_matrix[1,1] / (conf_matrix[1,1] + conf_matrix[2,1]) # Specificity
(conf_matrix[2,1] + conf_matrix[1,2]) / sum(conf_matrix) # Missclassification

