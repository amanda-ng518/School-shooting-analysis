# -----------------------------------------------------------
# 1. Load libraries
# -----------------------------------------------------------
library(tidyr)
library(dplyr)
library(arrow)
library(car)         # for VIF
library(broom)
library(pROC)
library(ggplot2)
# -----------------------------------------------------------
# 2. Read in the data 
# -----------------------------------------------------------
shootings = read_parquet("shootings_cleaned.parquet")

# -----------------------------------------------------------
# 3. Separate train and test datasets
# -----------------------------------------------------------
set.seed(123)  
index <- sample(1:nrow(shootings), size = 0.85 * nrow(shootings))  # 85% train, 15% test
train_data <- shootings[index, ]
test_data <- shootings[-index, ]

# -----------------------------------------------------------
# 3. Fit logistic regression model on train dataset
# -----------------------------------------------------------
model <- glm(killing_indicator ~ .,
             data = train_data, family = binomial)

# -----------------------------------------------------------
# 4. Model assumptions
# -----------------------------------------------------------
# 4.1 Binary outcome

# 4.2 Independence of Observations

# 4.3 No Multicollinearity
vif(model) # all below 5, pass
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
summary(model)

summaryOR = tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  rename(
    OddsRatio = estimate,
    Lower95CI = conf.low,
    Upper95CI = conf.high
  ) %>%
  mutate(across(c(OddsRatio, Lower95CI, Upper95CI, p.value), ~ round(., 4)))

# LRT
null_model <- glm(killing_indicator ~ 1, family = binomial, data = train_data)
anova(null_model, model, test = "Chisq") # 5.078e-07, significant

# -----------------------------------------------------------
# 6. Model diagnostics
# -----------------------------------------------------------
# AUC
pred_probs <- predict(model, newdata = test_data,type = "response")
roc_obj <- roc(test_data$killing_indicator, pred_probs)
plot(roc_obj, col = "blue", print.auc = TRUE) # 0.727

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

ggplot(perf_data_long, aes(threshold, value, color = metric)) +
  geom_line(size = 0.5) +
  labs(x = "Threshold", y = "Metric value", color = "Metric",
       title = "Performance Metrics vs. Threshold") +
  theme_minimal()

threshold = 0.23
pred_labels <- ifelse(pred_probs > threshold, 1, 0) 
conf_matrix <- table(Predicted = pred_labels, Actual = test_data$killing_indicator) 
conf_matrix[2,2] / (conf_matrix[2,2] + conf_matrix[1,2]) # Sensitivity
conf_matrix[1,1] / (conf_matrix[1,1] + conf_matrix[2,1]) # Specificity
(conf_matrix[2,1] + conf_matrix[1,2]) / sum(conf_matrix) # Missclassification

