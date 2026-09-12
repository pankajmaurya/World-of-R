## PQ 2.8 Q1

library(lmreg)
data("imf2015")
#lmun <- lm(UNMP~EXP+INFL+INV, data = imf2015)

# Fit the regression model
model <- lm(UNMP ~ EXP + INFL + INV, data = imf2015)
summary(model)

# Obtain 95% prediction intervals for all observed combinations of regressors
pred_int <- predict(model, newdata = imf2015, interval = "prediction", level = 0.95)

# Combine with predicted values for filtering
results <- data.frame(
  fit = pred_int[, "fit"],
  lwr = pred_int[, "lwr"],
  upr = pred_int[, "upr"]
)
results$width <- results$upr - results$lwr

# Filter cases with predicted value less than 12
subset_results <- results[results$fit < 12, ]

# Average width of prediction intervals for this subset
avg_width <- mean(subset_results$width)
round(avg_width, 2)

# Load package and data
library(lmreg)
data(imf2015)

# Fit the regression model on log(UNMP)
model_log <- lm(log(UNMP) ~ EXP + INFL + INV, data = imf2015)
summary(model_log)

# Obtain 95% prediction intervals on the log scale
pred_int_log <- predict(model_log, newdata = imf2015, interval = "prediction", level = 0.95)

# Retransform back to the original UNMP scale by exponentiating
results <- data.frame(
  fit = exp(pred_int_log[, "fit"]),
  lwr = exp(pred_int_log[, "lwr"]),
  upr = exp(pred_int_log[, "upr"])
)
results$width <- results$upr - results$lwr

# Filter cases with predicted value (retransformed) less than 12
subset_results <- results[results$fit < 12, ]

# Average width of the retransformed prediction intervals
avg_width <- mean(subset_results$width)
round(avg_width, 2)
