setwd("~/TechCareer/World-of-R")
df <- read.csv("LinearRegressionData1.csv")
print(df)

model <- lm(y ~ x, data = df)
summary(model)

# plot(df$x, df$y, main = "Linear Regression", xlab = "x", ylab = "y")
# abline(model, col = "blue", lwd = 2)

library(ggplot2)

ggplot(df, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Linear Regression Line") +
  theme_minimal()

residuals <- residuals(model)
fitted <- fitted(model)
head(residuals)

plot(model$fitted.values, residuals,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residual Plot")
abline(h = 0, col = "red", lwd = 2)