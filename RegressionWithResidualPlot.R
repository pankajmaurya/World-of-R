setwd("~/TechCareer/World-of-R")
df <- read.csv("cubic-term-data.csv")
print(df)

model <- lm(y ~ x, data = df)
summary(model)

residuals <- residuals(model)
fitted <- fitted(model)
head(residuals)

plot(model$fitted.values, residuals,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residual Plot")
abline(h = 0, col = "red", lwd = 2)

model <- lm(y ~ x + I(x^2), data = df)
summary(model)

residuals <- residuals(model)
fitted <- fitted(model)
head(residuals)

plot(model$fitted.values, residuals,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residual Plot")
abline(h = 0, col = "red", lwd = 2)

model <- lm(y ~ x + I(x^2) + I(x^3), data = df)
summary(model)

residuals <- residuals(model)
fitted <- fitted(model)
head(residuals)

plot(model$fitted.values, residuals,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residual Plot")
abline(h = 0, col = "red", lwd = 2)