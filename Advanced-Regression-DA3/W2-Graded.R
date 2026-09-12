rm(list = ls())
library(datasets)
library(MASS)
library(car)

data(trees)

# ============================================================
# MODEL 1: NO TRANSFORMATION -- Volume ~ Girth
# ============================================================

mod <- lm(Volume ~ Girth, data = trees)
summary(mod)

fit1   <- fitted(mod)
res1   <- resid(mod)
stdres1 <- stdres(mod)

# Observed vs fitted
plot(fit1, trees$Volume,
     xlab = "Fitted Values", ylab = "Observed Volume",
     main = "Observed vs Fitted (No Transformation)")
abline(0, 1, col = "red")

# Residuals vs fitted -- checks if variance depends on the mean
plot(fit1, res1,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted (No Transformation)")
abline(h = 0, lty = 2)

# Standardized residuals vs fitted
plot(fit1, stdres1,
     xlab = "Fitted Values", ylab = "Standardized Residuals",
     main = "Standardized Residuals vs Fitted (No Transformation)")
abline(h = 0, lty = 2)

# Squared standardized residuals vs fitted, with loess smoother
plot(fit1, stdres1^2,
     xlab = "Fitted Values", ylab = "Standardized Residuals^2",
     main = "Squared Std. Residuals vs Fitted (No Transformation)")
abline(h = mean(stdres1^2), lty = 2)
lo1 <- loess(stdres1^2 ~ fit1)
ord1 <- order(fit1)
lines(fit1[ord1], predict(lo1)[ord1], col = "red", lwd = 2)

# Box-Cox plot -- suggests the appropriate power transformation
boxcox(mod)

# Spread-level plot (car package) -- directly suggests a power transformation
spreadLevelPlot(mod)


# ============================================================
# MODEL 2: LOG TRANSFORMATION -- log(Volume) ~ Girth
# ============================================================

logy <- log(trees$Volume)
mod_log <- lm(logy ~ Girth, data = trees)
summary(mod_log)

fit2   <- fitted(mod_log)
res2   <- resid(mod_log)
stdres2 <- stdres(mod_log)

# Observed vs fitted
plot(fit2, logy,
     xlab = "Fitted Values", ylab = "Observed log(Volume)",
     main = "Observed vs Fitted (Log Transformation)")
abline(0, 1, col = "red")

# Residuals vs fitted
plot(fit2, res2,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted (Log Transformation)")
abline(h = 0, lty = 2)

# Standardized residuals vs fitted
plot(fit2, stdres2,
     xlab = "Fitted Values", ylab = "Standardized Residuals",
     main = "Standardized Residuals vs Fitted (Log Transformation)")
abline(h = 0, lty = 2)

# Squared standardized residuals vs fitted, with loess smoother
plot(fit2, stdres2^2,
     xlab = "Fitted Values", ylab = "Standardized Residuals^2",
     main = "Squared Std. Residuals vs Fitted (Log Transformation)")
abline(h = mean(stdres2^2), lty = 2)
lo2 <- loess(stdres2^2 ~ fit2)
ord2 <- order(fit2)
lines(fit2[ord2], predict(lo2)[ord2], col = "red", lwd = 2)

# Box-Cox plot on the log model -- lambda near 1 here would confirm
# that no further transformation is needed after taking logs
boxcox(mod_log)

# Spread-level plot
spreadLevelPlot(mod_log)

