library(alr4)
data(MinnWater)

# Fit the regression
mod <- lm(muniUse ~ muniPrecip + muniPop, data = MinnWater)
summary(mod)

# Standardized residuals
stdres <- rstandard(mod)

# Index plot (residuals vs. order/case number)
plot(stdres, type = "b",
     xlab = "Index (Case Number)", ylab = "Standardized Residuals",
     main = "Index Plot of Standardized Residuals")
abline(h = 0, lty = 2)

# Lag plot (residual_t vs residual_{t-1})
n <- length(stdres)
plot(stdres[-n], stdres[-1],
     xlab = expression(e[t-1]), ylab = expression(e[t]),
     main = "Lag Plot of Standardized Residuals")
abline(0, 1, col = "red", lty = 2)   # reference line, slope +1
abline(h = 0, v = 0, lty = 3)

library(lmtest)
dwtest(mod)

library(MASS)
sres <- stdres(mod)
plot(sres,type="b", ylab = "Standardized residual")
abline(h=0, lty=2)
title("Index plot")
plot(sres[1:(n-1)], sres[2:n], xlab = "Lagged standardized residual",
     ylab = "Standardized residual", type="l")
abline(a=0, b=1, lty=2)
title("Lag plot")
