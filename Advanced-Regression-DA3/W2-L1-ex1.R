# Adjusting for heterscedasticity through WLS

set.seed(5432)
n = 50
x1 = 10 + 3*rnorm(n) # x1 is normal
x2 = 2.5 + rnorm(n)    # x2 is normal, independent of x1
y = 10 + 0.5 * x1 - 2 *x2 + x2 * rnorm(n)

lm0 = lm(y~x1+x2)
# library(MASS)
par(mfrow=c(1,2))
plot(x1,stdres(lm0),ylab="Standardized residual")
title("OLS")
plot(x2,stdres(lm0),ylab="Standardized residual")
title("OLS")

par(mfrow=c(1,1))
e2 = stdres(lm0)^2
plot(x2,e2,ylab="Standardized residual squared")
lines(sort(x2),loess(e2[order(x2)]~sort(x2),span=2)$fit)

# Try WLS with weight proportional to 1/x2^2
lm1 <- lm(y~x1+x2,weights = 1/x2^2) 
par(mfrow=c(1,2))
plot(x1,stdres(lm1),ylab="Standardized residual")
title("WLS")
plot(x2,stdres(lm1),ylab="Standardized residual")
title("WLS")

# compare plots from the two models
plot(x2,stdres(lm0),ylab="Standardized residual")
title("OLS")
plot(x2,stdres(lm1),ylab="Standardized residual")
title("WLS")

# Compare regression summary
summary(lm0) # OLS
summary(lm1) # WLS

# estimated regression coefficients are similar in OLS and WLS
# Standard errors have reduced a bit
# Estimated residual standard error has reduced
# Multiple Rsq has improved

