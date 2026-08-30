# Detecting heterscedasticity: Simulated data example

set.seed(5432)
n = 50
x1 = 10 + 3*rnorm(n)
x2 = 2.5 + rnorm(n)
y = 10 + 0.5 * x1 - 2 *x2 + x2 * rnorm(n)
# x1 and x2 are normal
# Errors have stdev proportional to x1

lm0 = lm(y~x1+x2)

# Get the std resid vs regressor plots
library(MASS)
par(mfrow=c(1,2))
plot(x1,stdres(lm0),ylab="Standardized residual")
plot(x2,stdres(lm0),ylab="Standardized residual")

# Examine dependence of squared residuals on x2
par(mfrow=c(1,1))
e2 = stdres(lm0)^2
plot(x2,e2,ylab="Standardized residual squared")
lines(sort(x2),loess(e2[order(x2)]~sort(x2),span=2)$fit)
