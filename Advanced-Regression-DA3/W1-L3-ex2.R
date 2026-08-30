# Dealing with nonlinearity, simulated data examples
rm(list=ls())
set.seed(9999); n <- 20
x1 = 10 + 2*rnorm(n)
x2 = 20 - x1 + 3*rnorm(n)
x3 = 0.5 * (x1 + x2) + 3*rnorm(n)
y = 5 + 1 * x1 + 1 * x2 + 0.1 * x3^2 + rnorm(n)

lm1 = lm(y~x1+x2+x3)

#library(car)
crPlots(lm1,smooth=F)
avPlots(lm1)

# Include Polynomial in x3 in lieu of x3
lm3 = lm(y~x1+x2+poly(x3, degree=3, raw=TRUE))
summary(lm3)

lm2 = lm(y~x1+x2+poly(x3, degree=2, raw=TRUE))
summary(lm2)

# Drop the linear term
x3sq = x3^2
lm4 = lm(y~x1+x2+x3sq)
summary(lm4)

# Check CR and AV plots again
crPlots(lm4,smooth=F)
avPlots(lm4)

# Optimal Box-Tidwell transformation
lambda <- seq(-2,4,.05)
Rsq <- NULL
for (lamb in lambda) {
  tx3 <- (x3^lamb - 1) / lamb
  if (lamb==0) tx3 <- log(x3)
  Rsq <- c(Rsq, summary(lm(y~x1+x2+tx3))$r.sq)
}
plot(lambda,Rsq,type="l")
# Highest value is achieved when lambda is about 2 (correct power)