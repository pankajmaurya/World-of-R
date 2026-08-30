library(alr4)
data("wblake")

lmfish <- lm(Age~Length + Scale, data = wblake)
library(car)
crPlots(lmfish,smooth=T) # Component plus residual plot

# Optimal Box-Tidwell transformation
lambda <- seq(-2,4,.05)
Rsq <- NULL
for (lamb in lambda) {
  tLength <- (wblake$Length^lamb - 1) / lamb
  if (lamb==0) tLength <- log(wblake$Length)
  Rsq <- c(Rsq, summary(lm(wblake$Age~tLength+wblake$Scale))$r.sq)
}
plot(lambda,Rsq,type="l")

max(Rsq)
lambda[which.max(Rsq)]
# Max is for lambda = 1

best_lambda <- lambda[which.max(Rsq)]
plot(lambda, Rsq, type = "l",
     xlim = c(best_lambda - 1, best_lambda + 1),  # zoom window around the peak
     xlab = expression(lambda), ylab = expression(R^2))
abline(v = best_lambda, col = "red", lty = 2)      # mark the optimal lambda

rm(list=ls())
library(alr4)
data("MinnWater")
?MinnWater
lmwater <- lm(muniUse~muniPrecip+muniPop+year, data=MinnWater)
par(mfrow=c(1,3))
plot(MinnWater$muniPrecip, stdres(lmwater))
plot(MinnWater$muniPop, stdres(lmwater))
plot(MinnWater$year, stdres(lmwater))


par(mfrow=c(1,3))
plot(MinnWater$muniPrecip, stdres(lmwater)^2)
plot(MinnWater$muniPop, stdres(lmwater)^2)
plot(MinnWater$year, stdres(lmwater)^2)

x1=MinnWater$muniPrecip
x2=MinnWater$muniPop
x3=MinnWater$year

par(mfrow=c(1,3))
x = x1

e2=stdres(lmwater)^2
plot(x,e2,ylab="Standardized residual squared")
lines(sort(x),loess(e2[order(x)]~sort(x),span=2)$fit)

x = x2
e2=stdres(lmwater)^2
plot(x,e2,ylab="Standardized residual squared")
lines(sort(x),loess(e2[order(x)]~sort(x),span=2)$fit)

x = x3
e2=stdres(lmwater)^2
plot(x,e2,ylab="Standardized residual squared")
lines(sort(x),loess(e2[order(x)]~sort(x),span=2)$fit)
