rm(list=ls())
# Computational example of variance stabilizing transformation
#library(lmreg)
data(waist)
head(waist)
lm0 <- lm(AT~Waist,data=waist)
lm0fit <- lm0$fitted.values
plot(lm0fit,waist$AT,xlab="AT predicted from waist",ylab="Observed AT")
lines(range(lm0fit),range(lm0fit),col=2)
# library(MASS)
# Plot standardized residual and its square against fitted values
plot(lm0fit,stdres(lm0),xlab="AT predicted from waist",
     ylab="Standardized residual")
plot(lm0fit,(stdres(lm0))^2,xlab="AT predicted from waist",
     ylab="Standardized residual squared",ylim=c(0,3))
# Run a smoother
rstsmo<-loess(((stdres(lm0))^2)[order(lm0fit)]~sort(lm0fit),span=1.4)
lines(sort(lm0fit),rstsmo$fit)

# Quadratic pattern, use log transformation of response
logAT <- log(waist$AT)
lm1 <- lm(logAT~Waist,data=waist)
lm1fit <- lm1$fitted.values
plot(lm1fit,logAT,xlab="log(AT) predicted from waist",
     ylab="Observed log(AT)")
lines(range(lm1fit),range(lm1fit))

# Try log transformation of predictor
logwaist <- log(waist$Waist)
lm2 <- lm(logAT~logwaist)
lm2fit <- lm2$fitted.values
plot(lm2fit,logAT,xlab="log(AT) predicted from log(waist)",
     ylab="Observed log(AT)")
lines(range(lm2fit),range(lm2fit))
plot(lm2fit,stdres(lm2),xlab="log(AT) predicted from log(waist)",
     ylab="Standardized residual")

# Find best Box-Tidwell transformation
lambda <- seq(-3,3,.05)
Rsq <- NULL
for (lamb in lambda) {
  twaist <- (waist$Waist^lamb - 1) / lamb
  if (lamb==0) twaist <- log(waist$Waist)
  Rsq <- c(Rsq, summary(lm(logAT~twaist))$r.sq)
}
plot(lambda,Rsq,type="l")

# Apply power -3
twaist <- 1/(waist$Waist)^3
lm3 <- lm(logAT~twaist)
lm3fit <- lm3$fitted.values
plot(lm3fit,logAT,xlab="log(AT) predicted from 1/(waist)^3",
     ylab="Observed log(AT)")
lines(range(lm3fit),range(lm3fit))
plot(lm3fit,stdres(lm3),xlab="log(AT) predicted from 1/(waist)^3",
     ylab="Standardized residual")


