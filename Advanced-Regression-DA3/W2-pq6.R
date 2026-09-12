# For q1.
#### PQ 2.6 Q 1
library(lmreg)
data("imf2015")
?imf2015
lm6 <- lm(UNMP~EXP+INFL+INV, data = imf2015)
summary(lm6)

y <- imf2015$UNMP
lm6fit <- lm6$fitted.values
plot(lm6fit, y ,xlab="fitted y",ylab="observed y")
lines(range(lm6fit),range(lm6fit),col=2)

# Plot standardized residual and its square against fitted values
plot(lm6fit,stdres(lm6),xlab="predicted",
     ylab="Standardized residual")
plot(lm6fit,(stdres(lm6))^2,xlab="predicted",
     ylab="Standardized residual squared",ylim=c(0,3))
# Run a smoother
rstsmo<-loess(((stdres(lm6))^2)[order(lm6fit)]~sort(lm6fit),span=1.4)
lines(sort(lm6fit),rstsmo$fit)


# Quadratic pattern, use log transformation of response
logy <- log(y)
lm62 <- lm(logy~EXP+INFL+INV, data = imf2015)
lm62fit <- lm62$fitted.values
plot(lm62fit,logy,xlab="predicted",ylab="Observed")
lines(range(lm62fit),range(lm62fit))
summary(lm62)
# here also we do not get great fit. checked the summary.

# Try reciprocal
yreci <- 1/y
lm6a <- lm(yreci~EXP+INFL+INV, data = imf2015)
lm6afit <- lm6a$fitted.values
plot(lm6afit,yreci,xlab="predicted",ylab="Observed")
lines(range(lm6afit),range(lm6afit))
summary(lm6a)
# Not great.

# Try square
ysq <- y^2
lm6b <- lm(ysq~EXP+INFL+INV, data = imf2015)
lm6bfit <- lm6b$fitted.values
plot(lm6bfit,ysq,xlab="predicted",ylab="Observed")
lines(range(lm6bfit),range(lm6bfit))
summary(lm6b)
# Bad bad fit.

# Try square root
ysqrt <- y^0.5
lm6c <- lm(ysqrt~EXP+INFL+INV, data = imf2015)
lm6cfit <- lm6c$fitted.values
plot(lm6cfit,ysqrt,xlab="predicted",ylab="Observed")
lines(range(lm6cfit),range(lm6cfit))
summary(lm6c)
# Good fit.



# Try nothing
library(lmreg)
data("imf2015")
?imf2015
lm6d <- lm(UNMP~EXP+INFL+INV, data = imf2015)
lm6dfit <- lm6c$fitted.values
summary(lm6d)

par(mfrow=c(2,2))

plot(lm6afit,stdres(lm6a),xlab="predicted reciprocal",
     ylab="Standardized residual")
plot(lm6bfit,stdres(lm6b),xlab="predicted sq",
     ylab="Standardized residual")
plot(lm6cfit,stdres(lm6c),xlab="predicted sqrt",
     ylab="Standardized residual")
plot(lm6dfit,stdres(lm6d),xlab="predicted",
     ylab="Standardized residual")
# Using the log transformation.

logy <- log(y)
lm62 <- lm(logy~EXP+INFL+INV, data = imf2015)
lm62fit <- lm62$fitted.values
plot(lm62fit,logy,xlab="predicted",ylab="Observed")
lines(range(lm62fit),range(lm62fit))
summary(lm62)

plot(lm6fit,stdres(lm62),xlab="predicted",
     ylab="Standardized residual")
par(mfrow=c(1,2))
plot(lm62fit,(stdres(lm62))^2,xlab="predicted",
     ylab="Standardized residual squared",ylim=c(0,3))
# Run a smoother
rstsmo62<-loess(((stdres(lm62))^2)[order(lm62fit)]~sort(lm62fit),span=1.4)
lines(sort(lm62fit),rstsmo62$fit)

# Now as per code given 
library(lmreg);  data("imf2015")

lunm = log(imf2015$UNMP)

lmun = lm(lunm~EXP+INFL+INV,data=imf2015)
lmunfit <- lmun$fitted.values

#plot(lmunfit,stdres(lmun),xlab="Predicted values",ylab="Standardized residual")
#abline(h=0)
plot(lmunfit,(stdres(lmun))^2,xlab="Predicted values",
     ylab="Standardized residual squared",ylim=c(0,3))
rstsmo<-loess(((stdres(lmun))^2)[order(lmunfit)]~sort(lmunfit),span=1.4)
lines(sort(lmunfit),rstsmo$fit)
