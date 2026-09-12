# Issues with WLS: Simulated data example

set.seed(5432)
n = 50
x1 = 10 + 3*rnorm(n) # x1 is normal
x2 = 2.5 + rnorm(n)    # x2 is normal, independent of x1
y = 10 + 0.5 * x1 - 2 *x2 + x2 * rnorm(n)

# OLS and WLS fits
lm0 = lm(y~x1+x2) # OLS
lm1 <- lm(y~x1+x2,weights = 1/x2^2) # WLS

# Regression summaries
summary(lm0)
summary(lm1) 

# OLS rsq : 0.5329226
summary(lm0)$r.sq
# WLS rsq : 0.7044686 (much higher and misleading)
summary(lm1)$r.sq

# R square proxies for WLS
cor(lm1$fitted.values,y)^2
# gave 0.5313852 which is very close to summary(lm0)$r.sq
1 - sum((y - lm1$fitted.values)^2)/sum((y - mean(y))^2) 
# again value here was 0.5268978 which is 

# R square and proxies for OLS : all the 3 values are the same here for OLS which is WLS with no weights!
summary(lm0)$r.square
cor(lm0$fitted.values,y)^2
1 - sum((y - lm0$fitted.values)^2)/sum((y - mean(y))^2) 



# Prediction error

pred0 = predict(lm0,interval = "prediction", level = 0.95)
pred1 = predict(lm1,interval = "prediction", level = 0.95)
# Now plot the widths of the 95% prediction intervals
pred0wd = pred0[,3] - pred0[,2]
pred1wd = pred1[,3] - pred1[,2]
par(mfrow=c(1,1))
plot(x2,pred1wd,ylab = "Width of prediction interval")
points(x2,pred0wd, col = 2)
legend("bottomright",c("OLS","WLS"),lty=c(0,0),pch=c(1,1),col=c(2,1) )



library(lmreg)
data("waist")
?waist
lmwaist <- lm(AT~Waist, weights = 1/Waist^5, data = waist)
lmwaist0 <- lm(AT~Waist, data = waist)
rsq <- summary(lmwaist0)$r.sq
r1sq <- cor(lmwaist$fitted.values,waist$AT)^2

y <- waist$AT
r2sq <- 1 - sum((y - lmwaist$fitted.values)^2)/sum((y - mean(y))^2) 
round(rsq, 4)
round(r1sq, 4)
round(r2sq, 4)
