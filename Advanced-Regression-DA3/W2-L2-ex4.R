# Computational example of prediction with transformed response
library(lmreg)
data(waist)
# Fit simple linear regression model
lm0 = lm(AT~Waist,data=waist)
# Fit linear regression model after transformations
# to adjust for heteroscedasticity and nonlinearity
logAT = log(waist$AT)
twaist = 1/(waist$Waist)^3
lm3 = lm(logAT~twaist)

# prediction and prediction intervals from 2 models
# Original
par(mfrow=c(1,2))
waistrange = seq(min(waist), max(waist), length.out = 101)
newdat = data.frame(Waist = waistrange)
predold = predict(lm0,newdat,interval="prediction")
plot(waist$Waist,waist$AT,xlab="waist",ylab="AT")
lines(waistrange,predold[,1],col=4)
lines(waistrange,predold[,2],col=2)
lines(waistrange,predold[,3],col=2)
title("Prediction w/o transformation")

# Transformed
wrange = seq(max(twaist), min(twaist), length.out = 101)
newwaist = data.frame(twaist = wrange)
prednew = predict(lm3,newwaist,interval="prediction")
plot(waist$Waist,waist$AT,xlab="waist",ylab="AT")
lines((wrange)^(-1/3),exp(prednew[,1]),col=4)
lines((wrange)^(-1/3),exp(prednew[,2]),col=2)
lines((wrange)^(-1/3),exp(prednew[,3]),col=2)
title("Prediction w/ transformation")

