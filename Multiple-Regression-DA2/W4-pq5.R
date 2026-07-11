rm(list=ls())

# Predicting abdominal fat through waist circumference 
library(lmreg)
data(waist)
help(waist)

lm0 <- lm(AT~Waist,data=waist)
plot(waist)

# Box Cox transformation of response
boxcox(lm0,lambda = seq(-2, 2, 1/10), plotit = TRUE)

# Instead of log transformation, we can do the cube root transformation
cuberootAT2 = ((waist$AT)^(1/3) - 1) * 3
plot(waist$Waist,cuberootAT2)
waist1 = waist$Waist
waist2 = waist$Waist^2
lm1 <- lm(cuberootAT2 ~ waist1 + waist2)
summary(lm1)

sortW = sort(waist1)
newdat = data.frame(waist1=sortW, waist2=sortW^2)
predAT = predict(lm1,newdat,interval = "prediction", level = 0.95)
confAT = predict(lm1,newdat,interval = "confidence", level = 0.95)
plot(waist)
lines(sortW,((1 + (predAT[,1]) / 3))^3,col=1)
lines(sortW,((1 + (predAT[,2]) / 3))^3,col=3)
lines(sortW,((1 + (predAT[,3]) / 3))^3,col=3)
lines(sortW,((1 + (confAT[,2]) / 3))^3,col=2)
lines(sortW,((1 + (confAT[,3]) / 3))^3,col=2)

legend("topleft",c("fit","confidence limits","prediction limits"),lty=c(1,1,1),col=c(1,2,3))
# 0.7568 with transformation.

############### Just the cube root transformation #########
data(waist); AT13=(waist$AT)^(1/3)

waist1 = waist$Waist; waist2 = waist1^2

lm2 = lm(AT13 ~ waist1+waist2); 

summary(lm2)

sortw = sort(waist1)

newdat = data.frame(waist1=sortw, waist2=sortw^2)

predAT = predict(lm2, newdat, interval="prediction", level=0.95)

confAT = predict(lm2, newdat, interval="confidence", level=0.95)

plot(waist); 

lines(sortW,(predAT[,1])^3,col=1)
lines(sortW,(predAT[,2])^3,col=3)
lines(sortW,(predAT[,3])^3,col=3)
lines(sortW,(confAT[,2])^3,col=2)
lines(sortW,(confAT[,3])^3,col=2)

legend("topleft",c("fit","confidence limits","prediction limits"),lty=c(1,1,1),col=c(1,2,3))

