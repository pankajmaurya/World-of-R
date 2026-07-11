# Predicting abdominal fat through waist circumference 
library(lmreg)
data(waist)
help(waist)

lm0 <- lm(AT~Waist,data=waist)
plot(waist)

# Box Cox transformation of response
boxcox(lm0,lambda = seq(-2, 2, 1/10), plotit = TRUE)
logAT = log(waist$AT)
plot(waist$Waist,logAT)
waist1 = waist$Waist
waist2 = waist$Waist^2
lm1 <- lm(logAT ~ waist1 + waist2)
summary(lm1)

sortW = sort(waist1)
newdat = data.frame(waist1=sortW, waist2=sortW^2)
predAT = predict(lm1,newdat,interval = "prediction", level = 0.95)
confAT = predict(lm1,newdat,interval = "confidence", level = 0.95)
plot(waist)
lines(sortW,exp(predAT[,1]),col=1)
lines(sortW,exp(predAT[,2]),col=3)
lines(sortW,exp(predAT[,3]),col=3)
lines(sortW,exp(confAT[,2]),col=2)
lines(sortW,exp(confAT[,3]),col=2)
legend("topleft",c("fit","confidence limits","prediction limits"),lty=c(1,1,1),col=c(1,2,3))
