# Predicting abdominal fat after further transformation 
library(lmreg)
data(waist)

logAT = log(waist$AT)
plot(waist$Waist,logAT)
waist1 = waist$Waist

lambda <- (-340:200)/100 
Rsq <- NULL
for (lamb in lambda) {
  if (lamb==0) tw <- log(waist1) 
  else tw <- (waist1^lamb - 1) / lamb
  Rsq <- c(Rsq, summary(lm(logAT~tw))$r.sq)
}
plot(lambda,Rsq,type="l")

lambda[which(Rsq==max(Rsq))]

waist3 = waist$Waist^(-3)
y = logAT
x = waist3
lm2 <- lm(y ~ x)
summary(lm2)

# For lm2, what are the cross validation RMSEP and model based RMSEP?
n = length(waist$Waist)

CrossValSumSq = 0
for (i in 1:n) {
  beta0 = lm(y[-i]~x[-i])$coef[1]
  beta1 = lm(y[-i]~x[-i])$coef[2]
  CrossValSumSq = CrossValSumSq + ( y[i] - (beta0 + beta1 * x[i]) )^2 
}

RMSEPcrossval = sqrt(CrossValSumSq / n) 
round(RMSEPcrossval, 4)
k = 1
RMSEPmodelbased = sqrt((1 + (k+1)/n) * (summary(lm2)$sig^2)) 
round(RMSEPmodelbased, 4)
#sortW = sort(waist$Waist)
#sortW3 = (sortW)^(-3)
#newdat = data.frame(waist3=sortW3)
#predAT = predict(lm2,newdat,interval = "prediction", level = 0.95)
#confAT = predict(lm2,newdat,interval = "confidence", level = 0.95)
#plot(waist)
#lines(sortW,exp(predAT[,1]),col=1)
#lines(sortW,exp(predAT[,2]),col=3)
#lines(sortW,exp(predAT[,3]),col=3)
#lines(sortW,exp(confAT[,2]),col=2)
#lines(sortW,exp(confAT[,3]),col=2)
#legend("topleft",c("fit","confidence limits","prediction limits"),lty=c(1,1,1),col=c(1,2,3))