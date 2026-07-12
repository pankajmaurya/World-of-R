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
lm2 <- lm(logAT ~ waist3)
summary(lm2)

sortW = sort(waist$Waist)
sortW3 = (sortW)^(-3)
newdat = data.frame(waist3=sortW3)
predAT = predict(lm2,newdat,interval = "prediction", level = 0.95)
confAT = predict(lm2,newdat,interval = "confidence", level = 0.95)
plot(waist)
lines(sortW,exp(predAT[,1]),col=1)
lines(sortW,exp(predAT[,2]),col=3)
lines(sortW,exp(predAT[,3]),col=3)
lines(sortW,exp(confAT[,2]),col=2)
lines(sortW,exp(confAT[,3]),col=2)
legend("topleft",c("fit","confidence limits","prediction limits"),lty=c(1,1,1),col=c(1,2,3))