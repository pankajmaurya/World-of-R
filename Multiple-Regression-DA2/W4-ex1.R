library(lmreg)
data("stars1")
help(stars1)

plot(stars1)
lmstars = lm(Velocity ~ Distance, data = stars1)
sortD = sort(stars1$Distance)
newdat = data.frame(Distance=sortD)
predstars = predict(lmstars,newdat,interval = "prediction", level = 0.95)
confstars = predict(lmstars,newdat,interval = "confidence", level = 0.95)
lines(sortD,predstars[,1],col=1)
lines(sortD,predstars[,2],col=3)
lines(sortD,predstars[,3],col=3)
lines(sortD,confstars[,2],col=2)
lines(sortD,confstars[,3],col=2)
legend("topleft",c("fit","confidence limits","prediction limits"),lty=c(1,1,1),col=c(1,2,3))
pred_se = predict(lmstars,newdat,se.fit=TRUE)$se.fit
pred_se
pred_sig = predict(lmstars,newdat,se.fit=TRUE)$resid
pred_sig  
sqrt(max(pred_se)^2 + pred_sig^2)


# Extrapolate
data(stars2)
sortD = sort(stars2$Distance)
newdat = data.frame(Distance=sortD)
predstars = predict(lmstars,newdat,interval = "prediction", level = 0.95)
confstars = predict(lmstars,newdat,interval = "confidence", level = 0.95)
plot(stars2)
lines(sortD,predstars[,1],col=1)
lines(sortD,predstars[,2],col=3)
lines(sortD,predstars[,3],col=3)
lines(sortD,confstars[,2],col=2)
lines(sortD,confstars[,3],col=2)
legend("topleft",c("fit","confidence limits","prediction limits"),lty=c(1,1,1),col=c(1,2,3))


