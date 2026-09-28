# Bootstrap in fish data - prediction intervals
library(alr4)
data("wblake")
Lensq <- wblake$Length^2
lmfish <- lm(Age ~ Length + Lensq, data = wblake)
n <- length(wblake$Age)

# Model based Bootstrap for prediction
xset <- seq(min(wblake$Length), 
            max(wblake$Length), length.out = 100)
newdat <- data.frame(Length = xset, Lensq = xset^2)

# Bootstrap prediction limits
respool = lmfish$residuals
set.seed(1234); nboot <- 1000;  predlist <- NULL
uplim <- NULL; lolim <- NULL
for (i in 1:nboot) {
  yb <- lmfish$fitted.values +
    sample(respool, size = length(wblake$Age), replace = T)
  mboot <- lm(yb ~ Length + Lensq, data = wblake)
  bootfit <- predict(mboot, newdat, interval = "none")
  predlist <- rbind(predlist, bootfit
                    + sample(respool, size = length(xset), replace = T))
}
for (j in 1:length(xset)) {
  uplim <- c(uplim, quantile(predlist[,j],probs = 0.975))
  lolim <- c(lolim, quantile(predlist[,j],probs = 0.025))
}

# Normal theory prediction limits
pred <- predict(lmfish, newdat, interval = "prediction")

# Comparative graph
matplot(xset, cbind(pred,uplim,lolim),
        type="l", lty=c(1,1,1,1,1), col=c(1,2,2,4,4), 
        xlab="Length", ylab="Age")
points(wblake$Length,wblake$Age)
legend("topleft",c("Fitted line","95% Normal prediction limits",
                   "95% BS prediction limits","Observed data"),
       lty=c(1,1,1,0), col=c(1,2,4,1), pch=c(NA,NA,NA,1), cex=.7)

