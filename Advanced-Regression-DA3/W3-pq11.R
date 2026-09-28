library(bnlearn); data(marks)

lmarks = lm(ANL~ALG+STAT, data = marks)

respool = lmarks$residuals;  n <- length(marks$ANL)

set.seed(1234); nboot <- 1000;  anlm <- NULL

for (i in 1:nboot) {
  
  yb <- lmarks$fitted.values +
    
    sample(respool, size = n, replace = T)
  
  mboot <- lm(yb ~ ALG + STAT, data = marks)
  
  anlm <- c(anlm, 
            
            mboot$coef[1] + mboot$coef[2]*50  + mboot$coef[3]*40 
            
            + sample(respool, size = 1))
  
}

round(quantile(anlm,probs = 0.95))  

lmmarks = lmarks
# Now using a parametric bootstrap.
respool = lmmarks$residuals
x1 <- marks$ALG
x2 <- marks$STAT
n <- length(x1)
newdat = data.frame(x1 = c(50), x2 = c(40))
set.seed(1234); nboot <- 1000;  beta <- NULL; predlist <- NULL

for (i in 1:nboot) {
  yb <- lmmarks$fitted.values +
    sample(respool, size = n, replace = T)
  mboot <- lm(yb ~ x1 + x2)
  bootfit <- predict(mboot, newdat, interval = "none")
  beta <- rbind(beta, mboot$coefficients)
  predlist <- rbind(predlist, bootfit
                    + sample(respool, size = 1, replace = T))
}
quantile(predlist[,1], probs = 0.975)
round(quantile(predlist[,1], probs = 0.95))
