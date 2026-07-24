# PRESS, predicted R square, AIC and BIC for synthetic data
library(MASS)
set.seed(1234)
n <- 10
beta0 <- 20
beta1 <- -1
beta2 <- -1
x1 <- rnorm(n,mean=1,sd=3)
x2 <- 0.999*x1 + rnorm(n, mean=1, sd=0.05)
err <- rnorm(n)
y <- beta0 + beta1 * x1 + beta2 * x2 + err

lm1 <-  lm(y~x1)
lm2 <-  lm(y~x2)
lm12 <- lm(y~x1+x2)

criter <- function(lmobj) {
  y <- lmobj$model[[1]]
  n <- dim(lmobj$model)[[1]]
  p <- dim(lmobj$model)[[2]]
  resids <- lmobj$res
  leverage <- hatvalues(lmobj)
  press <- sum((resids / (1 - leverage))^2)
  yvar <- sum((y - mean(y))^2) 
  predictedrsq <- 1 - press / yvar
  aic <- n * log (mean(resids^2) * 2*pi) + 2 * p + n
  bic <- n * log (mean(resids^2) * 2*pi) + log(n) * p + n
  return(c(press,predictedrsq,aic,bic))
}

# values of the criteria for the three models
crit <- cbind(criter(lm1), criter(lm2), criter(lm12))
crit <- as.data.frame(crit, row.names = 
                        c("PRESS", "Predicted R square", "AIC","BIC"))
colnames(crit) <- c("y~x1","y~x2","y~x1+x2")
crit
# All these criteria favour y~x2

# PQ 10
library(lmreg)
data("girlgrowth")
head(girlgrowth)
lmgirl12 <- lm(Height~Age+I(Age^2), data = girlgrowth)
lmgirl1 <- lm(Height~Age, data = girlgrowth)
lmgirl2 <- lm(Height~I(Age^2), data = girlgrowth)

crit <- cbind(criter(lmgirl1), criter(lmgirl2), criter(lmgirl12))
crit <- as.data.frame(crit, row.names = 
                        c("PRESS", "Predicted R square", "AIC","BIC"))
colnames(crit) <- c("y~x1","y~x2","y~x1+x2")
crit

# PRESS best = y~x1+x2, all other criteria too