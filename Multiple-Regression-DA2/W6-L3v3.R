# R square, adjusted R square and Cp for synthetic data
set.seed(1234)
n <- 10
beta0 <- 20
beta1 <- -1
beta2 <- -1
x1 <- rnorm(n,mean=1,sd=3)
x2 <- 0.999*x1 + rnorm(n, mean=1, sd=0.05)
err <- rnorm(n)
y <- beta0 + beta1 * x1 + beta2 * x2 + err

lm1 <-  lm(y~x1)      # Regressor: "x1 only"
lm2 <-  lm(y~x2)      # Regressor: "x2 only"
lm12 <- lm(y~x1+x2)   # Regressor: "x1 and x2"
sighat <- summary(lm12)$sigma
criteria <- function(lmobj,sig) {
  M <- summary(lmobj)
  rsq <- M$r.sq
  arsq <- M$adj.r.squared
  cp <- M$df[2]*(M$sigma / sig)^2 - M$df[2] + M$df[1]
  return(c(rsq,arsq,cp))
}

# values of the criteria for the three models
criteria(lm1,sighat)
crit <- cbind(criteria(lm1,sighat),
              criteria(lm2,sighat),
              criteria(lm12,sighat))
crit <- as.data.frame(crit, row.names = c("R square", "Adjusted R square","Cp"))
colnames(crit) <- c("x1 only","x2 only","x1 and x2")
crit
# R square and Adjusted R square favour y~x12 
# y~x2 is not far behind in Adjusted R square
# Cp clearly favours y~x2


# PQ 7
library(lmreg)
data("girlgrowth")
lmgirl12 <- lm(Height~Age+I(Age^2), data=girlgrowth)
lmgirl1 <- lm(Height~Age, data=girlgrowth)
lmgirl2 <- lm(Height~I(Age^2), data = girlgrowth)
sighat <- summary(lmgirl12)$sigma
crit <- cbind(criteria(lmgirl1,sighat),
              criteria(lmgirl2,sighat),
              criteria(lmgirl12,sighat))
crit <- as.data.frame(crit, row.names = c("R square", "Adjusted R square","Cp"))
colnames(crit) <- c("x1 only","x2 only","x1 and x2")
crit
