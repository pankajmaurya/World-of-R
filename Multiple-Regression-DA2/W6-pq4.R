# Parsimony Quiz
rm(list=ls())
set.seed(1234)
n <- 10
beta0 <- 20
beta1 <- -1
beta2 <- -1
x1 <- rnorm(n,mean=1,sd=3)
x2 <- 0.999*x1 + rnorm(n, mean=1, sd=0.05)
err <- rnorm(n)
y <- beta0 + beta1 * x1 + beta2 * x2 + err

lm12 <- lm(y~x1+x2)
x1new <- rnorm(n,mean=1,sd=3)
x2new <- 0.999*x1new + rnorm(n, mean=1, sd=0.05)
err <- rnorm(n)
ynew <- beta0 + beta1 * x1new + beta2 * x2new + err
newdat = data.frame(x1 = x1new, x2 = x2new)
ynewoldfit <- predict(lm12,newdat,interval = "none")
prederror12 <- ynew - ynewoldfit
round(sqrt(mean(prederror12^2)) , 2)

# Model with x1 alone
lm1 <- lm(y~x1)
newdat = data.frame(x1 = x1new)
ynewoldfit <- predict(lm1,newdat,interval = "none")
prederror1 <- ynew - ynewoldfit
round(sqrt(mean(prederror1^2)) , 2)

######## With 0.8

lm12 <- lm(y~x1+x2)
x1new <- rnorm(n,mean=1,sd=3)
x2new <- 0.8*x1new + rnorm(n, mean=1, sd=0.05)
err <- rnorm(n)
ynew <- beta0 + beta1 * x1new + beta2 * x2new + err
newdat = data.frame(x1 = x1new, x2 = x2new)
ynewoldfit <- predict(lm12,newdat,interval = "none")
prederror12 <- ynew - ynewoldfit
round(sqrt(mean(prederror12^2)) , 2)

# Model with x1 alone
lm1 <- lm(y~x1)
newdat = data.frame(x1 = x1new)
ynewoldfit <- predict(lm1,newdat,interval = "none")
prederror1 <- ynew - ynewoldfit
round(sqrt(mean(prederror1^2)) , 2)
