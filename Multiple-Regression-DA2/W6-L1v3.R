rm(list=ls())
# Hazard from useful regressors 
set.seed(1234)
n <- 10
beta0 <- 20
beta1 <- -1
beta2 <- -1
x1 <- rnorm(n,mean=1,sd=3)
x2 <- 0.999*x1 + rnorm(n, mean=1, sd=0.05)
err <- rnorm(n)
y <- beta0 + beta1 * x1 + beta2 * x2 + err
cor(x1,x2)
summary(lm(y~x1+x2))
# Estimated Î²1 and Î²2 are different from true values
# p-values of Î²1 and Î²2 are large
# Sign of Î²1 is incorrect
# x1 and x2 act as proxies of one another

summary(lm(y~x1))
summary(lm(y~x2))
# Signs of Î²1 and Î²2 are correct
# values make sense once proxies are recognized 
