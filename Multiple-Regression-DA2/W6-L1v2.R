rm(list=ls())
# Problem of too few regressors 
set.seed(1234)
n <- 10
beta0 <- 20
beta1 <- -1
beta2 <- -1
x1 <- rnorm(n,mean=1,sd=3)
x2 <- 0.5*x1 + rnorm(n, mean=1, sd=1.5)
err <- rnorm(n)
y <- beta0 + beta1 * x1 + beta2 * x2 + err
summary(lm(y~x1+x2))
#  estimated values of Î²1 and Î²2 are close to true values
# p-values of Î²1 and Î²2 are small

summary(lm(y~x1))
# Î²1 = -1.5293, Î²2 = 0
# R-square is much smaller than .99

summary(lm(y~x2))
# Î²1 = 0, Î²2 = -2.1299
# R-square is much smaller than .99
# Estimated Î²1 and Î²2 are different from true values