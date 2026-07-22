# Parsimony 
set.seed(1234)
n <- 10
beta0 <- 20
beta1 <- -1
beta2 <- -1
x1 <- rnorm(n,mean=1,sd=3)
x2 <- 0.999*x1 + rnorm(n, mean=1, sd=0.05)
err <- rnorm(n)
y <- beta0 + beta1 * x1 + beta2 * x2 + err
summary(lm(y~x1+x2))
# Estimated Î²1 and Î²2 are different from true values
# p-values of Î²1 and Î²2 are large
# Sign of Î²1 is incorrect
# x1 and x2 act as proxies of one another

summary(lm(y~x1))
summary(lm(y~x2))
# Signs of Î²1 and Î²2 are correct
# values make sense once proxies are recognized 

# Does collinearity hurt "out of sample" prediction?
lm12 <- lm(y~x1+x2)
x1new <- rnorm(n,mean=1,sd=3)
x2new <- 0.999*x1new + rnorm(n, mean=1, sd=0.05)
err <- rnorm(n)
ynew <- beta0 + beta1 * x1new + beta2 * x2new + err
newdat = data.frame(x1 = x1new, x2 = x2new)
ynewoldfit <- predict(lm12,newdat,interval = "none")
prederror12 <- ynew - ynewoldfit
sqrt(mean(prederror12^2)) 
# Not bad. Error variance is 1.
# collinearity does not hurt prediction in this example
# but wrong sign is difficult to explain
# Explanation will require proxies


# Model with x2 alone may not be bad for 
# out of sample prediction
lm2 <- lm(y~x2)
newdat = data.frame(x2 = x2new)
ynewoldfit <- predict(lm2,newdat,interval = "none")
prederror2 <- ynew - ynewoldfit
sqrt(mean(prederror2^2)) 
boxplot(prederror12,prederror2)
# Simpler but wrong model is interpretable
# Reasonable prediction



# Does collinearity hurt "out of sample" prediction?
# Try different combinations of x1 and x2
lm12 <- lm(y~x1+x2)
x1new <- rnorm(n,mean=1,sd=3)
x2new <- 0.8*x1new + rnorm(n, mean=1, sd=0.05)
err <- rnorm(n)
ynew <- beta0 + beta1 * x1new + beta2 * x2new + err
newdat = data.frame(x1 = x1new, x2 = x2new)
ynewoldfit <- predict(lm12,newdat,interval = "none")
prederror12 <- ynew - ynewoldfit
sqrt(mean(prederror12^2)) 
# Bad. Error variance is 1.
# Collinearity does hurt prediction in this example


# Model with x2 alone
lm2 <- lm(y~x2)
newdat = data.frame(x2 = x2new)
ynewoldfit <- predict(lm2,newdat,interval = "none")
prederror2 <- ynew - ynewoldfit
sqrt(mean(prederror2^2)) 
# Not bad. Error variance is 1.
boxplot(prederror12,prederror2)
# Simpler but wrong model is interpretable
# Reasonable prediction
