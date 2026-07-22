rm(list=ls())
# Problem of too many regressors
set.seed(1234)
n <- 10
beta0 <- 0
beta1 <- -2
x <- rnorm(n,mean=1,sd=3)
err <- rnorm(n)
y <- beta0 + beta1 * x + err

lm1 = lm(y~x)
summary(lm1)
# estimated sigma is 1.115, R-squared is 0.9717 
xset = seq(from = min(x), to = max(x), length.out = 100)
yfitNice = lm1$coef[1]
for (i in 1:1) yfitNice = yfitNice + lm1$coef[i+1] * xset^i

polyDegree <- 5
lmOverFit <- lm(y~poly(x,polyDegree,raw=T))
summary(lmOverFit)
# estimated sigma is 0.1468, R-squared is 0.9999
# effect of linear term is not significant

par(mfrow=c(1,2))
plot(x,y)
abline(lm1, col = "blue")

yfit = lmOverFit$coef[1]
for (i in 1:polyDegree) yfit = yfit + lmOverFit$coef[i+1] * xset^i
lines(xset, yfit, col = "red")
# prediction in between observed x values is poor

round(max(abs(yfit - yfitNice)), 1)

# zoom out
plot(x, y, ylim = range(yfit) )
abline(lm1, col = "blue")
lines(xset, yfit, col = "red")
# prediction in between observed x values is very poor
# crowd of predictors hurts prediction