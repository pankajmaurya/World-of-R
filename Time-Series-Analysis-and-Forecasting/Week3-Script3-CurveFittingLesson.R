Data = data.frame(x = 1:15, y = c(5, 15, 26, 27, 23, 16, 12, 9, 13, 18, 19, 24, 34, 42, 52))
plot(Data$x, Data$y, xlab = 'Time', ylab = 'y')

fit1 = lm(y~x, data = Data)
fit2 = lm(y ~ poly(x, degree = 2, raw = TRUE), data = Data)
fit3 = lm(y ~ poly(x, degree = 3, raw = TRUE), data = Data)
fit4 = lm(y ~ poly(x, degree = 4, raw = TRUE), data = Data)
fit5 = lm(y ~ poly(x, degree = 5, raw = TRUE), data = Data)

plot(Data$x, Data$y, pch = 19, xlab = 'Time', ylab = 'y')
X = 1:15
lines(X, predict(fit1), col = 'red')
lines(X, predict(fit2), col = 'blue')
lines(X, predict(fit3), col = 'purple')
lines(X, predict(fit4), col = 'orange')
lines(X, predict(fit5), col = 'green')

summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared

# Best Rsquared metric was observed for fit4
plot(Data$x, Data$y, pch = 19, xlab='Time', ylab='y')
lines(X, predict(fit4), col='orange')

summary(fit4)$coeff
