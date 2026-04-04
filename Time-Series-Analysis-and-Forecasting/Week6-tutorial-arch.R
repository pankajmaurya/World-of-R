rm(list=ls())
set.seed(126)

library(TSA)
library(forecast)
library(rugarch)

N <- 500
beta0 <- 3
beta1 <- 0.8

noise <- garch.sim(alpha = c(beta0, beta1), beta = 0, n = N, rnd = rnorm)
alpha <- 5
phi <- 0.1

Y = numeric(N)
Y[1] = alpha + noise[1]
for (i in 2:N) {
  Y[i] = alpha + phi * Y[i-1] * noise[i]
}

plot.ts(Y)
plot.ts(noise)

arima_model <- auto.arima(Y)
summary(arima_model)

acf(Y)
pacf(Y)

library(tseries)
adf.test(Y)
# p value gt 0.05 means non-stationarity.


F <- forecast(arima_model, h = 100)
plot(F)

arima_residual <- arima_model$residuals
plot.ts(arima_residual)

par(mfrow=c(1,2))
acf(arima_residual, plot = TRUE)
pacf(arima_residual, plot = TRUE)

# Lets check e_t^2 acf, pacf plots now
par(mfrow=c(1,2))
acf(arima_residual^2, plot = TRUE)
pacf(arima_residual^2, plot = TRUE)

# Significant values for h = 1 indicates some AR like structure on the residual square
# which was not there on the residuals themselves.


library(FinTS)
ArchTest(arima_residual)

# pvalue is almost 0, reject null hypothesis. 

library(fGarch)
arch.fit <- garchFit(~garch(1, 0), data = arima_residual, trace = T)
summary(arch.fit)


cond.var <- arch.fit@h.t
plot.ts(cond.var)
