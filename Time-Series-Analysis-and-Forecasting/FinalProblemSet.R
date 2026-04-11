rm(list=ls())
set.seed(126)

y <- c(1, 2, 5, 9, 3, 4)
# Convert to time series object
y_ts <- ts(y)
library(forecast)
fit <- Arima(y_ts, order = c(1, 0, 0))
summary(fit)

# Forecast now
fc <- forecast(fit, h = 5)
fc
plot(fc)

# Fit MA(1) and forecast

fit <- Arima(y_ts, order = c(0, 0, 1))
summary(fit)
fc <- forecast(fit, h = 5)
fc
plot(fc)

# Fit ARMA(1, 1) and forecast

fit <- Arima(y_ts, order = c(1, 0, 1))
summary(fit)
fc <- forecast(fit, h = 5)
fc
plot(fc)


###### Use of auto.arima

rm(list=ls())
set.seed(126)

y <- c(1, 2, 5, 9, 3, 4)
y_ts <- ts(y)

library(forecast)

# Automatically select ARIMA model
fit <- auto.arima(y_ts)

summary(fit)

# 5-step ahead forecast
fc <- forecast(fit, h = 5)
fc
plot(fc)
