rm(list=ls())
set.seed(126)

y <- c(1, 3, 5, -2, 0, 2, 4)
# Convert to time series object
y_ts <- ts(y)

# Fit AR(1)
fit <- arima(y_ts, order=c(1, 0, 0))
pred <- predict(fit, n.ahead = 5)
pred$pred
plot(y_ts)
pred$se


# Using the forecast package now.

library(forecast)
fit <- Arima(y, order = c(1, 0, 0))
summary(fit)

# Forecast now
fc <- forecast(fit, h = 5)
fc
plot(fc)
