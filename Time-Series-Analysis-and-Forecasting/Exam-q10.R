rm(list=ls())
set.seed(126)

y <- c(1, 3, 5, 7, 10)
# Convert to time series object
y_ts <- ts(y)
library(forecast)
fit <- Arima(y_ts, order = c(1, 0, 0))
summary(fit)

# Forecast now
fc <- forecast(fit, h = 5)
fc
plot(fc)

# Answer is 6.93