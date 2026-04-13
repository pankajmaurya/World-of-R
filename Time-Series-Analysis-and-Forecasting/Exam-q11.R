rm(list=ls())
set.seed(126)
library(forecast)

y <- c(14,32,70,121,190,270,375,511,674,866,1082,1328,1602,1906,2232)
y_ts <- ts(y)
plot(y_ts)

# Find the p-value of ADF test for the above data set and draw your conclusion. (2 points).
library(tseries)
adf.test(y)
# p value is 0.98189 > 0.05 => accept null hypothesis that data is not stationary.

#Fit a linear and quadratic trend on the generated data and calculate the residual sum of squares and comment on which one is better. (2 points)

N <- 15
t <- 1:N
linear_model <- lm(y ~ t)
summary(linear_model)
sum(residuals(linear_model)^2)

quadratic_model <- lm(y ~ t + I(t^2))
summary(quadratic_model)
sum(residuals(quadratic_model)^2)

# Quadratic fit has better adjusted R squared (0.9995 as compared to 0.906) value.
# Also, we see that the residual sum of squares for linear = 644323.5 compared to 3459.786 for quadratic
# RSE is also much smaller: quadratic -> (16.98 on 12 degrees of freedom) vs linear -> (222.6 on 13 degrees of freedom)

# Find the forecast values using the above fitted trend equations for t=16,17,18,19,20 (2 points)

# My linear trend equation is y = -489.2 + 155.1 * t
# My quadratic trend equation is y = 75.8484 -44.3328 * t + 12.4639 * t^2
forecast_t <- c(16, 17, 18, 19, 20)
linear_forecasts = numeric(5)
quadratic_forecasts = numeric(5)
for (t in forecast_t) {
  linear_forecasts[t - 15] = -489.2 + 155.1 * t
  quadratic_forecasts[t - 15] = 75.8484 -44.3328 * t + 12.4639 * t^2
}
print(linear_forecasts)
print(quadratic_forecasts)
# > print(linear_forecasts) 
# [1] 1992.4 2147.5 2302.6 2457.7 2612.8
#> print(quadratic_forecasts)
# [1] 2557.282 2924.258 3316.162 3732.993 4174.752

##### Part 4
y_true <- c(2592, 2987, 3411, 3852, 4320)
squared_errors_linear <- sum((y_true - linear_forecasts)^2)
squared_errors_quadratic <- sum((y_true - quadratic_forecasts)^2)

if (squared_errors_linear < squared_errors_quadratic) {
  print("Linear model better")
} else {
  print("Quadratic model better")
}

# [1] "Quadratic model better"

###### Part 5 
residuals_linear <- residuals(linear_model)
adf.test(residuals_linear)

# We see p-value = 0.9818 > 0.05. We accept null hypothesis that residuals are NOT stationary.

##### Part 6
# Fit a suitable ARIMA model on the above residuals and report the values of the coefficient parameters and order of the ARIMA process. (2 points)
fit_arima_residuals <- auto.arima(residuals_linear)
summary(fit_arima_residuals)

# We got a ARIMA(1,0,0) with phi1 = 0.9466

###### Part 7
# Forecast value of residuals
# For AR(1) we can calculate our forecast as phi^h * yt
e15 <- tail(residuals_linear, 1)
phi <- 0.9466
res_forecast <- numeric(5)
for (t in forecast_t) {
  res_forecast[t - 15] <- (phi^(t - 15)) * e15
}

print(res_forecast)
# [1] 373.7571 353.7985 334.9057 317.0217 300.0927

#### Part 8
# How to improve forecast. We can simply add the residual to the linear trend estimation
# y = y_estimated_via_linear_model + residual
updated_linear_forecasts <- numeric(5)
for (t in 1:5) {
  updated_linear_forecasts[t] = linear_forecasts[t] + res_forecast[t]
}
print(updated_linear_forecasts)
# [1] 2366.157 2501.298 2637.506 2774.722 2912.893

# Updated error
updated_linear_err = sum((updated_linear_forecasts - y_true)^2)
print(updated_linear_err)
# 4025684
print(squared_errors_linear)
# 7151435
print(squared_errors_quadratic)
# 49395.74

### Part 9: Fit ARIMA(1, 1, 1), forecast yt and also compute forecast error. compare with errors at 8.
fit_111 <- arima(y_ts, order = c(1,1,1))
# Gives error
# Error in stats::arima(x = x, order = order, seasonal = seasonal, xreg = xreg,  : 
# non-stationary AR part from CSS

# Alternatively I try to fit auto arima now
fit_auto <- auto.arima(y_ts)
summary(fit_auto)
# This gives ARIMA(1, 2, 0) with AR1 coefficient 0.9579
auto_forecasts <- forecast(fit_auto, h = 5)
auto_forecasts
y_hat <- c(2579.075, 2946.337, 3332.938, 3738.064, 4160.936)
squared_auto_errors <- sum((y_true - y_hat)^2)
print(squared_auto_errors)
# 46196.98 # This is better than even the quadratic model!

#### Part 10
#Compute the forecast error using SES, DES and TES and compare it with forecast errors at 9.
fit_ses <- ses(y_ts, h = 5)
fit_des <- holt(y_ts, h = 5)

err_ses <- y_true - fit_ses$mean
err_des <- y_true - fit_des$mean

print(err_ses)
print(sum(err_ses^2))

print(err_des)
print(sum(err_des^2))

# SES error squared is 9074202, DES err squared is 361079. These are higher than step 9.

# I was unsure about the seasonal aspect of the data. As there is no seasonality for HW