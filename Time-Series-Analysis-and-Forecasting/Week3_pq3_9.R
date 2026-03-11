library(readxl)
setwd("/Users/pankaj/TechCareer/World-of-R/Time-Series-Analysis-and-Forecasting")
my_data <- read_excel("number_of_acres.xlsx")
my_data

# Create time series
tseries <- ts(my_data$`Number of acres burned in forest fires in Canada`,
              start = min(my_data$Year),
              frequency = 1)
plot(tseries)

TS = tseries

SES = HoltWinters(TS, alpha = NULL, beta = FALSE, gamma = FALSE)
plot(SES)
round(SES$alpha, 3)

library(forecast)
Forecast_SES = forecast::forecast(SES, h = 1)
plot(Forecast_SES)
