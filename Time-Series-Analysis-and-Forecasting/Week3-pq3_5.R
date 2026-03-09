library(readxl)
setwd("/Users/pankaj/TechCareer/World-of-R/Time-Series-Analysis-and-Forecasting")
my_data <- read_excel("pq3_5.xlsx")
my_data

# Create time series using only Sales column
tseries <- ts(my_data$`Sales (in thousands)`,
              start = min(my_data$Year),
              frequency = 1)

# Plot
plot(tseries)

# View
tseries

library(forecast)

a <- ma(tseries, order = 6, centre = TRUE)
value_2014 <- window(a, start = 2014, end = 2014)

ans <- round(value_2014, 2)
ans
