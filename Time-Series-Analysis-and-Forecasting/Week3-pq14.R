library(readxl)
setwd("/Users/pankaj/TechCareer/World-of-R/Time-Series-Analysis-and-Forecasting")
my_data <- read_excel("passengers.xlsx")
my_data
plot(my_data)
tseries = ts(my_data$`Number of Passengers`, start = c(1949, 1),
             frequency = 12)
plot(tseries)

HW = hw(tseries, seasonal = "multiplicative", h = 21)
plot(HW)
accuracy(HW)
summary(HW)
