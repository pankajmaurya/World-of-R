library(readxl)
setwd("/Users/pankaj/TechCareer/World-of-R/Time-Series-Analysis-and-Forecasting")
my_data <- read_excel("shampoo.xlsx")
my_data
tseries <- ts(my_data$`Sales of shampoo`)
plot(tseries)

TS = tseries
library(forecast)
DES = holt(TS, h = 8)
summary(DES)
accuracy(DES)
plot(DES)
