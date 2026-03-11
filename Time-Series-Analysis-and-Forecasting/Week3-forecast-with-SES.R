# Read file
setwd("/Users/pankaj/TechCareer/World-of-R/Time-Series-Analysis-and-Forecasting")

nifty <- read.csv("nifty.csv")

# Inspect
head(nifty)
names(nifty) <- make.names(names(nifty))

nifty$Date <- as.Date(nifty$Date, format="%d-%b-%Y")

nifty <- nifty[order(nifty$Date), ]

nifty$t <- 1:nrow(nifty)

plot(nifty$t, nifty$Close, pch=19,
     xlab="Time", ylab="Nifty Close")

TS = ts(nifty$Close, nifty$Date)
plot(TS)

SES = HoltWinters(TS, alpha = NULL, beta = FALSE, gamma = FALSE)
plot(SES)

library(forecast)
Forecast_SES = forecast::forecast(SES, h = 3)
plot(Forecast_SES)
