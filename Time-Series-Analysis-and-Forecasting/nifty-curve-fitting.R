# Read file
setwd("/Users/pankaj/TechCareer/World-of-R/Time-Series-Analysis-and-Forecasting")

nifty <- read.csv("nifty.csv")

# Inspect
head(nifty)
names(nifty) <- make.names(names(nifty))

nifty$Date <- as.Date(nifty$Date, format="%d-%b-%Y")

nifty <- nifty[order(nifty$Date), ]

nifty$t <- 1:nrow(nifty)

fit1 <- lm(Close ~ t, data=nifty)

fit2 <- lm(Close ~ poly(t, degree=2, raw=TRUE), data=nifty)

fit3 <- lm(Close ~ poly(t, degree=3, raw=TRUE), data=nifty)

fit4 <- lm(Close ~ poly(t, degree=4, raw=TRUE), data=nifty)

fit5 <- lm(Close ~ poly(t, degree=5, raw=TRUE), data=nifty)

plot(nifty$t, nifty$Close, pch=19,
     xlab="Time", ylab="Nifty Close")

lines(nifty$t, predict(fit1), col="red")
lines(nifty$t, predict(fit2), col="blue")
lines(nifty$t, predict(fit3), col="purple")
lines(nifty$t, predict(fit4), col="orange")
lines(nifty$t, predict(fit5), col="green")


summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared

plot(nifty$t, nifty$Close, pch=19,
     xlab="Time", ylab="Nifty Close")

lines(nifty$t, predict(fit3), col="orange", lwd=2)

# https://home.iitk.ac.in/~shalab/sptimeseries
