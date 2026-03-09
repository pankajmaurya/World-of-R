# Read file
setwd("/Users/pankaj/TechCareer/World-of-R/Time-Series-Analysis-and-Forecasting")

nifty <- read.csv("nifty1y.csv")

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

lines(nifty$t, predict(fit2), col="orange", lwd=2)


# number of days to forecast
k <- 90

n <- nrow(nifty)

future_t <- (n + 1):(n + k)
future_pred <- predict(fit4, newdata = data.frame(t = future_t))

# combine for axis limits
all_t <- c(nifty$t, future_t)
all_y <- c(nifty$Close, future_pred)

# plot with expanded limits
plot(nifty$t, nifty$Close,
     pch = 19,
     xlab = "Time",
     ylab = "Nifty Close",
     xlim = range(all_t),
     ylim = range(all_y))

# fitted curve
lines(nifty$t, predict(fit4), col = "orange", lwd = 2)

# forecast
lines(future_t, future_pred,
      col = "blue",
      lwd = 2,
      lty = 2)

points(future_t, future_pred,
       col = "blue",
       pch = 17)

# separator between history and forecast
abline(v = n, lty = 3)

legend("topleft",
       legend = c("Actual", "Fitted Curve", "Forecast"),
       col = c("black","orange","blue"),
       lty = c(NA,1,2),
       pch = c(19,NA,17))


# A loess smoother
lo <- loess(Close ~ t, data=nifty)

plot(nifty$t, nifty$Close, pch=19)

lines(nifty$t, predict(lo), col="blue", lwd=2)


# Volatility based forecast range


# compute log returns
nifty$ret <- c(NA, diff(log(nifty$Close)))

# remove NA
ret <- na.omit(nifty$ret)

# daily volatility
sigma <- sd(ret)

# last price
S0 <- tail(nifty$Close, 1)

# forecast horizon
k <- 30   # days

# time scaling
sigma_k <- sigma * sqrt(k)

# expected drift (optional)
mu <- mean(ret) * k

# forecast center
center <- S0 * exp(mu)

# confidence ranges
upper_1sd <- S0 * exp(mu + sigma_k)
lower_1sd <- S0 * exp(mu - sigma_k)

upper_2sd <- S0 * exp(mu + 2*sigma_k)
lower_2sd <- S0 * exp(mu - 2*sigma_k)

cat("Expected range in", k, "days:\n")
cat("1σ range:", lower_1sd, "-", upper_1sd, "\n")
cat("2σ range:", lower_2sd, "-", upper_2sd, "\n")




# https://home.iitk.ac.in/~shalab/sptimeseries


