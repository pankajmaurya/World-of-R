# Bandwidth selection for NW kernel regression

set.seed(2345)
n = 500
x = runif(n) 
mu = function(x) sin(6*pi*x) / (6*pi*x)
y = mu(x) + 0.2 * rnorm(n)

# library(gplm)

# Select bandwidth by cross validation
hrange = seq(0.01,0.2,0.01)
#hrange = seq(0.03,0.05,0.001)
cv = NULL
for (h in hrange) {
  cvh = 0
  for (i in 1:n) {
    xi = x[-i]
    yi = y[-i]
    fit = kreg(xi, yi, grid=x[i], bandwidth=h, 
               kernel = "epanechnikov")$y
    cvh = cvh + ((y[i] - fit)^2)
  }
  cv = rbind(cv,cvh/n)
}
plot(hrange, cv)

# Compare MSEP with square of estimated sigma from cubic model
lm2 = lm(y~poly(x,degree=3, raw = TRUE))
summary(lm2)$sig^2

plot(x, y, cex=0.7, col=3)
kr = kreg(x, y, bandwidth=0.01, grid=sort(x), kernel = "triangular")
lines(kr$x, kr$y, col=6)
kr = kreg(x, y, bandwidth=0.2,  grid=sort(x), kernel = "triangular")
lines(kr$x, kr$y, col=4)
kr = kreg(x, y, bandwidth=0.04, grid=sort(x), kernel = "triangular")
lines(kr$x, kr$y, col=1)
lines(sort(x), mu(sort(x)), col=2, lwd=2)
legend("topright",
       c("NWE, h=0.01","NWE, h=0.04","NWE, h=0.2","True function"),
       lty=c(1,1,1,1), col=c(6,1,4,2), lwd=c(1,1,1,2))


# PQ 6.6
data("EuStockMarkets")
y = EuStockMarkets[, "FTSE"]
x = EuStockMarkets[, "DAX" ]
plot(x, y, cex=0.7, col=3, xlab="DAX", ylab="FTSE")
kr = kreg(x, y, bandwidth=35, grid=sort(x), kernel = "triangular")
lines(kr$x, kr$y, lwd=2)
DAX = EuStockMarkets[,1]
FTSE = EuStockMarkets[,4]
n = length(DAX)
hrange=c(30, 35, 40, 45)
cv = NULL
for (h in hrange) {
  cvh = 0
  for (i in 1:n) {
    xi = x[-i]
    yi = y[-i]
    fit = kreg(xi, yi, grid=x[i], bandwidth=h, 
               kernel = "triangular")$y
    cvh = cvh + ((y[i] - fit)^2)
  }
  cv = rbind(cv,cvh/n)
}
plot(hrange, cv)

# Solution given is below
DAX = EuStockMarkets[,1]
FTSE = EuStockMarkets[,4]
n = length(DAX)
hrange = c(5:10)*5
cv = NULL
for (h in hrange) {
  cvh = 0
  for (i in 1:n) {
    xi = DAX[-i]
    yi = FTSE[-i]
    fit = kreg(xi, yi, grid=DAX[i], bandwidth=h, 
               kernel = "triangular")$y
    cvh = cvh + ((FTSE[i] - fit)^2)
  }
  cv = rbind(cv,cvh/n)
}
plot(hrange, cv)
