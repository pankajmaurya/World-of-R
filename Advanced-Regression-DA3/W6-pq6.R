rm(list=ls())
set.seed(2345)
n = 2000
x = runif(n) 
mu = function(x) sin(6*pi*x) / (6*pi*x)
y = mu(x) + 0.2 * rnorm(n)

# library(gplm)

# Select bandwidth by cross validation
hrange = seq(0.04,0.05,0.001)
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

