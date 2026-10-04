# Simulations of NW kernel regression

set.seed(2345)
n = 500
x = runif(n) 
mu = function(x) sin(6*pi*x) / (6*pi*x)
y = mu(x) + 0.2 * rnorm(n)
plot(x, y, cex=0.5, col = 3)
lines(sort(x), mu(sort(x)))

# Fit through Box-Tidwell transformed x
Rsquare = NULL; lambda = seq(-1,1,.01)
for (lam in lambda) {
  tx = (x^lam - 1)/lam
  if (lam == 0) tx = log(x)
  Rsquare = c(Rsquare,summary(lm(y~tx))$r.sq)
}
lamb = lambda[which(Rsquare==max(Rsquare))]
tx = x^lamb
lm1 = lm(y~tx)
sx = sort(x)^lamb
fit1 = predict(lm1,newdata = data.frame(tx=sx),interval = "none")
lines(sort(x), fit1, col=2)

# Fit through cubic polynomial transformed x
lm2 = lm(y~poly(x, degree=3, raw = TRUE))
sx = sort(x)
fit2 = predict(lm2,newdata = data.frame(x=sx),interval = "none")
lines(sort(x),fit2,col=4)
legend("topright",c("True mean function","Box-Tidwell fit","Cubic polynomial fit"),
       lty=c(1,1,1),col=c(1,2,4))


# Kernel Smoother
plot(x, y, cex=0.7, col=3)
kr = ksmooth(x, y, bandwidth = 0.02, kernel = "box", 
             x.points=sort(x))
lines(kr$x, kr$y, col=2)
library(gplm)
kr = kreg(x, y, bandwidth = 0.01, kernel = "uniform", grid=sort(x))
lines(kr$x, kr$y, col=6)
kr = kreg(x, y, bandwidth = 0.01, kernel = "triangular", grid=sort(x))
lines(kr$x, kr$y, col=4)
legend("topright", 
       c("NWE, uniform kernel","NWE, triangular kernel"),
       lty=c(1,1), col=c(6,4))
# highlight a jump for rectangular kernel
lines(.69+.02*sin(2*pi*sort(x)),-.14+.06*cos(2*pi*sort(x)),col=2)

# Effect of bandwidth selection
plot(x, y, cex=0.7, col=3)
kr = kreg(x, y, bandwidth=0.01, grid=sort(x), kernel = "triangular")
lines(kr$x, kr$y, col=2)
kr = kreg(x, y, bandwidth=0.2,  grid=sort(x), kernel = "triangular")
lines(kr$x, kr$y, col=4)
lines(sort(x), mu(sort(x)), col=1)
legend("topright", c("NWE, h=0.01", "NWE, h=0.2", "True function"),
       lty=c(1,1,1), col=c(6,4,1))
kr = kreg(x, y, bandwidth=0.1,  grid=sort(x), kernel = "triangular")
lines(kr$x, kr$y, col=4)

# PQ 6.3
data("EuStockMarkets")
plot(EuStockMarkets, main = "Major European Stock Indices (1991-1998)")
y = EuStockMarkets[, "FTSE"]
x = EuStockMarkets[, "DAX" ]
plot(x, y, cex=0.7, col=3, xlab="DAX", ylab="FTSE")
kr = kreg(x, y, bandwidth=200, grid=sort(x), kernel = "triangular")
lines(kr$x, kr$y, lwd=2)
