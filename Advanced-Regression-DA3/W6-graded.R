library(datasets)
DAX = EuStockMarkets[,1]
FTSE = EuStockMarkets[,4]
CAC = EuStockMarkets[,3]

n = length(CAC)
x = CAC
y = FTSE
xydat = data.frame(sx=sort(x),sy=y[order(x)])

plot(xydat, cex=0.7, col = "green")

# Natural spline 
library(Epi)
lm2 = lm(sy ~ Ns(sx, df=15), data = xydat)



with(data=xydat, lines(sx, lm2$fit, col=4))
legend("topright", c("True function","Regression spline, df 7","Natural spline, df 7"),
       lty=1, col=c(1,2,4), lwd = c(1.5,1,1))

# kNN regression
#install.packages("caret")
library(caret) # knnreg
kr = knnreg(sy~sx, data = xydat, k = 200)
lines(xydat$sx, predict(kr, xydat), col="black", lwd=2)

kr = kreg(x, y, bandwidth=250, grid=sort(x), kernel = "triangular")
lines(kr$x, kr$y, lwd=2)

kr = loess(sy~sx, data = xydat, span = 0.15, degree=1)
with(data=xydat, lines(sx,predict(kr, newdata = xydat),col=2))

alphas = seq(0.05, 0.20, 0.05)
cv = NULL

for (a in alphas) {
  
    cvh = 0
    for (i in 1:n) {
      xi = x[-i]
      yi = y[-i]
      fit = loess(yi~xi, span = a, degree=2)$y
      cvh = cvh + ((y[i] - fit)^2)
    }
    cv = rbind(a,cvh/n)
}


alphas = seq(0.05, 0.20, 0.05)
cv = numeric(length(alphas))

for (k in seq_along(alphas)) {
  cvh = 0; cnt = 0
  for (i in 1:n) {
    xi = x[-i]; yi = y[-i]
    fit = loess(yi ~ xi, span = alphas[k], degree = 2)
    p = predict(fit, newdata = data.frame(xi = x[i]))
    if (!is.na(p)) { cvh = cvh + (y[i] - p)^2; cnt = cnt + 1 }
  }
  cv[k] = cvh / cnt
}

data.frame(alphas, cv)
alphas[which.min(cv)]


plot(x, y, pch = 16, cex = 0.4, col = "grey60",
     xlab = "CAC", ylab = "FTSE")
o = order(x)
cols = c("blue", "red", "darkgreen", "orange")
for (k in 2:4) {
  fit = loess(y ~ x, span = alphas[k], degree = 2)
  lines(x[o], predict(fit)[o], col = cols[k], lwd = 2)
}
legend("topleft", legend = paste("span =", alphas),
       col = cols, lwd = 2, cex = 0.8)
