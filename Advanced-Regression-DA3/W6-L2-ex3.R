# Simulations of kNN kernel regression

set.seed(2345)
n = 500
x = runif(n)
mu = function(x) sin(6*pi*x) / (6*pi*x)
y = mu(x) + 0.2 * rnorm(n)
plot(x, y, cex=0.7, col="green")
lines(sort(x), mu(sort(x)))

xydat = data.frame(sx=sort(x),sy=y[order(x)])

# kNN regression
#install.packages("caret")
library(caret) # knnreg
kr = knnreg(sy~sx, data = xydat, k = 45)
lines(xydat$sx, predict(kr, xydat), col="blue")
library(FNN) # knn.reg
kr = knn.reg(xydat$sx, 
             test = data.frame(sx=xydat$sx), xydat$sy, k = 45)
lines(sort(x),kr$pred, col="red")

# Select neighborhood size k by cross validation
krange = (10:50)
cv = NULL
for (i in krange) {
  cv = rbind(cv,knn.reg(xydat$sx, 
                        test=NULL,xydat$sy, k=i)$PRESS/n)
}
plot(krange,cv)
kopt = krange[which(cv==min(cv))]
kopt
min(cv)

plot(xydat, cex=0.7, col = 3)
kr = knn.reg(xydat$sx, 
             test = data.frame(sx=xydat$sx), xydat$sy, k = kopt)
with(data=xydat, lines(sx, kr$pred, col=2))
with(data=xydat, lines(sx, mu(sx), col=1))
legend("topright",c("kNN, k by CV","True function"),
       lty=c(1,1),col=c(2,1))

