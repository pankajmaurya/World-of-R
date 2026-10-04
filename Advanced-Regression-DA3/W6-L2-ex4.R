# Simulations of LOESS regression

set.seed(2345)
n = 500
x = runif(n) 
mu = function(x) sin(6*pi*x) / (6*pi*x)
y = mu(x) + 0.2 * rnorm(n)
xydat = data.frame(sx=sort(x),sy=y[order(x)])
plot(xydat, cex=0.7, col = "green")

n = dim(xydat)[1] 

# LOESS regression with default Î± 
kr = loess(sy~sx, data = xydat)
lines(xydat$sx,predict(kr, newdata = xydat), col=4)

# Select span Î± by cross validation
alpharange = seq(.1,1.2,.1)
#alpharange = seq(.1,.3,.02)
cv = NULL
for (a in alpharange) {
  cvh = NULL
  for (i in 2:(n-1)) {
    xi = xydat$sx[-i]
    yi = xydat$sy[-i]
    lobj = loess(yi~xi, span = a) 
    cvh = c(cvh , ((xydat$sy[i] - predict(lobj,xydat$sx[i]))^2))
  }
  cv = c(cv,mean(cvh))
}
plot(alpharange,cv)

aopt = alpharange[which(cv==min(cv))]
aopt
min(cv)

plot(xydat, cex=0.7, col = 3)
kr = loess(sy~sx, data = xydat, span = aopt)
with(data=xydat, lines(sx,predict(kr, newdata = xydat),col=2))
with(data=xydat, lines(sx,mu(sx),col=1))
legend("topright",c("LOESS, Î± = 0.2","True function"),
       lty=c(1,1),col=c(2,1))
