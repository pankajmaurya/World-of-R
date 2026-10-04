# Simulations of splines
library(datasets)

DAX = EuStockMarkets[,1]

FTSE = EuStockMarkets[,4]

n = length(DAX)

library(FNN) # knn.reg

x = DAX
y = FTSE

xydat = data.frame(sx=sort(x),sy=y[order(x)])


plot(DAX,FTSE,cex=.7,col=3)
# Regression spline
lm1 = lm(sy ~ bs(sx, df=9), data = xydat)
summary(lm1)
with(data=xydat, lines(sx, lm1$fit, col=6))

# Natural spline 
lm2 = lm(sy ~ Ns(sx, df=9), data = xydat)
with(data=xydat, lines(sx, lm2$fit, col=4))

# Smoothing spline
ss3 = with(data=xydat,
           smooth.spline(sx, sy, df=9))
lines(ss3, col = 6)