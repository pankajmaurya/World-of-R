set.seed(2345)
n = 500
x = runif(n) 
mu = function(x) sin(6*pi*x) / (6*pi*x)
y = mu(x) + 0.2 * rnorm(n)
xydat = data.frame(sx=sort(x),sy=y[order(x)])
plot(xydat, cex=0.7, col = "green")


# Regression spline
#library(splines)
lm1 = lm(sy ~ bs(sx), data = xydat) 
summary(lm1)
with(data=xydat, lines(sx, lm1$fit, col=4))
lm1 = lm(sy ~ bs(sx, df=7), data = xydat)
summary(lm1)
with(data=xydat, lines(sx, lm1$fit, col=6))
lm1 = lm(sy ~ bs(sx, knots=quantile(sx,probs=c(.2,.4,.6, .8))), data = xydat)
with(data=xydat, lines(sx, lm1$fit, col=1))
lm1 = lm(sy ~ bs(sx, knots = c(.2, .4, .6, .8)), data = xydat)
with(data=xydat, lines(sx, lm1$fit, col=2))

# Use df=7 for fresh fit
plot(xydat, cex=0.7, col = "green")
lm1 = lm(sy ~ bs(sx, df=7), data = xydat)
with(data=xydat, lines(sx, lm1$fit, col=2))
with(data=xydat, lines(sx, mu(sx),  col=1, lwd = 1.5))
legend("topright", c("True function","Regression spline, df 7"),
       lty=1, col=c(1,2), lwd = c(1.5,1))

# Natural spline 
library(Epi)
lm2 = lm(sy ~ Ns(sx, df=7), data = xydat)
with(data=xydat, lines(sx, lm2$fit, col=4))
legend("topright", c("True function","Regression spline, df 7","Natural spline, df 7"),
       lty=1, col=c(1,2,4), lwd = c(1.5,1,1))

# Compare standard errors of fit
mean(predict(lm1, newdata=xydat, se.fit=TRUE)$se.fit[496:500]) # cubic spline
mean(predict(lm2, newdata=xydat, se.fit=TRUE)$se.fit[496:500]) # natural cubic spline

# Smoothing spline
ss3 = with(data=xydat,
           smooth.spline(sx, sy, df=7))
lines(ss3, col = 6)
legend("topright", 
       c("True function","Regression spline, df 7",
         "Natural spline, df 7","Smoothing spline, df 7"),
       lty=1, col=c(1,2,4,6), lwd = c(1.5,1,1,1))

plot(xydat, cex=0.7, col = "green")
with(data=xydat, lines(smooth.spline(sx, sy, df=7), col=6))
with(data=xydat, lines(smooth.spline(sx, sy, lambda = 0.001), col=4))
