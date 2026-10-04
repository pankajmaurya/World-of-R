# Improving a regression tree

set.seed(2345)
n <- 500
x <- runif(n)
mu <- function(x) sin(6*pi*x) / (6*pi*x)
y <- mu(x) + 0.2 * rnorm(n)
xydat <- data.frame(sx=sort(x),sy=y[order(x)])
n <- dim(xydat)[1] 

# library(rpart)
tree2 <- rpart(sy~sx, data=xydat)
plot(xydat,cex=0.7,col=3)
with(xydat, lines(sx, predict(tree2, xydat), col=4))

# Bagged trees 
set.seed(123)
nbag <- 500
treebags <- list()
for (i in 1:nbag)
{
  sid <- sort(sample(1:n,size=n,replace=T))
  treebags <- c(treebags,list(rpart(sy~sx,xydat[sid,])))
}
# Bagging
bagpred <- 0
for (model in treebags)  bagpred <- bagpred + predict(model,newdata = xydat)
bagpred <- bagpred / nbag

#bagged_result <- NULL
lines(xydat$sx, bagpred, col=2, lwd=2)
lines(xydat$sx, mu(xydat$sx), col=1)
legend("topright", 
       c("Fit from tree","Fit from bagged tree","Actual mean function"),
       lty=1,col=c(4,2,1))

# Average estimation error from single tree
mean((predict(tree2, xydat) - mu(xydat$sx))^2)
# Average estimation error after bagging
mean((bagpred - mu(xydat$sx))^2)

# Boston Housing Data
library(mlbench)
data("BostonHousing")
head(BostonHousing)

lmbh1 <- lm(medv~crim+zn+indus+chas+nox+rm+
              age+dis+rad+tax+ptratio+b+lstat, data=BostonHousing)
summary(lmbh1)
library(leaps)
with(data=BostonHousing,leaps(x=cbind(crim,zn,indus,chas,
                                      nox,rm,age,dis,rad,tax,ptratio,b,lstat),
                              y=medv,method="Cp",nbest=1))
lmbh2 <- lm(medv~crim+zn+chas+nox+rm+
              dis+rad+tax+ptratio+b+lstat, data=BostonHousing)
summary(lmbh2)
library(MASS)
plot(lmbh2$fit, stdres(lmbh2), cex = .7); abline(h=0)
boxcox(lmbh2,lambda = seq(-1, 3, 1/10), plotit = TRUE)
lmedv <- log(BostonHousing$medv)
lmbh3 <- lm(lmedv~crim+zn+chas+nox+rm+
              dis+rad+tax+ptratio+b+lstat, data=BostonHousing)
summary(lmbh3)
plot(lmbh3$fit, stdres(lmbh3), cex = .7); abline(h=0)

mean((BostonHousing$medv - exp(lmbh3$fit))^2)

# Random Forest
library(randomForest)
rfbh <- randomForest(lmedv~crim+zn+chas+nox+rm+
                       dis+rad+tax+ptratio+b+lstat, 
                     data=BostonHousing)
mean((BostonHousing$medv - exp(rfbh$pred))^2)
plot(rfbh$pred, lmedv - rfbh$pred, cex=.7); abline(h=0) 

rfbh2 <- randomForest(medv~crim+zn+chas+nox+rm+
                        dis+rad+tax+ptratio+b+lstat, 
                      data=BostonHousing)
mean((BostonHousing$medv - rfbh2$pred)^2)
plot(rfbh2$pred, BostonHousing$medv - rfbh2$pred, cex=.7) 