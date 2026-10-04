# Simulations of regression tree

set.seed(2345)
n = 500
x = runif(n)
mu = function(x) sin(6*pi*x) / (6*pi*x)
y = mu(x) + 0.2 * rnorm(n)
xydat = data.frame(sx=sort(x), sy=y[order(x)])

# tree
library(tree)
tree1 = tree(sy ~ sx, data = xydat) 
plot(tree1)
text(tree1, pretty=0, cex = .7)

with(xydat, plot(sx, sy, cex=0.7, col=3))
with(xydat, lines(sx, predict(tree1, xydat), col=4))

# rpart
library(rpart)
tree2 = rpart(sy~sx, data=xydat)
plot(tree2)
text(tree2, pretty=0, cex = .5)

library(rpart.plot)
rpart.plot(tree2, extra = 0)

plot(xydat,cex=0.7,col=3)
with(xydat, lines(sx, predict(tree2, xydat), col=2))
with(xydat, lines(sx, predict(tree1, xydat), col=4))