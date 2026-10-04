library(datasets)
DAX = EuStockMarkets[,1]
FTSE = EuStockMarkets[,4]
n = length(DAX)
x = DAX
y = FTSE
xydat = data.frame(sx=sort(x),sy=y[order(x)])

# rpart
library(rpart)
tree2 = rpart(sy~sx, data=xydat)
plot(tree2)
text(tree2, pretty=0, cex = .5)

library(rpart.plot)
rpart.plot(tree2, extra = 0)

plot(xydat,cex=0.7,col=3)
with(xydat, lines(sx, predict(tree2, xydat), col=2))


pred = predict(tree2, newdata = data.frame(sx = 3000))
round(pred)


tab = table(round(predict(tree2), 3))
round(100 * tab / sum(tab), 2)


xydat$fit = predict(tree2)
aggregate(sx ~ fit, data = xydat,
          FUN = function(v) c(min = min(v), max = max(v), width = max(v) - min(v)))


#########

DAX = EuStockMarkets[,1]; sx = sort(DAX)

FTSE = EuStockMarkets[,4]; sy = FTSE[order(DAX)]

library(rpart); library(rpart.plot)

treestock = rpart(sy~sx)

rpart.plot(treestock, extra = 0)
quantile(sx,probability=c(.25,.5,.75))


plot(DAX,FTSE,cex=.5,col=3)

lines(sx, predict(treestock, DAX), col=2)
