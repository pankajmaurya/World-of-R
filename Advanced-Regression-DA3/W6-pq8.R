library(datasets)

DAX = EuStockMarkets[,1]

FTSE = EuStockMarkets[,4]

n = length(DAX)

library(FNN) # knn.reg

krange = (10:50); cv = NULL

for (i in krange) cv = rbind(cv, 
                             
                             knn.reg(DAX, test=NULL,FTSE, k=i)$PRESS/n)

plot(krange,cv,xlab="k",ylab="CV avg sq pred error")

kopt = krange[which(cv==min(cv))]

plot(DAX,FTSE,cex=.7,col=3)

kr = knn.reg(DAX, 
             
             test = data.frame(DAX=sort(DAX)), FTSE, k = kopt)

lines(sort(DAX),kr$pred, col="red")

