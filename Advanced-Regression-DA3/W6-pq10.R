library(datasets)

DAX = EuStockMarkets[,1]

FTSE = EuStockMarkets[,4]

n = length(DAX)

library(FNN) # knn.reg

x = DAX
y = FTSE

xydat = data.frame(sx=sort(x),sy=y[order(x)])


plot(DAX,FTSE,cex=.7,col=3)
kr = loess(sy~sx, data = xydat, degree=2, span = 0.5)
lines(sort(DAX), kr$fitted, col = "red")


lr = loess(FTSE~DAX, degree = 1, span = 1)

lines(sort(DAX),predict(lr, newdata = sort(DAX)), col=6)

lr = loess(FTSE~DAX, degree = 1, span = 0.5)

lines(sort(DAX),predict(lr, newdata = sort(DAX)), col=2)

lr = loess(FTSE~DAX, degree = 2, span = 1)

lines(sort(DAX),predict(lr, newdata = sort(DAX)), col=4)

lr = loess(FTSE~DAX, degree = 2, span = 0.5)

lines(sort(DAX),predict(lr, newdata = sort(DAX)), col=1)


legend("bottomright", c("degree=1, span=1", "degree=1, span=0.5",
                        
                        "degree=2, span=1", "degree=2, span=0.5"), 
       
       lty=1, col=c(6,2,4,1))
