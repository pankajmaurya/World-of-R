# Better prediction of Exchange rate by using serial correlation
library(readxl)
setwd("/Users/pankaj/Downloads")
INRUSD <- read_excel("Xrate.xlsx", sheet = "Data")

n <- length(INRUSD$Xrate)
logxr <- log(INRUSD$Xrate)
logi <- log(INRUSD$IndiaCPI)
logu <- log(INRUSD$USCPI)

# predict nth observation from first n-1 observations
# Stage 1 computations (OLS)
lmlogxr <- lm(logxr[-n]~logi[-n]+logu[-n])
# now make predictions
newdat <- data.frame(logi=logi[n],logu=logu[n])
predols <- predict(lmlogxr, newdat, interval = "prediction")

predols <- exp(predols)
INRUSD$Xrate[n]

# Stage 2 computations for 2 stage LS
res <- lmlogxr$residuals
phi <- sum(res[-1]*res[-(n-1)])/sum((res[-(n-1)])^2) 
tlogxr <- logxr[-1] - phi * logxr[-n]
tlogi <- logi[-1] - phi * logi[-n]
tlogu <- logu[-1] - phi * logu[-n]
lmtlogxr <- lm(tlogxr[-(n-1)]~tlogi[-(n-1)]+tlogu[-(n-1)])
# now make predictions
newdat <- data.frame(tlogi=tlogi[(n-1)],tlogu=tlogu[(n-1)])
pred2ls <- predict(lmtlogxr, newdat, interval = "prediction")

pred2ls <- pred2ls + phi * logxr[(n-1)]

pred2ls <- exp(pred2ls)
INRUSD$Xrate[n]

# Graphical display of predictions
par(mfrow=c(1,1))
MY <- as.Date(ISOdate(year = INRUSD$Year, 
                      month = INRUSD$Month,
                      day = 1))
plot(MY, INRUSD$Xrate, xaxt = "n", ylim = c(60,85),
     xlab = "Month", ylab = "Exchange rate â‚¹-$")
xtics <- seq(3,n,24)
axis(1,at = MY[xtics],
     labels = paste(INRUSD$Month[xtics],"-",INRUSD$Year[xtics]))

legend("bottomright", cex=.7, 
       c("Observed values","Observed value Jan-23",
         "OLS prediction and 95% limits","2 stage LS prediction and 95% limits"),
       lty = c(0,0,0,0), col = c(1,2,3,4), pch = c(1,16,18,17))

points(rep(MY[n],3), predols, col=3, pch=18)
points(MY[n],INRUSD$Xrate[n], col=2, pch=16)
points(rep(MY[n],3), pred2ls, col=4, pch=17)
