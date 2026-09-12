rm(list=ls())
# Better prediction of Exchange rate by using serial correlation
library(readxl)
setwd("/Users/pankaj/Downloads")
INRUSD <- read_excel("Xrate.xlsx", sheet = "Data")
n <- length(INRUSD$Xrate)

logxr <- log(INRUSD$Xrate)
logi <- log(INRUSD$IndiaCPI)
logu <- log(INRUSD$USCPI)

# Locate the target row: Dec-2022
m <- which(INRUSD$Year == 2022 & INRUSD$Month == 12)

# Stage 1 computations (OLS) -- use clean pre-subsetted vectors, not inline indexing
logxr_tr <- logxr[1:(m-1)]
logi_tr  <- logi[1:(m-1)]
logu_tr  <- logu[1:(m-1)]

lmlogxr <- lm(logxr_tr ~ logi_tr + logu_tr)

# now make predictions
newdat <- data.frame(logi_tr = logi[m], logu_tr = logu[m])
predols <- predict(lmlogxr, newdat, interval = "prediction")
predols <- exp(predols)
INRUSD$Xrate[m]

# Stage 2 computations for 2 stage LS
res <- lmlogxr$residuals
phi <- sum(res[-1] * res[-(m-1)]) / sum((res[-(m-1)])^2)

tlogxr <- logxr_tr[-1] - phi * logxr_tr[-(m-1)]
tlogi  <- logi_tr[-1]  - phi * logi_tr[-(m-1)]
tlogu  <- logu_tr[-1]  - phi * logu_tr[-(m-1)]

lmtlogxr <- lm(tlogxr ~ tlogi + tlogu)

# now make predictions
newdat2 <- data.frame(tlogi = logi[m] - phi * logi[m-1],
                      tlogu = logu[m] - phi * logu[m-1])
pred2ls <- predict(lmtlogxr, newdat2, interval = "prediction")
pred2ls <- pred2ls + phi * logxr[m-1]
pred2ls <- exp(pred2ls)
INRUSD$Xrate[m]

# Graphical display of predictions
par(mfrow=c(1,1))
MY <- as.Date(ISOdate(year = INRUSD$Year, 
                      month = INRUSD$Month,
                      day = 1))
plot(MY, INRUSD$Xrate, xaxt = "n", ylim = c(60,85),
     xlab = "Month", ylab = "Exchange rate ₹-$")
xtics <- seq(3,n,24)
axis(1,at = MY[xtics],
     labels = paste(INRUSD$Month[xtics],"-",INRUSD$Year[xtics]))
legend("bottomright", cex=.7, 
       c("Observed values","Observed value Dec-22",
         "OLS prediction and 95% limits","2 stage LS prediction and 95% limits"),
       lty = c(0,0,0,0), col = c(1,2,3,4), pch = c(1,16,18,17))
points(rep(MY[m],3), predols, col=3, pch=18)
points(MY[m],INRUSD$Xrate[m], col=2, pch=16)
points(rep(MY[m],3), pred2ls, col=4, pch=17)

# true value
INRUSD$Xrate[m]
# OLS pred interval
predols

# 2sls pred interval
pred2ls

width_ols  <- predols[1,"upr"]  - predols[1,"lwr"]
width_2sls <- pred2ls[1,"upr"] - pred2ls[1,"lwr"]

ratio <- width_2sls / width_ols
round(ratio, 3)
