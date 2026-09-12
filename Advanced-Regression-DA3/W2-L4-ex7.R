# Dealing with serial correlation in Exchange rate data
library(readxl)
setwd("/Users/pankaj/Downloads")
INRUSD <- read_excel("Xrate.xlsx", sheet = "Data")
n <- length(INRUSD$Xrate)
logxr <- log(INRUSD$Xrate)
logi <- log(INRUSD$IndiaCPI)
logu <- log(INRUSD$USCPI)

# Stage 1 computations
lmlogxr <- lm(logxr~logi+logu)

# Stage 2 computations
res <- lmlogxr$residuals
phi <- sum(res[-1]*res[-n])/sum((res[-n])^2) #LSE 
tlogxr <- logxr[-1] - phi * logxr[-n]
tlogi <- logi[-1] - phi * logi[-n]
tlogu <- logu[-1] - phi * logu[-n]
lmtlogxr <- lm(tlogxr~tlogi+tlogu)

# Check plots before and after transformation
par(mfrow=c(2,2))
library(MASS)

# plots from OLS analysis
sres <- stdres(lmlogxr)
plot(sres,type="b", ylab = "Standardized residual")
abline(h=0, lty=2)
title("Index plot, OLS")
plot(sres[1:(n-1)], sres[2:n], xlab = "Lagged standardized residual",
     ylab = "Standardized residual", type="l")
abline(a=0, b=1, lty=2)
title("Lag plot, OLS")

# plots from 2 stage LS analysis
srest <- stdres(lmtlogxr)
plot(srest,type="b", ylab = "Standardized residuals")
abline(h=0, lty=2)
title("Index plot, 2 stage LS")

plot(srest[1:(n-1)], srest[2:n], xlab = "Lagged standardized residuals",
     ylab = "Standardized residuals", type="l")
abline(a=0, b=1, lty=2)
title("Lag plot, 2 stage LS")
# library(lmtest)
dwtest(lmtlogxr)

# PQ 2.14
library(alr4)
data("MinnWater")
head(MinnWater)
lm1 <- lm(muniUse~muniPrecip+muniPop, data=MinnWater)
n <- length(MinnWater$year)
# Stage 2 computations
res <- lm1$residuals
phi <- sum(res[-1]*res[-n])/sum((res[-n])^2) #LSE 
round(phi, 3)
y <- MinnWater$muniUse
x1 <- MinnWater$muniPrecip
x2 <- MinnWater$muniPop

ty <- y[-1] - phi * y[-n]
tx1 <- x1[-1] - phi * x1[-n]
tx2 <- x2[-1] - phi * x2[-n]
lmt <- lm(ty~tx1+tx2)
res2 <- lmt$residuals
dwtest(lmt)
summary(lmt)
summary(lm1)
