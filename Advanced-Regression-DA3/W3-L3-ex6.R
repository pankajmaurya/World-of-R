# Case influence in IMF unemployment data 

library(lmreg)
data(imf2015)
cases <- imf2015$Country
row.names(imf2015) <- cases
head(imf2015)
lm3 <- lm(UNMP ~ EXP + INFL + INV, data = imf2015)
k <- 3       # number of regression parameters
n <- length(imf2015$UNMP) # sample size
cooks.distance(lm3)
which(cooks.distance(lm3) > 1)
which(cooks.distance(lm3) > 4/n)
dffits(lm3)
which(abs(dffits(lm3)) > 2*sqrt((k+1)/n))
dfbetas(lm3)
which(abs(dfbetas(lm3))[,1] > 2/sqrt(n))
which(abs(dfbetas(lm3))[,2] > 2/sqrt(n))
which(abs(dfbetas(lm3))[,3] > 2/sqrt(n))
which(abs(dfbetas(lm3))[,4] > 2/sqrt(n))
covratio(lm3)
which(abs(covratio(lm3) - 1) > 3*(k+1)/n)

influence.measures(lm3)
