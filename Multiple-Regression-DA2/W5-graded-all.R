rm(list=ls())
library(alr4)
data("MinnWater")
head(MinnWater)
length(MinnWater$muniUse)
MinnWater
plot(MinnWater$muniPop, MinnWater$muniUse)
lmwater <- lm(muniUse~muniPop, data=MinnWater)
abline(lmwater)
boxplot(MinnWater$muniPop)
summary(MinnWater$muniPop)
hatvalues(lmwater)
which(hatvalues(lmwater) > 2*mean(hatvalues(lmwater)))  # common rule of thumb: > 2p/n
library(car)
outlierTest(lmwater)   # gives a Bonferroni-adjusted p-value for the largest studentized residual
#======================

library(HistData)
data("Wheat")
head(Wheat)
length(Wheat$Year)
Wheat
lmwheat <- lm((Wheat$Wages/Wheat$Wheat)~Wheat$Year)
summary(lmwheat)

library(MASS)
hatvalues(lmwheat)
plot(lmwheat); abline(lmwheat, col=2)

Wheat # we see that 3 rows are having NA values.
# 1 explanatory variables 'Year' and 'Year^2' => k + 1 = 3, n = 50
(2/50) * 2 # this is the threshold for high leverage
plot(Wheat$Year, Wheat$Wages/Wheat$Wheat)
abline(lmwheat, col=2)

sum(hatvalues(lmwheat) > ((2/50) * 2)) # We get 2 high leverage values.
n <- 50
cases = 1:n
k <- 1
hihi <- which(hatvalues(lmwheat)>2*(k+1)/n) # cases with hi leverage
hihi


sum(abs(stdres(lmwheat)) > 2) # 2 cases have high standardized residuals

abs(stdres(lmwheat)) > 2 # gives 2 years

# Get hat values with names/index matching Wheat rows
hv <- hatvalues(lmwheat)

# Find the row index where Year == 1730
idx <- which(Wheat$Year == 1730)
idx

# Leverage value for that row
hv[idx]


# Combine hat values with the Year used in the regression
# (only the 50 complete rows go into the model)
complete_rows <- complete.cases(Wheat$Wages, Wheat$Wheat, Wheat$Year)
years_used <- Wheat$Year[complete_rows]

# Now match by year
hv[years_used == 1810]
hv[years_used == 1730]
plot(lmwheat)


hv <- hatvalues(lmwheat)
threshold <- (2/50) * 2

# Which years exceed the threshold?
years_used[hv > threshold]

# And explicitly check 1730 and 1810
hv[years_used == 1730] > threshold
hv[years_used == 1810] > threshold
plot(hatvalues(lmwheat),stdres(lmwheat))
#===============================================
rm(list=ls())
library(alr4)
data("dwaste")
head(dwaste) 
dwaste

lmwaste <- lm(log(O2UP)~BOD+TKN+TS+TVS+COD, data = dwaste)
library(car)
sum(vif(lmwaste) > 10)
round(max(vif(lmwaste)), 2)
cisv(lmwaste)

