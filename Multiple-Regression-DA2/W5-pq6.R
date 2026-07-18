rm(list=ls())
library(HistData)
head(Wheat)
# We have to consider the quadratic model
lmw = lm(Wages~Year + I(Year^2), data = Wheat)

library(MASS)
hatvalues(lmw)

length((Wheat$Year))
length((Wheat$Wages))

Wheat # we see that 3 rows are having NA values.
# 2 explanatory variables 'Year' and 'Year^2' => k + 1 = 3, n = 50
(3/50) * 2 # this is the threshold for high leverage
plot(Wheat$Year, Wheat$Wages)
abline(lmw, col=2)

sum(hatvalues(lmw) > ((3/50) * 2)) # We get 2 high leverage values.

sum(stdres(lmw) > 2) # 2 cases have high standardized residuals
sum(studres(lmw) > 2) # 2 cases have high studentized residuals

