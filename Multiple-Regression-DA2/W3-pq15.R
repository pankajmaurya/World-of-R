# World record sprint times of men and women
library(lmreg)
data("worldrecord")
head(worldrecord)
plot(worldrecord$Distance,worldrecord$MenRecord)
plot(worldrecord$Distance,worldrecord$WomenRecord)

logD = log(worldrecord$Distance)
logM = log(worldrecord$MenRecord)
logW = log(worldrecord$WomenRecord)

plot(logD,logM)
plot(logD,logW)

summary(lm(logM~logD))$coef
summary(lm(logW~logD))$coef

# Is beta_1 same for men and women? Are the regression lines parallel?

# construct combined model with different intercepts 
# but same slope for men and women
n = length(logD)
logT = c(logM,logW)
logDCombined = c(logD,logD)
genderM = c(rep(1,n), rep(0,n))
cbind(logT,genderM,logDCombined)

lmsimple = lm(logT ~ genderM + logDCombined)
summary(lmsimple)$coef

logDMen = c(logD, rep(0, n))
logDWomen = c(rep(0, n), logD)
lmcomplex = lm(logT~genderM+logDMen+logDWomen)
hanova(lmsimple, lmcomplex)

# pvalue of 0.83 implies that the null hypothesis is accepted.
# Simpler model is found to be adequate

# test for the null hypothesis that the regressions of men's and women's log–record times on log–distances have the same intercept, 
# assuming these have the same slope.

# Simplest model: same intercept, same slope for both genders
lmnull = lm(logT ~ logDCombined)
summary(lmnull)$coef
# Hierarchical F-test: same-intercept model vs different-intercept model
# (both assume the same slope, since lmsimple already assumes that)
hanova(lmnull, lmsimple)
