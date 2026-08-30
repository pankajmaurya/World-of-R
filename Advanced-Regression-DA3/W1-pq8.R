rm(list=ls())
library(lmreg)
data("girlgrowth")
head(girlgrowth)
lmgirl <- lm(Height~Age+I(Age^2), data=girlgrowth)
lmgirl2 <- lm(Height~Age+I(Age^2)+I(Age^3), data=girlgrowth)
lmfull <- lm(Height~factor(Age), data=girlgrowth)
# Lack of fit F-test
anova(lmgirl, lmfull)
hanova(lmgirl, lmfull)

# Since the p=0.918, we fail to reject H0: quadratic is adequate
# We accept H0: quadratic is adequate or no lack of fit.
# So, no significant lack of fit
# The quadratic regression of Height on Age filts the data
# about as well as the saturated model that allows a completely
# separate mean for every integer Age value

anova(lmgirl, lmgirl2)
summary(lmgirl2)$coeff
hanova(lmgirl, lmgirl2)
# p value = 0.7899, so no significant lack of fit

Agesq = girlgrowth$Age^2

Agecb = girlgrowth$Age^3

lm3 = lm(Height~Age+Agesq+Agecb,data=girlgrowth);summary(lm3)$coeff

# As there is no significant lack of fit, adequacy of the model is indicated.

library(alr4)
data(UN11)
head(UN11)
Africa = binaries(UN11$group)[,2]
lmfer = lm(fertility~log(ppgdp)+Africa,data=UN11)
lmfer2 = lm(fertility~log(ppgdp)+I(log(ppgdp)^2)+Africa,data=UN11)
anova(lmfer,lmfer2)
# p value < 0.0001 so we reject H0: 
