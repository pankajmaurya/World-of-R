rm(list = ls())
library(lmreg)
data("LAcrime")
head(LAcrime)
LAcrime["homicideRate"] = LAcrime["Homicide"] / (LAcrime["Population"] / 100000)
homicidelm = lm(homicideRate ~ Year + TempCelsius, data = LAcrime)
summary(homicidelm)
homicidelm1 <- lm(homicideRate~1, data=LAcrime)
anova(homicidelm1, homicidelm)
hanova(homicidelm1, homicidelm)

# Define a response variable called rate of rape, representing the number of number of rapes per 100,000 population
LAcrime["rapeRate"] = LAcrime["Rape"] / (LAcrime["Population"] / 100000)
head(LAcrime)

# regress it on Year and Temperature in Celsius (TempCelsius)
rapelm = lm(rapeRate ~ Year + TempCelsius, data = LAcrime)

# The p-value of the F statistics for the hypothesis of no effect of regressors is 2.98 x 10^-33
summary(rapelm)
rapelm1 <- lm(rapeRate~1, data=LAcrime)
anova(rapelm1, rapelm)
hanova(rapelm1, rapelm)


# How should this finding be interpreted?
# Temperature and year together have a significant effect on the rate of rape, at the level 0.001.
