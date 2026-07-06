library(HistData)
data(Guerry)
help(Guerry)
head(Guerry)
# dependence of the variable Suicide
# on MainCity
# and Wealth
# The hypothesis of interest is that MainCity has no impact on Suicide, once the effect of Wealth is taken into account. 

guerrylm1 = lm(Suicides ~ MainCity + Wealth, data = Guerry)
summary(guerrylm1)

guerrylm0 = lm(Suicides ~ Wealth, data = Guerry)
anova(guerrylm0, guerrylm1)
hanova(guerrylm0, guerrylm1)
