library(HistData)
library(lmreg)
data("Guerry")
goodGuerry <- na.omit(Guerry)
head(goodGuerry)

lm1 <- lm(Crime_prop~factor(Region)+Wealth, data = goodGuerry)
lm2 <- lm(Crime_prop~Wealth, data = goodGuerry)

hanova(lm2, lm1)
# We get p value = 0.025 => H0 is rejected at level 0.05 but accepted at 0.01