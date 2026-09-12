# Normality in the abdominal fat data
library(lmreg)
data(waist)

lm0 <- lm(AT~Waist,data=waist)
logAT <- log(waist$AT)
waist3 = waist$Waist^(-3)
lm3 <- lm(logAT~waist3)
library(MASS)
par(mfrow=c(1,2))
qqnorm(stdres(lm0))
qqline(stdres(lm0))
qqnorm(stdres(lm3))
qqline(stdres(lm3))

library(nortest)
shapiro.test(stdres(lm0))
ks.test(stdres(lm0), pnorm)
ad.test(stdres(lm0))
shapiro.test(stdres(lm3))
ks.test(stdres(lm3), pnorm)
ad.test(stdres(lm3))


qqnorm(stdres(lm0))
qqline(stdres(lm0))
sw <- signif(shapiro.test(stdres(lm0))$p.value,3) 
text(-1,2,paste("SW p-value", sw))
ks <- signif(ks.test(stdres(lm0), pnorm)$p.value,3) 
text(1,-2,paste("KS p-value", ks))
ad <- signif(ad.test(stdres(lm0))$p.value,3) 
text(1,-3,paste("AD p-value", ad))

qqnorm(stdres(lm3))
qqline(stdres(lm3))
sw <- signif(shapiro.test(stdres(lm3))$p.value,3) 
text(-1,2,paste("SW p-value", sw))
ks <- signif(ks.test(stdres(lm3), pnorm)$p.value,3) 
text(1,-1,paste("KS p-value", ks))
ad <- signif(ad.test(stdres(lm3))$p.value,3) 
text(1,-2,paste("AD p-value", ad))

# PQ 3.5
data("drugprice")
?drugprice
