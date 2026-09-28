# Box-Cox transformation for abdominal fat data
library(lmreg)
data(waist)

lm0 <- lm(AT~Waist,data=waist)

# Carry out Box Cox transformation
par(mfrow=c(1,1))
boxcox(lm0,lambda = seq(-2, 2, 1/10), plotit = TRUE)

# Use cube root transformation of AT
tat <- (waist$AT)^(1/3)
plot(waist$Waist,tat,xlab = "Waist circumference",
     ylab = "Adipose tissue^(1/3)")

# Use quadratic term to adjust for nonlinearity
Waistsq <- waist$Waist^2
lm2 <- lm(tat~Waist+Waistsq, data=waist)
summary(lm2)

# Check nonlinearity, heteroscedasticity
plot(waist$Waist,stdres(lm2))

# Check normality
library(MASS)
qqnorm(stdres(lm2))
qqline(stdres(lm2))
sw <- signif(shapiro.test(stdres(lm2))$p.value,3) 
text(-1.5,2,paste("SW p-value", sw))
ks <- signif(ks.test(stdres(lm2), pnorm)$p.value,3) 
text(-1.5,1,paste("KS p-value", ks))
ad <- signif(ad.test(stdres(lm2))$p.value,3) 
text(-1.5,0,paste("AD p-value", ad))

#  Check normality of earlier heuristically chosen model
logAT <- log(waist$AT)
waist3 = waist$Waist^(-3)
lm3 <- lm(logAT~waist3, data = waist)

qqnorm(stdres(lm3))
qqline(stdres(lm3))
sw <- signif(shapiro.test(stdres(lm3))$p.value,3) 
text(-1.5,2,paste("SW p-value", sw))
ks <- signif(ks.test(stdres(lm3), pnorm)$p.value,3) 
text(-1.5,1,paste("KS p-value", ks))
ad <- signif(ad.test(stdres(lm3))$p.value,3) 
text(-1.5,0,paste("AD p-value", ad))

# Compare correlation of re-transformed fitted values with response
cor((lm2$fitted.values)^3,waist$AT)
cor(exp(lm3$fitted.values),waist$AT)
