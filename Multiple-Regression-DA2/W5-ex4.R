# Measures of collinearity for 
# IMF unemployment country data
library(lmreg)
library(car)
data(imf2015)
head(imf2015)
help("imf2015")
lmUnemploy <- lm(UNMP~CAB+DEBT+EXP+GDP+INFL+INV, data = imf2015)
summary(lmUnemploy)
# All regression coefficients insignificant, Individually probability are larger than 0.1 for all coefficients
# but p-value of F-test is small, so we cannot accept the H0 that all coefficients are zero.

vif(lmUnemploy)  # Variance Inflation factors
ivif(lmUnemploy) # Intercept adjusted VIFs
# Three IVIFs more than 30 => these indicate some level of collinearity here

cisv(lmUnemploy) # Table of condition indices and singular vectors
# One condition index (condition number) more than 30
# There is collinearity involving intercept

library(alr4)
data("Highway")
head(Highway)
lmHighway <- lm(log(rate)~adt+trks+lane+acpt+sigs+itg+slim+len+lwid+shld, data = Highway)
ivif(lmHighway)
sum(ivif(lmHighway) > 100)
round(max(vif(lmHighway)), 2)
cisv(lmHighway)


#############
library(alr4);  data("Highway")

lograte = log(Highway$rate)

lmrate = lm(lograte~adt+len+trks+lane+acpt+sigs+itg+slim+lwid+shld,data=Highway)

library(lmreg);  ivif(lmrate)
library(car)
vif(lmrate)
round(max(vif(lmHighway)), 2)
