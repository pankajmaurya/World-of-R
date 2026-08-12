### Please note that the answers and the R code is inline.
library(alr4)
data("dwaste")
head(dwaste)
################# Part 1: 25055
n = 25055
set.seed(n);  y <- log(dwaste$O2UP) + 0.01*rnorm(length(dwaste$O2UP))
################# Part 2 comments 
plot(dwaste$BOD, y)
# We observe variance of y increases with increase in BOD, for lower values of BOD in range 0 - 600, the correlation is very weak with y
plot(dwaste$TKN, y)
# The plot of y against TKN has some outliers - notably around TKN = 240
# Further the relationship does not look linear and the correlation seems weak.
plot(dwaste$TS, y)
# No unusual patterns here except the outlier for Case 1
plot(dwaste$TVS, y)
# The plot indicates that we do not have data for TVS 60 - 70
# We also have an outlier case for TVS = 57.7, this has high leverage as it is away from mean TVS
# Another outlier seems to be case 1 with very high y value (for TVS = 85.9).
plot(dwaste$COD, y)
# No unusual patterns here, good linear relation observed. Case 1 has high leverage as it is far from mean COD value.

################ Part 3
m = lm(y~dwaste$BOD+dwaste$TKN+dwaste$TS+dwaste$TVS+dwaste$COD)
summary(m)
# None of the coefficients are significant at 5% level.
# Fitted equation:
# y = -4.943 -2.361e-05*BOD + 3.072e-03*TKN + 2.957e-04*TS + 1.771e-02*TVS + 3.261e-04*COD

################ Part 4
# The p-value of the F-statistic for the significance of the regressors is 0.0001264

################ Part 5
# The coefficients are individually not significant as per 3.
# As per part 4, we reject H0 that all regressors are zero.
# There are no contradictions here, this is quite possible with collinearity.
# We see large values of IVIF for TVS regression coefficient of dwaste$TVS and the intercept.
# We also see high vif for BOD. Hence indicators of collinearity present.
ivif(m)
vif(m)
################ Part 6
plot(hatvalues(m), stdres(m))
plot(m, which=5, cook.levels=c(), add.smooth=F, id.n=0)
abline(h=2, lty=3); abline(h=-2, lty=3)
num = length(dwaste$O2UP)
num
k = 5
abline(v = 2*(k+1)/num, lty = 3)
cases = 1:num
hihi <- which(hatvalues(m)>2*(k+1)/num) # cases with hi leverage
hiri <- which(abs(stdres(m))>1.5) # cases with high std resid
mark <- unique(c(hihi,hiri)) # cases w high lev/stdres
text(hatvalues(m)[mark], stdres(m)[mark], cases[mark], pos=2)
# We examined the standardized residual vs leverage plot
# We have a high leverage point - Case 17
# and a large residual point - Case 1 which has an outlier y value.

################ Part 7
# linkage between plot of standardized residuals and scatter plots:
# Plot of y vs dwaste$BOD clearly showed Case 1 as an outlier with highest y
# Plot of y vs dwaste$TKN clearly showed Case 1 as an outlier with highest y
# Plot of y vs dwaste$TS clearly showed Case 1 as an outlier with highest y
# Plot of y vs dwaste$TVS clearly showed Case 1 as an outlier with highest y

################ Part 8
# We will use Mallow's Cp for finding the best subset model
xlist = cbind(dwaste$BOD, dwaste$TKN, dwaste$TS, dwaste$TVS, dwaste$COD)
oury = y
leaps(x = xlist, y = oury, method = "Cp", nbest = 1)
# We see Cp is min for size = 2 with value = 1.744414
# Min Cp model is lm(y~dwaste$TS+dwaste$COD)

################ Part 9
m2 = lm(y~dwaste$TS+dwaste$COD)
summary(m2)
# Both coefficients are significant
# Fitted equation is y = -3.1542591 + 0.0003437 * TS + 0.0003258 * COD
# p-value of F-statistic is 2.12e-06 (reject H0 that both beta coefficients are zero)
# No contradiction now, coefficients are individually significant as well as collectively not all zero.

################ Part 10
# RMSEP estimation from smaller model is 0.6254
k = 2
RMSEPmodelbased = sqrt((1 + (k+1)/num) * (summary(m2)$sig^2)) 
round(RMSEPmodelbased, 4)

# Comparing with full model, RMSEP is 0.6909
k = 5
RMSEPmodelbasedOld = sqrt((1 + (k+1)/num) * (summary(m)$sig^2)) 
round(RMSEPmodelbasedOld, 4)

### We see that the smaller model is having a lower RMSEP than the full model.

# Further we can also compare these numbers by the cross validation approach.

CrossValSumSq = 0
for (i in 1:num) {
  beta0 = lm(y[-i]~dwaste$BOD[-i]+dwaste$TKN[-i]+dwaste$TS[-i]+dwaste$TVS[-i]+dwaste$COD[-i])$coef[1]
  beta1 = lm(y[-i]~dwaste$BOD[-i]+dwaste$TKN[-i]+dwaste$TS[-i]+dwaste$TVS[-i]+dwaste$COD[-i])$coef[2]
  beta2 = lm(y[-i]~dwaste$BOD[-i]+dwaste$TKN[-i]+dwaste$TS[-i]+dwaste$TVS[-i]+dwaste$COD[-i])$coef[3]
  beta3 = lm(y[-i]~dwaste$BOD[-i]+dwaste$TKN[-i]+dwaste$TS[-i]+dwaste$TVS[-i]+dwaste$COD[-i])$coef[4]
  beta4 = lm(y[-i]~dwaste$BOD[-i]+dwaste$TKN[-i]+dwaste$TS[-i]+dwaste$TVS[-i]+dwaste$COD[-i])$coef[5]
  beta5 = lm(y[-i]~dwaste$BOD[-i]+dwaste$TKN[-i]+dwaste$TS[-i]+dwaste$TVS[-i]+dwaste$COD[-i])$coef[6]
  
  CrossValSumSq = CrossValSumSq + ( y[i] - (beta0 + beta1 * dwaste$BOD[i] + beta2 * dwaste$TKN[i] + beta3 * dwaste$TS[i] + beta4 * dwaste$TVS[i] + beta5 * dwaste$COD[i]) )^2 
}

RMSEPcrossval = sqrt(CrossValSumSq / num) 
round(RMSEPcrossval, 4)
# gave 0.8710
CrossValSumSq2 = 0
for (i in 1:num) {
  beta0 = lm(y[-i]~dwaste$TS[-i]+dwaste$COD[-i])$coef[1]
  beta1 = lm(y[-i]~dwaste$TS[-i]+dwaste$COD[-i])$coef[2]
  beta2 = lm(y[-i]~dwaste$TS[-i]+dwaste$COD[-i])$coef[3]
  CrossValSumSq2 = CrossValSumSq2 + ( y[i] - (beta0 + beta1 * dwaste$TS[i] + beta2 * dwaste$COD[i]))^2 
}

RMSEPcrossval2 = sqrt(CrossValSumSq2 / num) 
round(RMSEPcrossval2, 4)
# gave 0.6946

# We see by the cross validation approach the RMSEP is lower for the parsimonious model