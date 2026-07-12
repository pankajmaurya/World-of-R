rm(list=ls())
library("alr4")
data(wblake)
head(wblake)
y=wblake$Age
x1=wblake$Length
x2=wblake$Scale
m=lm(y~x1+x2)
summary(m)

# What is the leave-one-out cross validation estimate of the root mean squared error of prediction?
# Compute Root Mean Square of Prediction
# from Cross Validation
n = length(wblake$Age) # sample size
n
CrossValSumSq = 0
for (i in 1:n) {
  beta0 = lm(y[-i]~x1[-i]+x2[-i])$coef[1]
  beta1 = lm(y[-i]~x1[-i]+x2[-i])$coef[2]
  beta2 = lm(y[-i]~x1[-i]+x2[-i])$coef[3]
  CrossValSumSq = CrossValSumSq + ( y[i] - (beta0 + beta1 * x1[i] + beta2 * x2[i]) )^2 
}

RMSEPcrossval = sqrt(CrossValSumSq / n) 
round(RMSEPcrossval, 3)
round(unname(RMSEPcrossval), 3)

#write.csv(wblake, "wblake.csv", row.names = FALSE)
