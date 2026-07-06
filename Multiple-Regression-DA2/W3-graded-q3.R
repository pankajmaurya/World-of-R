# Code from lectures.

#install.packages("HistData")
library(HistData)
data(GaltonFamilies)
head(GaltonFamilies)
help(GaltonFamilies)
y <- GaltonFamilies$childHeight[GaltonFamilies$gender=="male" &
                                  GaltonFamilies$childNum==1]
x1 <- GaltonFamilies$father[GaltonFamilies$gender=="male" &
                              GaltonFamilies$childNum==1]
x2 <- GaltonFamilies$mother[GaltonFamilies$gender=="male" &
                              GaltonFamilies$childNum==1]
lmGF1 <- lm(y~x1+x2)
lmGF1
# Test whether father's height has a stronger effect, 
# in relation to mother's height, on height of first born son 
# Claude: This is a one-sided test of whether the father's coefficient exceeds the mother's coefficient
library(lmreg)
help(hyptest)

p <- c(0,1,-1)
hyptest(lmGF1, p, type = "upper")

# Since the p-value (0.0226) is less than the conventional 0.05 significance level, you reject H0
# There is statistically significant evidence, at the 5% level, that the father's height has a 
# larger positive effect on the first-born son's height than the mother's height does, 
# after accounting for both variables in the model.
################################## 

y <- GaltonFamilies$childHeight[GaltonFamilies$gender=="male" &
                                  GaltonFamilies$childNum==1]
# In regression of first born son's height on midparentHeight,
# test whether regression coefficient is greater than one 
# Claude: This tests the slope on midparentHeight against a hypothesized value of 1 
# (i.e., testing whether a son's height changes less-than, equal-to, or more-than one-for-one with midparent height).
help(GaltonFamilies)
x <- GaltonFamilies$midparentHeight[GaltonFamilies$gender=="male" &
                                      GaltonFamilies$childNum==1]
lmGF2 <- lm(y~x)
lmGF2
p <- c(0,1)
hyptest(lmGF2, p, xi = 1, type = "upper")
hyptest(lmGF2, p, xi = 1, type = "lower")

####################### 
# PQ 3.10
# Repeat the regression analysis of the GaltonFamilies data presented in the foregoing video, by using the mature height of first born daughter as the response. 
# The objective is to test whether the heights tend to move towards the middle over generations. 
# Please be careful about the formulation of the hypothesis, as the midparent height is defined as the average of the father's height 
# and a male equivalent of the mother's height (obtained by scaling up the mother's height by the factor 1.08). Report the p-value of the test. 
y <- GaltonFamilies$childHeight[GaltonFamilies$gender=="female" &
                                  GaltonFamilies$childNum==1]
x <- GaltonFamilies$midparentHeight[GaltonFamilies$gender=="female" &
                                      GaltonFamilies$childNum==1]
lmGF3 <- lm(y~x)
lmGF3
p <- c(0,1)
hyptest(lmGF3, p, xi = 1/1.08, type = "upper")
hyptest(lmGF3, p, xi = 1/1.08, type = "lower")


###### Solution of PQ
library(HistData); data(GaltonFamilies)

y <- GaltonFamilies$childHeight[GaltonFamilies$gender=="female" & GaltonFamilies$childNum==1]
x <- GaltonFamilies$midparentHeight[GaltonFamilies$gender=="female" & GaltonFamilies$childNum==1]

lmGF4 <- lm(y~x)
library(lmreg)
p <- c(0,1)

hyptest(lmGF4, p, xi = 1/1.08, type = "lower")

##### Graded now
# test whether the mature height of first born daughter is affected more by the height of the father than by the height of the mother.
# Report the p-value of the test.

y <- GaltonFamilies$childHeight[GaltonFamilies$gender=="female" &
                                  GaltonFamilies$childNum==1]
x1 <- GaltonFamilies$father[GaltonFamilies$gender=="female" &
                              GaltonFamilies$childNum==1]
x2 <- GaltonFamilies$mother[GaltonFamilies$gender=="female" &
                              GaltonFamilies$childNum==1]
lmGF1 <- lm(y~x1+x2)
lmGF1
# Test whether father's height has a stronger effect, 
# in relation to mother's height, on height of first born son 
# Claude: This is a one-sided test of whether the father's coefficient exceeds the mother's coefficient
library(lmreg)
help(hyptest)

p <- c(0,1,-1)
hyptest(lmGF1, p, type = "upper")
round(0.7516369, 2)
