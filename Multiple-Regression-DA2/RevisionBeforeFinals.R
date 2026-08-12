library(alr4)
data("wblake")
head(wblake)
summary(lm(Age~Length+Scale, data = wblake))


library(ordPens)
data(ICFCoreSetCWP)
help(ICFCoreSetCWP)

library(lmreg)
data("stars1")
head(stars1)
summary(lm(Velocity~Distance, data = stars1))

data("waist")
# plot AT against Waist
head(waist)
plot(waist$Waist, waist$AT)

abline(lm(AT~Waist, data = waist))


##
# Standardized residuals vs leverages
# for three sets of synthetic data
library(lmreg)
data(anscombeplus)
head(anscombeplus)
lma = lm(y1~x1, data = anscombeplus)
lme = lm(y5~x1, data = anscombeplus)
lmf = lm(y6~x2, data = anscombeplus)

library(MASS)
cbind(hatvalues(lma),hatvalues(lme),hatvalues(lmf))
plot(anscombeplus$x2,anscombeplus$y6); abline(lmf, col=2)

cbind(abs(stdres(lma)),abs(stdres(lme)),abs(stdres(lmf)))
cbind(abs(studres(lma)),abs(studres(lme)),abs(studres(lmf)))

# Checking the lme in detail as we found a large studres value of 67.1 for the 10th point 
# standarized residual was also large - 4.2346532
plot(anscombeplus$x1,anscombeplus$y5); abline(lme, col=2)

##
library(HistData)
data("Wheat")
head(Wheat)
lmw = lm(Wages~Year+I(Year^2), data = Wheat)
plot(lmw)
hatvalues(lmw)

# leverage values to be compared with 2 * (K+1)/n
# k = 2, n = 50 actually
k = 2
length(Wheat$Year)
n = length(na.omit(Wheat)$Year)

sum(hatvalues(lmw) > (2 * (k + 1) / n))
# 4 have high leverage.
sum(abs(stdres(lmw)) > 2)
# 2 have high standardized residual values

# Visually verifying this now
plot(lmw, which = 5, cook.levels = c(), add.smooth=F, id.n=0)
abline(h = 2, lty = 3)
abline(h = -2, lty = 3)
abline(v = 2 * (k + 1) / n, lty = 3)

cases = 1:n
hihi <- which(hatvalues(lmw)>2*(k+1)/n) # cases with hi leverage
hiri <- which(abs(stdres(lmw))>2) # cases with high std resid
mark <- unique(c(hihi,hiri)) # cases w high lev/stdres
text(hatvalues(lmw)[mark], stdres(lmw)[mark], cases[mark], pos=2)
mark
hihi
hiri

# consider the quadratic model of regression of Age on Length for data=wblake, library = alr4
library(alr4)
data(wblake)
lmfish = lm(wblake$Age ~ wblake$Length + I(wblake$Length^2))
# how many cases of high hi and hi ri
n = length(na.omit(wblake)$Age)
n
k = 2

# both conditions together?
which(hatvalues(lmfish)>2*(k+1)/n & abs(stdres(lmfish))>2)

cases = 1:n
hihi <- which(hatvalues(lmfish)>2*(k+1)/n) # cases with hi leverage
hiri <- which(abs(stdres(lmfish))>2) # cases with high std resid
intersect(hihi,hiri) # cases w high lev/stdres

library(lmreg)
library(car)
data("imf2015")
head(imf2015)
lmUnemploy <- lm(UNMP~CAB+DEBT+EXP+GDP+INFL+INV, data = imf2015)
summary(lmUnemploy)
# all coefficients have large P values => insignificant?
# but F statistic has small P value => not all coefficients are null.

vif(lmUnemploy)
ivif(lmUnemploy)
# larger than 30 for EXP, INV and intercept term

# Now we review the condition indices
cisv(lmUnemploy)
# condition number of 34 here.

####
data("Highway")
head(Highway)
lmhigh = lm(log(rate)~adt+trks+lane+acpt+sigs+itg+slim+len+lwid+shld+htype, data = Highway)
sum(ivif(lmhigh) > 100)


#####

# PRESS, predicted R square, AIC and BIC for synthetic data
library(MASS)
set.seed(1234)
n <- 10
beta0 <- 20
beta1 <- -1
beta2 <- -1
x1 <- rnorm(n,mean=1,sd=3)
x2 <- 0.999*x1 + rnorm(n, mean=1, sd=0.05)
err <- rnorm(n)
y <- beta0 + beta1 * x1 + beta2 * x2 + err

lm1 <-  lm(y~x1)
lm2 <-  lm(y~x2)
lm12 <- lm(y~x1+x2)

criter <- function(lmobj) {
  y <- lmobj$model[[1]]
  n <- dim(lmobj$model)[[1]]
  p <- dim(lmobj$model)[[2]]
  resids <- lmobj$res
  leverage <- hatvalues(lmobj)
  press <- sum((resids / (1 - leverage))^2)
  yvar <- sum((y - mean(y))^2) 
  predictedrsq <- 1 - press / yvar
  aic <- n * log (mean(resids^2) * 2*pi) + 2 * p + n
  bic <- n * log (mean(resids^2) * 2*pi) + log(n) * p + n
  return(c(press,predictedrsq,aic,bic))
}

# values of the criteria for the three models
crit <- cbind(criter(lm1), criter(lm2), criter(lm12))
crit <- as.data.frame(crit, row.names = 
                        c("PRESS", "Predicted R square", "AIC","BIC"))
colnames(crit) <- c("y~x1","y~x2","y~x1+x2")
crit
# All these criteria favour y~x2

######

# Search over all subsets for best regression model
library(lmreg)
library(leaps)
library(MASS)
data(imf2015)
head(imf2015)

# Regression over all subsets of CAB, DEBT, EXP, INFL, INV
help(leaps)
xlist = cbind(imf2015$CAB,imf2015$DEBT,imf2015$EXP,
              imf2015$INFL,imf2015$INV)
leaps(x = xlist, y = imf2015$UNMP, method = "Cp", nbest = 1)
# Cp favours subset of size 3
leaps(x = xlist, y = imf2015$UNMP, method = "adjr2", nbest = 1)
# Adj R sq favours subset of size 4
leaps(x = xlist, y = imf2015$UNMP, method = "r2", nbest = 1)
# R sq favours full set
# For given size, all criteria favour same subset
lm0 <- lm(UNMP ~ 1, data = imf2015)
lm1 <- lm(UNMP ~ INV, data = imf2015) # best model with 1 regressor
lm2 <- lm(UNMP ~ CAB + INV, data = imf2015) # best model size 2
lm3 <- lm(UNMP ~ EXP + INFL + INV, data = imf2015) # best model size 3
lm4 <- lm(UNMP ~ CAB + EXP + INFL + INV, data = imf2015) # best model size 4
lm5 <- lm(UNMP ~ CAB + DEBT + EXP + INFL + INV, data = imf2015) # full model
modelnames <- c("1","INV","CAB+INV","EXP+INFL+INV",
                "CAB+EXP+INFL+INV","CAB+DEBT+EXP+INFL+INV")
criter <- function(lmobj) {
  y <- lmobj$model[[1]]
  n <- dim(lmobj$model)[[1]]
  p <- dim(lmobj$model)[[2]]
  resids <- lmobj$res
  leverage <- hatvalues(lmobj)
  press <- sum((resids / (1 - leverage))^2)
  yvar <- sum((y - mean(y))^2) 
  predictedrsq <- 1 - press / yvar
  aic <- n * log (mean(resids^2) * 2*pi) + 2 * p + n
  bic <- n * log (mean(resids^2) * 2*pi) + log(n) * p + n
  return(c(press,predictedrsq,aic,bic))
}

crit <- rbind(criter(lm0), criter(lm1), criter(lm2), 
              criter(lm3), criter(lm4), criter(lm5))
crit <- as.data.frame(crit,row.names=modelnames)
colnames(crit) <- c("PRESS", "Predicted R square", "AIC","BIC")
crit
# PRESS, Predicted R sq, AIC, BIC favour lm4
# UNMP ~ CAB + EXP + INFL + INV

####

# Stepwise search over subsets for best regression model
library(lmreg)
library(MASS)
data(imf2015)
help(stepAIC)

lm0 <- lm(UNMP ~ 1, data = imf2015)
lm5 <- lm(UNMP ~ CAB + DEBT + EXP + INFL + INV, data = imf2015) # full model

# Use stepAIC of MASS 
# help(stepAIC)
# Stepwise Regression by AIC (differs by a constant)
summary(stepAIC(lm0, scope=UNMP~CAB+DEBT+EXP+INFL+INV,
                direction="forward"))$anova
# Best model by AIC is UNMP ~ INV + CAB + INFL + EXP
# Model of 3 variables is not best
summary(stepAIC(lm5, direction = "backward"))$anova
# Best model by AIC is UNMP ~ CAB + EXP + INFL + INV

######### Selection Bias Demonstration by Simulation

# Selection bias 
set.seed(1234)
n <- 10; beta0 <- 20; beta1 <- 1; beta2 <- 1
x1 <- rnorm(n); x2 <- rnorm(n); err <- rnorm(n)
y <- beta0 + beta1 * x1 + beta2 * x2 + err
b1 <- lm(y~x1)$coef[2]
b2 <- lm(y~x2)$coef[2]
c(b1,b2)

b1list <- NULL; b2list <- NULL; indlist <- NULL
for (iter in 1:1000) {
  x1 <- rnorm(n); x2 <- rnorm(n); err <- rnorm(n)
  y <- beta0 + beta1 * x1 + beta2 * x2 + err
  b1 <- lm(y~x1)$coef[2]; b2 <- lm(y~x2)$coef[2]
  ind <- 2; if(cor(y,x1)>cor(y,x2)) ind <- 1
  b1list <- c(b1list,b1); b2list <- c(b2list,b2)
  indlist <- c(indlist,ind)
}
# Generate box plots of LSEs in the 2 groups, and
boxplot(b1list~indlist,names=c("x1 winner","x2 winner"))
abline(h=1,lty=3)
boxplot(b2list~indlist,names=c("x1 winner","x2 winner"))
abline(h=1,lty=3)
# LSE of Î²1 overestimates Î²1 in the first group 
# and underestimates it in the second group
# Generate box plots of LSEs in combined sample
boxplot(b1list,names=c("all data sets"))
abline(h=1,lty=3)
boxplot(b2list,names=c("all data sets"))
abline(h=1,lty=3)
# Neither estimator is biased in the combined sample

##### Practice Quizzes Revision for Module 6
set.seed(1234)
n <- 10
beta0 <- 0
beta1 <- -2
x <- rnorm(n,mean=1,sd=3)
err <- rnorm(n)
y <- beta0 + beta1 * x + err
lm1 = lm(y~x)
xset = seq(from = min(x), to = max(x), length.out = 100)
yfit1 = lm1$coef[1] + lm1$coef[2] * xset
lm5 <- lm(y~poly(x,5,raw=T))
yfit = lm5$coef[1]
for (i in 1:5) yfit = yfit + lm5$coef[i+1] * xset^i
round(max(abs(yfit1-yfit)), 1)

#
library(lmreg)
data("girlgrowth")
lmgirl <- lm(Height~Age, data = girlgrowth)
lmgirl2 <- lm(Height~Age+I(Age^2), data = girlgrowth)
xlist = cbind(girlgrowth$Age,girlgrowth$Age^2)
leaps(x = xlist, y = girlgrowth$Height, method = "Cp", nbest = 2)
# Cp favours subset of size 2
leaps(x = xlist, y = girlgrowth$Height, method = "adjr2", nbest = 2)
# Adj R sq favours subset of size 2
leaps(x = xlist, y = girlgrowth$Height, method = "r2", nbest = 2)
# R sq favours subset of size 2

y = girlgrowth$Height
x1 = girlgrowth$Age
x2 = girlgrowth$Age^2
lm1 <-  lm(y~x1)      # Regressor: "x1 only"
lm2 <-  lm(y~x2)      # Regressor: "x2 only"
lm12 <- lm(y~x1+x2)   # Regressor: "x1 and x2"
sighat <- summary(lm12)$sigma
criteria <- function(lmobj,sig) {
  M <- summary(lmobj)
  rsq <- round(M$r.sq, 4)
  arsq <- round(M$adj.r.squared, 4)
  cp <- round(M$df[2]*(M$sigma / sig)^2 - M$df[2] + M$df[1], 4)
  return(c(rsq,arsq,cp))
}

# values of the criteria for the three models
criteria(lm1,sighat)
crit <- cbind(criteria(lm1,sighat),
              criteria(lm2,sighat),
              criteria(lm12,sighat))
crit <- as.data.frame(crit, row.names = c("R square", "Adjusted R square","Cp"))
colnames(crit) <- c("Age only","Age sq only","Age and Age sq")
crit
# R square and Adjusted R square favour y~x12 
# y~x2 is not far behind in Adjusted R square
# Cp clearly favours y~x2

library(lmreg)
data("girlgrowth")
criter <- function(lmobj) {
  y <- lmobj$model[[1]]
  n <- dim(lmobj$model)[[1]]
  p <- dim(lmobj$model)[[2]]
  resids <- lmobj$res
  leverage <- hatvalues(lmobj)
  press <- sum((resids / (1 - leverage))^2)
  yvar <- sum((y - mean(y))^2) 
  predictedrsq <- 1 - press / yvar
  aic <- n * log (mean(resids^2) * 2*pi) + 2 * p + n
  bic <- n * log (mean(resids^2) * 2*pi) + log(n) * p + n
  return(c(press,predictedrsq,aic,bic))
}

crit <- rbind(criter(lm1), criter(lm2), criter(lm12))
crit <- as.data.frame(crit,row.names=modelnames)
colnames(crit) <- c("PRESS", "Predicted R square", "AIC","BIC")
crit
# as per all the 4 criteria the lm12 model is chosen as best.

library(alr4)
data("dwaste")
head(dwaste)
summary(lm(log(O2UP)~BOD+TKN+TS+TVS+COD, data=dwaste))
# all except intercept are not significant, we need to find the subset.

xlist = cbind(dwaste$BOD,dwaste$TKN, dwaste$TS, dwaste$TVS, dwaste$COD)
leaps(x = xlist, y = log(dwaste$O2UP), method = "Cp", nbest = 1)
# Cp favours subset of size 2
leaps(x = xlist, y = log(dwaste$O2UP), method = "adjr2", nbest = 1)
# Adj R2 favours subset of size 3.

criter <- function(lmobj) {
  y <- lmobj$model[[1]]
  n <- dim(lmobj$model)[[1]]
  p <- dim(lmobj$model)[[2]]
  resids <- lmobj$res
  leverage <- hatvalues(lmobj)
  press <- sum((resids / (1 - leverage))^2)
  yvar <- sum((y - mean(y))^2) 
  predictedrsq <- 1 - press / yvar
  aic <- n * log (mean(resids^2) * 2*pi) + 2 * p + n
  bic <- n * log (mean(resids^2) * 2*pi) + log(n) * p + n
  return(c(press,predictedrsq,aic,bic))
}

crit <- rbind(criter(lm(log(O2UP)~TS+COD, data=dwaste)), criter(lm(log(O2UP)~TKN+TS+COD, data=dwaste)))
crit <- as.data.frame(crit,row.names=modelnames)
colnames(crit) <- c("PRESS", "Predicted R square", "AIC","BIC")
crit
# all 4 criteria favour smaller model