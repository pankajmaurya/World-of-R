rm(list=ls())
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

# PQ 12
library(alr4)
data("dwaste")
head(dwaste)
xlist = cbind(dwaste$BOD, dwaste$TKN, dwaste$TS, dwaste$TVS, dwaste$COD)
leaps(x = xlist, y = log(dwaste$O2UP), method = "Cp", nbest = 1)
# Cp favours subset of size 2
leaps(x = xlist, y = log(dwaste$O2UP), method = "adjr2", nbest = 1)
# adj Rsq favours subset of size 3.
leaps(x = xlist, y = log(dwaste$O2UP), method = "r2", nbest = 1)
# Rsq favours full set.
# For given size, all criteria favour same subset
lm0 <- lm(log(dwaste$O2UP) ~ 1, data = dwaste)
lm1 <- lm(log(dwaste$O2UP) ~ TS, data = dwaste) # best model with 1 regressor
lm2 <- lm(log(dwaste$O2UP) ~ TS + COD, data = dwaste) # best model size 2
lm3 <- lm(log(dwaste$O2UP) ~ TKN + TS + COD, data = dwaste) # best model size 3
lm4 <- lm(log(dwaste$O2UP) ~ TKN + TS + TVS + COD, data = dwaste) # best model size 4
lm5 <- lm(log(dwaste$O2UP) ~ BOD + TKN + TS + TVS + COD, data = dwaste) # full model
modelnames <- c("1","TS","TS+COD","TKN+TS+COD",
                "TKN+TS+TVS+COD","BOD+TKN+TS+TVS+COD")

crit <- rbind(criter(lm0), criter(lm1), criter(lm2), 
              criter(lm3), criter(lm4), criter(lm5))
crit <- as.data.frame(crit,row.names=modelnames)
colnames(crit) <- c("PRESS", "Predicted R square", "AIC","BIC")
crit


modelnames <- c("TS+COD","TKN+TS+COD")
crit <- rbind(criter(lm2), criter(lm3))
crit <- as.data.frame(crit,row.names=modelnames)
colnames(crit) <- c("PRESS", "Predicted R square", "AIC","BIC")
crit
