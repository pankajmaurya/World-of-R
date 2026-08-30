# plots for detecting nonlinearity, simulated data examples

set.seed(9999)
n <- 20
x1 <- 10 + 2*rnorm(n)
x2 <- 20 - x1 + 3*rnorm(n)
x3 <- 0.5 * (x1 + x2) + 3*rnorm(n)
y <- 5 + 1 * x1 + 1 * x2 + 
  0.1 * x3^2 + rnorm(n)

lm1 <- lm(y~x1+x2+x3)
par(mfrow=c(1,3))
plot(x1,y); plot(x2,y); plot(x3,y)

library(car)
crPlots(lm1,smooth=F) # Component plus residual plot
avPlots(lm1) # Added variable plot

# PQ 1.7
library(alr4)
data(UN11)
head(UN11)
lm2 <- lm(fertility~log(ppgdp)+factor(group), data = UN11)
crPlots(lm2, smooth=F)
summary(lm2)
?UN11

head(UN11)
UN11["IsAfrica"] <- UN11["group"] == "africa"

lm3 <- lm(fertility~log(ppgdp)+IsAfrica, data = UN11)
crPlots(lm3,smooth=T)
avPlots(lm3) # Added variable plot

# Actual solution
library(lmreg)
Africa = binaries(UN11$group)[,2]
lmfer = lm(fertility~log(ppgdp)+Africa,data=UN11)
crPlots(lmfer)
avPlots(lmfer)




