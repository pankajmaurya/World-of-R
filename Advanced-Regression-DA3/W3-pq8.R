rm(list=ls())
data("drugprice")
library(nortest)
library(MASS)
library(lmreg)
# PQ 3.8

data("drugprice")
?drugprice
lmdrugs <- lm(OriginatorMPR~GenericMPR, data = drugprice)
lm0 = lmdrugs

boxcox(lm0,lambda = seq(-2, 2, 1/10), plotit = TRUE)
logOriginatorMPR <- log(drugprice$OriginatorMPR)
lmdrugs2 <- lm(logOriginatorMPR~GenericMPR, data = drugprice)

shapiro.test(stdres(lmdrugs2))
ks.test(stdres(lmdrugs2), pnorm)
ad.test(stdres(lmdrugs2))

plot(drugprice$GenericMPR, stdres(lmdrugs2))

# To get box cox which gives max Rsq:
lambda <- seq(-2,4,.01)
Rsq <- NULL
logorg = logOriginatorMPR
for (lamb in lambda) {
  tx <- (drugprice$GenericMPR^lamb - 1) / lamb
  if (lamb==0) tx <- log(drugprice$GenericMPR)
  Rsq <- c(Rsq, summary(lm(logorg~tx))$r.sq)
}
lambda[which(Rsq==max(Rsq))]

# Had a silly mistake here in formula for x, fixed now.
x <- drugprice$GenericMPR^(1/3)
lmdrugs3 <- lm(logOriginatorMPR~x)
plot(drugprice$GenericMPR, stdres(lmdrugs3))
abline(h=0)


