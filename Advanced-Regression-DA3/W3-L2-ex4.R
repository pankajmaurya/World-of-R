rm(list=ls())
# Bootstrap in fish data - confidence intervals and testing
library(alr4)
data("wblake")
head(wblake)
Lensq <- wblake$Length^2
lmfish <- lm(Age ~ Length + Lensq, data = wblake)

# Check plots of standardized residuals
library(MASS)
plot(wblake$Length,stdres(lmfish))
plot(lmfish$fitted.values,stdres(lmfish))

# Check normality
qqnorm(stdres(lmfish))
qqline(stdres(lmfish))
sw <- signif(shapiro.test(stdres(lmfish))$p.value,3) 
text(-1.5,2, paste("SW p-value", sw), cex = .7)
ad <- signif(ad.test(stdres(lmfish))$p.value,3) 
text(-1.5,1, paste("AD p-value", ad), cex = .7)

# Summary of LS estimates, confidence limits
summary(lmfish)[[4]]
confint(lmfish)

# Nonparametric bootstrap confidence intervals
set.seed(1234); nboot <- 1000; betanp <- NULL
n <- length(wblake$Age)
for (i in 1:nboot) {
  ib <- sample((1:n), size = n, replace = T)
  yb <- wblake$Age[ib]
  x1b <- wblake$Length[ib]
  x2b <- Lensq[ib]
  mboot <- lm(yb ~ x1b + x2b)
  betanp <- rbind(betanp, mboot$coefficients)
}

confnp <- quantile(betanp[,1],probs = c(0.025,0.975))
confnp <- rbind(confnp, quantile(betanp[,2],probs = c(0.025,0.975)))
confnp <- rbind(confnp, quantile(betanp[,3],probs = c(0.025,0.975)))
confnp
# for comparison - confidence intervals for normal theory
confint(lmfish)

# Model based bootstrap confidence intervals
respool = lmfish$residuals
set.seed(1234); nboot <- 1000;  beta <- NULL
for (i in 1:nboot) {
  yb <- lmfish$fitted.values +
    sample(respool, size = n, replace = T)
  mboot <- lm(yb ~ Length + Lensq, data = wblake)
  beta <- rbind(beta, mboot$coefficients)
}
confp <- quantile(beta[,1],probs = c(0.025,0.975))
confp <- rbind(confp, quantile(beta[,2],probs = c(0.025,0.975)))
confp <- rbind(confp, quantile(beta[,3],probs = c(0.025,0.975)))
confp


# Model based Bootstrap for Testing 
# Null hypothesis is beta0 = -2
# Lower sided test, alternative is beta0 < -2. 
hist(beta[,1])
abline(v=-2, col=2)
length(which(beta[,1] < -2)) / length(beta[,1]) 
# coverage prob of (-âˆž,-2]

length(which(beta[,1] > -2)) / length(beta[,1]) 
# 1 sided pvalue
# p value came to be 0.084 => p > 5%, hence accept Null hypo
# two sided test
Pestminus  <- length(which(beta[,1] > -2)) / length(beta[,1])
Pestplus  <- 1 - Pestminus
2*min(Pestplus, Pestminus) # 2 sided pvalue = 0.168, hence accept Null Hypothesis.
# Contrast test based on normal theory
library(lmreg); p = c(1,0,0)
hyptest(lmfish, p, xi = -2, type = "lower")

######################## PQ 3.10
library(bnlearn)
data(marks)
?marks
plot(marks$STAT)
lmmarks <- lm(ANL~ALG+STAT, data=marks)
summary(lmmarks)
plot(lmmarks)

# Nonparametric bootstrap confidence intervals
set.seed(1234); nboot <- 1000; betanp <- NULL
n <- length(marks$MECH)
for (i in 1:nboot) {
  ib <- sample((1:n), size = n, replace = T)
  yb <- marks$ANL[ib]
  x1b <- marks$ALG[ib]
  x2b <- marks$STAT[ib]
  mboot <- lm(yb ~ x1b + x2b)
  betanp <- rbind(betanp, mboot$coefficients)
}

confnp <- quantile(betanp[,1],probs = c(0.025,0.975))
confnp <- rbind(confnp, quantile(betanp[,2],probs = c(0.025,0.975)))
confnp <- rbind(confnp, quantile(betanp[,3],probs = c(0.025,0.975)))
confnp

confint(lmmarks)

# Now using a parametric bootstrap.
respool = lmmarks$residuals
x1 <- marks$ALG
x2 <- marks$STAT
n <- length(x1)
newdat = data.frame(x1 = c(50), x2 = c(40))
set.seed(123); nboot <- 1000;  beta <- NULL; predlist <- NULL

for (i in 1:nboot) {
  yb <- lmmarks$fitted.values +
    sample(respool, size = n, replace = T)
  mboot <- lm(yb ~ x1 + x2)
  bootfit <- predict(mboot, newdat, interval = "none")
  beta <- rbind(beta, mboot$coefficients)
  predlist <- rbind(predlist, bootfit
                    + sample(respool, size = 1, replace = T))
}
quantile(predlist[,1], probs = 0.975)
round(quantile(predlist[,1], probs = 0.95))
# Testing the hypothesis that STAT has no effect on ANL

# Model based Bootstrap for Testing 
# Null hypothesis is beta0 = -2
# Lower sided test, alternative is beta0 < -2. 
hist(beta[,3])
abline(v=0, col=2)
length(which(beta[,3] < 0)) / length(beta[,3]) 
# coverage prob of (-âˆž,-2]

length(which(beta[,3] > 0)) / length(beta[,3]) 
# 1 sided pvalue
# p value came to be 0.084 => p > 5%, hence accept Null hypo
# two sided test
Pestminus  <- length(which(beta[,3] > 0)) / length(beta[,3])
Pestplus  <- 1 - Pestminus
2*min(Pestplus, Pestminus) # 2 sided pvalue = 0.168, hence accept Null Hypothesis.
# Contrast test based on normal theory
library(lmreg); p = c(0,0,1)
hyptest(lmmarks, p, xi = 0, type = "both")

# for Q2 solution given is below:
library(bnlearn); data(marks)
lmarks = lm(ANL~ALG+STAT, data = marks)
respool = lmarks$residuals;  n <- length(marks$ANL)
set.seed(1234); nboot <- 1000;  theta <- NULL

for (i in 1:nboot) {
  yb <- lmarks$fitted.values +
    sample(respool, size = n, replace = T)
  
  mboot <- lm(yb ~ ALG + STAT, data = marks)
  theta <- c(theta, 
             mboot$coef[1] + mboot$coef[2]*50 + mboot$coef[3]*40)
  
}
round(quantile(theta,probs = 0.95)) 
