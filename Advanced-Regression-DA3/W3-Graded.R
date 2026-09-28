library(lmreg)
data("imf2015")
?imf2015
head(imf2015)
# which of the following transformations of UNMP (Unemployment as % of labor force) should be regressed on EXP Government total expenditure as % of GDP), INFL (Inflation, average consumer prices in %) and INV (Total investment as % of GDP) for best adherence to normality, in terms of likelihood

lm0 <- lm(UNMP~EXP+INFL+INV, data=imf2015)

# Carry out Box Cox transformation
par(mfrow=c(1,1))
boxcox(lm0,lambda = seq(-2, 2, 1/10), plotit = TRUE)

# confirm now
lm_none <- lm(UNMP ~ EXP + INFL + INV, data = imf2015)
lm_log  <- lm(log(UNMP) ~ EXP + INFL + INV, data = imf2015)
lm_recip <- lm(1/UNMP ~ EXP + INFL + INV, data = imf2015)
lm_sqrt <- lm(sqrt(UNMP) ~ EXP + INFL + INV, data = imf2015)

models <- list(none = lm_none, log = lm_log, recip = lm_recip, sqrt = lm_sqrt)

sapply(models, function(m) shapiro.test(residuals(m))$p.value)

library(nortest)  # provides Anderson-Darling and Lilliefors (KS variant for estimated params)
sapply(models, function(m) ad.test(residuals(m))$p.value)
sapply(models, function(m) lillie.test(residuals(m))$p.value)

par(mfrow = c(2,2))
for (nm in names(models)) {
  qqnorm(residuals(models[[nm]]), main = nm)
  qqline(residuals(models[[nm]]))
}

plot(lm_log)  # residuals vs fitted, scale-location, leverage, etc.

# Q5.

rm(list=ls())
library(alr4)
data("MinnWater")
cases <- MinnWater$year
row.names(MinnWater) <- cases
lm3 <- lm(allUse~irrUse+agPrecip+statePop, data=MinnWater)
k <- 3       # number of regression parameters
n <- length(MinnWater$year) # sample size
n
MinnWater # no NA here.
# Now we need to carry out diagnostics
#cooks.distance(lm3)
which(cooks.distance(lm3) > 1)
which(cooks.distance(lm3) > 4/n)

#dffits(lm3)
plot(cases, dffits(lm3))
which(abs(dffits(lm3)) > 2*sqrt((k+1)/n))

hicd <- which(cooks.distance(lm3)>4/n) # cases with high CookD
hihi <- which(hatvalues(lm3)>2*(k+1)/n) # high leverage
hiri <- which(abs(stdres(lm3))>2) # high std resid
mark <- sort(unique(c(hihi,hiri))) # high lev/stdres
mark <- setdiff(mark,hicd) # Avoid double labelling

plot(lm3, which=6, add.smooth=F, sub.caption="", id.n=0)
abline(h = 4/n, lty = 3)
abline(v = (2*(k+1)/n)/(1 - (2*(k+1)/n)), lty = 3)
mark <- c(mark,hicd) # Expand earlier list with large Cook's distance cases
text(hatvalues(lm3)[mark]/(1-hatvalues(lm3)[mark]),
     cooks.distance(lm3)[mark], cases[mark], pos=1, cex=.7)
4/n
hihi

hatvalues(lm3)[c("2011","1990","1988")]
2*(k+1)/n

# Plot and label DFFITS
plot(dffits(lm3))
abline(h=0)
segments(1:n,0,1:n,dffits(lm3))
abline(h = sqrt(4*(k+1)/n), lty = 3)
abline(h = -sqrt(4*(k+1)/n), lty = 3)
hidft <- which(abs(dffits(lm3)) > sqrt(4*(k+1)/n))
text(hidft, dffits(lm3)[hidft], 
     cases[hidft], pos=2, cex=.7)
