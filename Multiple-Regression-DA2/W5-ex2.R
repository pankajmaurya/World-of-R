# Plots of standardized residuals vs leverages
# for three sets of synthetic data
library(lmreg)
data(anscombeplus)
head(anscombeplus)
lma = lm(y1~x1, data = anscombeplus)
lme = lm(y5~x1, data = anscombeplus)
lmf = lm(y6~x2, data = anscombeplus)

library(MASS)
plot(hatvalues(lme),stdres(lme))
plot(lme, which=5, cook.levels=c(), add.smooth=F, id.n=0)
abline(h=2, lty=3); abline(h=-2, lty=3)
n = length(anscombeplus$x1); k = 1
abline(v = 2*(k+1)/n, lty = 3)

cases = 1:n
hihi <- which(hatvalues(lme)>2*(k+1)/n) # cases with hi leverage
hiri <- which(abs(stdres(lme))>2) # cases with high std resid
mark <- unique(c(hihi,hiri)) # cases w high lev/stdres
text(hatvalues(lme)[mark], stdres(lme)[mark], cases[mark], pos=2)

plot(lmf, which=5, cook.levels=c(), add.smooth=F, id.n=0)
abline(h=2, lty=3); abline(h=-2, lty=3)
abline(v = 2*(k+1)/n, lty = 3)

hihi <- which(hatvalues(lmf)>2*(k+1)/n) # cases with hi leverage
hiri <- which(abs(stdres(lmf))>2) # cases with high std resid
mark <- unique(c(hihi,hiri)) # cases w high lev/stdres
text(hatvalues(lmf)[mark], stdres(lmf)[mark], cases[mark], pos=2)
