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

cbind(stdres(lma),stdres(lme),stdres(lmf))
cbind(studres(lma),studres(lme),studres(lmf))
plot(anscombeplus$x1,anscombeplus$y5); abline(lme, col=2)
