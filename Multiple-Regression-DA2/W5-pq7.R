rm(list=ls())
library(alr4)
head(wblake)
# We have to consider the quadratic model
lmw = lm(Age~Length + I(Length^2), data = wblake)

library(MASS)
hatvalues(lmw)

n = length(wblake$Age)
length(wblake$Length)
plot(hatvalues(lmw),stdres(lmw))
plot(lmw, which=5, cook.levels=c(), add.smooth=F, id.n=0)
abline(h=2, lty=3); abline(h=-2, lty=3)
k = 2
abline(v = 2*(k+1)/n, lty = 3)

# which cases are high leverage , high residual?
cases = 1:n
hihi <- which(hatvalues(lmw)>2*(k+1)/n) # cases with hi leverage
hiri <- which(abs(stdres(lmw))>2) # cases with high std resid
mark <- unique(c(hihi,hiri)) # cases w high lev/stdres
text(hatvalues(lmw)[mark], stdres(lmw)[mark], cases[mark], pos=2)
