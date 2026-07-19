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


###########

# Case 382 is the only one. The requisite R commands are
rm(list=ls())

library(alr4);  data(wblake)

Lengthsq = wblake$Length^2

lmlake = lm(Age~Length+Lengthsq,data=wblake)

library(MASS)

n = length(lmlake$fit); k = 2

hihi = which(hatvalues(lmlake)>2*(k+1)/n) # cases with hi leverage

hiri = which(abs(stdres(lmlake))>2) # cases with high std resid

intersect(hihi,hiri) # cases with high lev and stdres


cases = 1:n;  mark <- unique(c(hihi,hiri))

plot(lmlake, which=5, cook.levels=c(), add.smooth=F, id.n=0)

abline(h=2, lty=3); abline(h=-2, lty=3)

abline(v = 2*(k+1)/n, lty = 3)

text(hatvalues(lmlake)[mark], stdres(lmlake)[mark], cases[mark], pos=2)



cases = 1:n;  mark <- unique(c(hihi,hiri))

plot(lmlake, which=5, cook.levels=c(), add.smooth=F, id.n=0)

abline(h=2, lty=3); abline(h=-2, lty=3)
