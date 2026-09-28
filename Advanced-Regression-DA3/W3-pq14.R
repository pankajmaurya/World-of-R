library(alr4)
data(sleep1)
?sleep1
head(sleep1)
lmsleep <- lm(TS~GP + P + D, data=sleep1)
lmsleep <- lm(TS ~ GP + as.numeric(P) + as.numeric(D), data = sleep1)

nmax = length(sleep1$GP)
n <- nobs(lmsleep)

which(cooks.distance(lmsleep) > 4/n)
which.max(cooks.distance(lmsleep))
# number of cases with high influence on fit in terms of DFFITS

dffits(lmsleep)
plot(dffits(lmsleep))
abline(h=2*sqrt(4/n))
abline(h=-2*sqrt(4/n))
k = 3
which(abs(dffits(lmsleep)) > 2*sqrt((k+1)/n))


# Report most unusual value of COVRATIO

covratio(lmsleep)
which(abs(covratio(lmsleep) - 1) > 3*(k+1)/n)

influence.measures(lmsleep)
