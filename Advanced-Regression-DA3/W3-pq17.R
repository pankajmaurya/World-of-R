rm(list=ls())
library(alr4)
data("sleep1")
head(sleep1)
sleep1
lmsleep <- lm(TS ~ GP + as.numeric(P) + as.numeric(D), data = sleep1)
k = 3
#n <- length(sleep1$GP)
n <- length(lmsleep$resid) # sample size 
which(cooks.distance(lmsleep) > 4/n)
which.max(cooks.distance(lmsleep))
hihi <- which(hatvalues(lmsleep)>2*(k+1)/n) # high leverage
hihi
subgroup <-sleep1[-1,][-4,]
plot(subgroup$GP)
abline(h=400)
subgroup[which(subgroup$GP>400 ), ] # Okapi has GS > 400
subgroup[which(subgroup$P==3 ), ] # many have P = 3
subgroup[which(subgroup$TS<4 ), ] # many have TS< 4
subgroup[which(subgroup$D!= 3 &  subgroup$D!= 4), ] # many have 

lmsleep2 <- lm(TS ~ GP + as.numeric(P) + as.numeric(D), data = subgroup)

summary(lmsleep)
summary(lmsleep2)

which(cooks.distance(lmsleep) > 4/n)
which(cooks.distance(lmsleep2) > 4/n)
4/n
40/n
which(cooks.distance(lmsleep) > 40/n)
which(cooks.distance(lmsleep2) > 40/n)

which(hatvalues(lmsleep)>2*(k+1)/n) # high leverage
which(hatvalues(lmsleep2)>2*(k+1)/n) # high leverage

which(hatvalues(lmsleep)>4*(k+1)/n) # high leverage
which(hatvalues(lmsleep2)>4*(k+1)/n) # high leverage

plot(lmsleep, which=5, cook.levels=c(4/n,0.5,1), add.smooth=F, sub.caption="")
plot(lmsleep, which=6, add.smooth=F, sub.caption="", id.n=0, pch=16, col=2, cex=0.7)
plot(lmsleep2, which=6, add.smooth=F, sub.caption="", id.n=0, pch=16, col=2, cex=0.7)


# Partly wrong analysis, Okapi was to be excluded.
lmsl = lm(TS ~ GP+as.numeric(D)+as.numeric(P), data = sleep1)

omitindex <- summary(lmsl)$na
omitindex

sleep2 = sleep1[-c(omitindex,1,5),]

range(sleep2$GP)

sort(sleep2$D)

sort(sleep2$P)

range(sleep2$TS)



lmsl2 = lm(TS ~ GP+as.numeric(D)+as.numeric(P), data = sleep2)


k <- 3

n <- length(lmsl2$resid) # sample size 

plot(lmsl2, which=6, add.smooth=F, sub.caption="", 
     
     cex=0.7, id.n=0, pch = 16, col = 2)

abline(h = 4/n, lty = 3)

abline(v = (2*(k+1)/n)/(1 - (2*(k+1)/n)), lty = 3)

hihi <- which(hatvalues(lmsl2)>2*(k+1)/n)

hiri <- which(abs(stdres(lmsl2))>2)

hicd <- which(cooks.distance(lmsl2)>4/n)

mark <- sort(unique(c(hihi,hiri,hicd)))

cases <- row.names(sleep2)

text(hatvalues(lmsl2)[mark]/(1-hatvalues(lmsl2)[mark]),
     
     cooks.distance(lmsl2)[mark], cases[mark], pos=1, cex=.7)
