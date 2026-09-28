library(alr4)
data("sleep1")
head(sleep1)
?sleep1
lmsleep <- lm(TS ~ GP + as.numeric(P) + as.numeric(D), data = sleep1)
k = 3
n <- length(sleep1$GP)

cooks.distance(lmsleep)
which(cooks.distance(lmsleep) > 4/n)
which.max(cooks.distance(lmsleep))

lm3=lmsleep
# Plot and label DEFBETAS
matplot(dfbetas(lm3), 
        xlab = "Obs. number", ylab = "DFBETAS",
        pch = c("0","1","2","3"), col = c(1,2,4,6), 
        cex = 0.7)
abline(h=0)
abline(h=2/sqrt(n), lty=3)
abline(h=-2/sqrt(n), lty=3)
legend("bottomleft",
       c("Intercept","GP","P","D"),
       lty=c(0,0,0,0), col = c(1,2,4,6),
       pch = c("0","1","2","3"), cex = 0.7)
hidb <- NULL
for (j in 1:4) {
  hidb <- c(hidb, which(abs(dfbetas(lm3)[,j]) > 2/sqrt(n)))
}
hidb <- unique(hidb)

cases <-rownames(sleep1)
hidb
cases[1]
cases[hidb]
text(hidb, rep(0.6,length(hidb)), 
     cases[hidb], srt = 90, cex=.7)


# Plot standardized resids vs leverage with const CookD curves

plot(lm3, which=5, cook.levels=c(4/n,0.5,1), add.smooth=F, sub.caption="")
abline(h=2, lty=3); abline(h=-2, lty=3)
abline(v = 2*(k+1)/n, lty = 3)
hihi <- which(hatvalues(lm3)>2*(k+1)/n) # high leverage
mark <- sort(c(hihi))
text(hatvalues(lm3)[mark], stdres(lm3)[mark], cases[mark], pos=2, cex=.7)
# African element is fine in cook's distance and stdres

# Plot cooks distance against leverage
# Plot CookD vs leverage with const standardized resid rays
plot(lm3, which=6, add.smooth=F, sub.caption="", id.n=0, pch=16, col=2, cex=0.7)
abline(h = 4/n, lty = 3)

# if leverage is 2/n
# hii / (  1 - hii)
t = 2/n
abline(v = t/(1-t))
# we want leverage less than 2/n, not cooks distance.
abline(v = (2*(k+1)/n)/(1 - (2*(k+1)/n)), lty = 3)
mark <- c(mark,hicd) # Expand earlier list with large Cook's distance cases
text(hatvalues(lm3)[mark]/(1-hatvalues(lm3)[mark]),
     cooks.distance(lm3)[mark], cases[mark], pos=1, cex=.7)
# My solution is wrong

library(alr4); data(sleep1)

lmsl = lm(TS ~ GP+as.numeric(D)+as.numeric(P), data = sleep1)

k <- 3

n <- length(lmsl$resid) # sample size 

plot(lmsl, which=6, add.smooth=F, sub.caption="", 
     
     cex=0.7, id.n=0, pch = 16, col = 2)

abline(h = 4/n, lty = 3)

abline(v = (2*(k+1)/n)/(1 - (2*(k+1)/n)), lty = 3)

hihi <- which(hatvalues(lmsl)>2*(k+1)/n)

hiri <- which(abs(stdres(lmsl))>2)

hicd <- which(cooks.distance(lmsl)>4/n)

mark <- sort(setdiff(intersect(hicd,hiri),hihi))

omitindex <- summary(lmsl)$na

cases <- row.names(sleep1)[-omitindex]

text(hatvalues(lmsl)[mark]/(1-hatvalues(lmsl)[mark]),
     
     cooks.distance(lmsl)[mark], cases[mark], pos=1, cex=.7)
