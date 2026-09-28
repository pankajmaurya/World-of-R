# Plots for case influence in IMF unemployment data 
rm(list=ls())
library(lmreg)
data(imf2015)
cases <- imf2015$Country
row.names(imf2015) <- cases
lm3 <- lm(UNMP ~ EXP + INFL + INV, data = imf2015)
k <- 3       # number of regression parameters
n <- length(imf2015$UNMP) # sample size

# Plot and label Cook's distance
plot(lm3, which=4, sub.caption="", id.n=0)
abline(h = 1, lty = 3)
abline(h = 4/n, lty = 3)
hicd <- which(cooks.distance(lm3)>4/n) # cases with high CookD
text(hicd, cooks.distance(lm3)[hicd], 
     cases[hicd], pos=2, cex=.7)

# Plot and label DFFITS
plot(dffits(lm3))
abline(h=0)
segments(1:n,0,1:n,dffits(lm3))
abline(h = sqrt(4*(k+1)/n), lty = 3)
abline(h = -sqrt(4*(k+1)/n), lty = 3)
hidft <- which(abs(dffits(lm3)) > sqrt(4*(k+1)/n))
text(hidft, dffits(lm3)[hidft], 
     cases[hidft], pos=2, cex=.7)

# Plot and label DEFBETAS
matplot(dfbetas(lm3), 
        xlab = "Obs. number", ylab = "DFBETAS",
        pch = c("0","1","2","3"), col = c(1,2,4,6), 
        cex = 0.7)
abline(h=0)
abline(h=2/sqrt(n), lty=3)
abline(h=-2/sqrt(n), lty=3)
legend("bottomleft",
       c("Intercept","EXP","INFL","INV"),
       lty=c(0,0,0,0), col = c(1,2,4,6),
       pch = c("0","1","2","3"), cex = 0.7)
hidb <- NULL
for (j in 1:4) {
  hidb <- c(hidb, which(abs(dfbetas(lm3)[,j]) > 2/sqrt(n)))
}
hidb <- unique(hidb)
text(hidb, rep(-0.8,length(hidb)), 
     cases[hidb], srt = 90, cex=.7)

# Plot and label COVRATIO
plot(covratio(lm3), type="n")
abline(h=1)
segments(1:n,1,1:n,covratio(lm3))
abline(h = 1 + 3*(k+1)/n, lty = 3)
abline(h = 1 - 3*(k+1)/n, lty = 3)
hicr <- which(abs(covratio(lm3) - 1) > 3*(k+1)/n)
text(hicr, covratio(lm3)[hicr], 
     cases[hicr], pos=2, cex=.7)

# Plot standardized resids vs leverage with const CookD curves

plot(lm3, which=5, cook.levels=c(4/n,0.5,1), add.smooth=F, sub.caption="")
abline(h=2, lty=3); abline(h=-2, lty=3)
abline(v = 2*(k+1)/n, lty = 3)
hihi <- which(hatvalues(lm3)>2*(k+1)/n) # high leverage
hiri <- which(abs(stdres(lm3))>2) # high std resid
mark <- sort(unique(c(hihi,hiri))) # high lev/stdres
mark <- setdiff(mark,hicd) # Avoid double labelling
text(hatvalues(lm3)[mark], stdres(lm3)[mark], cases[mark], pos=2, cex=.7)

# Plot CookD vs leverage with const standardized resid rays
plot(lm3, which=6, add.smooth=F, sub.caption="", id.n=0)
abline(h = 4/n, lty = 3)
abline(v = (2*(k+1)/n)/(1 - (2*(k+1)/n)), lty = 3)
mark <- c(mark,hicd) # Expand earlier list with large Cook's distance cases
text(hatvalues(lm3)[mark]/(1-hatvalues(lm3)[mark]),
     cooks.distance(lm3)[mark], cases[mark], pos=1, cex=.7)
