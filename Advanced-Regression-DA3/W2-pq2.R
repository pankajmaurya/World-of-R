# PQ 2.2
library(lmreg); data("waist")

lmAT = lm(AT~Waist, data=waist)

Waist = waist$Waist

plot(Waist,stdres(lmAT),ylab="Standardized residual")

lmAT1 = lm(AT~Waist,data=waist,weights = 1/Waist)

lmAT2 = lm(AT~Waist,data=waist,weights = 1/Waist^2)

lmAT3 = lm(AT~Waist,data=waist,weights = 1/Waist^3)

lmAT5 = lm(AT~Waist,data=waist,weights = 1/Waist^5)

par(mfrow=c(2,2))

plot(Waist,stdres(lmAT1),ylab="Standardized residual")
e2 = stdres(lmAT1)^2
x = waist$Waist
lines(sort(x),loess(e2[order(x)]~sort(x),span=2)$fit)

title("weights = 1/Waist")

plot(Waist,stdres(lmAT2),ylab="Standardized residual")
e2 = stdres(lmAT2)^2
x = waist$Waist
lines(sort(x),loess(e2[order(x)]~sort(x),span=2)$fit)
title("weights = 1/Waist^2")

plot(Waist,stdres(lmAT3),ylab="Standardized residual")
e2 = stdres(lmAT3)^2
x = waist$Waist
lines(sort(x),loess(e2[order(x)]~sort(x),span=2)$fit)
title("weights = 1/Waist^3")

plot(Waist,stdres(lmAT5),ylab="Standardized residual")
e2 = stdres(lmAT5)^2
x = waist$Waist
lines(sort(x),loess(e2[order(x)]~sort(x),span=2)$fit)
title("weights = 1/Waist^5")



library(lmreg)
data("waist")
?waist 
lmwaist <- lm(AT~Waist, data=waist)
plot(waist$Waist, stdres(lmwaist))

par(mfrow=c(1,1))
e2 = stdres(lmwaist)^2
plot(waist$Waist,e2,ylab="Standardized residual squared")
x = waist$Waist
lines(sort(x),loess(e2[order(x)]~sort(x),span=2)$fit)
