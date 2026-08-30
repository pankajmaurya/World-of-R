library(alr4)
data("sniffer")

?sniffer
lm1 <- lm(Y~TankTemp+GasTemp+TankPres+GasPres,data=sniffer)
library(MASS)
x=sniffer$TankPres
e2 = stdres(lm1)^2
plot(x,e2,ylab="Standardized residual squared")
lines(sort(x),loess(e2[order(x)]~sort(x),span=2)$fit)
