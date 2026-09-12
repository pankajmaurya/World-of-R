rm(list=ls())
library(lmreg)
data("waist")
?waist
lmwaist <- lm(AT~Waist, weights = 1/Waist^5, data = waist)
lmwaist0 <- lm(AT~Waist, data = waist)
rsq <- summary(lmwaist0)$r.sq
r1sq <- cor(lmwaist$fitted.values,waist$AT)^2

y <- waist$AT
r2sq <- 1 - sum((y - lmwaist$fitted.values)^2)/sum((y - mean(y))^2) 
round(rsq, 4)
round(r1sq, 4)
round(r2sq, 4)
