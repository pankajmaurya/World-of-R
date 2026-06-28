library(lmreg)
data("stars1")
head(stars1)
help(stars1)
lmstars1 = lm(Velocity ~ Distance, data=stars1)
summary(lmstars1)
stars1
