library(alr4)
data("wblake")
head(wblake)
summary(lm(Age~Length+Scale, data = wblake))


library(ordPens)
data(ICFCoreSetCWP)
help(ICFCoreSetCWP)

library(lmreg)
data("stars1")
head(stars1)
summary(lm(Velocity~Distance, data = stars1))
