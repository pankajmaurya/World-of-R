library(MASS)
data("birthwt")
head(birthwt)
help(birthwt)

lmbwt1=lm(bwt~lwt+smoke+race, data = birthwt)
summary(lmbwt1)

#install.packages("alr4")
library(alr4)
data("wblake")
head(wblake)
wblakelm=lm(Age~Length+Scale, data = wblake)
summary(wblakelm)

## Graded stuff also uses the same dataset
agelist = unique(wblake$Age)
agelist
condmeans = NULL

for (i in agelist) {
  condmeans = c(condmeans, mean(wblake$Length[wblake$Age==i]))
}

summary(lm(condmeans~agelist))
agelist2 = agelist^2
agelist
agelist2
summary(lm(condmeans~agelist+agelist2))

summary(lm(Length ~ poly(Age, 2, raw = T), data = wblake))

