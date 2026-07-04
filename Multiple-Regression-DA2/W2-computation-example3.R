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

library(lmreg)
data(lifelength)
help("lifelength")

historian = 0* lifelength$Category;  historian[lifelength$Category==1] = 1
poet = 0* lifelength$Category;  poet[lifelength$Category==2] = 1
painter = 0* lifelength$Category;  painter[lifelength$Category==3] = 1
musician = 0* lifelength$Category;  musician[lifelength$Category==4] = 1
mathematician_astronomer = 0* lifelength$Category;  mathematician_astronomer[lifelength$Category==5] = 1
chemist_philosopher = 0* lifelength$Category;  chemist_philosopher[lifelength$Category==6] = 1
naturalist = 0* lifelength$Category;  naturalist[lifelength$Category==7] = 1
engineer_architect_surveyor = 0* lifelength$Category;  engineer_architect_surveyor[lifelength$Category==8] = 1

# 
head(lifelength)
summary(lm(lifelength$Lifelength ~ historian + poet + painter + musician + mathematician_astronomer + chemist_philosopher + naturalist + engineer_architect_surveyor))
