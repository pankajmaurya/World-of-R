#install.packages("lmreg")
library(lmreg)

data("girlgrowth")
head(girlgrowth)

plot(girlgrowth, cex=.4, xlab="Age(years)", ylab="Height (cm)")
lmgirl1 = lm(Height ~ Age, data=girlgrowth)
lmgirl1
summary(lmgirl1)

cor(girlgrowth$Height, girlgrowth$Age)^2
# Note that the correlation^2 is same as the Multiple R-squared value

abline(lmgirl1)
condmeans = NULL

for (i in 7:12) {
  condmeans = c(condmeans, mean(girlgrowth$Height[girlgrowth$Age == i]))
}
points(7:12, condmeans, col="red", pch=16)
lines(7:12, condmeans, col="red")

# conditional means line and fitted model are very close.

#### PQ 2.13 is also on the same data. but it uses a polynomial model.
lmgirl2 = lm(Height ~ poly(Age, 2, raw = T), data = girlgrowth)
summary(lmgirl2)

girlgrowth$Age2 = girlgrowth$Age^2
lmgirl2_2 = lm(Height ~ Age + Age2, data = girlgrowth)
summary(lmgirl2_2)
