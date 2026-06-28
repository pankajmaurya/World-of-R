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