# M2L4V2
rm(list=ls())

library(lmreg)
data("girlgrowth")
lmgirl2 = lm(Height~poly(Age,2,raw=T),data=girlgrowth)
summary(lmgirl2)
round(summary(lmgirl2)$r.sq, 4)

# what is the largest value of R square that may be achieved
# through a Box-Tidwell transformation of the regressor?

age = girlgrowth$Age
lambda <- (-200:200)/100 
Rsq <- NULL
for (lamb in lambda) {
  if (lamb==0) ta <- log(age) 
  else ta <- (age^lamb - 1) / lamb
  Rsq <- c(Rsq, summary(lm(girlgrowth$Height~ta))$r.sq)
}
plot(lambda,Rsq,type="l")
maxl <- lambda[which(Rsq==max(Rsq))]
maxl
ta <- (age^maxl - 1) / maxl
round(summary(lm(girlgrowth$Height~ta))$r.sq, 4)


# Answer says lambda = 0
# We are getting 0.24??
summary(lm(girlgrowth$Height~log(age)))$r.sq
lamb <- 0.24
ta <- (age^lamb - 1) / lamb
summary(lm(girlgrowth$Height~ta))$r.sq
