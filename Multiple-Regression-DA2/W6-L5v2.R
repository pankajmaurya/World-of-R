# Selection bias 
set.seed(1234)
n <- 10; beta0 <- 20; beta1 <- 1; beta2 <- 1
x1 <- rnorm(n); x2 <- rnorm(n); err <- rnorm(n)
y <- beta0 + beta1 * x1 + beta2 * x2 + err
b1 <- lm(y~x1)$coef[2]
b2 <- lm(y~x2)$coef[2]
c(b1,b2)

b1list <- NULL; b2list <- NULL; indlist <- NULL
for (iter in 1:1000) {
  x1 <- rnorm(n); x2 <- rnorm(n); err <- rnorm(n)
  y <- beta0 + beta1 * x1 + beta2 * x2 + err
  b1 <- lm(y~x1)$coef[2]; b2 <- lm(y~x2)$coef[2]
  ind <- 2; if(cor(y,x1)>cor(y,x2)) ind <- 1
  b1list <- c(b1list,b1); b2list <- c(b2list,b2)
  indlist <- c(indlist,ind)
}
# Generate box plots of LSEs in the 2 groups, and
boxplot(b1list~indlist,names=c("x1 winner","x2 winner"))
abline(h=1,lty=3)
boxplot(b2list~indlist,names=c("x1 winner","x2 winner"))
abline(h=1,lty=3)
# LSE of Î²1 overestimates Î²1 in the first group 
# and underestimates it in the second group
# Generate box plots of LSEs in combined sample
boxplot(b1list,names=c("all data sets"))
abline(h=1,lty=3)
boxplot(b2list,names=c("all data sets"))
abline(h=1,lty=3)
# Neither estimator is biased in the combined sample