xx <- rnorm(100, mean = 10, sd = 3)
t.test(xx)$conf.int

confint_mean <- function(x) {
  ans <- t.test(x)$conf.int
  names(ans) <- c("lower", "upper")
  ans
}

print(confint_mean(xx))

print(aggregate(len ~ supp + dose, ToothGrowth, confint_mean))

boxplot.stats(xx)$conf

help(boxplot.stats)

confint_median_normal <- function(x) {
  ans <- boxplot.stats(x)$conf
  names(ans) <- c("lower", "upper")
  ans
}
print(confint_median_normal(xx))

print(aggregate(len ~ supp + dose, ToothGrowth, confint_median_normal))

# Part 2 now
yy <- rexp(1000)
boxplot.stats(yy)$conf

qexp(0.5)

zz <- sample(yy, size = length(yy), replace = TRUE)
median(zz)

# doing this 50000 times
nrep <- 50000
zz.median <- numeric(nrep)
for (i in seq_len(nrep)) {
  zz.median[[i]] <- median(sample(yy, size = length(yy), replace = TRUE))
}
str(zz.median)

# plot them
options(repr.plot.width = 15)
hist(zz.median, nclass = 40)

# To obtain an approximate 95% confidence interval, we can simply report the 2.5% and 97.5% quantiles of this distribution.
ci <- quantile(zz.median, c(0.025, 0.975))
names(ci) <- c("lower", "upper")
print(ci)

confint_median_resample <- function(x, nrep) {
  y.median <- numeric(nrep)
  for (i in seq_len(nrep)) {
    y.median[[i]] <- median(sample(x, size = length(x), replace = TRUE))
  }
  ci <- quantile(y.median, c(0.025, 0.975))
  names(ci) <- c("lower", "upper")
  ci
}


stopifnot(identical(names(formals(confint_median_resample)), c("x", "nrep")))
ci2 <- confint_median_resample(xx, nrep = 5000)
stopifnot(identical(names(ci2), c("lower", "upper")))
