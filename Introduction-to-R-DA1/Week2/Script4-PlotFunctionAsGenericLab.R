methods(plot)
m <- sample(month.name, 30, replace = TRUE)
m
class(m)
plot(m)
plot(factor(m, levels = month.name), las = 2) # las = label style, 2 is for vertically down


# Continuous distributions
# For continuous data, two common estimates of the underlying distribution are 
#  kernel density estimates and empirical CDFs (cumulative distribution functions). 
# These are obtained by the density() and ecdf() functions respectively. 
# Both return objects that have plot() methods.

x <- c(rnorm(30, sd = 0.5), rnorm(40, mean = 3))
d <- density(x)
e <- ecdf(x)

plot(d)
x
plot(e)

# Data frames
library(package = "MASS")
plot(crabs[3:8], col = as.numeric(crabs$sp), pch = as.numeric(crabs$sex))

# Regression Models
y <- rnorm(length(x), mean = 2 + 3 * x + x^2, sd = 1.5)
plot(y ~ x)

fm <- lm(y ~ x)
summary(fm)

plot(fm)
