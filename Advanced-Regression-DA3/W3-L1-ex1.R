# Examples of normal plot with simulated data 

# Standard normal data
set.seed(1234)
z <- rnorm(200)
qqnorm(z)
qqline(z)

# Smaller sample size
set.seed(1234)
z <- rnorm(30)
qqnorm(z)
qqline(z)

# Normal data with standard deviation 2
set.seed(1234)
z <- rnorm(200, sd = 2)
qqnorm(z)
qqline(z)

# Positively skewed distribution
# Gamma distribution with sd = 1, shifted to make mean zero
set.seed(1234)
x <- rgamma(200, shape = 4, rate = 2) - 2
mean(x)
sd(x)
# shape of probability density function
grid <-  seq(-2,4,length.out = 200)
plot(grid, dgamma(grid+2, shape = 4, rate = 2), 
     type = "l", xlab = "x", ylab = "pdf at x")
lines(grid, dnorm(grid), col = 2)

set.seed(1234)
x <- rgamma(200, shape = 4, rate = 2) - 2
qqnorm(x)
qqline(x)

# Negatively skewed distribution
set.seed(1234)
x <- 2 - rgamma(200, shape = 4, rate = 2)
qqnorm(x)
qqline(x)


# Heavy tailed distribution 
# Student's t distribution scaled to ensure sd = 1
set.seed(1234)
x <- rt(200, df = 3) / sqrt(3)
mean(x)
sd(x)
# shape of probability density function
grid <-  seq(-6,6,length.out = 200)
plot(grid, sqrt(3) * dt(grid * sqrt(3), df = 3), 
     type = "l", xlab = "x", ylab = "pdf at x") 
lines(grid, dnorm(grid), col = 2)
# Check log scale for difference in tails
plot(grid, sqrt(3) * dt(grid * sqrt(3), df = 3), 
     type = "l", xlab = "x", ylab = "pdf at x", 
     log = "y") 
lines(grid, dnorm(grid), col = 2)

set.seed(1234)
x <- rt(200, df = 3) / sqrt(3)
qqnorm(x)
qqline(x)