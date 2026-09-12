# Understanding plots for detecting serial correlation
rm(list=ls())
set.seed(456)
n = 50

# Simulate from the AR(1) model 
# x_i = Ï†.x_{iâˆ’1} + z_i, with iid errors z_i
# var(x_i) = var(z_i)/(1 - Ï†^2)

# Assume stdev(x_i) = 1, stdev(z_i)= âˆš(1 - Ï†^2).
phi = 0.7
x = array(dim=n)
x[1] = rnorm(1)
z = rnorm(n) * sqrt(1 - phi^2)
for (i in 2:n) x[i] = phi * x[i-1] + z[i]

par(mfrow=c(2,2))
# Get index plot
plot(x,type="b", ylab = "x")
abline(h=0,lty=2)
title("Serial plot of x, Ï† = 0.7")

# Get lag plot
plot(x[1:(n-1)], x[2:n], xlab = "Lagged x", ylab = "x", type="l")
abline(a=0, b=1, lty = 2)
title("Lag plot of x, Ï† = 0.7")

# Repeat for phi = 0
phi = -0.98
x[1] = rnorm(1)
z = rnorm(n) * sqrt(1 - phi^2)
for (i in 2:n) x[i] = phi * x[i-1] + z[i]
plot(x,type="b", ylab = "x")
abline(h=0,lty=2)
title("Serial plot of x, Ï† = 0")
plot(x[1:(n-1)], x[2:n], xlab = "Lagged x", ylab = "x", type="l")
abline(a=0, b=1, lty = 2)
title("Lag plot of x, Ï† = 0")
