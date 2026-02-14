round(pnorm(2), digits = 4)
round(pnorm(2, sd = 2) - pnorm(0, sd = 2), digits = 4)
pnorm(1.96) - pnorm(-1.96)
qnorm(0.975)

# P(−a<Z<a)=0.8
# P(Z > a ) = P(Z < -a) as normal curve is symmetrical
# also P(-a < Z < a) = 1 - P (Z < -a) - P ( Z > a) = 1 - 2 * P (Z > a)
# From above 3, we get 0.8 = 1 - 2 * P(Z > a), OR P(Z > a) = 0.1 OR P(Z < a) = 0.9
round(qnorm(0.9), 4)


# More messing around
x <- seq(-10, 10, 0.01)
y <- round(pnorm(x), digits = 4)
plot(x, y)

# More messing around
x <- seq(-5, 5, 0.1)
y <- round(pnorm(x), digits = 4)
plot(x, y)

