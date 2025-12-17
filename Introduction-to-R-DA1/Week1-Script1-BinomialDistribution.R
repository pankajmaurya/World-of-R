N <- 15
x <- seq(0, N)
p <- 0.25
choose(N, x) * p^x * (1-p)^(N-x)

sum(dbinom(seq(0, 7), N, p))
pbinom(7, N, p)


px <- dbinom(x, size = N, prob = p)
plot(x, px, pch = 16, ylab = "Probability")
abline(h = 0, col = "grey")
