plot(1:100, type = "n", ylim = c(0, 1), ylab = "Sample proportion")
p <- 0.25
for (i in 1:50) {
  z <- rbinom(100, size = 1, prob = p)
  lines(1:100, cumsum(z) / 1:100, col = sample(colors(), 1))
}