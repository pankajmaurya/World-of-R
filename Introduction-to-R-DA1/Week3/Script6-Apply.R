dup_birthday <- function(n) {
  s <- sample(365, n, replace = TRUE)
  any(duplicated(s))
}
N <- 1:50
s100 <- sapply(N, function(n) replicate(100, dup_birthday(n)))
str(s100)

phat100 <- apply(s100, 2, mean)
plot(N, phat100)

dup_birthday_exact <- function(n) {
  1 - prod(seq(365, by = -1, length.out = n) / 365)
}

lines(N, sapply(N, dup_birthday_exact), col = 4)

# repeating this 50 times
for (i in 1:50) {
  s100 <- sapply(N, function(n) replicate(100, dup_birthday(n)))
  points(N, apply(s100, 2, mean))
}

plot(N, sapply(N, dup_birthday_exact), col = 4, type = "l", ylim = c(0, 1),
     ylab = "Common Birthday Probability")

# repeating this 50 times for N = 1000
for (i in 1:50) {
  s1000 <- sapply(N, function(n) replicate(1000, dup_birthday(n)))
  points(N, apply(s1000, 2, mean))
}

true_prob <- sapply(N, dup_birthday_exact)
r <- 3 * sqrt(0.5 * 0.5 / 1000)
plot(N, rep(0, length(N)), col = 4, type = "l",
     ylim = c (-r, r), ylab = "Error in Estimated Probability")
for (i in 1:50) {
  s1000 <- sapply(N, function(n) replicate(1000, dup_birthday(n)))
  points(jitter(N), apply(s1000, 2, mean) - true_prob, col = "#00000044")
}

sd1000 <- apply(s1000, 2, sd)
plot(N, sd1000 / sqrt(1000), ylab = "Estimated SD for 1000 replications")
lines(N, sqrt((true_prob * (1 - true_prob)) / 1000), col = 3)
