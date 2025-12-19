# Parameters
n <- 1000
p <- 0.01
X <- 10
# Calculate exact binomial probability P(X <= 10)
exact_binom <- pbinom(X, size = n, prob = p)
poisson_approx <- ppois(X, lambda = n * p)
mu <- n * p
sigma <- sqrt(n * p * (1 - p))
normal_approx <- pnorm(X, mean = mu, sd = sigma)

print(exact_binom)
print(poisson_approx)
print(normal_approx)
if (abs(exact_binom - poisson_approx) < abs(exact_binom - normal_approx)) {
  print("Poisson better")
} else {
  print("Normal better")
}

round(pnorm(19, mean = mu, sd = sigma), digits = 6)
round(ppois(15, lambda = n * p), digits = 6)
round(pnorm(20, mean = mu, sd = sigma), digits=6)
round(1 - ppois(20, lambda = n * p), digits = 6)

X <- 10
n <- 1000
p <- 0.01
exact_binom <- pbinom(X, size = n, prob = p)
poisson_approx <- ppois(X, lambda = n * p)
mu <- n * p
sigma <- sqrt(n * p * (1 - p))
normal_approx <- pnorm(X, mean = mu, sd = sigma)

print(exact_binom)
print(poisson_approx)
print(normal_approx)
if (abs(exact_binom - poisson_approx) < abs(exact_binom - normal_approx)) {
  print("Poisson better")
} else {
  print("Normal better")
}




# Programming Assignment

N <- 12
m <- 50
nrep <- 10000
count <- 0
for (i in 1:nrep) {
  s <- sample(N, size = m, replace = TRUE)
  if (length(unique(s)) == N)
    count <- count + 1
}
phat_12_30 <- count / nrep
phat_12_30


