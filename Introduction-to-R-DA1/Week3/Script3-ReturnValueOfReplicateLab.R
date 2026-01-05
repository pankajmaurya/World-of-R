print(quantile(rnorm(500), probs = c(0.25, 0.5, 0.75)))
print(replicate(6, quantile(rnorm(500), probs = c(0.25, 0.5, 0.75))))

set.seed(20251228)
print(replicate(6, unique(sample(10, 5, replace = TRUE))))

set.seed(20200101)
print(replicate(6, unique(sample(10, 5, replace = TRUE))))

set.seed(20200101)
print(replicate(2, unique(sample(10, 5, replace = TRUE))))

set.seed(20200101)
print(replicate(2, unique(sample(10, 5, replace = TRUE)), simplify = FALSE))
