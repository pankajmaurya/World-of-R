# The birthday paradox, in a group of ppl as small as 23 there is atleast 50% chance of 2 ppl with same birthday.
n <- 23
k <- 365
x <- sample(1:k, n, replace = TRUE)
any(duplicated(x))
anyDuplicated(x)
length(unique(x)) < n


# Use of replicate
replicate(10, any(duplicated(sample(1:k, n, replace = TRUE))))

# Note that we cannot do the following:
#x <- any(duplicated(sample(1:k, n, replace = TRUE)))
#replicate(10, x)


NREP <- 10000
outcomes <- replicate(NREP, any(duplicated(sample(1:k, n, replace = TRUE))))
sum(outcomes) / NREP
