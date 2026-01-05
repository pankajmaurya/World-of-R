dup_birthday <- function(n) {
  s <- sample(365, n, replace = TRUE)
  any(duplicated(s))
}
N <- 20:30
s20 <- sapply(N, function(n) replicate(20, dup_birthday(n)))
print(s20)


print(apply(s20, 2, mean))
# These are 2 vectorized alternatives
print(colMeans(s20))
print( (1/20) * matrix(1, 1, nrow(s20)) %*% s20 )


print(apply(s20, 2, sd))
print(apply(s20, 2, median))

s5000 <- sapply(N, function(n) replicate(5000, dup_birthday(n)))
# We see that the median value flips at N = 23
print(apply(s5000, 2, median))
