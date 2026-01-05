dup_birthday_exact <- function(n) {
  1 - prod(seq(365, by = -1, length.out = n) / 365)
}
N <- 10:50
print(sapply(N, dup_birthday_exact))

print((1 - cumprod(seq(365, 1) / 365))[10:50])

