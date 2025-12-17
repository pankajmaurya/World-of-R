# Hardy Ramanujam numbers will show up as duplicate in the sorted list output
N <- 12
result <- numeric(N * (N+1) / 2)
k <- 1
for (i in 1:N)
  for (j in i:N) {            
    result[k] <- i^3 + j^3
    k <- k + 1
  }
print(sort(result))

# Same using outer
(x <- outer((1:N)^3, (1:N)^3, FUN = `+`))
(keep <- outer(1:N, 1:N, FUN = `<=`))
result <- x[keep]
print(sort(result))

# Using rep
i <- rep(1:N, each = N)
j <- rep(1:N, N)
cbind(i, j)


keep <- (i <= j)
i <- i[keep]
j <- j[keep]
result <- i^3 + j^3
print(sort(result))


# Finding lot more numbers now
N <- 500
i <- rep(1:N, each = N)
j <- rep(1:N, N)
keep <- (i <= j)
i <- i[keep]
j <- j[keep]
result <- i^3 + j^3
which_dup <- which(duplicated(result))
twices <- sort(result[which_dup])

twices
twices_dup <- which(duplicated(twices))
thrices <- sort(twices[twices_dup])
thrices

# getting their forms of i^3 + j^3
n <- 119824488
which_ij <- which(result == n)
print(cbind(i = i[which_ij], j = j[which_ij]))
