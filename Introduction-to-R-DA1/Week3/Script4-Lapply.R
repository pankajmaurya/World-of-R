dup_birthday <- function(n, prob) {
  s <- sample(365, n, replace = TRUE, prob = prob)
  any(duplicated(s))
}

dup_prop <- function(n, nrep, prob = NULL) {
  replicate(nrep, dup_birthday(n, prob = prob)) |> mean()
}

N <- 10:50
pdup <- lapply(N, dup_prop, nrep = 10000)

plot(N, unlist(pdup), type = "o", pch = 16)

pdup <- sapply(N, dup_prop, nrep = 10000)
