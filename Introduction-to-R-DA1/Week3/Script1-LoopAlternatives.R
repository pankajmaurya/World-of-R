replicate(10, any(duplicated(sample(1:365, 30, replace = TRUE))))

# Custom implementation
rep_experiment <- function(N, expr, mode = "list") {
  e <- substitute(expr)
  ans <- vector(mode = mode, length = N)
  for (i in 1:N) ans[[i]] <- eval(e)
  ans
}

rep_experiment(10, any(duplicated(sample(1:365, 30, replace = TRUE))),
               mode = "logical")
nrep <- 1000000
system.time(dup1 <- replicate(nrep, any(duplicated(sample(1:365, 30, replace = TRUE)))))
system.time(dup2 <- rep_experiment(nrep, any(duplicated(sample(1:365, 30, replace = TRUE))), mode = "logical"))

sum(dup1) / nrep
sum(dup2) / nrep

1 - prod(seq(365, by = -1, length.out = 30) / 365)
