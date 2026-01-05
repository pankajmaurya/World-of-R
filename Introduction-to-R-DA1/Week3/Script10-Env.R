search()

e1 <- new.env()
e1$x <- seq(0, 2 * pi, length.out = 101)
e1$y <- cos(e1$x)
plot(y ~ x, data = e1, type = "l", ylim = c(-1, 1)) # Formula interface

# Environments are not duplicated when copied as an exception.
ls(e1)
ls.str(e1)

environment(dplyr::filter)

e <- .GlobalEnv
while (TRUE) {
  str(e, give.attr = FALSE);
  e <- parent.env(e);
}

find("sd")

rbirthday <- function(n, nrep = 100) {
  s <- replicate(nrep, any(duplicated(sample(365, n, replace = TRUE))))
  phat <- mean(s)
  sdhat <- sd(s)
  return (environment())
}

e <- rbirthday(30)
ls.str(e)
parent.env(e)

# Environment can be changed too
environment(rbirthday) <- as.environment("package:stats")
e <- rbirthday(30)
ls.str(e)
parent.env(e)

environment(rbirthday) <- as.environment("package:base")
# Error as sd is not in base but in stats.
e <- rbirthday(30)
find("sd")
