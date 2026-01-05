help(optim)
curve(sin(x) / x, -20, 20)
# Define the objective function
f <- function(x) if (x == 0) 1 else sin(x) / x
optim(par = 0, fn = f)

optim(par = 0, fn = f, method = "Brent", lower = -20, upper = 20) 

res <- suppressWarnings(optim(par = 0, fn = f))
str(res)
abline(v = res$par, col = 2)

# If we start at -2, the corresponding minimum is at a different position.
res <- suppressWarnings(optim(par = -2, fn = f))
str(res)
abline(v = res$par, col = 2)

# Starting at 10 we get a local minimum
res <- suppressWarnings(optim(par = 10, fn = f))
str(res)
abline(v = res$par, col = 3)

# Starting at -10 we get the corresponding local minimum
res <- suppressWarnings(optim(par = -10, fn = f))
str(res)
abline(v = res$par, col = 4)

# How to find the least squares regression line using optim.
SSE <- function(par) {
  a <- par[[1]]
  b <- par[[2]]
  e <- (y - a - b * x) # error
  sum(e^2)
}

# Now we set x and y
x <- airquality$Temp
y <- airquality$Ozone
plot(x, y)
keep <- is.finite(x) & is.finite(y) # calculates the positions where both exist. array with those positions as 1
x <- x[keep]
y <- y[keep]
res <- optim(par = c(0, 0), fn = SSE)
abline(res$par)

# If we want to minimize the sum of absolute errors, we cannot use calculus. must use a numerical method.
SAE <- function(par) {
  a <- par[[1]]
  b <- par[[2]]
  e <- (y - a - b * x) # error
  sum(abs(e))
}
res2 <- optim(par = c(0, 0), fn = SAE)
abline(res2$par, col = 2)

# This approach requires x, y to be in the global env.
# Alternatively we can create closures:
makeSSE <- function(x, y) {
  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]
  y <- y[keep]
  SSE <- function(par) {
    a <- par[[1]]
    b <- par[[2]]
    e <- (y - a - b * x) # error
    sum(e^2)
  }
}
makeSAE <- function(x, y) {
  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]
  y <- y[keep]
  SAE <- function(par) {
    a <- par[[1]]
    b <- par[[2]]
    e <- (y - a - b * x) # error
    sum(abs(e))
  }
}

plotWithFitted <- function(x, y) {
  plot(x, y)
  optim(c(0, 0), fn = makeSSE(x, y))$par |> abline()
  optim(c(0, 0), fn = makeSAE(x, y))$par |> abline(col = 2)
}

library(lattice)
# Defining a custom panel function and using it for couple of plots.
panel.linreg <- function(x, y, ...) {
  panel.grid(h = -1, v = -1)
  panel.points(x, y, ...)
  optim(c(0, 0), fn = makeSSE(x, y))$par |> panel.abline()
  optim(c(0, 0), fn = makeSAE(x, y))$par |> panel.abline(col = 2)
}
xyplot(Ozone ~ Solar.R | factor(Month), data = airquality, panel = panel.linreg, as.table = TRUE)

library(NHANES) 
xyplot(I(BPSysAve) ~ Age | Gender, data = NHANES, subset = Age > 20, panel = panel.linreg)

library(dplyr)
nhsub <- filter(NHANES, Age >= 20 & Race1 == "White") |> select(Age, Gender, BPSysAve)
