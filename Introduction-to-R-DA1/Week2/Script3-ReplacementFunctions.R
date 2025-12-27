sReLU <- function(u) {
  if (u < 0) u = 0
  u
}

x <- -5
sReLU(x)
x

vReLU <- function(u) {
  for (i in seq_len(length(u))) {
    if (u[i] < 0) {
      u[i] <- 0
    }
  }
  return(u)
}

x <- rnorm(10)
y <- x
x
y
z <- vReLU(y)
y
x
z

# Alternative: Direct assignment
y[y < 0] <- 0
y
x


# Modifying names of a dataframe
d <- data.frame(1, rnorm(5), rexp(5))
names(d)
d2 <- setNames(d, c("Constant", "Normal", "Exponential"))
# d is not modified!
d
d2

# Replacement function approach
names(d) <- c("Constant", "Normal", "Exponential")
d # d names are now replaced.

# Other examples of replacement functions.
d$Normal[d$Normal < 0] <- 0
d 

