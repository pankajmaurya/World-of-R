n <- 1000
k <- 3650

c(k, k-n) 
factorial(c(k, k-n)) # gives Inf Inf

# This is also unstable computation
prod(seq(k, k-n+1)) / k^n

prod(seq(k, k-n+1) / k)

exp(sum(log(1:k)) - sum(log(1:(k-n))) - n * log(k))

exp(sum(log((k-n+1):k)) - n * log(k))


k <- 365

## Your code here 

## After being run, your code should produce the following variables

## min25: minimum n such that P(A) <= 1/4
x <- 1:k
y <- numeric(k)
min25 <- 1
min33 <- 1
min50 <- 1
min66 <- 1
min75 <- 1

for (n in k:1) {
  # Compute P(A) for n and keep on 
  p <- exp(sum(log((k-n+1):k)) - n * log(k))
  if (p <= 1/4) {
    min25 <- n
  }
  if (p <= 1/3) {
    min33 <- n
  }
  if (p <= 1/2) {
    min50 <- n
  }
  if (p <= 2/3) {
    min66 <- n
  }
  if (p <= 3/4) {
    min75 <- n
  }
  #y[[n]] <- p
  
}
plot(x, y)
# We see that as n increases, P decreases.

## min33: minimum n such that P(A) <= 1/3
## min50: minimum n such that P(A) <= 1/2
## min66: minimum n such that P(A) <= 2/3
## min75: minimum n such that P(A) <= 3/4

## REMEMBER: A is the event that all birthdays are DISTINCT

# your code here