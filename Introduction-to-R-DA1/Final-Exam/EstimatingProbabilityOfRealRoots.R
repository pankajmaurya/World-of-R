probRealRoot <- function(a, b, c, nrep = 1000) {
  # your code here
  A <- rpois(nrep, a)
  B <- rpois(nrep, b)
  C <- rpois(nrep, c)
  count <- sum(B * B >= A * C)
  count / nrep
}

if (abs(probRealRoot(1, 2, 3, nrep = 10000) - 0.693) > 0.01) 
  message("Check your function again! Remember that the two solutions can be equal.")

