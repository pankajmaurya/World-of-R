nums <- c(10313, 10859, 20523, 22907, 36567, 54059)
for ( n in nums) {
  notprime <- any(n %% seq(2, sqrt(n)) == 0)
  cat(n, if (notprime) " is not a prime" else " is a prime", "\n")
  
}