## Placeholder vector to contain result
ans <- numeric(0)

# your code here
for (a in 0:9)
  for (b in 0:9)
    for (c in 0:9)
      for (d in 0:9) {
        if ((a + b + c + d) * (a^2 + b^2 + c^2 + d^2)^2 == 1000*a + 100*b + 10*c + d)
          ans <- c(ans, 1000*a + 100*b + 10*c + d)                
      }

ans

stopifnot(0 %in% ans, 1 %in% ans)
