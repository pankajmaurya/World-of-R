library(magrittr)
n <- 10
mu <- 5
y <- rpois(n, lambda = rexp(n, rate = 1/mu))
y

y <- rexp(n, rate = 1/mu) %>% rpois(n = n)
y

y <- (1/mu) %>% rexp(n = n) %>% rpois(n = n)
y

# Equivalent ways, but former is pipeline and perhaps more clear
sample(1:365, 30, replace = TRUE) %>% duplicated() %>% any()
any(duplicated(sample(1:365, 30, replace = TRUE)))
