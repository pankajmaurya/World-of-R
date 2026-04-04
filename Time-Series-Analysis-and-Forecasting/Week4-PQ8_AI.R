set.seed(125)

# Parameters
N <- 100
alpha <- 5
phi <- 0.8
theta <- 0.5
sigma <- 3   # since variance = 9 → sd = 3

# Generate white noise
epsilon <- rnorm(N, mean = 0, sd = sigma)

# Initialize series
y <- numeric(N)

# Initial values
y[1] <- alpha / (1 - phi)   # steady-state mean
epsilon_lag <- 0

# Generate series
for (t in 2:N) {
  y[t] <- alpha + phi * y[t-1] + epsilon[t] + theta * epsilon_lag
  epsilon_lag <- epsilon[t]
}

# Convert to time series object
y_ts <- ts(y)

# Plot
plot(y_ts, main = "Simulated ARMA(1,1) Series", ylab = "y_t")
acf(y_ts)
