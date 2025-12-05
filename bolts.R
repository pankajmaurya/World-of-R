setwd("~/TechCareer/World-of-R")
test <- read.csv("bolts.csv")
head(test)
hist(test$length)

y <- sqrt(var(test$length))

z <- y / sqrt(100)
print(z)

# Run t-test
tt <- t.test(test$length, mu = 5)

# Extract t-value and degrees of freedom
t_value <- tt$statistic
df <- tt$parameter

# Print for reference
print(tt)

# -------------------------
# Plot t-distribution
# -------------------------

# Range of t-values for plotting
t_vals <- seq(-14, 14, length = 400)

# Plot the t-distribution curve
plot(t_vals, dt(t_vals, df), type = "l", lwd = 2,
     xlab = "t-value", ylab = "Density",
     main = paste("t-distribution (df =", df, 
                  ") with observed t =", round(t_value, 3)))

# Add vertical red line for the observed t statistic
abline(v = t_value, col = "red", lwd = 2)

# Add two-tailed critical values at alpha = 0.05
crit <- qt(0.975, df)
abline(v = c(-crit, crit), col = "blue", lwd = 2, lty = 2)

# Add legend
legend("topright",
       legend = c("t-distribution", "Observed t", "Critical values"),
       col = c("black", "red", "blue"),
       lwd = c(2, 2, 2),
       lty = c(1, 1, 2))
