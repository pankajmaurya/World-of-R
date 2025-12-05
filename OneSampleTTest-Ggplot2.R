# Install if needed
# install.packages("ggplot2")

library(ggplot2)

# --------------------------------
# Sample data and one-sample t-test
# --------------------------------
x <- c(9.8, 11.2, 10.4, 12.0, 8.8)

tt <- t.test(x, mu = 10)
t_value <- as.numeric(tt$statistic)
df <- as.numeric(tt$parameter)

print(tt)

# --------------------------------
# Build data frame for t-distribution curve
# --------------------------------
t_vals <- seq(-4, 4, length = 400)
df_curve <- data.frame(
  t = t_vals,
  density = dt(t_vals, df)
)

# Critical values (two-sided, alpha = 0.05)
crit <- qt(0.975, df)

# --------------------------------
# ggplot
# --------------------------------
ggplot(df_curve, aes(x = t, y = density)) +
  geom_line(size = 1.2) +
  
  # Observed t-value
  geom_vline(xintercept = t_value, color = "red", size = 1) +
  
  # Critical values
  geom_vline(xintercept = c(-crit, crit), 
             color = "blue", linetype = "dashed", size = 1) +
  
  labs(
    title = paste("t-distribution (df =", df, ") with observed t =", round(t_value, 3)),
    x = "t-value",
    y = "Density"
  ) +
  
  theme_minimal(base_size = 14)
