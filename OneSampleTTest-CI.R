x <- c(9.8, 11.2, 10.4, 12.0, 8.8)

# Basic scatterplot
plot(x, rep(1, length(x)), pch = 19, xlim = range(x) + c(-1, 1),
     ylab = "", yaxt = "n",
     main = "Sample Observations with Mean and CI")

# Mean line
abline(v = mean(x), col = "red", lwd = 2)

# 95% CI from t.test
tt <- t.test(x, mu = 10)
segments(tt$conf.int[1], 1, tt$conf.int[2], 1, col = "blue", lwd = 3)

legend("topright",
       legend = c("Observations", "Sample mean", "95% CI"),
       col = c("black", "red", "blue"),
       pch = c(19, NA, NA),
       lwd = c(NA, 2, 3))
