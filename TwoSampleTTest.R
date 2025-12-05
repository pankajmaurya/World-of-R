# Read CSV
df <- read.csv("two-samples-boy-girl.csv")   # <-- replace filename

# ---------------------------------------
# 1. Heteroscedastic (Welch) t-test
# ---------------------------------------
cat("Welch Two-Sample t-test (unequal variances):\n")
print(t.test(df$boy, df$girl))   # default = Welch

# ---------------------------------------
# 2. Homoscedastic t-test (equal variances)
# ---------------------------------------
cat("\nEqual Variance Two-Sample t-test:\n")
print(t.test(df$boy, df$girl, var.equal = TRUE))
