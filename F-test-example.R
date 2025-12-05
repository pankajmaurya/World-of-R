df <- read.csv("f-test-data.csv")   # <-- replace filename

# Perform F-test for equality of variances
var_test_result <- var.test(df$boy, df$girl)

print(var_test_result)