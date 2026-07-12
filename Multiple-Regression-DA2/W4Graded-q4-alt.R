library("alr4")
data(wblake)
y <- wblake$Age
x1 <- wblake$Length
x2 <- x1^2
m <- lm(y ~ x1 + x2)

n <- nrow(wblake)
p <- length(coef(m))   # number of parameters (intercept + 2 slopes) = 3

# 1. sigma_hat (residual standard error)
sigma_hat <- summary(m)$sigma
sigma_hat

# 2. t critical value for 95% PI, df = n - p
t_crit <- qt(0.975, df = n - p)
t_crit

# 3. x0 vector for Length = 200
x0 <- c(1, 200, 200^2)

# 4. (X'X)^{-1} -- get X matrix directly from the model
X <- model.matrix(m)
XtX_inv <- solve(t(X) %*% X)

# 5. leverage term: x0' (X'X)^{-1} x0
v <- as.numeric(t(x0) %*% XtX_inv %*% x0)
v

# 6. assemble the formula
PI_length_manual <- 2 * t_crit * sigma_hat * sqrt(1 + v)
round(PI_length_manual, 2)

# Compare to predict()'s version
newdat <- data.frame(x1 = 200, x2 = 200^2)
predAT <- predict(m, newdat, interval = "prediction", level = 0.95)
PI_length_predict <- predAT[, "upr"] - predAT[, "lwr"]
round(PI_length_predict, 2)
