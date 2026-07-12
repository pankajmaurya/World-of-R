# Read data
wblake <- read.csv("wblake.csv")

# Problem 1: Width of the 95% prediction interval at Length = 200
fit1 <- lm(Age ~ Length + I(Length^2), data = wblake)

pi <- predict(
  fit1,
  newdata = data.frame(Length = 200),
  interval = "prediction",
  level = 0.95
)

cat(round(pi[1, "upr"] - pi[1, "lwr"], 2), "\n")

# Problem 2: LOOCV RMSE for Age ~ Length + Scale
fit2 <- lm(Age ~ Length + Scale, data = wblake)

e <- resid(fit2)
h <- hatvalues(fit2)

rmse <- sqrt(mean((e / (1 - h))^2))

cat(round(rmse, 3), "\n")


x <- c(10, 9, 8, 7, 6)
y <- c(12, 19, 32, 35, 48)

fit <- lm(y ~ x)

newx <- data.frame(x = c(9, 6, 7.8, 7))
pi <- predict(fit, newx, interval = "prediction")

width <- pi[, "upr"] - pi[, "lwr"]

cbind(newx, width)