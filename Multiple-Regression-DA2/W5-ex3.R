# Graphical view of collinearity from simulated data
library(rgl)
set.seed(6231)
n <- 25
x1 <- 10 * runif(n); x1 <- x1 - mean(x1) # x1 orthogonal with 1
x1 <- x1 * sqrt(n) / sqrt(sum(x1^2))     # x1 has squared norm n
v <- 5* rnorm(n); v <- v - mean(v)       # v orthogonal with 1
# cor(x1,v)
v <- v - x1 * (sum(v*x1) / sum(x1^2))    # v orthogonal with x1
v <- v * sqrt(n) / sqrt(sum(v^2))        # v has squared norm n
# cor(x1,v)
err <- rnorm(n)
alpha <- 1
x2 <- x1 + alpha * v
# plot(x1,x2)
y1 <- 10 + 2 * x1 + 1 * x2 + err
coef1 <- lm(y1~x1+x2)$coef
regr1 <- function(x1, x2) {
  coef1[1] + coef1[2] * x1 + coef1[3] * x2
}

mfrow3d(1, 2, sharedMouse = TRUE)
persp3d(regr1, 
        xlim = range(x1), ylim = range(x2),
        n = 30, zlab = "y1", col=5)
points3d(x1, x2, y1, col=2)

alpha <- 0.01
x3 <- x1 + alpha * v
# plot(x1,x3)
y2 <- 10 + 2 * x1 + 1 * x3 + err
coef2 <- lm(y2~x1+x3)$coef
regr2 <- function(x1, x3) {
  coef2[1] + coef2[2] * x1 + coef2[3] * x3
}

next3d()
persp3d(regr2, 
        xlim = range(x1), ylim = range(x3),
        n = 30, zlab = "y2", col=5)
points3d(x1, x3, y2, col=2)

# two replicates of x3 and y for fixed alpha
set.seed(6231); n <- 25; alpha <- 0.01

x1 <- 10 * runif(n); x1 <- x1 - mean(x1)
x1 <- x1 * sqrt(n) / sqrt(sum(x1^2))    
v <- 5* rnorm(n); v <- v - mean(v)      
v <- v - x1 * (sum(v*x1) / sum(x1^2))   
v <- v * sqrt(n) / sqrt(sum(v^2))       
err <- rnorm(n)
x2 <- x1 + alpha * v
y1 <- 10 + 2 * x1 + 1 * x2 + err

err <- rnorm(n)
x3 <- x1 + alpha * v
y2 <- 10 + 2 * x1 + 1 * x3 + err

coef1 <- lm(y1~x1+x2)$coef
regr1 <- function(x1, x2) {
  coef1[1] + coef1[2] * x1 + coef1[3] * x2
}
coef2 <- lm(y2~x1+x3)$coef
regr2 <- function(x1, x3) {
  coef2[1] + coef2[2] * x1 + coef2[3] * x3
}

mfrow3d(1, 2, sharedMouse = TRUE)
persp3d(regr1, 
        xlim = range(x1), ylim = range(x2),
        n = 30, zlab = "y1", col=5)
points3d(x1, x2, y1, col=2)
next3d()
persp3d(regr2, 
        xlim = range(x1), ylim = range(x3),
        n = 30, zlab = "y2", col=5)
points3d(x1, x3, y2, col=2)