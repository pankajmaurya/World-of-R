x <- c(10.25, 10.06, 10.0, 10.78, 10.56, 10.08, 10.72, 10.56, 10.66)
y <- c(10.93, 10.73, 10.2, 10.72, 10.68, 10.86, 10.32, 10.18, 10.77, 10.29)
m <- length(x)
n <- length(y)
xbar <- mean(x)
ybar <- mean(y) 
Sp2 <- (sum((x - xbar)^2) + sum((y - ybar)^2)) / (m + n - 2)
T  <- (xbar - ybar) / sqrt(Sp2 * (1/m + 1/n))
degFreedom <- m + n - 2
pval <- 2 * pt(abs(T), df = degFreedom, lower.tail = FALSE)
result <- list(means = c(xbar, ybar),
               sizes = c(m, n),
               statistic = T,
               pooled.variance = Sp2,
               p.value = pval,
               alternative = "both-sided")

two_sample_ttest <- function(x, y) {
  m <- length(x)
  n <- length(y)
  xbar <- mean(x)
  ybar <- mean(y) 
  Sp2 <- (sum((x - xbar)^2) + sum((y - ybar)^2)) / (m + n - 2)
  T  <- (xbar - ybar) / sqrt(Sp2 * (1/m + 1/n))
  degFreedom <- m + n - 2
  pval <- 2 * pt(abs(T), df = degFreedom, lower.tail = FALSE)
  result <- list(means = c(xbar, ybar),
                 sizes = c(m, n),
                 statistic = T,
                 pooled.variance = Sp2,
                 p.value = pval,
                 alternative = "both-sided")
  class(result) <- "ttest2"
  result
}

two_sample_ttest(x, y)

# Note that due to the class(result) being ttest2, this print works
print.ttest2 <- function(x, ...) {
  cat("Two-sample t-test with equal variance\n")
  cat("- Statistic:", x$statistic, "with", sum(x$sizes) - 2, "degrees of freedom\n")
  cat("- p-value  :", x$p.value)
}    

two_sample_ttest(x, y)

tpaired <- function(x, y) {
  # the t statistic is mean(w)/sd(w) where w = x - y
  w <- x - y
  n <- length(x)
  T <- mean(w) / (1/sqrt(n) * sqrt(sum((w-mean(w))^2)) / sqrt(n-1))
  df <- n - 1
  # twice of upper tailed probability gives the pvalue.
  pval <- 2 * pt(abs(T), df = df, lower.tail = FALSE)
  result <- list(statistic = T, df = df, p.value = pval, alternative = "two.sided")  
  class(result) <- "tpaired"

  result
}

print.tpaired <- function(x, ...) {
  cat("Paired t-test \n")
  cat("- Statistic:", x$statistic, "with", x$df, "degrees of freedom\n")
  cat("- p-value  :", x$p.value)
}    

tpaired_with_help <- function(x, y) {
  stopifnot(length(x) == length(y))
  
  w <- x - y
  n <- length(w)
  if (n < 2) stop("not enough observations")
  wbar <- mean(w)
  sw <- sd(w)
  T <- wbar / (sw / sqrt(n))
  
  df <- n - 1
  pval <- 2 * pt(abs(T), df = df, lower.tail = FALSE)
  
  result <- list(
    statistic = T,
    df = df,
    p.value = pval,
    alternative = "two.sided",
    estimate = wbar
  )
  
  class(result) <- "tpaired_with_help"
  result
}

print.tpaired_with_help <- function(x, ...) {
  cat("Paired t-test (production like)\n")
  cat("- Statistic:", x$statistic, "with", x$df, "degrees of freedom\n")
  cat("- p-value  :", x$p.value)
}    


# For reference
# -----------------------------
# 1. READ CSV
# -----------------------------
setwd("~/TechCareer/World-of-R")
df <- read.csv("marks-10-and-12.csv")
#df <- read.csv("yourfile.csv")   # <-- replace with your CSV filename
df$id <- 1:nrow(df)

# -----------------------------
# 2. PAIRED t-TEST
# -----------------------------
#print(t.test(df$before, df$after, paired = TRUE))
print(t.test(df$marks10, df$marks12, paired = TRUE))


if (tpaired(rnorm(10), rnorm(10))$df != 9) cat("ERROR: wrong degrees of freedom")

tpaired(df$marks10, df$marks12)
tpaired_with_help(df$marks10, df$marks12)
