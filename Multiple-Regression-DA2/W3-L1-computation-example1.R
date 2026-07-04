library(lmreg)
data("girlgrowth")
head(girlgrowth)
lmgirl1 = lm(Height~Age, data = girlgrowth)
summary(lmgirl1)$coefficients
# Note summary(lmgirl1$coefficients) is different!

# For PQ 3.2
library(lmreg)
data("stars1")
head(stars1)
hubble=lm(Velocity~Distance, data=stars1)
summary(hubble)$coeff


library(lmreg)
data("leprosy")
head(leprosy)
Treat = as.data.frame(binaries(leprosy$treatment))
head(Treat)
colnames(Treat) = c("A", "D", "F") # F is placebo treatment
summary(lm(post ~ Treat$A + Treat$D, data = leprosy))$coef

summary(lm(post ~ Treat$A + Treat$D + pre, data = leprosy))$coef


fit1 <- lm(post ~ Treat$A + Treat$D, data = leprosy)
fit2 <- lm(post ~ Treat$A + Treat$D + pre, data = leprosy)

one_sided_table <- function(fit, direction = "less") {
  cf <- summary(fit)$coef
  df <- fit$df.residual
  t_vals <- cf[, "t value"]
  
  if (direction == "less") {
    p_one <- pt(t_vals, df, lower.tail = TRUE)
  } else {
    p_one <- pt(t_vals, df, lower.tail = FALSE)
  }
  
  cf <- cbind(cf, "Pr(one-sided)" = p_one)
  cf
}

one_sided_table(fit1, direction = "less") # Useful
one_sided_table(fit2, direction = "less") # Useful
