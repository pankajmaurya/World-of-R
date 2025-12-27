library(ggplot2)
p <- ggplot(ToothGrowth) + facet_wrap(~ supp)
p + geom_point(aes(x = dose, y = len))

fm1 <- lm(len ~ dose, data = ToothGrowth)
summary(fm1)
# First test is the hypotheses that the intercept is zero, not of interest.
# The p value is really small, hence length does depend on dose.

expand.grid(supp=c("VC", "OJ"), dose = c(0.5, 1, 2))

with(ToothGrowth, expand.grid(supp = unique(supp), dose = unique(dose)))

# leaving out the response variable.
udf <- unique(ToothGrowth[-1])

predict(fm1, udf)
udf1 <- cbind(udf, fit = predict(fm1, udf))

p + geom_point(aes(x = dose, y = len)) + geom_line(aes(x = dose, y = fit), data = udf1)

udf1conf <- cbind(udf, predict(fm1, udf, interval = "confidence"))

p + geom_point(aes(x = dose, y = len)) + 
  geom_line(aes(x = dose, y = fit), data = udf1) + 
  geom_errorbar(aes(x = dose, ymin = lwr, ymax = upr), data = udf1conf, width = 0.1, col = 2)

# Now we get the prediction intervals
udf1pred <- cbind(udf, predict(fm1, udf, interval = "prediction"))
udf1pred

p + geom_point(aes(x = dose, y = len)) + 
  geom_line(aes(x = dose, y = fit), data = udf1) + 
  geom_errorbar(aes(x = dose, ymin = lwr, ymax = upr), data = udf1pred, width = 0.1, col = 2)


# Anova model session now
fm2 <- lm(len ~ factor(dose), data = ToothGrowth)
summary(fm2)
# This above is not very easily interpretable.

fm2 <- aov(len ~ factor(dose), data = ToothGrowth)
udf2conf <- cbind(udf, predict(fm2, udf, interval = "confidence"))
udf2conf

p + geom_point(aes(x = dose, y = len)) + 
  geom_line(aes(x = dose, y = fit), data = udf2conf) + 
  geom_errorbar(aes(x = dose, ymin = lwr, ymax = upr), data = udf2conf, width = 0.1, col = 2)

sum(residuals(fm1)^2)
sum(residuals(fm2)^2)

# compares the 2 models
anova(fm1, fm2)
# P value is 0.001 implying the improvement with the anova model is statistically significant.

# another model where we use log(dose) as predictor since we see that increase in len from dose level 1 to 2 is more than 2 to 3
fm3 <- lm(len ~ log(dose), data = ToothGrowth)
sum(residuals(fm3)^2)

anova(fm3, fm2)
# H0: The simpler model (log(dose)) fits as well as factor(dose)
# H1: factor(dose) provides a significantly better fit.
# p = 0.239 > 0.05 => Fail to reject null Hypotheses. Accept the null hypotheses then.
# Now the P value is no longer significant at the 5% cutoff level. However the predict method continues to work.
udf3conf <- cbind(udf, predict(fm3, udf, interval = "confidence"))
udf3conf

p + geom_point(aes(x = dose, y = len)) + 
  geom_line(aes(x = dose, y = fit), data = udf3conf) + 
  geom_errorbar(aes(x = dose, ymin = lwr, ymax = upr), data = udf3conf, width = 0.1, col = 2)

# Exploring the supplement type now.
# Approach 1, combine supp and levels to be 6 combinations. Gives anova model with 6 groups instead of 3.

# Create a new variable combining supp and dose.

fm4 <- aov(len ~ paste(supp, dose, sep = "_"), data = ToothGrowth)
udf4conf <- cbind(udf, predict(fm4, udf, interval = "confidence"))
p + geom_point(aes(x = dose, y = len)) + 
  geom_line(aes(x = dose, y = fit), data = udf4conf) + 
  geom_errorbar(aes(x = dose, ymin = lwr, ymax = upr), data = udf4conf, width = 0.1, col = 2)

# Is this an improvement over fm2?
anova(fm2, fm4)
# Yes, this is an improvement as the P value is quite small.

# Such models are known as interaction models.
# Notation for such models which are very common:
fm5 <- aov(len ~ supp * factor(dose), data = ToothGrowth)
anova(fm2, fm5)

udf5conf <- cbind(udf, predict(fm5, udf, interval = "confidence"))
p + geom_point(aes(x = dose, y = len)) + 
  geom_line(aes(x = dose, y = fit), data = udf5conf) + 
  geom_errorbar(aes(x = dose, ymin = lwr, ymax = upr), data = udf5conf, width = 0.1, col = 2)

# explore additive model.one way Anova models.
# Count number of combinations using the xtabs function.
xtabs(~ supp + dose, ToothGrowth)

# One way ANOVA (dose)
# y_ijk = u_i + e_ijk, i = 1,2,3; j = 1,2 and k takes 1 to 10 values

# Two way ANOVA with interaction (dose * supp)
# y_ijk = u_ij + e_ijk

# Two way ANOVA additive (dose + supp)
# y_ijk = alpha_i + beta_j + e_ijk (writing u_ij into an additive form)

fm6 <- aov(len ~ supp + factor(dose), data = ToothGrowth)
sum(residuals(fm6)^2)

udf6conf <- cbind(udf, predict(fm6, udf, interval = "confidence"))
p + geom_point(aes(x = dose, y = len)) + 
  geom_line(aes(x = dose, y = fit), data = udf6conf) + 
  geom_errorbar(aes(x = dose, ymin = lwr, ymax = upr), data = udf6conf, width = 0.1, col = 2)
