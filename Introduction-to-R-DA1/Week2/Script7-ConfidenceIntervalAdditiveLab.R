library(ggplot2)
p <- ggplot(ToothGrowth) + facet_wrap(~ supp)

fm3 <- lm(len ~ log(dose), data = ToothGrowth)
udf <- unique(ToothGrowth[-1])
predict(fm3, udf)
udf3 <- cbind(udf, fit = predict(fm3, udf))
udf3conf <- cbind(udf, predict(fm3, udf, interval = "confidence"))
print(udf3conf)

# Lets plot.
p + geom_point(aes(x = dose, y = len)) + 
  geom_line(aes(x = dose, y = fit), data = udf3) + 
  geom_errorbar(aes(x = dose, ymin = lwr, ymax = upr), data = udf3conf, width = 0.1, col = 2)

# Extension of the above model to allow different intercepts for different supplement types.
# This is an additive model.
fm7 <- aov(len ~ supp + log(dose), data = ToothGrowth)

# Objective: Obtain confidence intervals for this model and plot them as before.

udf <- unique(ToothGrowth[-1])
predict(fm7, udf)
udf7 <- cbind(udf, fit = predict(fm7, udf))
udf7conf <- cbind(udf, predict(fm7, udf, interval = "confidence"))
print(udf7conf)

# Lets plot.
p + geom_point(aes(x = dose, y = len)) + 
  geom_line(aes(x = dose, y = fit), data = udf7) + 
  geom_errorbar(aes(x = dose, ymin = lwr, ymax = upr), data = udf7conf, width = 0.1, col = 2)

