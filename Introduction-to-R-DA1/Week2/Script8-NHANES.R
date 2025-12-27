library(ggplot2)
library(NHANES)

nhsub <- subset(NHANES, Age >= 21)
nhsub <- nhsub[c("BPSysAve", "Age", "Gender")]

str(nhsub)

fm1 <- lm(BPSysAve ~ Age, data = nhsub)

# How to get predictions
# For continuous variable?
# Age is rounded to whole year

sort(unique(nhsub$Age))

udf <- with(nhsub, expand.grid(Gender = unique(Gender), Age = sort(unique(Age))))
udf



udf1conf <- cbind(udf, predict(fm1, udf, interval = "confidence"))
head(udf1conf)
p <- ggplot(nhsub) + facet_wrap(~ Gender) + geom_point(aes(x = Age, y = BPSysAve, alpha = 0.2))

p + geom_errorbar(aes(x = Age, ymin = lwr, ymax = upr), data = udf1conf, col = 2)

# prediction intervals
udf1pred <- cbind(udf, predict(fm1, udf, interval = "prediction"))
p + geom_errorbar(aes(x = Age, ymin = lwr, ymax = upr), data = udf1pred, col = 2)

# Note that the interaction model can be obtained easily
fm2 <- lm(BPSysAve ~ Age * Gender, data = nhsub)
udf2conf <- cbind(udf, predict(fm2, udf, interval = "confidence"))

# Plot with different colors for comparison
p + geom_errorbar(aes(x = Age, ymin = lwr, ymax = upr), data = udf1conf, col = 2) + 
  geom_errorbar(aes(x = Age, ymin = lwr, ymax = upr), data = udf2conf, col = 4)

# Additive model allows intercepts to differ but not slope.
fm3 <- lm(BPSysAve ~ Age + Gender, data = nhsub)
udf3conf <- cbind(udf, predict(fm3, udf, interval = "confidence"))

# Plot with different colors for comparison
p + geom_errorbar(aes(x = Age, ymin = lwr, ymax = upr), data = udf1conf, col = 2) + 
  geom_errorbar(aes(x = Age, ymin = lwr, ymax = upr), data = udf3conf, col = 4)

anova(fm2)
summary(fm2)

# Quadratic model example.
fm4 <- lm(BPSysAve ~ poly(Age, 2) * Gender, data = nhsub)
udf4conf <- cbind(udf, predict(fm4, udf, interval = "confidence"))

# Plot with different colors for comparison
p + geom_errorbar(aes(x = Age, ymin = lwr, ymax = upr), data = udf1conf, col = 2) + 
  geom_errorbar(aes(x = Age, ymin = lwr, ymax = upr), data = udf4conf, col = 4)
