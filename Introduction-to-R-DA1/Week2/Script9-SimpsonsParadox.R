library(ggplot2)
library(NHANES)

nhsub <- subset(NHANES, Age >= 21)

fm <- lm(DirectChol ~ Height, data = nhsub)
summary(fm)

# Model DirectChol = beta0 + beta1*height + e
# The large absolute t-value indicates strong evidence against H0: beta_height = 0
# Height is statistically associated with cholestrol.

# Adjusted R-squared = 0.0454 => Height explains only 4.5% of the variation in Cholestrol.
# A classic example of Statistically significant but weak explanatory power.
# F test:
# Null hypothesis that all slope coefficients are zero.
# p < 2.2e-16, strong evidence that the model fits better than an intercept only model.

# The test for the hypothesis that the slope is zero is strongly rejected.

# If we conclude that direct cholestrol depends on height, our conclusion would be flawed.
# Reason: We are ignoring another covariate which is correlated with both direct cholestrol
# and height, namely gender.

fm.male <- lm(DirectChol ~ Height, data = nhsub, subset = (Gender == "male"))
fm.female <- lm(DirectChol ~ Height, data = nhsub, subset = (Gender == "female"))

summary(fm.male)
summary(fm.female)

library(lattice)
xyplot(DirectChol ~ Height, data = nhsub, groups = Gender, 
       alpha = 1, type = c("p", "r"), grid = TRUE,
       abline = list(coef(fm), col = "black", lwd = 2))

# Even though the individual regression lines for the subgroups are more or less flat, the location
# shift in the distributions of both direct cholestrol and height lead to a significant negative slope
# when the two groups are combined.