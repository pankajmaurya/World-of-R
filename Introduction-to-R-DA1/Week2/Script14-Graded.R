library(ggplot2)
library(NHANES)
library(dplyr)
nhsub <- subset(NHANES, Age >= 25)
nhsub <- nhsub[c("DirectChol", "Gender")]

male_values <- nhsub |> filter(Gender == "male") |> select(DirectChol)
female_values <- nhsub |> filter(Gender == "female") |> select(DirectChol)

t.test(male_values, female_values, alternative = "two.sided")

# Using the subset of the  NHANES  data that contains all female participants between the age of 20 and 30 (inclusive), 
# fit a simple linear regression model for response BMI and predictor Pulse. 
# What is the p-value, correct upto 3 digits after the decimal, for the test that BMI does not depend on Pulse?

nhsub2 <- subset(NHANES, Age >= 20 & Age <= 30 & Gender == "female")
nhsub3 <- nhsub2[c("BMI", "Pulse")]

library(ggplot2)
p <- ggplot(nhsub3)
p + geom_point(aes(x = Pulse, y = BMI))

fm1 <- lm(BMI ~ Pulse, data = nhsub3)
summary(fm1)



with(nhsub3, expand.grid(BMI = unique(BMI), Pulse = unique(Pulse)))

# leaving out the response variable.
udf <- unique(nhsub3[-1])
predict(fm1, udf)
udf1 <- cbind(udf, fit = predict(fm1, udf))
udf1
p + geom_point(aes(x = Pulse, y = BMI)) + geom_line(aes(x = Pulse, y = fit), data = udf1)


# Q.3
# Using the subset of the NHANES data that excludes all participants of age 24 or less, 
# determine the Race1 subgroup that has the highest correlation between Height and Weight among males.
nhsub4 <- subset(NHANES, Age >= 25 & Gender == "male")[c("Height", "Weight", "Race1")]
sample(nhsub4)

unique(nhsub4["Race1"])

races <- c("White", "Hispanic", "Mexican", "Black", "Other")
#races <- c("White")
p <- ggplot(nhsub4) + facet_wrap(~ Race1)

print(race)
for_race <- subset(nhsub4, Race1 == race)
print(cor(for_race$Height, for_race$Weight, use = "complete.obs"))
fm <- lm(Weight ~ Height, data = for_race)
udf <- unique(for_race[c("Height")])
udfconf <- cbind(udf, predict(fm, udf, interval = "confidence"))
p + geom_point(aes(x = Height, y = Weight)) + geom_line(aes(x = Height, y = fit), data = udfconf)



for (race in races) {
  print(race)
  for_race <- subset(nhsub4, Race1 == race)
  print(cor(for_race$Height, for_race$Weight, use = "complete.obs"))
  fm <- lm(Weight ~ Height, data = for_race)
  udf <- unique(for_race[c("Height")])
  udfconf <- cbind(udf, predict(fm, udf, interval = "confidence"))
  p + geom_point(aes(x = Height, y = Weight)) +
    geom_line(aes(x = Height, y = fit), data = udfconf)
}

# anova interaction model

fm6 <- aov(Weight ~ Height * factor(Race1), data = nhsub4)
sum(residuals(fm6)^2)
udf <- nhsub[c("Height", "Race1")]
udf6conf <- cbind(udf, predict(fm6, udf, interval = "confidence"))
p + geom_point(aes(x = Height, y = Weight)) + 
  geom_line(aes(x = Height, y = fit), data = udf6conf) + 
  geom_errorbar(aes(x = Height, ymin = lwr, ymax = upr), data = udf6conf, width = 0.1, col = 2)
