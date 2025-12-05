setwd("~/TechCareer/World-of-R")
df <- read.csv("bodyfat.csv")
print(df)

model <- lm(BodyFat ~ Weight + Neck + Abdomen, data = df)
summary(model)

# model <- lm(BodyFat ~ Hip + Knee + Biceps + Forearm, data = df)
# summary(model)