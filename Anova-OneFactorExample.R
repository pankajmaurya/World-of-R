setwd("~/TechCareer/World-of-R")
df <- read.csv("fertilizer-yields.csv")
print(df)

# H0: Mean Yield is the same for all fertiliser types
# H1: At least one fertiliser has a different mean

# Since fertilizer is numeric, it is not being treated as factor
df$Fertiliser <- factor(df$Fertiliser)

anova_result <- aov(Yield ~ Fertiliser, data = df)
summary(anova_result)

plot(Yield ~ Fertiliser, data = df)
