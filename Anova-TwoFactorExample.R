setwd("~/TechCareer/World-of-R")
df <- read.csv("VarietyFertilizerYield.csv")
print(df)

# Since fertilizer is numeric, it is not being treated as factor
df$Fertiliser <- factor(df$Fertiliser)
df$Variety <- factor(df$Variety)

# Interaction model
model_int <- aov(Yield ~ Variety * Fertiliser, data = df)
summary(model_int)

interaction.plot(df$Fertiliser, df$Variety, df$Yield,
                 xlab="Fertiliser", ylab="Mean Yield")

# Additive model (no interaction)
anova_result <- aov(Yield ~ Fertiliser + Variety, data = df)
summary(anova_result)
