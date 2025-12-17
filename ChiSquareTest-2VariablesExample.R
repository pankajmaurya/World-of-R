setwd("~/TechCareer/World-of-R")
df <- read.csv("items-with-type-defective.csv")
#print(df)

obs <- table(df$type, df$defective)

chisq.test(obs)

