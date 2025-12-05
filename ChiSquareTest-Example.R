setwd("~/TechCareer/World-of-R")
df <- read.csv("plant-categories-100-sample.csv")
#print(df)

# We are given A,B,C,D probabilities as 0.1,0.4,0.2,0.3

obs <- table(df$type)
#obs

p <- c(A = 0.1, B = 0.4, C = 0.2, D = 0.3)
chisq.test(obs, p = p)