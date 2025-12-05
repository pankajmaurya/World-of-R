setwd("~/TechCareer/World-of-R")
df <- read.csv("multiplicative-timeseries.csv")
print(df)

plot(df$time, df$y, type="l",
     xlab="time", ylab="Y",
     main="Time Series")


plot(df$time, log(df$y), type="l",
     xlab="time", ylab="log Y",
     main="Time Series")