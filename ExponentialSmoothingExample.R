setwd("~/TechCareer/World-of-R")
df <- read.csv("SmoothingData.csv")
print(df)

ts_x <- ts(df$x, start = df$tm[1], frequency = 1)
ts_x

fit <- HoltWinters(ts_x, beta = FALSE, gamma = FALSE)
fit
plot(fit)
# 
# fit_ses <- HoltWinters(df$x, beta = FALSE, gamma = FALSE)
# fit_ses
# plot(fit_ses)