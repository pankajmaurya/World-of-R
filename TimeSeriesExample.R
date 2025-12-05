setwd("~/TechCareer/World-of-R")
df <- read.csv("sunspot.csv")
print(df)

df$date <- as.Date(paste(df$year, df$months, "1", sep="-"), format="%Y-%b-%d")

plot(df$date, df$sunspot, type="l",
     xlab="Year", ylab="Sunspot Count",
     main="Monthly Sunspot Time Series")

library(ggplot2)

ggplot(df, aes(x=date, y=sunspot)) +
  geom_line() +
  labs(title="Monthly Sunspot Time Series",
       x="Year", y="Sunspot Count")

#head(df)

ts_sun <- ts(df$sunspot, start=min(df$year), frequency=12)
plot(ts_sun)

acf(ts_sun, lag.max = 200, main="ACF of Sunspot Series")

spec <- spectrum(ts_sun, log="no")

# Period in months for the dominant frequency:
period_months <- 1 / spec$freq[which.max(spec$spec)]
period_years <- period_months / 12

period_years
period_months
