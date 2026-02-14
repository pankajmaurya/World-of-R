# write.csv(cars, "TechCareer/World-of-R/cars.csv")
setwd("~")
nifty <- read.csv("TechCareer/World-of-R/ytfinance-nifty-2010-now.csv")
head(nifty)

plot(volume ~ factor(date), data = nifty)

plot(high ~ factor(date), data = nifty)
head(nifty)
daily_highs <- nifty[, 4]
daily_lows <- nifty[, 5]
daily_closes <- nifty[, 3]
daily_opens <- nifty[, 6]
volumes <- nifty[, 7]

length(daily_lows)
daily_ranges <- daily_highs - daily_lows
dates <- nifty[, 2]
plot(factor(dates), daily_ranges)

end <- length(daily_closes)
num_days <- 400
plot(factor(dates[(end - num_days):end]), daily_ranges[(end - num_days):end])


plot(factor(dates[(end - num_days):end]), volumes[(end - num_days):end])

plot(factor(dates[(end - num_days):end]), daily_lows[(end - num_days):end])

plot(factor(dates[(end - num_days):end]), daily_closes[(end - num_days):end])

daily_returns <- diff(daily_closes)

plot(factor(dates[(end - num_days):end]), daily_returns[(end - 1 - num_days):(end - 1)])
