library(dplyr)
library(readr)

setwd("~")
df <- read.csv("TechCareer/World-of-R/ytfinance-nifty-2010-now.csv")
df <- df %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

df <- df %>%
  mutate(
    ret = close / lag(close) - 1,
    ret_t1 = lag(ret, 1),   # yesterday
    ret_t2 = lag(ret, 2)    # day before yesterday
  )

df <- df %>%
  mutate(
    cum2 = (1 + ret_t2) * (1 + ret_t1) - 1
  )

df <- df %>%
  mutate(
    setup = (
      ret_t2 <= -0.005 &
        ret_t1 <= -0.005 &
        cum2   <= -0.018
    )
  )

df <- df %>%
  mutate(
    scenario1 = setup & ret <= -0.002,
    scenario2 = setup & ret >=  0.005
  )

total_setups <- sum(df$setup, na.rm = TRUE)
s1_count     <- sum(df$scenario1, na.rm = TRUE)
s2_count     <- sum(df$scenario2, na.rm = TRUE)

prob_s1 <- s1_count / total_setups
prob_s2 <- s2_count / total_setups

cat("Total setups:", total_setups, "\n")
cat("Scenario 1 (continuation) probability:", prob_s1, "\n")
cat("Scenario 2 (rebound) probability:", prob_s2, "\n")

df %>%
  filter(setup) %>%
  summarise(
    avg_ret_day3 = mean(ret, na.rm = TRUE),
    median_ret_day3 = median(ret, na.rm = TRUE)
  )



df %>%
  filter(setup) %>%
  summarise(
    avg_day3 = mean(ret, na.rm = TRUE),
    median_day3 = median(ret, na.rm = TRUE),
    avg_when_down = mean(ret[ret <= -0.002], na.rm = TRUE),
    avg_when_up = mean(ret[ret >= 0.005], na.rm = TRUE)
  )

df <- df %>%
  mutate(ma200 = zoo::rollmean(close, 200, fill = NA, align = "right"),
         bull = close > ma200)
library(zoo)

df <- df %>%
  arrange(date) %>%
  mutate(
    ma200 = rollmean(close, 200, fill = NA, align = "right"),
    bull = close > ma200
  )

df %>%
  filter(setup, bull == TRUE) %>%
  summarise(
    count = n(),
    mean_day3 = mean(ret, na.rm=TRUE),
    prob_cont = mean(ret <= -0.002, na.rm=TRUE),
    prob_reb  = mean(ret >= 0.005, na.rm=TRUE)
  )
df %>%
  filter(setup, bull == FALSE) %>%
  summarise(
    count = n(),
    mean_day3 = mean(ret, na.rm=TRUE),
    prob_cont = mean(ret <= -0.002, na.rm=TRUE),
    prob_reb  = mean(ret >= 0.005, na.rm=TRUE)
  )


df %>%
  filter(setup, bull == TRUE) %>%
  summarise(
    avg_down = mean(ret[ret <= -0.002], na.rm=TRUE),
    avg_up   = mean(ret[ret >= 0.005], na.rm=TRUE)
  )

df %>%
  filter(setup, bull == FALSE) %>%
  summarise(
    avg_down = mean(ret[ret <= -0.002], na.rm=TRUE),
    avg_up   = mean(ret[ret >= 0.005], na.rm=TRUE)
  )


##### How often regime changes on 3rd day?
df <- df %>%
  arrange(date) %>%
  mutate(
    ma200 = zoo::rollmean(close, 200, fill = NA, align = "right"),
    bull = close > ma200
  )

# Identify setup days that were in bull regime
events <- df %>%
  filter(setup, bull == TRUE)

# Now check if next day closes below its own 200 SMA
flip_data <- events %>%
  mutate(
    next_close = lead(close),
    next_ma200 = lead(ma200),
    flip = next_close < next_ma200
  )

flip_data %>%
  summarise(
    total = n(),
    flips = sum(flip, na.rm = TRUE),
    flip_probability = mean(flip, na.rm = TRUE)
  )


#### How often regime changes after X days from setup?

library(dplyr)
library(zoo)

# Ensure sorted
df <- df %>%
  arrange(date) %>%
  mutate(
    ma200 = rollmean(close, 200, fill = NA, align = "right"),
    bull = close > ma200
  )

# Indices of setup events in bull regime
event_idx <- which(df$setup & df$bull)

check_flip_within <- function(x_days) {
  flips <- sapply(event_idx, function(i) {
    # Define forward window safely
    end_idx <- min(i + x_days, nrow(df))
    
    # Check if any close below SMA in forward window
    any(df$close[(i+1):end_idx] < df$ma200[(i+1):end_idx], na.rm = TRUE)
  })
  
  mean(flips, na.rm = TRUE)
}

results <- data.frame(
  X = 2:5,
  flip_probability = sapply(2:5, check_flip_within)
)

results

## How probability changes with cushion from 200 SMA
library(dplyr)
library(zoo)

df <- df %>%
  arrange(date) %>%
  mutate(
    ma200 = rollmean(close, 200, fill = NA, align = "right"),
    bull = close > ma200,
    cushion_pct = (close - ma200) / ma200
  )

check_flip_within <- function(i, x_days) {
  end_idx <- min(i + x_days, nrow(df))
  future_idx <- (i + 1):end_idx
  
  any(df$close[future_idx] < df$ma200[future_idx], na.rm = TRUE)
}

event_idx <- which(df$setup & df$bull)

events <- data.frame(
  index = event_idx,
  cushion_pct = df$cushion_pct[event_idx]
)

events$flip5 <- sapply(events$index, check_flip_within, x_days = 5)

events$cushion_bin <- cut(
  events$cushion_pct,
  breaks = c(0, 0.0025, 0.005, 0.01, 0.02, 0.05),
  include.lowest = TRUE
)

flip_stats <- events %>%
  group_by(cushion_bin) %>%
  summarise(
    count = n(),
    flip_probability = mean(flip5)
  )

flip_stats

library(ggplot2)

ggplot(flip_stats, aes(x = cushion_bin, y = flip_probability)) +
  geom_bar(stat = "identity") +
  labs(
    title = "5-Day Regime Flip Probability vs Cushion from 200 SMA",
    x = "Cushion at Setup",
    y = "Flip Probability (within 5 days)"
  ) +
  theme_minimal()

events$cushion_pct100 <- events$cushion_pct * 100

events$cushion_bin <- cut(
  events$cushion_pct100,
  breaks = c(0, 0.25, 0.5, 1, 2, 5),
  include.lowest = TRUE
)

flip_stats <- events %>%
  group_by(cushion_bin) %>%
  summarise(
    count = n(),
    flip_probability = mean(flip5) * 100
  )

current_cushion <- (25450 - 25292) / 25292 * 100
current_cushion

library(ggplot2)

ggplot(flip_stats, aes(x = cushion_bin, y = flip_probability)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_vline(xintercept = which(levels(flip_stats$cushion_bin) ==
                                  levels(events$cushion_bin)[findInterval(current_cushion,
                                                                          c(0,0.25,0.5,1,2,5))]),
             color = "red",
             linewidth = 1.2) +
  labs(
    title = "5-Day Regime Flip Probability vs Cushion from 200 SMA",
    subtitle = paste("Current Cushion ≈", round(current_cushion,2), "%"),
    x = "Distance Above 200 SMA (%)",
    y = "Flip Probability within 5 Days (%)"
  ) +
  theme_minimal(base_size = 14)



# Alternative visualization
ggplot(events, aes(x = cushion_pct100, y = as.numeric(flip5))) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_vline(xintercept = current_cushion, color = "red", linewidth = 1.2) +
  labs(
    title = "Smoothed 5-Day Flip Probability vs Cushion",
    subtitle = paste("Red line = Current Cushion (", round(current_cushion,2), "%)", sep=""),
    x = "Distance Above 200 SMA (%)",
    y = "Flip Probability (smoothed)"
  ) +
  theme_minimal(base_size = 14)
