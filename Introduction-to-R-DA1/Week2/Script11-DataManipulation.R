library(dplyr)
library(NHANES)

nhsub <- NHANES |> filter(Age >= 21) |>
  select(Race = Race1, Gender, Height, Weight) |>
  mutate(BMI = Weight / (Height/100)^2)
str(nhsub)

bmi.median <- with(nhsub, tapply(BMI, list(race = Race, gender = Gender),
                                 FUN = median, na.rm = TRUE))
bmi.median <- as.data.frame(as.table(bmi.median), responseName = "median")
bmi.median

library(lattice)
dotplot(reorder(race, median) ~ median, bmi.median, groups = gender,
        par.settings = simpleTheme(pch = 16), auto.key = TRUE,
        xlab = "Median BMI")

# Summarize the mean
bmi.mean <- summarise(group_by(nhsub, Race, Gender),
                      mean_bmi = mean(BMI, na.rm = TRUE))
# Equivalent pipeline notation.
bmi.mean <- nhsub |> group_by(Race, Gender) |>
  summarise(mean_bmi = mean(BMI, na.rm = TRUE))
bmi.mean
library(ggplot2)
ggplot(bmi.mean) +
  geom_point(aes(x = mean_bmi, y = reorder(Race, mean_bmi),
                 color = Gender)) + ylab("Race")

# calculating and plotting confidence intervals
normalci <- function(x, conf=0.95) {
  tt <- t.test(x)
  with(tt, data.frame(center = statistic,
                      lower = conf.int[[1]],
                      upper = conf.int[[2]]))
}

p <- nhsub |> group_by(Race, Gender) |> summarise(normalci(BMI)) |> ggplot()
p + geom_errorbar(aes(y = reorder(Race, lower + upper),
                      xmin = lower, xmax = upper,
                      color = Gender, width = 0.2),
                  position = "dodge") + xlab("BMI") + ylab("Race")
