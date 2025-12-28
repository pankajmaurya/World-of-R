
## Follow instructions given in the assignment.

## Write your code below. Your code must produce the variables

measles <- read.csv("/Users/pankaj/TechCareer/World-of-R/Introduction-to-R-DA1/Week2/data/measles.csv")
filtered_measles <- measles[!is.na(measles[c("rate")]), ]

numNA <- nrow(measles) - nrow(filtered_measles)
library(dplyr)
medianRateNA <- numeric(8)
meanRateNA <- numeric(8)
years <- c(1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000)
years_str <- c("1930", "1940", "1950", "1960", "1970", "1980", "1990", "2000")
index <- 1
for (y in years) {
  print(y)
  t <- filtered_measles |> filter(year == y) |> select(rate)
  medianRateNA[index] <- median(t[,1])
  meanRateNA[index] <- mean(t[, 1])
  index <- index + 1
}

medianRate0 <- numeric(8)

measles2 <- measles
nrow(measles2[is.na(measles2$rate), ])
# Fix values to be 0 for all NA in measles2
measles2$rate[is.na(measles2$rate)] <- 0
nrow(measles2[is.na(measles2$rate), ])

medianRate0 <- with(measles2, tapply(rate, list(year), FUN = median))[years_str]
meanRate0  <- with(measles2, tapply(rate, list(year), FUN = mean))[years_str]

## Do not write or edit any code below this line, but run the code
## to make sure that your code has defined the required variables.

## Tests: 

stopifnot(exists("numNA"), is.numeric(numNA),length(numNA) == 1)
stopifnot(exists("medianRateNA"), is.numeric(medianRateNA),length(medianRateNA) == 8)
stopifnot(exists("medianRate0"), is.numeric(medianRate0),length(medianRate0) == 8)
stopifnot(exists("meanRateNA"), is.numeric(meanRateNA),length(meanRateNA) == 8)
stopifnot(exists("meanRate0"), is.numeric(meanRate0),length(meanRate0) == 8)
