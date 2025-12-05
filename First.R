x <- c(1,2,3,4);
x

setwd("~/TechCareer/World-of-R")
test <- read.csv("test.csv")
head(test)

tosses <- read.csv("tosses.csv")
head(tosses)

hist(tosses$X1)

# https://eleuven.github.io/statthink/ChapDescriptiveStat.html

# https://r4ds.hadley.nz/intro.html
# https://book.stat420.org/introduction-to-r.html
# file:///Users/pankaj/Downloads/ISLP_website.pdf
# https://book.stat420.org/simple-linear-regression.html
View(cars)
