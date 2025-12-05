setwd("~/TechCareer/World-of-R")
marks <- read.csv("marks-10-and-12.csv")
head(marks)

t.test(marks$marks10, marks$marks12, paired = TRUE)
