# -----------------------------
# 1. READ CSV
# -----------------------------
setwd("~/TechCareer/World-of-R")
df <- read.csv("marks-10-and-12.csv")
#df <- read.csv("yourfile.csv")   # <-- replace with your CSV filename
df$id <- 1:nrow(df)

# -----------------------------
# 2. PAIRED t-TEST
# -----------------------------
#print(t.test(df$before, df$after, paired = TRUE))
print(t.test(marks$marks10, marks$marks12, paired = TRUE))

before <- marks$marks10
after <- marks$marks12


# ---------------------------------------
# 3. Plots
# ---------------------------------------
library(ggplot2)
library(tidyr)

# ---- Paired Line Plot ----
p1 <- ggplot(df, aes(x = factor(id))) +
  geom_point(aes(y = marks10), size = 3) +
  geom_point(aes(y = marks12), size = 3, color = "blue") +
  geom_line(aes(y = marks10, group = id)) +
  geom_line(aes(y = marks12, group = id), color = "blue") +
  labs(
    x = "Student ID",
    y = "Marks",
    title = "Paired Plot: Class 10 vs Class 12 Marks"
  ) +
  theme_minimal()

print(p1)

# ---- Difference Plot ----
df$diff <- df$marks12 - df$marks10

p2 <- ggplot(df, aes(x = factor(id), y = diff)) +
  geom_point(size = 3, color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Student ID",
    y = "Difference (12th - 10th)",
    title = "Difference Plot"
  ) +
  theme_minimal()

print(p2)

# ---- Boxplot ----
df_long <- pivot_longer(df, cols = c(marks10, marks12),
                        names_to = "class",
                        values_to = "marks")

p3 <- ggplot(df_long, aes(x = class, y = marks)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Marks Comparison: Class 10 vs Class 12",
    x = "Class",
    y = "Marks"
  ) +
  theme_minimal()

print(p3)

