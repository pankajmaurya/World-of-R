m <- sample(month.name, 100, replace = TRUE)
mt <- sort(table(m), decreasing = TRUE)
mdf <- data.frame(mt)
str(mdf)

library(ggplot2)
library(lattice)
library(graphics)
options(repr.plot.width = 12, repr.plot.height = 6) # change image size in Jupyter
gp <- ggplot(mdf, aes(x = m, y = Freq)) + geom_col()
lp <- barchart(Freq ~ m, mdf, origin = 0)
tp <- barplot(Freq ~ m, mdf)
mt
print(gp)
print(lp)
update(lp, xlab = "month", ylab = "frequency")

gp + xlab("month") + ylab("frequency")
print(tp)
