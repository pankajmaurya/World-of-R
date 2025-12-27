library(ggplot2)
library(NHANES)

levels(NHANES$BMI_WHO)

# Perform a chi-squared test for the hypothesis that Race1 and BMI_WHO are independent
with(NHANES, chisq.test(Race1, BMI_WHO))

# Common to use the test in its contingency table form. Table or xtabs function can be used.

rbtab <- xtabs(~Race1 + BMI_WHO, data = NHANES)
print(rbtab)

chisq.test(rbtab)
# gives identical output

# To exclude children
rbtab <- xtabs( ~Race1 + BMI_WHO, data = NHANES, subset = (Age >= 21))
print(rbtab)

# Lets merge first column with next column as counts are small ( < 5)
print(cbind(upto_24.9 = rbtab[,1] + rbtab[,2], rbtab[, 3:4]))

# We create a new table from the raw BMI data using the cut function
# This can be used to bin a numerical variable to create a factor by providing a vector of break points.
rbtab2 <- xtabs(~ Race1 + cut(BMI, breaks = c(0,25, 30, Inf), right = FALSE),
                data = NHANES, subset = (Age >= 21))
rbtab2
names(dimnames(rbtab2))[2] <- "BMI_cat"
print(rbtab2)

chisq.test(rbtab2)
# The chi-square statistic is even larger now, indicating even stronger dependence between Race and BMI

rbdf <- as.data.frame(rbtab2)
print(rbdf)


# Visualize
options(repr.plot.width = 12)
library(lattice)
dotplot(Freq ~ reorder(BMI_cat, Freq), data = rbdf, groups = Race1, 
        type = c("p", "a"), scales = list(y = list(log = TRUE, equispaced.log = FALSE)),
        par.settings = simpleTheme(pch = 16), auto.key = list(space = "right"))

# Another way to visualize contingency tables is via mosaic plots - areas are proportional

mosaicplot(rbtab2)
