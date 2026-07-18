# Plot the variable AT against Waist for the data set waist of the R package lmreg, along with the least squares fitted line. 
# Do you see any pattern in the plot that needs special attention?
library("lmreg")
#waist1 = lm(AT~Waist, data = waist)
#plot(waist$Waist, waist$AT,
#     xlab = "Waist circumference (cm)",
#     ylab = "Deep abdominal AT (adipose tissue) area",
#     main = "AT vs Waist with fitted line",
#     pch = 19, col = "steelblue")
#abline(waist1, col = "red", lwd = 2)


data(waist); 
plot(waist)

abline(lm(AT~Waist,data=waist))
