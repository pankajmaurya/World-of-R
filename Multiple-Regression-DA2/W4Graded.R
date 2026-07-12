rm(list=ls())
library("alr4")
data(wblake)
head(wblake)
y=wblake$Age
x1=wblake$Length
x2=x1^2
m=lm(y~x1+x2)
summary(m)

# length of prediction interval for fish of length 200?
newdat <- data.frame(x1 = 200, x2 = 200^2)
newdat
predAT = predict(m,newdat,interval = "prediction", level = 0.95)
round(predAT[,3] - predAT[,2], 2)

predAT
length_PI <- predAT[, "upr"] - predAT[, "lwr"]
round(length_PI, 2)
#head(wblake)

# scatter plot of raw data
plot(x1, y, pch = 16, col = "gray40",
     xlab = "Length", ylab = "Age",
     main = "Age vs Length with fitted quadratic curve")

# generate a smooth sequence of x1 values for the curve
xseq <- seq(min(x1), max(x1), length.out = 200)
newdat_seq <- data.frame(x1 = xseq, x2 = xseq^2)

# fitted curve
fit_seq <- predict(m, newdat_seq)
lines(xseq, fit_seq, col = "blue", lwd = 2)

# add 95% prediction interval band
pred_band <- predict(m, newdat_seq, interval = "prediction", level = 0.95)
lines(xseq, pred_band[, "lwr"], col = "red", lty = 2)
lines(xseq, pred_band[, "upr"], col = "red", lty = 2)

# mark the specific prediction at x1 = 200
newdat <- data.frame(x1 = 200, x2 = 200^2)
predAT <- predict(m, newdat, interval = "prediction", level = 0.95)
points(200, predAT[, "fit"], col = "darkgreen", pch = 17, cex = 1.5)
segments(200, predAT[, "lwr"], 200, predAT[, "upr"], col = "darkgreen", lwd = 2)

legend("topleft", legend = c("Data", "Fitted curve", "95% PI band", "Prediction at x1=200"),
       col = c("gray40", "blue", "red", "darkgreen"),
       pch = c(16, NA, NA, 17), lty = c(NA, 1, 2, NA), lwd = c(NA, 2, 1, 2))
