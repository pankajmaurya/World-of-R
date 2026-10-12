# Computational example of detection of serial correlation
library(readxl)
setwd("/Users/pankaj/Downloads")
rainfall <- read_excel("Rainfall.xlsx", sheet = "July16to31")
head(rainfall)
print(rainfall, n=30)
length(rainfall)
library(leaps)

# Keep only the years with complete Basirhat data (1991-2018; data starts in 1991)
rain <- as.data.frame(rainfall[rainfall$Year <= 2018, ])
nrow(rain)   # should be 28

# Predictors and response
xlist <- cbind(rain$Alipur, rain$Canning, rain$DHarbour, rain$Dumdum)
colnames(xlist) <- c("Alipur", "Canning", "DHarbour", "Dumdum")

# Mallows' Cp, best subset of each size
out <- leaps(x = xlist, y = rain$Basirhat, method = "Cp",
             nbest = 1, names = colnames(xlist))
out

# Subset with the smallest Cp
best <- which.min(out$Cp)
colnames(xlist)[out$which[best, ]]
out$Cp[best]


fit <- lm(Basirhat ~ Alipur + Dumdum, data = rain)
summary(fit)$r.squared   # R-square
summary(fit)$sigma       # residual standard error


library(MASS)
fit <- lm(Basirhat ~ Alipur + Dumdum, data = rain)
bc <- boxcox(fit, lambda = seq(-2, 2, 0.01))
bc$x[which.max(bc$y)]   # about 0.03 -> choose 0, the log transform


fit2 <- lm(I(Basirhat^0.03) ~ Alipur + Dumdum, data = rain)
summary(fit2)   # look at the Alipur row: t value and Pr(>|t|)



fit2 <- lm(I(Basirhat^0.03) ~ Alipur + Dumdum, data = rain)
library(lmtest)
dwtest(fit2)                 # serial correlation
acf(residuals(fit2))         # lag plot
shapiro.test(residuals(fit2))
library(nortest)
e <- residuals(lm(I(Basirhat^0.03) ~ Alipur + Dumdum, data = rain))
ad.test(e)       # Anderson-Darling
lillie.test(e)   # Kolmogorov-Smirnov with Lilliefors correction
# ks.test(e, "pnorm", mean(e), sd(e))   # not valid: parameters estimated


qqnorm(residuals(fit2)); qqline(residuals(fit2))




fit2 <- lm(I(Basirhat^0.03) ~ Alipur + Dumdum, data = rain)
newd <- data.frame(Alipur = c(7.5, 11.5), Dumdum = c(3.77, 13.2))
p <- predict(fit2, newdata = newd, interval = "prediction")
p^(1/0.03)   # back-transformed fit, lwr, upr


fit2 <- lm(I(Basirhat^0.03) ~ Alipur + Dumdum, data = rain)
which(hatvalues(fit2) > 2*3/28)       # high leverage (row indices; row 1 = 1991)
which(abs(rstandard(fit2)) > 2)       # large standardized residuals


cd <- cooks.distance(fit2)
which.max(cd)          # row index (8 = 1998)
rain$Year[which.max(cd)]
