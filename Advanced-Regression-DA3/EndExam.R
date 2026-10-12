library(GLMsData)
data("nminer")
y=nminer$Minerab
x=nminer$Eucs
pfit=glm(y~x, family = poisson(link = "log"), nminer)
summary(pfit)

library(ISLR)
data("Credit")
y=Credit$Balance
x1=Credit$Income
x2=Credit$Limit
x3=Credit$Age
library(car) # for VIF
fit = lm(y~x1+x2+x3)
vif(fit)


data("mtcars") 
y = mtcars$am
x1 = mtcars$mpg
x2 = mtcars$cyl
x3 = mtcars$hp

fit_F = glm(y~x1 + x2 + x3, family = binomial(link="probit"))
fit_R1 = glm(y ~ x1 + x2, family = binomial(link="probit"))
fit_R2 = glm(y ~ x1 + x3, family = binomial(link="probit"))


anova(fit_R1, fit_F, test ="LRT" )
# pvalue = 0.0007 => Model 1 is not adequate.
anova(fit_R2, fit_F, test ="LRT" )
# pvalue = 0.3742 => Model 2 is as good as full at 10% sig

library(glmnet) # for penalized regressions: ridge, lasso, elastic net
library(GLMsData)
data("nhospital")
X=model.matrix(MainHours~., nhospital)[,-1] # design matrix
y=na.exclude(nhospital$MainHours)
lasso_fit=glmnet(X, y, alpha=1, lambda=10)
y_pred=predict(lasso_fit, s=10, newx = X)
MSE=mean((y-y_pred)^2)
MSE

grid = 10^seq(3,-3, length=100) # from 10^3 to 10^(-2)
set.seed(567)
ri.cv = cv.glmnet(X, y, alpha = 0.50, lambda = grid, nfolds = 4)
bestlam = ri.cv$lambda.min 
bestlam


########

library(datasets)
?EuStockMarkets
head(EuStockMarkets)
SMI = EuStockMarkets[,2]
FTSE = EuStockMarkets[,4]
n = length(SMI)
x = SMI
y = FTSE
xydat = data.frame(sx=sort(x),sy=y[order(x)])


library(splines)

sx = xydat$sx
sy = xydat$sy

# 1. Natural cubic spline with 7 degrees of freedom
# ns() with df = 7 places 6 interior knots (intercept is separate in lm)
fit_ns = lm(sy ~ ns(sx, df = 7))
pred_ns = predict(fit_ns, newdata = data.frame(sx = sx))

# 2. Smoothing spline with 7 effective degrees of freedom
fit_ss = smooth.spline(x = sx, y = sy, df = 7)
pred_ss = predict(fit_ss, x = sx)$y

# 3. Kernel regression with Epanechnikov kernel, bandwidth 500
# ksmooth() has only "box" and "normal" kernels, so we implement Epanechnikov directly
epan = function(u) ifelse(abs(u) <= 1, 0.75 * (1 - u^2), 0)

kern_reg = function(x0, x, y, h) {
  w = epan((x - x0) / h)
  if (sum(w) == 0) return(NA)
  sum(w * y) / sum(w)
}

pred_kr = sapply(sx, kern_reg, x = sx, y = sy, h = 500)

# Plot
plot(sx, sy, pch = 16, cex = 0.5, col = "green",
     xlab = "SMI", ylab = "FTSE",
     main = "FTSE vs SMI: Three Smoothers")
lines(sx, pred_ns, col = "cyan",  lwd = 2)
lines(sx, pred_ss, col = "black", lwd = 2)
lines(sx, pred_kr, col = "red",   lwd = 2)

legend("topleft",
       legend = c("Natural cubic spline (df = 7)",
                  "Smoothing spline (df = 7)",
                  "Kernel regression (Epanechnikov, h = 500)"),
       col = c("cyan", "black", "red"),
       lwd = 2, bty = "n")

