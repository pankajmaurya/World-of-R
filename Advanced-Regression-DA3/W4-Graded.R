rm(list = ls(all.names = TRUE))

library(ISLR) # data from book ISLR
data(Credit)
?Credit
head(Credit)

library(corrplot) # plot correlation
X = model.matrix(Balance~., Credit[, -1])[,-1] # design matrix without intercept

#(2) Variance Inflation Factor 
library(car) # for VIF
fit = lm(Balance~., data = Credit[, -1])
vif(fit)

#(3) Condition number
lam = eigen(t(X)%*%X)$values # eigenvalues of X^T*X
condition_indices = max(lam)/lam # condition indexes
condition_number= max(condition_indices)  # condition number 
condition_number

####

library(glmnet) # for penalized regressions:ridge, lasso, elastic net

y = na.exclude(Credit$Balance) # response excluding missing values

en_fit = glmnet(X,y, alpha = 0.5, lambda = 10)

coef(en_fit)
predict(en_fit, type = "coefficients", s = 10)


# contrast with pure ridge at alpha = 0
coef(glmnet(X,y, alpha = 0, lambda = 10))
# contrast with pure lasso at alpha = 1
coef(glmnet(X,y, alpha = 1, lambda = 10))

########
########

library(GLMsData)
data("nhospital")
library(glmnet) 

X = model.matrix(MainHours~., data=nhospital)[,-1] 
y = nhospital$MainHours

grid = 10^seq(3,-2, length=100) # from 10^3 to 10^(-2)

lasso_fit = glmnet(X,y, alpha = 1, lambda = grid)
lasso_fit_exact_lambda = glmnet(X,y, alpha = 1, lambda = 300)

# (1) L1 norm for lambda = 0 (least square)
betaLS = predict(lasso_fit, type = "coefficients", s = 0)[1:4, ]
(L1norm = sum( abs(betaLS[-1])))

# another way
round(sum(abs(summary(lm(MainHours ~ Cases + Eligible + OpRooms, data = nhospital))$coef[2:4])), 2)


# (2) L1 norm for lambda = 30
betaLASSO = predict(lasso_fit_exact_lambda, type = "coefficients", s = 300)[1:4, ]
(L1norm = sum( abs(betaLASSO[-1])) )

# See the interpolation value, but do not use in solution.
betaLASSO = predict(lasso_fit, type = "coefficients", s = 300)[1:4, ]
(L1norm = sum( abs(betaLASSO[-1])) )

######
######

# The mean square errors for ordinary least square regression and ridge regression with penalty parameter λ = 200

y_pred = predict(lasso_fit, s=0, newx = X)
MSE = mean((y - y_pred)^2) 
(MSE_OLS = MSE)

ols = lm(MainHours ~ Cases + Eligible + OpRooms, data = nhospital)
ypred_ols = predict(ols)
MSE = mean((y - ypred_ols)^2) 
MSE
(MSE_OLS2 = MSE)

ridge_fit = glmnet(X,y,alpha = 0,lambda = 200, standardize = TRUE)
y_pred = predict(ridge_fit, s=200, newx = X)
MSE = mean((y - y_pred)^2) 
(MSE_RIDGE = MSE)



grid = 10^seq(3,-3, length=100) # from 10^3 to 10^(-2)
set.seed(123)

ri.cv = cv.glmnet(X, y, alpha = 1, lambda = grid, nfolds = 3)
bestlam = ri.cv$lambda.min 
bestlam
