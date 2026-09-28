#### RIDGE REGRESSION #########
rm(list = ls(all.names = TRUE))

library(ISLR) # data from book ISLR
data(Hitters)



# === check for multicollinearity ===

#(1) check correlation matrix
library(corrplot) # plot correlation
X = model.matrix(Salary~., Hitters)[,-1] # design matrix without intercept
corrplot(cor(X), method="number")


#(2) Variance Inflation Factor 
library(car) # for VIF
fit = lm(Salary~., data = Hitters)
vif(fit)

#(3) Condition number
X = model.matrix(Salary~., Hitters)[,-1] # design matrix without intercept
lam = eigen(t(X)%*%X)$values # eigenvalues of X^T*X
condition_indices = max(lam)/lam # condition indexes
condition_number= max(condition_indices)  # condition number 
condition_number # > 424 million 

#================================================================
summary(lm(Salary~., data = Hitters)) # check fitted linear model
#================================================================

###################### Ridge Regression #########################

library(glmnet) # for penalized regressions:ridge, lasso, elastic net
X = model.matrix(Salary~., Hitters)[,-1] # design matrix
y = na.exclude(Hitters$Salary) # response excluding missing values


grid = 10^seq(10,-2, length=100) # from 10^(10) to 10^(-2)

ridge_fit = glmnet(X,y,alpha = 0,lambda = grid, standardize = TRUE)
plot(ridge_fit, xvar="lambda", label=TRUE)

dim( coef(ridge_fit) )

#= Comparing L2 norm of ridge regression coefficient estimates

# (1) lambda = 2848.036
ridge_fit$lambda[55]
coef(ridge_fit)[,55]
L2norm = sqrt(sum(  coef(ridge_fit)[-1, 55]^2 ))
L2norm

# (2) lambda = 174.7528
ridge_fit$lambda[65]
coef(ridge_fit)[, 65]
L2norm = sqrt(sum(  coef(ridge_fit)[-1, 65]^2 ))
L2norm

#== Prediction and MSE ========

y_pred = predict(ridge_fit, s=5, newx = X)
MSE = mean((y - y_pred)^2) 
MSE

y_pred = predict(ridge_fit, s=10^10, newx = X)
(MSE = mean((y - y_pred)^2) )


# Ridge best penalty using cv
set.seed(123) # set seed to get reproducible results

ri.cv = cv.glmnet(X, y, alpha = 0, lambda = grid, nfolds = 10)
bestlam = ri.cv$lambda.min 
bestlam

# Ridge fit for the best lambda
# MSE
y_pred = predict(ridge_fit, s=bestlam, newx = X)
MSE = mean((y - y_pred)^2) 
MSE
# regression coefficients
predict(ridge_fit, type = "coefficients", s = bestlam)[1:20, ]
















#=================================================================================
# # variance of Ridge upto a constant sigma^2
# p = ncol(X)
# lambda = 0.01
# A = solve(t(X)%*%X + lambda*diag(p))%*%(t(X)%*%X)%*%solve(t(X)%*%X + lambda*diag(p))
# sum(diag(A)) # sum of variance of beta_j except sigma^2
# # exploring glmnet for a given lambda = 100
# ridge_fit = glmnet(X,y,alpha = 0, lambda = 0, standardize = TRUE, exact=T)
# #ridge_fit
# coef(ridge_fit) # regression coefficients
