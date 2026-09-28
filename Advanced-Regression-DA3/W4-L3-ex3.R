#### LASSO #########
rm(list = ls(all.names = TRUE))

library(ISLR) # data from book ISLR
data(Hitters)

#=== least square regression =====
summary(lm(Salary~., data = Hitters)) # check fitted linear model

###################### lasso #########################

library(glmnet) # for penalized regressions: ridge, lasso, elastic net
X = model.matrix(Salary~., Hitters)[,-1] # design matrix
y = na.exclude(Hitters$Salary) # response excluding missing values


grid = 10^seq(3,-2, length=100) # from 10^3 to 10^(-2)

lasso_fit = glmnet(X,y, alpha = 1, lambda = grid)
coef(lasso_fit)[,1] # lambda = 10^3 gives null model
coef(lasso_fit)[,100] # lambda = 10^(-2) gives full model

plot(lasso_fit, xvar="lambda", xlim=c(-5,7), label=TRUE)

predict(lasso_fit,type="coefficients",s = 1)[1:20,] # coef for log-lambda=0

#= Comparing L1 norm of lasso coefficient estimates

# (1) L1 norm for lambda = 0 (least square)
betaLS = predict(lasso_fit, type = "coefficients", s = 0)[1:20, ]
(L1norm = sum( abs(betaLS[-1])))

# (2) L1 norm for lambda = 30
betaLASSO = predict(lasso_fit, type = "coefficients", s = 30)[1:20, ]
(L1norm = sum( abs(betaLASSO[-1])) )

# Lasso best penalty using cv
set.seed(123) # set seed to get reproducible results
la.cv = cv.glmnet(X, y, alpha = 1, lambda = grid, nfolds = 10)
bestlam = la.cv$lambda.min 
bestlam     # 2.656

# Lasso fit for the best lambda
# MSE
y_pred = predict(lasso_fit, s=bestlam, newx = X)
MSE = mean((y - y_pred)^2) 
y[22]
y_pred[22]
MSE
# regression coefficients
predict(lasso_fit, type = "coefficients", s = bestlam)[1:20, ]


























# #==after removing 6 predictors, refit the model with 13 predictors===
lmfit=lm(Salary~AtBat+Hits+Walks+Years+CHmRun+CRuns+CRBI+CWalks+League+
          Division+PutOuts+Assists+Errors, Hitters)
summary(lmfit) # Still some unimportant predictors are present
# #-------------------------------

## Alternate way to choose lambda: training and test data
#========================================================
# 
set.seed(1)
n = nrow(X)
train = sample(1:n, n/2) # randomly selects n/2 numbers from 1 to n
test = (-train)
y.test = y[test] # y variables in test dataset

# 
la.cv = cv.glmnet(X[train, ], y[train], alpha = 1)
bestlam = la.cv$lambda.min
# bestlam
y_pred2 = predict(lasso_fit, s=bestlam, newx = X[test, ])
predict(lasso_fit, type = "coefficients", s = bestlam)[1:20, ]
MSE2 = mean((y.test - y_pred2)^2) 
MSE2
