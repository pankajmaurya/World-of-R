#### Elastic Net #########
rm(list = ls(all.names = TRUE))

library(ISLR) # data from book ISLR
data(Hitters)

library(glmnet) # for penalized regressions:ridge, lasso, elastic net
X = model.matrix(Salary~., Hitters)[,-1] # design matrix
y = na.exclude(Hitters$Salary) # response excluding missing values


grid = 10^seq(3,-2, length=100) # from 10^3 to 10^(-2)

en_fit = glmnet(X,y, alpha = 0.7, lambda = grid)

coef(en_fit)[,1] # lambda = 10^3 gives null model
coef(en_fit)[,100] # lambda = 10^(-2) gives full model


plot(en_fit, xvar="lambda", xlim=c(-5,7), label=TRUE)

lasso_fit = glmnet(X,y, alpha = 1, lambda = grid)
plot(lasso_fit, xvar="lambda", xlim=c(-5,7), label=TRUE)


# regression coefficients for lambda= 2.656
lam = 2.656
predict(lasso_fit, type = "coefficients", s = lam)[1:20, ] # lasso coef
predict(en_fit, type = "coefficients", s = lam)[1:20, ] # elastic net, alpha =0.7

























# # select alpha, lambda simultaneously by cross-validation 
# library(caret)
# set.seed(123) # set seed to get reproducible results
# 
# lambda_grid = seq(0.1, 100, length=41)
# alpha_grid = seq(0, 1, 0.1)
# 
# #trnCtrl = trainControl(method = "repeatedCV", number = 10, repeats = 5)
# srchGrid = expand.grid(.alpha = alpha_grid, .lambda = lambda_grid)
# 
# # Cross validation
# my_train = train(X,y, method = "glmnet", tuneGrid = srchGrid )
# 
# #my_train = train(X,y, method = "glmnet", )
# # Best parameters
# bestalpha = as.numeric(my_train$bestTune[1]) # alpha according to caret
# bestlam = as.numeric(my_train$bestTune[2]) # lambda according to caret
# bestalpha; bestlam
# 
# en_fit = glmnet(X,y, alpha = bestalpha, lambda = grid)
# predict(en_fit, type = "coefficients", s = bestlam)[1:20, ]
# 
# 
# y_pred = predict(en_fit, s=bestlam, newx = X)
# MSE = mean((y - y_pred)^2) 
# MSE


# library(glmnetUtils)
# en.cv = cva.glmnet(X, y, alpha = seq(0,1,length=50))
#    
# # Get all parameters.
# get_model_params <- function(fit) {
#   alpha <- fit$alpha
#   lambdaMin <- sapply(fit$modlist, `[[`, "lambda.min")
#   lambdaSE <- sapply(fit$modlist, `[[`, "lambda.1se")
#   error <- sapply(fit$modlist, function(mod) {min(mod$cvm)})
#   best <- which.min(error)
#   data.frame(alpha = alpha[best], lambdaMin = lambdaMin[best],
#              lambdaSE = lambdaSE[best], eror = error[best])
# }
# 
# get_model_params(en.cv)

# # regression coefficients
# set.seed(123)
# en.cv = cv.glmnet(X, y)
# bestlam = en.cv$lambda.min 
# bestlam

#en_fit = glmnet(X,y, alpha = 0.6)
#predict(en_fit, type = "coefficients", s = bestlam)[1:20, ]

#la_fit = glmnet(X,y, alpha = 1)
#predict(la_fit, type = "coefficients", s = bestlam)[1:20, ]

