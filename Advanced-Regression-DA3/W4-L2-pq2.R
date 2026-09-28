library(GLMsData)

data("nhospital")

library(glmnet) 

X = model.matrix(MainHours~., data=nhospital)[,-1] 

y = nhospital$MainHours

beta_LS = lm(MainHours~., data=nhospital)$coefficients[-1]

L2norm0 = sqrt(sum(beta_LS^2))

ridge_fit1 = glmnet(X,y,alpha = 0,lambda = 5000, standardize = TRUE)

L2norm1 = sqrt( sum( coef(ridge_fit1)[-1]^2 ) )

L2norm1; L2norm0
