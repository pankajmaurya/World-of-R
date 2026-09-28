library(GLMsData)
data("nhospital")
library(glmnet) 

X = model.matrix(MainHours~., data=nhospital)[,-1] 
y = nhospital$MainHours

grid = 10^seq(3,-3, length=100) # from 10^3 to 10^(-2)
set.seed(123)

ri.cv = cv.glmnet(X, y, alpha = 1, lambda = grid, nfolds = 3)
bestlam = ri.cv$lambda.min 
bestlam
