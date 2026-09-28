rm(list = ls(all.names = TRUE))
library(GLMsData)

data("nhospital")

library(glmnet) 

X = model.matrix(MainHours~., data=nhospital)[,-1] 
y = nhospital$MainHours


en_fit = glmnet(X,y,alpha = 0.5,lambda = 50, standardize = TRUE)
y_pred = predict(en_fit, s=50, newx = X)
MSE = mean((y - y_pred)^2) 
MSE



