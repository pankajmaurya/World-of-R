#### MULTICOLLINEARITY #########
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

# PQ 4.1
library(GLMsData)
data(nhospital)
vif(lm(MainHours~Cases+Eligible+OpRooms, data=nhospital))
X = model.matrix(MainHours~., nhospital)[,-1]
lam = eigen(t(X)%*%X)$values # eigenvalues of X^T*X
condition_indices = max(lam)/lam # condition indexes
condition_number= max(condition_indices)  # condition number 
condition_number # > 424 million 
