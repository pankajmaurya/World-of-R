# Dependence of transaction time on counts of 2 types of transactions
library(alr4)
data(Transact)
help(Transact)
lmtransact1 = lm(time~t1+t2,data=Transact)
summary(lmtransact1)
# How do we test if both types have same effect?
# Test if Î²1 = Î²2
# What does the hypothesis Î²1 = Î²2 imply?
library(lmreg)
tr = Transact$t1+Transact$t2
lmtransact0 = lm(time~tr,data=Transact)  # simpler model
hanova(lmtransact0,lmtransact1)

p <- c(0, 1, -1)
library(lmreg)
hyptest(lmtransact1, p, xi = 0, type = "both")

# The square of the t-statistic is equal to the F-ratio