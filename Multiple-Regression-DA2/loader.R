load("/Users/pankaj/TechCareer/World-of-R/Multiple-Regression-DA2/ICFCoreSetCWP.RData")

head(ICFCoreSetCWP)
x = as.data.frame(binaries(ICFCoreSetCWP$d455))
colnames(x) = c("2", "3", "0", "1", "4")
m1 <- lm(phcs ~ x$`0` + x$`1`+ x$`2` + x$`3` + x$`4`, data = ICFCoreSetCWP)
summary(m1)$coeff


data("birthwt")
library(lmreg)
races = as.data.frame(binaries(birthwt$race))
races
head(birthwt)
help(birthwt)


m2 <- lm(phcs ~ x$`1`+ x$`2` + x$`3` + x$`0`, data = ICFCoreSetCWP)
summary(m2)$coeff
