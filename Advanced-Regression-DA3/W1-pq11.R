# PQ 1.11
library(alr4)
data(UN11)
head(UN11)
Africa = binaries(UN11$group)[,2]
lmfer2 = lm(fertility~log(ppgdp)+I((log(ppgdp))^2)+Africa,data=UN11)
lmfer3 = lm(fertility~log(ppgdp)+I((log(ppgdp))^2) + I((log(ppgdp))^3)+Africa,data=UN11)
anova(lmfer2, lmfer3)

lm_a <- lm(fertility~I(log(ppgdp)^2)+Africa, data=UN11)
# I() forces R to compute 1/log(ppgdp) as ordinary arithmetic 
# (elementwise division) first, producing a numeric vector, 
# and then include that as a single regressor
lm_b <- lm(fertility~I(1/log(ppgdp))+Africa, data=UN11)

# without I, a/b means a + a:b
lm_c <- lm(fertility~log(ppgdp)+Africa, data=UN11)
lm_d <- lm(fertility~log(log(ppgdp))+Africa, data=UN11)


summary(lm_a)$r.sq
summary(lm_b)$r.sq
summary(lm_c)$r.sq
summary(lm_d)$r.sq


r2_values <- c(
  lm_a = summary(lm_a)$r.squared,
  lm_b = summary(lm_b)$r.squared,
  lm_c = summary(lm_c)$r.squared,
  lm_d = summary(lm_d)$r.squared
)

r2_values          # view all four
max(r2_values)     # the maximum R^2 value
names(r2_values)[which.max(r2_values)]   # which model achieves it


models <- list(lm_a = lm_a, lm_b = lm_b, lm_c = lm_c, lm_d = lm_d)
r2_values <- sapply(models, function(m) summary(m)$r.squared)
r2_values[which.max(r2_values)] 


lambda <- seq(-2,4,.05)

Rsq <- NULL
logdp = log(UN11$ppgdp)



for (lamb in lambda) {
  
  tlogdp <- (logdp^lamb - 1) / lamb
  
  if (lamb==0) tlogdp <- log(logdp)
  
  Rsq <- c(Rsq, summary(lm(fertility~tlogdp+Africa,data=UN11))$r.sq)
  
}
plot(lambda,Rsq,type="l")

lambda[which(Rsq==max(Rsq))]

