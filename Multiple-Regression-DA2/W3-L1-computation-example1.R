library(lmreg)
data("girlgrowth")
head(girlgrowth)
lmgirl1 = lm(Height~Age, data = girlgrowth)
summary(lmgirl1)$coefficients
# Note summary(lmgirl1$coefficients) is different!

# For PQ 3.2
library(lmreg)
data("stars1")
head(stars1)
hubble=lm(Velocity~Distance, data=stars1)
summary(hubble)$coeff


library(lmreg)
data("leprosy")
head(leprosy)
Treat = as.data.frame(binaries(leprosy$treatment))
head(Treat)
colnames(Treat) = c("A", "D", "F") # F is placebo treatment
summary(lm(post ~ Treat$A + Treat$D, data = leprosy))$coef

summary(lm(post ~ Treat$A + Treat$D + pre, data = leprosy))$coef


fit1 <- lm(post ~ Treat$A + Treat$D, data = leprosy)
fit2 <- lm(post ~ Treat$A + Treat$D + pre, data = leprosy)

one_sided_table <- function(fit, direction = "less") {
  cf <- summary(fit)$coef
  df <- fit$df.residual
  t_vals <- cf[, "t value"]
  
  if (direction == "less") {
    p_one <- pt(t_vals, df, lower.tail = TRUE)
  } else {
    p_one <- pt(t_vals, df, lower.tail = FALSE)
  }
  
  cf <- cbind(cf, "Pr(one-sided)" = p_one)
  cf
}

one_sided_table(fit1, direction = "less") # Useful
one_sided_table(fit2, direction = "less") # Useful

###### Computational example 
library(MASS)
data("birthwt")
head(birthwt)
library(lmreg)
races = as.data.frame(binaries(birthwt$race))
colnames(races) = c("Black", "Other", "White")
lmbwt3 = lm(bwt~smoke+races$White+lwt, data = birthwt)
summary(lmbwt3)

lmbwt1 <- lm(bwt~1, data=birthwt)
anova(lmbwt1, lmbwt3)
hanova(lmbwt1, lmbwt3)

# by using races$Black along with the explanatory variables earlier
# this is referred to as model specification 1
lmbwt4 = lm(bwt~smoke+races$White+races$Black+lwt, data = birthwt)
anova(lmbwt1, lmbwt4)
hanova(lmbwt1, lmbwt4)

# now using factor(race), smoke, lwt as explanatory variables.
# this is referred to as model specification 2
lmbwt5 = lm(bwt~smoke+factor(race)+lwt, data = birthwt)
anova(lmbwt1, lmbwt5)
hanova(lmbwt1, lmbwt5)

# outputs of anova and hanova of model 1 and 2 are identical.

data("LAcrime")
head(LAcrime)
LAcrime["homicideRate"] = LAcrime["Homicide"] / (LAcrime["Population"] / 100000)
homicidelm = lm(homicideRate ~ Year + TempCelsius, data = LAcrime)
summary(homicidelm)
homicidelm1 <- lm(homicideRate~1, data=LAcrime)
anova(homicidelm1, homicidelm)
hanova(homicidelm1, homicidelm)

# PQ 3.9
data("worldpop")
head(worldpop)
# y = gamma6 + gamma4 * x4 + gamma5 * x5 + epsilon
worldpop
c0 <- 1990

worldpop$X2 <- pmax(worldpop$Year - c0, 0)   # (Year - 1990)_+

fit <- lm(Pop.billion ~ Year + X2, data = worldpop)
summary(fit)
confint(fit, "X2")

# y = gamma6 + gamma4 * x4 + gamma5 * x5 + epsilon
x4 = worldpop$Year - 1990; x5 = x4
x4[worldpop$Year>1990] = 0; x5[worldpop$Year<=1990] = 0
lmpop = lm(worldpop$Pop.billion~x4+x5)
p = c(0,1,-1)
hyptest(lmpop,p)

#install.packages("HistData")
library(HistData)
data(GaltonFamilies)
head(GaltonFamilies)
