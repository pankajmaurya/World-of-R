rm(list = ls(all.names = TRUE))

#======== Likelihood inference & Deviance ========

# 1. Read the local .dat file
# locally in Downloads
crab <- read.table("Crabs.dat", header = TRUE)

# fit logistic and check summary
fit = glm(y~width+factor(color), 
          family = binomial(link="logit"), data=crab)
summary(fit) # summarize fitted model 

# deviance of a model
deviance(fit)
summary(fit)$deviance

# confidence intervals
confint(fit)
confint(fit, level = 0.90) # can adjust confidence level


# comparing nested models by LR test
#-------------------------------------
fit_F = glm(y ~ width+factor(color), 
            family = binomial(link="logit"), crab)
fit_R = glm(y ~ width, 
            family = binomial(link="logit"), crab)
anova(fit_R, fit_F, test ="LRT" )

