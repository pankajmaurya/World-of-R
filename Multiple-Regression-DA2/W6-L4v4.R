rm(list = ls())
# Stepwise search over subsets for best regression model
library(lmreg)
library(MASS)
data(imf2015)
help(stepAIC)

lm0 <- lm(UNMP ~ 1, data = imf2015)
lm5 <- lm(UNMP ~ CAB + DEBT + EXP + INFL + INV, data = imf2015) # full model

# Use stepAIC of MASS 
# help(stepAIC)
# Stepwise Regression by AIC (differs by a constant)
summary(stepAIC(lm0, scope=UNMP~CAB+DEBT+EXP+INFL+INV,
                direction="forward"))$anova
# Best model by AIC is UNMP ~ INV + CAB + INFL + EXP
# Model of 3 variables is not best
summary(stepAIC(lm5, direction = "backward"))$anova
# Best model by AIC is UNMP ~ CAB + EXP + INFL + INV

# "both" starting from the null model — still need a scope with upper/lower bounds
step_both_from_null <- stepAIC(lm0,
                               scope = list(lower = ~1,
                                            upper = ~CAB + DEBT + EXP + INFL + INV),
                               direction = "both")
summary(step_both_from_null)$anova


# "both" starting from the full model
step_both_from_full <- stepAIC(lm5,
                               scope = list(lower = ~1,
                                            upper = ~CAB + DEBT + EXP + INFL + INV),
                               direction = "both")
summary(step_both_from_full)$anova

summary(stepAIC(lm0, scope=UNMP~CAB+DEBT+EXP+INFL+INV,
                direction="both"))$anova
summary(stepAIC(lm5, direction = "both"))$anova
