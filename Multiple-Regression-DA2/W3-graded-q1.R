install.packages("ordPens", repos = "https://cloud.r-project.org")  # if not already installed

load("ICFCoreSetCWP.RData")

# M2L5V3 Dealing with ordinal predictors
# Subjective measure of physical health of patients suffering
# from Chronic Widespread Pain (CWP), 
# predicted by international classification of functioning (ICF)
# in various categories, e.g., walking difficulty level,
# measured clinically in ordinal scale (starting with 0, no difficulty) 
library(ordPens)
data(ICFCoreSetCWP)
help(ICFCoreSetCWP)


lmphcs1 = lm(phcs ~ d450, data = ICFCoreSetCWP)
summary(lmphcs1)


physical_health = ICFCoreSetCWP$phcs
library(lmreg)
walking_difficulty = binaries(ICFCoreSetCWP$d450)
head(walking_difficulty)

# See alternative strategy to bypass next three line
wd_more_than_0 = walking_difficulty[,3] + walking_difficulty[,4] +
  walking_difficulty[,2]
wd_more_than_1 = walking_difficulty[,4] + walking_difficulty[,2]
wd_more_than_2 = walking_difficulty[,2]

# Alternative strategy: Create a data frame and use default names
walking_difficulty = as.data.frame(walking_difficulty)
wd_more_than_2 = walking_difficulty$v.3
wd_more_than_1 = walking_difficulty$v.2 + wd_more_than_2
wd_more_than_0 = walking_difficulty$v.1 + wd_more_than_1
# End of alternative strategy

lmphcs2 = lm(physical_health ~ wd_more_than_0 + 
               wd_more_than_1 + wd_more_than_2)
summary(lmphcs2)
plot(lmphcs2$fit,physical_health, 
     xlab = "Fitted values", ylab = "Observed values")
abline(lm(physical_health~lmphcs2$fit))


