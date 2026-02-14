hwdata <-
  data.frame(sex = c("F","F","M","F","F","F","M","F","M","F","F","M","F","M","M","M","F","F","F","M","F","M","M","F","M","F","F","M","M","M","M","F","M","M","F","F","F","M","F","M","M","M","F","M","F","M","F","F","F","M","F","F","M","M","F","F","F","M","M","F","F","F","M","M","M","M","M","M","M","M","F","F","F","M","F","F","F","M","F","M","F","F","M","M","M","M","F","F","F","M","M","F","M","F","M","F","F","F","M","F","M","F","F","F","F","M","F","F","M","M","M","M","M","M","F","F","F","M","F","M","F","F","M","M","F","F","M","F","F","M","F","F","M","M","F","F","F","M","F","M","M","M","F","F","F","F","F","F","M","M","F","M","F","F","F","F","F","M","M","F","F","M","F","F","F","F","F","F","F","M","F","F","F","M","F","M","F","F","M","M","F","F","F","M","M","F","M","F","F","M","F","F","M","M","M","F","F","F","M","M"),
             weight = c(54,54,88,51,53,68,71,56,69,57,61,84,50,89,90,83,61,62,51,97,64,66,82,66,102,60,56,79,80,80,67,43,71,76,166,52,50,65,55,83,80,76,53,59,62,70,63,47,53,76,64,52,62,62,55,58,54,85,81,50,47,47,87,74,65,119,64,85,64,71,59,58,64,65,70,58,55,66,56,55,52,57,88,84,78,103,61,57,52,69,82,54,68,75,69,64,56,45,73,52,73,59,49,63,59,68,60,55,88,67,65,69,77,69,53,62,58,101,50,78,39,54,65,70,50,76,61,52,50,75,62,53,54,90,56,54,59,79,56,96,82,69,68,62,64,56,55,50,76,81,59,76,45,71,53,54,48,79,88,47,52,66,62,53,64,57,63,55,68,96,66,60,65,63,62,69,63,78,75,56,57,63,55,83,66,44,71,55,53,68,59,51,74,92,75,59,60,56,70,57),
             height = c(163,161,184,163,162,169,178,165,174,167,175,183,160,173,181,184,165,175,156,189,166,173,181,166,185,162,163,173,178,178,179,154,180,169,57,163,166,175,165,180,176,197,161,182,175,175,160,150,169,183,164,159,168,178,165,166,174,191,175,158,153,163,185,169,187,180,177,179,176,177,157,161,171,176,173,169,165,173,170,168,158,162,178,184,178,185,170,163,164,172,176,160,165,162,167,172,165,163,183,169,180,170,161,163,157,177,172,155,189,179,178,180,182,183,158,166,169,183,171,183,157,176,171,173,166,167,170,152,169,169,164,165,169,188,166,164,172,177,161,184,182,182,171,167,168,162,174,148,170,178,164,167,157,166,165,171,163,179,185,162,163,170,168,164,165,167,169,164,178,191,170,174,166,178,168,186,170,173,178,163,168,165,162,177,175,157,178,160,162,174,166,161,175,187,172,159,167,160,173,173))
str(hwdata)

hwsubf <- subset(hwdata, sex == "F")
hwsubm <- subset(hwdata, sex == "M")

str(hwsubf)
head(hwsubf)
head(hwsubm)

hwsubf

fm.female <- lm(weight ~ height, data = hwsubf)
fm.male <- lm(weight ~ height, data = hwsubm)

slopef <- fm.female$coefficients["height"]
slopem <- fm.male$coefficients["height"]

s.female <- summary(fm.female)
s.male <- summary(fm.male)
pvalf <- s.female$coefficients["height", "Pr(>|t|)"]
pvalm <- s.male$coefficients["height", "Pr(>|t|)"]

print(c(slopef, slopem, pvalf, pvalm)) # check that these have been defined
summary(fm.female)
summary(fm.male)


library(ggplot2)
library(lattice)
xyplot(weight ~ height | sex, data = hwdata)
ggplot(data = hwdata) + facet_wrap(~sex) + geom_smooth(aes(x = height, y = weight), method = "lm", se = FALSE) + geom_point(aes(x = height, y = weight))
hatvalues(fm.female)
indexf <- which.max(hatvalues(fm.female))
indexm <- which.max(hatvalues(fm.male))
hatvalues(fm.female)[indexf]
hatvalues(fm.male)[indexm]
plot(hatvalues(fm.male))

hwsubf[indexf, ]
hwsubm[indexm, ]

indexf <- which.max(hatvalues(fm.female))
indexm <- which.max(hatvalues(fm.male))

print(c(indexf, indexm)) # check that these have been defined

indexf

hwsubf2 <- hwsubf[-indexf, ]
fm.female2 <- lm(weight ~ height, data = hwsubf2)
summary(fm.female2)

hwsubm2 <- hwsubm[-indexm, ]
fm.male2 <- lm(weight ~ height, data = hwsubm2)
summary(fm.male2)

# compute dslopef and dslopem
dslopef <- fm.female2$coefficients["height"]
dslopem <- fm.male2$coefficients["height"]

# compute dpvalf and dpvalm
s.female2 <- summary(fm.female2)
s.male2 <- summary(fm.male2)
dpvalf <- s.female2$coefficients["height", "Pr(>|t|)"]
dpvalm <- s.male2$coefficients["height", "Pr(>|t|)"]

print(c(dslopef, dslopem, dpvalf, dpvalm)) # check that these have been defined

# comparison with earlier values
print(c(slopef, slopem, pvalf, pvalm)) # check that these have been defined

par(mfrow = c(1, 2))
plot(weight ~ height, data = hwsubf2)
abline(lm(weight ~ height, data = hwsubf2))
plot(weight ~ height, data = hwsubm2)
abline(lm(weight ~ height, data = hwsubm2))

# for (s in c("M", "F")) {
#    plot(weight ~ height, data = hwdata, subset = (sex == s))
#    abline(lm(weight ~ height, data = hwdata, subset = (sex == s)))
# }
# 

ggplot(data = hwdata) + facet_wrap(~sex) + geom_smooth(aes(x = height, y = weight), method = "lm", se = FALSE) + geom_point(aes(x = height, y = weight))
ggplot(data = rbind(hwsubf2, hwsubm2)) + facet_wrap(~sex) + geom_smooth(aes(x = height, y = weight), method = "lm", se = FALSE) + geom_point(aes(x = height, y = weight))
#rbind(hwsubf2, hwsubm2)
       