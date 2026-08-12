rm(list=ls())

library(alr4)
data("wblake")
head(wblake)

x = sqrt(wblake$Length)
y = wblake$Age
m = lm(y~x)
summary(m)

# length of prediction interval for fish of length 200?
newdat <- data.frame(x = sqrt(200))
newdat
predAT = predict(m,newdat,interval = "prediction", level = 0.95)
round(predAT[,3] - predAT[,2], 2)

predAT
length_PI <- predAT[, "upr"] - predAT[, "lwr"]
round(length_PI, 2)
############

library(lmreg)
data("LAcrime")
head(LAcrime)
Pop_100k = LAcrime$Population / 100000
HomicideRate = LAcrime$Homicide / Pop_100k
m3 = lm(HomicideRate ~ LAcrime$Year + factor(LAcrime$Month) + LAcrime$Fahrenheit)
summary(m3)



raperate = LAcrime$Rape / Pop_100k

m2 = lm(raperate~LAcrime$Year+LAcrime$TempCelsius)
summary(m2)

# coefficient of TempCelsicum * 9
round(9 * 0.02303, 2)

