library(fpp2)
ausair
TS = window(ausair, start = 1990)
plot(TS)
library(forecast)
DES = holt(TS, h = 5)
summary(DES)
accuracy(DES)
plot(DES)
