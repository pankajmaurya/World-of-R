library(forecast)

data("AirPassengers")
class(AirPassengers)

print(AirPassengers)

start(AirPassengers)
end(AirPassengers)

summary(AirPassengers)

plot(AirPassengers)

tsdata <- ts(AirPassengers, frequency=12)

ddata <- decompose(tsdata, "multiplicative")

plot(ddata)

plot(ddata$trend)
plot(ddata$seasonal)
plot(ddata$random)

#help(ts)

trend = seq(10, 30, length = 100)
X = 10 + trend + sin(10 * trend)

tsdata2 <- ts(X, frequency=12)

ddata2 <- decompose(tsdata2, "multiplicative")

plot(ddata2)

ddata3 <- decompose(tsdata2, "additive")

plot(ddata3)

