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
