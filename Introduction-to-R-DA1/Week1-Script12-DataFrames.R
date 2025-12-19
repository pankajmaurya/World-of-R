# data(Cars93, package="MASS")
# carsub <- Cars93[sample(nrow(Cars93), 6), c("Manufacturer", "Make", "Model", "EngineSize", "Man.trans.avail")]
# 
# write.csv(carsub, "TechCareer/World-of-R/carsub.csv")
# 
# cars <- Cars93[, c("Man.trans.avail", "Weight", "EngineSize", "Make", "Model", "MPG.city")]
# 
# write.csv(cars, "TechCareer/World-of-R/cars.csv")

carsdf <- read.csv("TechCareer/World-of-R/cars.csv")
head(carsdf)

# plots 
plot(MPG.city ~ Weight, data = carsdf)
plot(MPG.city ~ EngineSize, data = carsdf)
plot(MPG.city ~ factor(Manual), data = carsdf)

stripchart(MPG.city ~ factor(Manual), data = carsdf, method = "stack")
# t-tests