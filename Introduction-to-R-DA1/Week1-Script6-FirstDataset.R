year <- seq(1952, 2007, by = 5)
year

au <- c(69.120, 70.330, 70.930, 71.100, 71.930, 73.490, 74.740, 76.320, 77.560, 78.830, 80.370, 81.235)
plot(year, au, pch=20)
au[[12]] - au[[1]]
max(au)

#au[-1] - au[-12]
# au[-1] leaves out first element
# au[-12] leaves out last element

diff(au)
mean(diff(au))

fr <- c(67.410, 68.930, 70.510, 71.550, 72.380, 73.830, 74.890, 76.340, 77.460, 78.640, 79.590, 80.657)
plot(year, fr, pch=20)
mean(diff(fr))
fr[[12]] - fr[[1]]
max(fr)

ind <- c(37.373, 40.249, 43.605, 47.193, 50.651, 54.208, 56.596, 58.553, 60.223, 61.765, 62.879, 64.698)
plot(year, ind, pch=20)
mean(diff(ind))
ind[[12]] - ind[[1]]
max(ind)

zi <- c(48.451, 50.469, 52.358, 53.995, 55.635, 57.674, 60.363, 62.351, 60.377, 46.809, 39.989, 43.487)
plot(year, zi, pch=20)
mean(diff(zi))
zi[[12]] - zi[[1]]
max(zi)

countries <- c()
for (i in 1:12) {
  values <- c(au[[i]], fr[[i]], ind[[i]], zi[[i]])
  max_value <- max(values)
  
  if (au[[i]] == max_value)
    countries <- c(countries, "au")
  if (fr[[i]] == max_value)
    countries <- c(countries, "fr")
  if (ind[[i]] == max_value)
    countries <- c(countries, "ind")
  if (zi[[i]] == max_value)
    countries <- c(countries, "zi")
}
countries
