rm(list=ls())
set.seed(126)
noise=rnorm(100, 0, 3)
par(mfrow=c(1, 2))
plot.ts(noise)
acf(noise)

# model y_t = y_{t - 1} + e_t
RW = numeric(100)
RW = noise[1]
for (i in 2:100)
{
  RW[i] = RW[i-1] + noise[i]
}

par(mfrow=c(1, 2))
plot.ts(RW)
acf(RW)


# model y_t = a + phi*y_{t - 1} + e_t + theta*e_{t-1}
N = 100
noise=rnorm(N, 0, 3)
y = numeric(N)
a = 5
phi = 0.8
theta = 0.5
y[1] = a + noise[1] 
for (i in 2:N)
{
  y[i] = a + phi*y[i-1] + noise[i] + theta * noise[i-1]
}

par(mfrow=c(1, 2))
plot.ts(y)
acf(y)
