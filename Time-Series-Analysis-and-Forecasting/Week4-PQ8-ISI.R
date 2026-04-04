rm(list=ls())

#ARMA process
set.seed(125)
noise = rnorm(100, 0, 3)
a = 5
phi = 0.8
theta = 0.5

ARMA11 = numeric(100)
ARMA11[1] = a + noise[1]
for (i in 2:100) 
{
  ARMA11[i] = a + phi * ARMA11[i-1] + noise[i] + theta * noise[i-1]
}
plot.ts(ARMA11)

par(mfrow = c(1,2))

ACF = numeric(11)
ACF[1] = 1
# closed form expression for lag-1 autocorrelation of ARMA(1, 1)
ACF[2] = (theta+phi)*(1+theta*phi)/(1+theta^2+2*theta*phi)
for(i in 3:11)
{
  # implements rho(h) = phi*rho(h-1) for h >= 2
  ACF[i] <- phi*ACF[i-1] 
}

h <- seq(0,10,1)

plot(h,ACF,type = "h", xlab="Lag",ylab="ACF",main="Population ACF")


acf(ARMA11, lag.max = 10)

#ARMA process using inbuild function

a = 5
phi = 0.8
theta = 0.5

ARMA_11 =  arima.sim(model = list(ar = phi, ma = theta), n = 100, innov = a + noise)  

par(mfrow = c(1,2))

plot.ts(ARMA_11)
acf(ARMA_11,lag.max = 10)
