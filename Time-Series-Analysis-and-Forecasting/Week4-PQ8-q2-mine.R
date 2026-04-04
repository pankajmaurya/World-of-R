set.seed(126)
rm(list=ls())

a = 5
b = 0.5
N = 100
noise = rnorm(100, 0, 3)
g = c(4, 7, -5, -6)
y = numeric(100)
x = numeric(100)
for (t in 1:100)
{
  x[t] = 0
  for (i in 0:3)
  {

    if (t %% 4 == i) {
      dti_cond_val <- 1
    } else {
      dti_cond_val <- 0
    }
    
    
    x[t] = x[t] + g[i + 1] * dti_cond_val  
  }
}

for (i in 1:100)
{
  y[i] = a + b * i + noise[i] + x[i]
}

plot.ts(y)

t <- 1:N
trend_model <- lm(y ~ t)
summary(trend_model)

Tt <- fitted(trend_model)
plot.ts(y, col = "black", main = "Time Series with Fitted Trend")
lines(Tt, col = "red", lwd = 2)

y_detrended <- y - Tt

season <- factor((t - 1) %% 4)
season_model <- lm(y_detrended ~ season)
summary(season_model)

season_coefs <- coef(season_model)
St_hat <- fitted(season_model)

t_future <- (N+1):(N+6)
Tt_future <- predict(trend_model,
                     newdata = data.frame(t = t_future))
season_future <- factor((t_future - 1) %% 4,
                        levels = levels(season))

St_future <- predict(season_model,
                     newdata = data.frame(season = season_future))

y_forecast <- Tt_future + St_future
y_forecast
