### Chandrima code
lm12=lm(y~x1+x2)
x1_1=rnorm(n,mean=1,sd=3)
x2_2=0.8*x1_1+rnorm(n,mean=1,sd=0.05)
err=rnorm(n)

# Use multiple reg model to generate y
ynew=beta0+beta1*x1_1+beta2*x2_2+err
lm12new=lm(ynew~x1_1+x2_2)

# pred for new set of values

x1_3=rnorm(n,mean=1,sd=3)
x2_4=0.8*x1_3+rnorm(n,mean=1,sd=0.05)
ynew3=beta0+beta1*x1_3+beta2*x2_4+err

newdat=data.frame(x1_1=x1_3,x2_2=x2_4)
ynewoldfit=predict(lm12new,newdat,interval="none")

# observed y value- predicted value
prederror12=ynew3-ynewoldfit

#RMSE - root mean sqr error
sqrt(mean(prederror12^2))
#0.6917124