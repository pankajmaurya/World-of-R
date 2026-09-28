data("mtcars")

?mtcars
# fit logistic and check summary
fit = glm(am~hp+wt, 
          family = binomial(link="logit"), data=mtcars)
summary(fit) # summarize fitted model 


# extract coefficients only 
coef(fit)
