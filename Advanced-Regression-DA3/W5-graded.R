data(mtcars)
?mtcars

# fit logistic and check summary
fit6 = glm(am~mpg+wt, 
          family = binomial(link="logit"), data=mtcars)
coef(fit6)

fit7 = glm(am~mpg+wt+hp, 
          family = binomial(link="logit"), data=mtcars)
coef(fit7)
predict(fit7, newdata = data.frame(mpg=17, wt=3, hp=150), 
        type = "response")

fit8 = glm(am~mpg+wt, 
           family = binomial(link="logit"), data=mtcars)
summary(fit8)


install.packages("foreign")
library(foreign)
d9 = read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")
head(d9)
# building baseline category logit model for y=prog using x = math
# default will be "vocation" category


library(VGAM)
d9fit = vglm(prog~math, family = multinomial(refLevel = "vocation"), data = d9)

summary(d9fit)

# extract coefficient in a matrix form
coef(d9fit, matrix=TRUE)

### Q10.
library(GLMsData)
data("nminer")

?nminer
pfit=glm(Minerab~Eucs+Bulokes, family=poisson(link="log"), nminer)
confint(pfit)

