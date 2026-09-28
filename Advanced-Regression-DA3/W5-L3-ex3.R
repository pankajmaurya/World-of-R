rm(list = ls(all.names = TRUE))
#====== multiclass logistic for nominal response ========

insur = read.csv("http://peopleanalytics-regression-book.org/data/health_insurance.csv")

library(VGAM)
mfit = vglm(product~age+absent, family = multinomial, data = insur)
summary(mfit)

# extract coefficient in a matrix form
coef(mfit, matrix=TRUE)

# can specify baseline category
mfit1 = vglm(product~age+absent, 
             family = multinomial(refLevel = "A"),
             data = insur)
coef(mfit1, matrix=TRUE)


# predict for a new obs
predict(mfit, newdata = data.frame(age=50, absent=20), 
        type = "response")

# confidence intervals for regression parameters
confint(mfit, level = 0.90) 


# LR test for nested model comparison: Full vs reduced
mfit_F = vglm(product~age+absent, family = multinomial, insur)
mfit_R = vglm(product~age, family = multinomial, insur) # without "absent"

lrtest(mfit_F, mfit_R) # LR test full model vs reduced model

mfit_R = vglm(product~absent, family = multinomial, insur) # without "age"
lrtest(mfit_F, mfit_R) # LR test full model vs reduced model