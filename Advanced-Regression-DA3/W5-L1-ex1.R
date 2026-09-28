rm(list = ls(all.names = TRUE))
#======== Models for binary response ========

#Load the Crab data
#crab = read.table("https://users.stat.ufl.edu/~aa/cat/data/Crabs.dat", 
#                  header = TRUE)
crab <- read.table("Crabs.dat", header = TRUE)

# fit logistic and check summary
fit = glm(y~width+factor(color), 
          family = binomial(link="logit"), data=crab)
summary(fit) # summarize fitted model 


# extract coefficients only 
coef(fit)

# predict at a new point or for a whole dataset
#----------------------------------------------
fit = glm(y~width + factor(color), 
          family = binomial(link="logit"), data=crab)
predict(fit, newdata = data.frame(width=30, color=1), type = "response")

predict(fit, type = "response") # predict for all x values 
fitted(fit) # alternate way

# predicted prob gives class levels
#---------------------------------------
fit = glm(y~width + factor(color), family = binomial(link="logit"), crab)
fit_prob = predict(fit, type = "response")
y.pred = rep(0, length(crab$y) )
y.pred[fit_prob>0.5] = 1 
y.pred

(y.actual = crab$y) # actual class levels 

( C = table(y.pred, y.actual) ) # confusion matrix

(class_accuracy = sum(diag(C))/sum(C) ) # classification accuracy
(class_error = 1-class_accuracy) # classification error


# for probit
#---------------------------------------
fit = glm(y~width + factor(color), family = binomial(link="probit"), crab)
fit_prob = predict(fit, type = "response")
y.pred = rep(0, length(crab$y) )
y.pred[fit_prob>0.5] = 1 
y.pred

(y.actual = crab$y) # actual class levels 

( C = table(y.pred, y.actual) ) # confusion matrix

(class_accuracy = sum(diag(C))/sum(C) ) # classification accuracy
(class_error = 1-class_accuracy) # classification error

# cloglog
fit = glm(y~width + factor(color), family = binomial(link="cloglog"), crab)
fit_prob = predict(fit, type = "response")
y.pred = rep(0, length(crab$y) )
y.pred[fit_prob>0.5] = 1 
y.pred

(y.actual = crab$y) # actual class levels 

( C = table(y.pred, y.actual) ) # confusion matrix

(class_accuracy = sum(diag(C))/sum(C) ) # classification accuracy
(class_error = 1-class_accuracy) # classification error

# logistic with predictor "width" & fitted curves for diff links
#---------------------------------------------------------------
plot(jitter(y, 0.01)~width, crab)

fit_logit = glm(y~width, family = binomial(link="logit"), crab)
curve(predict(fit_logit, data.frame(width=x), type = "response"), 
      add=TRUE, col="green")

fit_probit = glm(y~width, family = binomial(link="probit"), crab)
curve(predict(fit_probit, data.frame(width=x), type = "response"),
      add=TRUE, col="blue")

fit_cloglog = glm(y~width, family = binomial(link="cloglog"), crab)
curve(predict(fit_cloglog, data.frame(width=x), type = "response"), 
      add=TRUE, col="red")
#---------------------------------------------------------------




