library(foreign)
Dat = read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")
# building baseline category logit model for y=prog using x = math
# default will be "vocation" category


library(VGAM)
mfit = vglm(prog~ses + math + science, 
            
            family = multinomial(refLevel = "vocation"),
            
            data = Dat)

predict(mfit, newdata = data.frame(ses="low",math=40,science=39), 
        
        type = "response")

# My solution.
pq3fit = vglm(prog~factor(ses)+math+science, family = multinomial(refLevel = "vocation"), data = Dat)
predict(pq3fit, newdata = data.frame(ses="low", math=40, science=39), 
        type = "response")

pq3fit2 = vglm(prog~math+science, family = multinomial(refLevel = "vocation"), data = Dat)
summary(pq3fit2)
