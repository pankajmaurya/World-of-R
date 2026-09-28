data(mtcars)
?mtcars
fitcars = glm(am~mpg+cyl+wt, 
              family = binomial(link="logit"), data=mtcars)
fitcars_r1 = glm(am~mpg+wt, 
                 family = binomial(link="logit"), data=mtcars)
fitcars_r2 = glm(am~mpg, 
                 family = binomial(link="logit"), data=mtcars)
anova(fitcars_r1, fitcars)
anova(fitcars_r2, fitcars)


p = seq(0.01, 0.99, 0.001)
?seq
v = p/(1-p)
g = log(v)
plot(p, g)

#### My solution is wrong for Q1

data=c(1.74, 1.87, 2.87, 2.01, 1.66)
wdata=c(0.245777546106106,
        0.248001436437343,
        0.0266525118334568,
        0.22507113320765,
        0.233076769793655
)
X = matrix(c(1,1,1,1,1, data), nrow=2, ncol=5, byrow=TRUE)
X
t(X)
solve(X %*% diag(wdata) %*% t(X)) # gives the inverse.

H = -1 * X %*% diag(wdata) %*% t(X)
H
solve(-1 * H)[2,2]
### Given solution is below

beta_0 = -6.14

beta_1 = 3.39

x = c(1.74, 1.87, 2.87, 2.01, 1.66)

e = exp(beta_0 + beta_1*x)
e
p = e/(1 + e) 
p
W = diag(p*(1 - p)) 
W
X1 = matrix(c(1,1,1,1,1,1.74, 1.87, 2.87, 2.01, 1.66), ncol = 2) 
t(X1)
M = solve(t(X1)%*%W%*%X1)

var = M[2,2] ; var
