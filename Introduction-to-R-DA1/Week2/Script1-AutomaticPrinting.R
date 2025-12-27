x <- rnorm(10)
y <- rnorm(10, sd = 3)
tt <- t.test(x, y, var.equal = TRUE)
t.test(x, y, var.equal = TRUE)
print(tt)

pval <- tt$p.value
(pval <- tt$p.value)
invisible(tt$p.value) ## no output

fm <- lm(y ~ x)
summary(fm)

plot(x, y)
s <- summary(fm)
class(s)
str(s)

coef(fm)
coef(s)
cm <- coef(s)
cm
print(cm)
cm["(Intercept)", "Pr(>|t|)"]
