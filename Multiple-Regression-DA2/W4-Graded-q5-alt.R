library("alr4")
data(wblake)
head(wblake)
y=wblake$Age
x1=wblake$Length
x2=wblake$Scale

m_full <- lm(y ~ x1 + x2)
e <- residuals(m_full)
h <- hatvalues(m_full)

loo_resid <- e / (1 - h)
RMSEP_shortcut <- sqrt(mean(loo_resid^2))
round(RMSEP_shortcut, 3)