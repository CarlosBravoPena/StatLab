library(MASS)
library(ISLR)
install.packages("ISLR",lib="C:/users/Carlos Bravo/desktop/R/packages")
lm.fit=lm(medv~lstat,data=Boston)
lm.fit
summary(lm.fit)
attach(Boston)

plot(medv,lstat,col="red")
abline(lm.fit,lwd=3,col="red")
plot(medv,lstat,col="red",pch=20)
plot(medv,lstat,col="red",pch="+")
plot(1:20,1:20,pch=1:20)

#graficos de diagnostico de una regresion lineal
par(mfrow=c(2,2))
plot(lm.fit)

lm.fit2=lm(medv~lstat+I(lstat*lstat))
summary(lm.fit2)

anova(lm.fit,lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)
