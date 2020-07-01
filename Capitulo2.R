Auto=read.table("Auto.data")
fix(Auto)

Auto=read.table("Auto.data",header=T,na.strings="?")
fix(Auto)
dim(Auto)
describe(Auto)
dim(Auto)
names(Auto)

#attach () permite que llamemos a las variables por el nombre sin tener que poner $
attach(Auto)
plot(cylinders,mpg)

#podemos convertir variables cuantitativas en qualitativas con as.factor()

cylinders=as.factor(cylinders)
plot(cylinders,mpg,col="red",varwidth=T,xlab="cylinders",ylab="Mpg")
hist(mpg,col=2,breaks=15)
pairs(Auto)
pairs(~mpg+displacement+horsepower+weight+acceleration,Auto)
#uso de indetify()
plot(horsepower,mpg)
identify(horsepower,mpg,mpg)

summary(Auto)

#college data set

college=read.csv("College.csv")
fix(college)
rownames(college)=college[,1]
fix(college)
college=college[,-1]
fix(college)



attach(college)

summary(college)
names(college)
Private=as.factor(Private)
pairs(~Apps+Accept+Enroll+Top10perc+Top25perc+F.Undergrad+P.Undergrad+Outstate+Room.Board,college)
plot(Private,Outstate,col="red",varwidth=T,xlab="Private",ylab="Outstate")

Elite=rep("No",nrow(college))
Elite[Top10perc>50]="Yes"
Elite=as.factor(Elite)
college=data.frame(college,Elite)

plot(Elite,Outstate,col="red",varwidth=T,xlab="Elite",ylab="Outstate")

#uso de par(mfrow)

names(college)
attach(college)

par(mfrow=c(1,4))
hist(Apps,main="Histograma Apps")
hist(Accept,main="Aceptados")
plot(Elite,Outstate,col="red",varwidth=T,xlab="Elite",ylab="Outstate")
plot(Private,Outstate,col="red",varwidth=T,xlab="Private",ylab="Outstate")

attach(Auto)

names(Auto)
head(Auto)
range(Auto)
range(Auto$cylinders)
summary(Auto)
describe(Auto)
#removiendo filas

library(psych)
install.packages("psych",lib="C:/users/Carlos Bravo/desktop/R/packages")
Autoshort=Auto[-c(1:10),]
describe(Autoshort)
Auto
pairs(~mpg+cylinders+horsepower+weight+acceleration, Auto)
#ejercicio MASS con datos Boton
library(MASS)
Boston=read.table("Boston")
Boston
fix(Boston)
?Boston
pairs(Boston, labels = colnames(Boston), main = "Pairs matrix")
