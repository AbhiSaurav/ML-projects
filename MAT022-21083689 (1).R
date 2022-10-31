#load the dataset
library(MASS)
crab = crabs

#1. 
#FL
mean(crab[,4])
var(crab[,4])
min(crab[,4])
max(crab[,4])
quantile(crab[,4])

#RW
mean(crab[,5])
var(crab[,5])
min(crab[,5])
max(crab[,5])
quantile(crab[,5])

#CL
mean(crab[,6])
var(crab[,6])
min(crab[,6])
max(crab[,6])
quantile(crab[,6])

#CW
mean(crab[,7])
var(crab[,7])
min(crab[,7])
max(crab[,7])
quantile(crab[,7])

#BD
mean(crab[,8])
var(crab[,8])
min(crab[,8])
max(crab[,8])
quantile(crab[,8])

#2
par(mfrow=c(1, 1))
boxplot(crab[5], main = "Q2. boxplot of rear width")

#3
par(mfrow=c(2, 2))
boxplot(crab[crab[,1]=="B" & crab[,2]=="M",5], main = "species-B, Sex-Male", col = "blue")
boxplot(crab[crab[,1]=="B" & crab[,2]=="F",5], main = "species-B, Sex-Female", col = "blue")
boxplot(crab[crab[,1]=="O" & crab[,2]=="M",5], main = "species-O Sex-Male", col = "orange")
boxplot(crab[crab[,1]=="O" & crab[,2]=="F",5], main = "species-O, Sex-Female", col = "orange")

#4
par(mfrow=c(1, 1))
hist(data[,8], freq=FALSE)
lines(density(data[,8]), main = "body depth", col="red")

#5
#check for normality
shapiro.test(crab[,6])
shapiro.test(crab[,7]) 
#do not reject normality

#check for equality of variance
var.test(crab[,6],crab[,7]) 
#do not reject equality of variance 

#pooled t-test
t.test(crab[,7],crab[,6],alternative="greater",var.equal=TRUE)

#6
#check for normality
shapiro.test(crab[,4])
shapiro.test(crab[,5]) 

cor.test(crab[,4], crab[,5], alternative="greater")

#7
#test normality
shapiro.test(crab[crab[,1]=="B",4])
shapiro.test(crab[crab[,1]=="O",4])

#test for equality of variannce
var.test(crab[crab[,1]=="B",4],crab[crab[,1]=="O",4])

#pooled t test
t.test(crab[crab[,1]=="B",4],crab[crab[,1]=="O",4],var.equal=TRUE)

#8
#check for normality
shapiro.test(crab[crab[,1]=="B",8])
shapiro.test(crab[crab[,1]=="B",8])

#check for equality of variance
leveneTest(crab[,8], crab[,1])

#anova test
len<-crab[,5]>12
anova(lm(crab[,8]~crab[,1]+len))

#9
#a
model1<-lm(crab[,6] ~ crab[,7]+ crab[,2]+crab[,7]*crab[,2])
summary(model1)

#b
#interaction on model

model2<-lm(crab[,6] ~ crab[,7]+crab[,2])
summary(model2)

#10
install.packages("avplots")
FL<- crabs[,4]
RW<- crabs[,5]
BD<- crabs[,8]
sex=crabs[,2]
CL=crabs[,6]
CW=crabs[,7]

#a)
m10f<- lm(as.numeric(sex)~ FL+RW+CL+CW+BD)
#each 0.03815mm decrease in frontal lobe size there is an effect on Sex
m10= lm(as.numeric(sex)~ 1)
step(m10, scope = list(upper=m10f, lower=~1), direction="both", trace=10)
m10final<-lm(as.numeric(sex)~ RW+CL+CW)
summary(m10final)
#b)check if all coefficients have value equal to 0
summary(m10f)
anova(m10f)
#c)
#As per their significance from the F-Test, we will keep Rear width, CL and cW in the model.
#d)
avPlots(m10f)
avPlots(m10final)



