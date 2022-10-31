library(lawstat)
library(EnvStats)
library(TeachingDemos)
library(Deducer)
library(pwr) 



abalone<-read.table("abalone.data", sep=",")





# See if length can be predicted from the other variables (ignore sex and rings)
modelt<-lm(abalone[,2] ~ abalone[,3] + abalone[,4] + abalone[,5]+ abalone[,6]+ abalone[,7] + abalone[,8])
summary(modelt)
# check multicollinearity  -anything more than 10 needs to be removed 


# Another case where a plot would have shown us useful information
pairs(abalone[, 2:8])
# You see the positive relationship for all the variables
# but some coefficients have negative slopes

vif(modelt) 

modelt2<-lm(abalone[,2] ~ abalone[,3] + abalone[,4] + abalone[,6]+ abalone[,7] + abalone[,8])
summary(modelt2)
# check multicollinearity  - now only 7 is more than 10... 
vif(modelt2) 


modelt3<-lm(abalone[,2] ~ abalone[,3] + abalone[,4] + abalone[,6]+ abalone[,8])
summary(modelt3)
# nothing else is more than 10... 
vif(modelt3) 
# Here I need to say that there are different rules of thumb in the literature: 
# Some people put the cutoff point for vif to 5, some to 10, some to 20... 

pairs(abalone[, c(3, 4, 6, 8)])



# check assumptions and remove points
par(mfrow=c(2, 3))
plot(modelt3, which=1:6)
# we will ignore discussing transformations here because there are other things we want to focus
modelta<-lm(abalone[-2052,2] ~ abalone[-2052,3] + abalone[-2052,4] + abalone[-2052,6] + abalone[-2052,8])
modelta2<-lm(abalone[-c(1418,2052),2] ~ abalone[-c(1418,2052),3] + abalone[-c(1418,2052),4] + abalone[-c(1418,2052),6] + abalone[-c(1418,2052),8])
modelta3<-lm(abalone[-c(1211,1418,2052),2] ~ abalone[-c(1211,1418,2052),3] + abalone[-c(1211,1418,2052),4] + abalone[-c(1211,1418,2052),6] + abalone[-c(1211,1418,2052),8])





###### Interactions
# Model after eliminating multicollinerity
modeli1<-lm(abalone[,2] ~ abalone[,3] * abalone[,4] * abalone[,6]* abalone[,8])
summary(modeli1)


###### Removing what is not necessary we have 
modeli2<-lm(abalone[,2] ~ abalone[,3] * abalone[,4] * abalone[,8] + abalone[,6])
summary(modeli2)

# Maybe remove more
###### Removing what is not necessary we have 
modeli3<-lm(abalone[,2] ~ abalone[,3] + abalone[,4] + abalone[,6] + abalone[,8] + abalone[,3] * abalone[,4] + abalone[,3] * abalone[,8])
summary(modeli3)









#  see what will happen if we have all the variables in to do variable selection. 
modelf <- lm(abalone[,2] ~ abalone[,3] + abalone[,4] + abalone[,5] +  abalone[,6] + abalone[,7] + abalone[,8])
summary (modelf) 

modelb = lm(abalone[,2]~ 1)
summary(modelb)

drop1(modeltvs,test="Chisq")
step(modelb, scope = list(upper=modelf, lower=~1), direction="both", trace=10)

modeltvs <- lm(abalone[,2] ~ abalone[,3] + abalone[,4]  +  abalone[,6] + abalone[,7] + abalone[,8])
summary(modeltvs)
vif(modeltvs)

# using intuition 
modeltvs3 <- lm(abalone[,2] ~ abalone[,3]  +  abalone[,6] + abalone[,7] )
summary(modeltvs3)

# using intuition is important here as there are variables that add a lot less to AIC and we 
# may be better off removing them if it is costly to get data for them  






# predict the number of rings (related to age) 
#  Let's now include all variables including sex in the model 
modelf2 <- lm(abalone[,9] ~abalone[,1] + abalone[,2] + abalone[,3] + abalone[,4] + abalone[,5]+ abalone[,6]+ abalone[,7] + abalone[,8])
summary (modelf2) 


modelb2 = lm(abalone[,9]~ 1)
summary(modelb2)

step(modelb2, scope = list(upper=modelf2, lower=~1), direction="both", trace=10)

modelfinal2 <- lm(abalone[,9] ~ abalone[,1] + abalone[,3] + abalone[,4] + abalone[,5]+ abalone[,6]+ abalone[,7] + abalone[,8])
summary(modelfinal2)


# let's try and fit poisson model
pmodel1<-glm(abalone[,9] ~abalone[,1] + abalone[,2] + abalone[,3] + abalone[,4] + abalone[,5]+ abalone[,6]+ abalone[,7] + abalone[,8], family="poisson")
summary(model1)

# to test individual effects: 
drop1(pmodel1, test="Chisq")



# To run lofistic regression
aba2 <-abalone
aba2[aba2[,1]=="M",1] = 1
aba2[aba2[,1]=="F",1] = 1 
aba2[aba2[,1]=="I",1] = 0
table(aba2[,1])



lmodel1<-glm(aba2[,1]~ abalone[,2] + abalone[,3] + abalone[,4] + abalone[,5]+ abalone[,6]+ abalone[,7] + abalone[,8] + abalone[,9], family=binomial)
#  the error is because the first column of aba2 is not numeric. 

lmodel1<-glm(as.numeric(aba2[,1])~ abalone[,2] + abalone[,3] + abalone[,4] + abalone[,5]+ abalone[,6]+ abalone[,7] + abalone[,8] + abalone[,9], family=binomial)
summary(lmodel1) 

drop1(lmodel1, test="Chisq")

vif(lmodel1)
# let's play with VIF to see which factors should be removed 

lmodel2<-glm(as.numeric(aba2[,1])~ abalone[,2] + abalone[,3] + abalone[,4] + abalone[,6]+ abalone[,7] + abalone[,8] + abalone[,9], family=binomial)
summary(lmodel2)
vif(lmodel2)


lmodel3<-glm(as.numeric(aba2[,1])~ abalone[,2]  + abalone[,4] + abalone[,6]+ abalone[,7] + abalone[,8] + abalone[,9], family=binomial)
summary(lmodel3)
vif(lmodel3)

# another case a plot can be useful
boxplot(abalone[,2]~abalone[,1])
# being an adult increases length so it doesnt make sense the coefficient for 2 is negative

# remove variable 2 as well 
lmodel5<-glm(as.numeric(aba2[,1])~  abalone[,4] + abalone[,6]+ abalone[,7] + abalone[,8] + abalone[,9], family=binomial)
summary(lmodel5)
vif(lmodel5)


# Important note here: 
# There is not a magic order on how to decide which regression model is better
# Check vif -> perform variable selection -> interactions -> use transformation -> remove



#### ANCOVA
#  We want to see if the average length is different for the three sexes at the present of diameter:


#  First check the slopes
par(mfrow=c(1, 3))
plot(abalone[abalone[,1]=="I",2], abalone[abalone[,1]=="I",3], xlim=c(0, 1), ylim=c(0, 1))
plot(abalone[abalone[,1]=="F",2], abalone[abalone[,1]=="F",3], xlim=c(0, 1), ylim=c(0, 1))
plot(abalone[abalone[,1]=="M",2], abalone[abalone[,1]=="M",3], xlim=c(0, 1), ylim=c(0, 1))

# Now check the assumptions of regression 
ANCOVAI = lm(abalone[abalone[,1]=="I",2] ~abalone[abalone[,1]=="I",3])
ANCOVAF = lm(abalone[abalone[,1]=="F",2] ~abalone[abalone[,1]=="F",3])
ANCOVAM = lm(abalone[abalone[,1]=="M",2] ~abalone[abalone[,1]=="M",3])

par(mfrow=c(2, 3))
# Check assumptions
plot(ANCOVAI, which=1:6)
plot(ANCOVAF, which=1:6)
plot(ANCOVAM, which=1:6)
###### 
# BE CAREFUL IF YOU DECIDE TO REMOVE POINTS... the numbers that you get are not the numbers of the original rows of the dataset
# since we have selected only part of the dataset. 

# Run an ANOVA to see if there is difference in the means 
anova(lm(abalone[,2]~abalone[,3]*abalone[,1]))

