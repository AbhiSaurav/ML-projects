library(lawstat)
library(EnvStats)
library(TeachingDemos)
library(Deducer)
library(pwr) 



abalone<-read.table("abalone.data", sep=",")



# Fit a regession to see if length can predict the whole weight
lm(abalone[,5] ~abalone[,2])

model1 <- lm(abalone[,5] ~abalone[,2])

summary (model1) 
# We see very high R^2 which seems to suggest excellent fit... 

# Let's check assumptions
plot(model1, which=1:6)
# Residuals show trend
# Normality seem to be violated. 
# A few outliers and influential points ... 
# Let's check a bit... 



plot(abalone[,2], abalone[,5])
abline(model1)
# everything is clear now that there is a nonlinear relationship..

#  Let's try some transformation
plot(abalone[,2]^2, abalone[,5])
# still a bit of curvature
# let's change the power...
plot(abalone[,2]^3, abalone[,5])


model1a <- lm(abalone[,5] ~abalone[,2]^3)
summary(model1a) 
# something doesn't work... it gives me the same results as the previous model... 
# I need to create the length^3 variable... 

lengthcube <- abalone[,2]^3
model1a <- lm(abalone[,5] ~lengthcube)
summary(model1a)
# improved R^2... let's check assumptions
plot(model1a, which=1:6)


# I will do one more transformation
model1b <- lm(log(abalone[,5]) ~ log(lengthcube))
summary(model1b)
# even better R^2... let's check assumptions
plot(model1b, which=1:6)
# Normality is better... still violated a bit... but we are not going to get it perfect in real datasets
# A few influential points... 

# Let's remove that observation 1211
model1c <- lm(log(abalone[-1211,5]) ~ log(lengthcube[-1211]))
summary(model1c)
plot(model1c, which=1:6)




# Still one influential point ... this is 1986  (but you have to be careful...) 
#  This is observation 1987 in the original dataset as you have already removed 1211... 
model1d <- lm(log(abalone[-c(1211,1987),5]) ~ log(lengthcube[-c(1211,1987)]))
summary(model1d)
plot(model1d, which=1:6)



# Still one influential point ... this is 3715  (but you have to be careful...) 
#  This is observation 3717 in the original dataset as you have already removed 1211 and 1987... 
model1e <- lm(log(abalone[-c(1211,1987, 3717),5]) ~ log(lengthcube[-c(1211,1987, 3717)]))
summary(model1e)
plot(model1e, which=1:6)



# Here is where nothing is black or white... different people may make different decisions... 
# Someone may stop because everything seems a bit stabilized
# Someone may say, no I want to remove the two points that have clearly higher Cook's distance than the rest (1764, 2122)... Remember these are 1765 and 2124 in the original dataset
# Someone may say, I am happy with Cook's Distance but I do not like that observation (2640) that has  standardzed residual almost equal to 10 (in the original dataset this is 2642)
# I will stop here as this is a never ending cycle!!!



# Second example!!!

# Fit a regression model to see if whole weight predicts the age (number of rings) 
# Whole weight is the fifth column and rings is the number on the last column
lm(abalone[,9]~abalone[,5])

model2 <- lm(abalone[,9]~abalone[,5])

summary(model2) 

plot(model2, which=1:6)

# Many observations.  Normality is violated, a few observations seem to be influential. 
# Let's plot the two variables 
plot(abalone[,5], abalone[,9])
#maybe log will work

#
model2a <- lm(log(abalone[,9])~abalone[,5])
summary(model2a)
plot(model2a, which=1:6)
# remove 237 
# 
model2b <- lm(log(abalone[-237,9])~abalone[-237,5])
summary(model2b)
plot(model2b, which=1:6)



