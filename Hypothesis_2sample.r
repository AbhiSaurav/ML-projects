library(lawstat)
library(EnvStats)
library(TeachingDemos)
library(Deducer)

abalone<-read.table("abalone", sep=",")

# Test if there is evidence that the average height of the shells of females is
#  significantly different than the average height of males 
# Assume normality. 

# We should have tested the equality of variances 
var.test(abalone[abalone[,1]=="M",4] , abalone[abalone[,1]=="F",4])

#Reject equality so we use t-test 
t.test(abalone[abalone[,1]=="M",4], abalone[abalone[,1]=="F",4])
#av height of male is sig diff than av height of female

# If you want to change the significance level and make it 0.01 
# Notice that the code does not change... 

# Let's use the CI to do the test.  Using the CI to do the test we get the same answer
#  But if we want to change the significance level of the test to 0.01 we need to change the 
# confidence level of the CI. 
t.test(abalone[abalone[,1]=="M",4], abalone[abalone[,1]=="F",4], conf.level =0.99)


# Test if there is evidence that the average height of the shells of females 
# is larger than 0.05 from the average height of infants
# Assuming normality 

# First check the equality of variances
var.test(abalone[abalone[,1]=="I",4] , abalone[abalone[,1]=="F",4]) # reject

# Now make the actual test
t.test(abalone[abalone[,1]=="F",4], abalone[abalone[,1]=="I",4], alternative="greater", mu=0.05, conf.level =0.95)
# or 
t.test(abalone[abalone[,1]=="I",4], abalone[abalone[,1]=="F",4], alternative="less", mu=-0.05, conf.level =0.95)
#there is no statically significant evidence

#  How to make the test without assuming normality
# check normality for both samples
shapiro.test(abalone[abalone[,1]=="F",4])
shapiro.test(abalone[abalone[,1]=="I",4])
# reject normality for both 

# test symmetry
symmetry.test(abalone[abalone[,1]=="F",4])
symmetry.test(abalone[abalone[,1]=="I",4])
# reject for both (sometimes for females is not rejected but it doesn't really affect anything as we need both to be assumed symmetric to use Wilcoxon)

# use permutation tests
perm.t.test(abalone[abalone[,1]=="F",4], abalone[abalone[,1]=="I",4], alternative="greater", mu=0.05)

# the above does not run because you cannot specify the null value... Can we do something else?  Yes(add or subtract 0.05 from one of the variables)
perm.t.test(abalone[abalone[,1]=="F",4], abalone[abalone[,1]=="I",4]+0.05, alternative="greater")
#there is no statically significant evidence


# What if we couldn't have used the perm.t.test
# Well in situations where the test we want is not available we go with our next best choice - in this case the Wilcoxon rank sum test. 



#  Test if there is evidence that the average length and the diameter of the infant abalones
#  differ more than 0.1. 


lengdiame <- abalone[abalone[,1]=="I", 2] - abalone[abalone[,1]=="I", 3]

shapiro.test(lengdiame) # reject

symmetry.test(lengdiame) # rejected 4 out of 5 times

oneSamplePermutationTest(lengdiame, alternative="greater", mu=0.1) #reject
oneSamplePermutationTest(lengdiame, alternative="greater", mu=0.1)$p.value



# if we can assume normality
t.test(lengdiame, alternative="greater", mu=0.1)


# or you can also run!!!
t.test(abalone[abalone[,1]=="I", 2],  abalone[abalone[,1]=="I", 3], alternative="greater", mu=0.1, paired=TRUE) 

# Test to see if the proportion of male abalones that have diameter more than 0.25 is 
# equal to the proportion of females that have diameter more than 0.25


table(abalone[, 1])   # males 1528, females 1307
table(abalone[abalone[,3]>0.25, 1]) # males: 1474, females: 1300
fisher.test(matrix(c(54, 1474, 7, 1300), nrow=2))

# approximate test
prop.test(c(1474, 1300), c(1528, 1307))

# Test to see if the proportion of male abalones that have diameter more than 0.25 
# is the same as the proportion of male abalones that have length more than 0.35 


# Dependent samples use McNemar
table(abalone[abalone[,3]>0.25 & abalone[,2]>0.35, 1])   #1450
table(abalone[abalone[,3]>0.25, 1])  #1474    - 24
table(abalone[abalone[,2]>0.35, 1])  # 1451   - 1
table(abalone[abalone[,3]<= 0.25 & abalone[,2]<=0.35, 1])  # 53

mcnemar.test(matrix(c(1450, 24, 1, 53), nrow=2))


