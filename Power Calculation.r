library(lawstat)
library(EnvStats)
library(TeachingDemos)
library(Deducer)
library(pwr) 


?pwr.t.test
?pwr.p.test
?pwr.t2n.test
?pwr.2p2n.test
?pwr.2p.test
?pwr.anova.test
?pwr.chisq.test
?pwr.f2.test
?pwr.norm.test



abalone<-read.table("abalone.data", sep=",")



#  Test if the proportion of males that have diameter more than 0.25 is equal to 0.95
#  Find the power of the above test if the effect size is 0.02  (effect size is the difference between 2*arcsine(sqrt(p_i))
pwr.p.test(h=0.02, n=1528, alternative="two.sided")
pwr.p.test(h=0.02, power=0.5, alternative="two.sided")



# Similarly when we want to compare whether the proprotion of males and females that have diameter more than 0.25 are equal. 
pwr.2p2n.test(h=0.02, n1=1528, n2=1307, alternative="two.sided")


# If you want to see whether the proportion of females that have diameter more than 0.25 is larger than the proportion of males that have diameter more than 0.25
pwr.2p2n.test(h=0.02, n1=1528, n2=1307, alternative="less")

# If we want males and female abalones to have the same sample and achieve specific power. 
pwr.2p.test(h=0.02, power=0.25, alternative="two.sided")


# Test if the average diameter of males is different than 0.45
# Find the power at effet size 0.02 (effect size this is the difference between the means divided by the standard deviation (pooled if we have two samples) )
pwr.t.test(h=0.02, n=1528, alternative ="two.sided")

# the correct one is the one sample case. 
pwr.t.test(d=0.02, n=1528, type="one", alternative ="two.sided")



pwr.t.test(d=0.02, power=0.75, type="one", alternative ="two.sided")



# Test if the average diameter of males and females are equal   - using different sample sizes. 
pwr.t2n.test(d=0.02, n1=1528, n2=1307, alternative="two.sided")


#plot the power. 
s <- seq(0.001, 0.5, 0.001)
plot(s, pwr.t2n.test(d=s, n1=1528, n2=1307, sig.level =0.05, alternative="two.sided")$power, type="l")



