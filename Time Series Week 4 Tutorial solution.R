######################################
############# Question 1 #############
######################################

Time_period = 1:10
Sales = c(30,20,45,35,30,60,40,50,45,65)

# part a
mean(Sales)
mean(Time_period)

# part b
model1 = lm(Sales~Time_period)

# part c
# Note we could also use the predict function
fitted_sales = model1$coefficients[1]+model1$coefficients[2]*Time_period

# part d
plot(Time_period,Sales,type='l')
lines(Time_period,fitted_sales)




######################################
############# Question 2 #############
######################################



Unit_produced = c(5,2,8,4,6)
Total_cost = c(25,11,34,23,32)

# part a
model2= lm(Total_cost~Unit_produced)


# part b

Covariance = (sum(c(Unit_produced-mean(Unit_produced))*(c(Total_cost-mean(Total_cost))) ))/(length(Total_cost)-1)
sample_var_x=sum(c(Unit_produced-mean(Unit_produced))^2)/(length(Total_cost)-1)
sample_var_y=sum(c(Total_cost-mean(Total_cost))^2)/(length(Total_cost)-1)

Covariance/sqrt(sample_var_x*sample_var_y)

#using built in R function

cor(Unit_produced,Total_cost)


# part c
Y_fitted =  model2$coefficients[1]+model2$coefficients[2]*Unit_produced
cor(Total_cost,Y_fitted)^2

# R^2 is also shown in the summary function
summary(model2)


######################################
############# Question 3 #############
######################################

# Ozone
X <-c(5, 7, 13, 14, 17, 20, 26, 30, 34, 39, 44)
# Melanoma
Y <-c(1, 1, 3, 4, 6, 5, 6, 8, 7, 10, 9)

# part a
plot(X,Y,type='l',lwd=3)
model3 = lm(Y~X)

# part b  - look for R^2 in summary
summary(model3)


# part c
model3$coefficients[1]+model3$coefficients[2]*40


# part d
# For the confidence interval, we need the sample standard error

Y_fitted = model3$coefficients[1]+model3$coefficients[2]*X

Standard_error_residual = sqrt(sum((Y-Y_fitted)^2)/(length(X)-2))

Standard_error_coeff = Standard_error_residual*sqrt(1/(sum(c(X-mean(X))^2)))

# take quantiles from the student t distribution  
lower_CI = model3$coefficients[2]- qt(0.975,length(X)-2)*Standard_error_coeff
upper_CI=model3$coefficients[2]+ qt(0.975,length(X)-2)*Standard_error_coeff

c(lower_CI,upper_CI)


# part e
#  Look at the F statistic in the last line of the summary. A very small p-value suggests there is statistically significant evidence the model is useful.
summary(model3)



######################################
############# Question 4 #############
######################################


# Year
X <-c(1896, 1900, 1904, 1908, 1912, 1920, 1924, 1928, 1932, 1936, 1948, 1952,1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996)
# Time
Y <-c(54.2, 49.4, 49.2, 50, 48.2, 59.6, 47.6, 47.8, 46.2, 46.5, 46.2, 45.9,46.7, 44.9, 45.1, 43.8, 44.66, 44.26, 44.6, 44.27, 43.87, 43.5, 43.69)


# part a 
plot(X,Y,col='blue',pch=4)

# part b
model4 = lm(Y~X)

# part c
plot(X,model4$residuals)

# part d (have only included the 2000 prediction)
model4$coefficients[1]+model4$coefficients[2]*2000

# In general, we should not use regression toe predict outside the window of data



######################################
############# Question 5 #############
######################################



deom <-c(NA, 1.146, -2.443, 1.497, -0.132, 2.025, 0.737, -1.023, -0.956, 0.385,0.983, 5.092, 3.649, 2.703, -0.271, 2.055, -0.714, 0.653, -0.034, -1.058,-2.051, 1.451, -0.989, 1.358, 0.746, 1.855, -1.894, 0.781, -0.161, 2.233,2.425, 2.169, 0.982, 4.708, 6.063, 9.382, 9.304, 10.69, 6.531, 7.873, 3.882,4.96, 1.301, 1.154, 0.116, 4.928, 2.53, 8.425, 5.291, 5.192, 0.257, 4.402,3.173, 5.104, 4.646, 1.06, -0.758, 4.702, 1.878, 6.62)
AAA <-c(5.94, 6, 6.08, 6.17, 6.14, 6.09, 5.87, 5.84, 5.99, 6.12, 6.42, 6.48,6.52, 6.64, 6.75, 6.73, 6.89, 6.98, 6.98, 7.1, 7.19, 7.29, 7.65, 7.75, 7.72,7.67, 7.66, 7.89, 8.14, 8.21, 8.05, 7.94, 7.88, 7.79, 7.41, 7.17, 7.15,7.27, 7.37, 7.54, 7.58, 7.62, 7.58, 7.48, 7.35, 7.19, 7.19, 7.11, 7.16,7.22, 7.36, 7.34, 7.3, 7.3, 7.27, 7.3, 7.31, 7.26, 7.24, 7.25)
rate34 <-c(5.31, 5.6, 5.49, 5.8, 5.61, 5.28, 5.19, 5.18, 5.3, 5.23, 5.64, 5.62,5.67, 5.83, 5.53, 5.76, 6.09, 6.52, 6.68, 7.07, 7.12, 7.25, 7.85, 8.02,7.87, 7.14, 7.2, 7.59, 7.74, 7.51, 7.46, 7.09, 6.82, 6.22, 5.61, 5.48, 4.78,4.14, 4.64, 5.52, 5.95, 6.2, 6.03, 5.6, 5.26, 4.96, 5.28, 5.37, 5.53, 5.72,6.04, 5.66, 5.75, 5.82, 5.9, 6.11, 6.05, 5.98, 6, 6.24)
D34 <-c(NA, 0.29, -0.11, 0.31, -0.19, -0.33, -0.09, -0.01, 0.12, -0.07, 0.41,-0.02, 0.05, 0.16, -0.3, 0.23, 0.33, 0.43, 0.16, 0.39, 0.05, 0.13, 0.6,0.17, -0.15, -0.73, 0.06, 0.39, 0.15, -0.23, -0.05, -0.37, -0.27, -0.6,-0.61, -0.13, -0.7, -0.64, 0.5, 0.88, 0.43, 0.25, -0.17, -0.43, -0.34, -0.3,0.32, 0.09, 0.16, 0.19, 0.32, -0.38, 0.09, 0.07, 0.08, 0.21, -0.06, -0.07,0.02, 0.24)


# Keep the first 53 rows
deom_reduced=deom[2:54]
AAA_reduced=AAA[2:54]
rate34_reduced=rate34[2:54]
D34_reduced=D34[2:54]


# part a
model5 = lm(deom_reduced~1+AAA_reduced+rate34_reduced+D34_reduced)

# part b - can be done with formulas or by using R
# Using $ we can get many properties of our model

sum(model5$residuals^2)


# part c

# Predict using formula
model5$coefficients[1]+model5$coefficients[2]*AAA[55:60]+model5$coefficients[3]*rate34[55:60]+model5$coefficients[4]*D34[55:60]
# Predict using R function. Note that in the dataframe, they must have the same name as the one used in the model construction.
new_data= data.frame(AAA_reduced=AAA[55:60],rate34_reduced=rate34[55:60],D34_reduced=D34[55:60] )
predict(model5,newdata=new_data)

# part d


model5 = lm(deom[2:60]~1+AAA[2:60]+rate34[2:60]+D34[2:60])

data_summary = summary(model5)

data_summary$coefficients[4,4]
# Since this is >0.05, there is not significant statistical evidience to reject thr hypothesis beta_4=0


######################################
############# Question 6 #############
######################################



compA <-c(7, 11, 11, 3, 2, 3, 21, 1, 11, 10)
compB <-c(26, 52, 55, 71, 31, 54, 47, 40, 66, 68)
compC <-c(60, 20, 22, 6, 44, 22, 26, 34, 12, 14)
heat <-c(78, 104, 109, 102, 74, 93, 115, 83, 113, 109)


# part a
model6 = lm(heat~compA+compB+compC)
        

# part b
plot(model6$residuals)

# part c
predict(model6,data.frame(compA=10,compB=40,compC=30))


# part d

X = cbind(1,compA,compB,compC)
Y = heat

solve(t(X)%*%X)%*%t(X)%*%Y
model6$coefficients

