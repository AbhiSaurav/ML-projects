
# Question 1

randomdat <- read.table("~/Documents/cardiff teaching/MAT005 Time Series/2022/OneDrive_1_01-02-2022-2/Week 5/latest/randomdata.csv", sep =",", header = T)

# For table
acf(randomdat$Value, lag.max = 10, plot = F)

# For plot
acf(randomdat$Value, lag.max = 10)

# Question 2

# Box-Pierce Q Statistic

randomdat.ac <- as.vector(acf(randomdat$Value, lag.max = 10, plot = F)$acf)[-1]

36*sum(randomdat.ac^2) # 5.62 (same as in the lecture slides)

# Box-Pierce Q Statistic using Box.test function

Box.test(randomdat$Value, lag = 10) # Same as the value above

# Lyung-Box Qstar Statistic

36*38*sum(randomdat.ac^2/(35:26)) # 7.2174

# Lyung-Box Qstar Statistic using Box.test function

Box.test(randomdat$Value, lag = 10, type = "Ljung-Box") # Same as the value above

# Question 3

# For table
pacf(randomdat$Value, lag.max = 10, plot = F)

# Question 4

randomdat.diff1 <- randomdat$Value[-1] - randomdat$Value[-length(randomdat$Value)]

plot(randomdat$Value, type = "l", ylim = c(-100,100))
lines(randomdat.diff1, col = "blue", type = "l")

# Both data sets have a similar randomness, e.g. no significant serial correlation, perhaps a bit less in the differenced data. The difference data are also centered around 0 while the original are not.

# Question 5

time <- 1:30
error <- c(0.01, 1.38, 0.53, 1.58, 1.32, 1.04, 0.33, -0.2, 1.90, 0.72, -0.27, -1.43, -1.15, -0.07, 1.69, 0.28, 0.01, 0.94, -2.10, 0.09,0.91, 1.76, 0.84, -1.13, 0.92, 1.67, -1.03, -1.71, 1.18, -0.59)

con <- 10 # value of c set to 10

# part (a)

y.ar1 <- con + error[1]
for(i in 1:(length(error)-1)){
	y.ar1new <- con + 0.6*y.ar1[i] + error[i+1]
	y.ar1 <- c(y.ar1, y.ar1new)
}

# part (b)

y.ma1 <- con + error[1] + 0.6*0

for(i in 1:(length(error)-1)){
	y.ma1new <- con + error[i+1] + 0.6*error[i]
	y.ma1 <- c(y.ma1,y.ma1new)
}

# part (c)

plot(y.ar1, type = "l", ylim = c(0,30))
lines(y.ma1, col = "blue", type = "l")

# Despite the starting points being the same the two time series are very different

# part (d)

y.arma11 <- con + error[1]
for(i in 1:(length(error)-1)){
	y.arma11new <- con + 0.6*y.arma11[i] + error[i+1] + 0.6*error[i]
	y.arma11 <- c(y.arma11, y.arma11new)
}

# part (e)

y.arima211 <- 7 + error[1]
y.arima211new <- 7 + 1.3*y.arima211 - 0.3*0 + error[2] - 0.7*error[1]

y.arima211 <- c(y.arima211,y.arima211new)

for(i in 2:(length(error)-1)){
	y.arima211new <- 7 + 1.3*y.arima211[i] - 0.3*y.arima211[i-1] + error[i+1] - 0.7*error[i]
	y.arima211 <- c(y.arima211, y.arima211new)
}

# Question 6

usersdat <- read.table("~/Documents/cardiff teaching/MAT005 Time Series/2022/OneDrive_1_01-02-2022-2/Week 5/latest/users.csv", sep =",", header = T)

user.arima310 <- arima(usersdat$Users, order = c(3,1,0))
user.arima310$coef # values slightly different to the lecture slides as the estimation method is different here

user.arima310.fitted <- usersdat$Users - user.arima310$residuals

plot(usersdat, type = "l")
lines(user.arima310.fitted, col = "blue", type = "l")
# a pretty decent fit

# Question 7

# prediction 5 time points ahead using model in the lecture slides

user.arima310.forecast <- usersdat$Users

for(i in (length(usersdat$Users)+1):(length(usersdat$Users)+5)){
user.arima310.forecast.new <- 2.1239*user.arima310.forecast[i-1] - 1.7369*user.arima310.forecast[i-2] + 0.9334*user.arima310.forecast[i-3] - 0.3204*user.arima310.forecast[i-4]
user.arima310.forecast <- c(user.arima310.forecast,user.arima310.forecast.new)
}

user.arima310.forecast[101:105]

# prediction 5 time points ahead using model obtained in R in Question 7

predict(user.arima310, n.ahead = 5) # this yields very similar predictions



