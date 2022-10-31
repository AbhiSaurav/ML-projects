
# Question 1

q1data <- read.table("~/Documents/cardiff teaching/MAT005 Time Series/2022/OneDrive_1_01-02-2022-2/Week 2/latest/tutorial2_q1.csv", sep = ",", header = T)

# 1(a)

plot(q1data$Shipments~q1data$Month, type = "l")

# 1(b)

ship.ma3 <- filter(q1data$Shipments, c(1/3,1/3,1/3), sides = 2)
ship.ma3

# 1(c)

ship.ma5 <- filter(q1data$Shipments, c(1/5,1/5,1/5,1/5,1/5), sides = 2)
ship.ma5

# 1(d)

ship.ma7 <- filter(q1data$Shipments, c(1/7,1/7,1/7,1/7,1/7,1/7,1/7), sides = 2)
ship.ma7

# 1(e)

# apply MA3 again to ship.ma3

ship.ma3.2 <- filter(ship.ma3, c(1/3,1/3,1/3), sides = 2)
ship.ma3.2

# 1(e)

# apply MA5 again to ship.ma5

ship.ma5.2 <- filter(ship.ma5, c(1/5,1/5,1/5,1/5,1/5), sides = 2)
ship.ma5.2

plot(q1data$Month,q1data$Shipments,type="l") 
lines(ship.ma3,col=2) 
lines(ship.ma5,col=3) 
lines(ship.ma7,col=4) 
lines(ship.ma3.ma3,col=5) 
lines(ship.ma5.ma5,col=6)

legend(locator(1), c("observed","MA3","MA5","MA7","MA3.2","MA5.2"), lty = 1, col = c(1:6))

# 1(h)
# They all provide a similar fit to the middle points of the series
# The 3-month moving average gives a good fit and uses more points in the series

# Question 2

q2data <- read.table("~/Documents/cardiff teaching/MAT005 Time Series/2022/OneDrive_1_01-02-2022-2/Week 2/latest/tutorial2_q2.csv", sep = ",", header = T)

gasprod <- q2data$Gas.Production

# 2(a)

tgasprod<-ts(gasprod,start=c(1986,1),frequency=4)

plot(tgasprod)

# 2(b)

gasprod.ma4.uncen <- filter(gasprod, c(1/4,1/4,1/4,1/4), sides = 2)
gasprod.ma4.uncen

# 2(c)

gasprod.ma4.cen <- filter(gasprod.ma4.uncen, c(1/2,1/2), sides = 1)
gasprod.ma4.cen

# 2(d)

plot(gasprod,type="l") 
lines(gasprod.ma4.cen,col="blue")
legend(locator(1),c("observed","trend"), lty = 1, col = c("black","blue")) # click on an empty space in the plot, e.g. top left area

# Question 3

q3data <- read.table("~/Documents/cardiff teaching/MAT005 Time Series/2022/OneDrive_1_01-02-2022-2/Week 2/latest/tutorial2_q3.csv", sep = ",", header = T)

sales <- q3data$Sales

# 3(a)

tsales <- ts(sales, start = c(1990,1),frequency = 12)
tsales

plot(tsales)

# 3(b)(i)

# uncentered first

sales.ma12.uncen <- filter(sales, rep(1/12,12), sides = 2)
sales.ma12.uncen

# now centered - this is the trend cycle

sales.ma12.cen <- filter(sales.ma12.uncen, c(1/2,1/2), sides = 1)
sales.ma12.cen

# 3(b)(ii)

# Removing the trend

sales.seasmult <- sales/sales.ma12.cen
sales.seasmult

# 3(b)(iii)
# Calculating seasonal indices

month<-rep(seq(1,12),5) 
sales.seasmult.ind <-rep(NA,12)

for(i in 1:12){
sales.seasmult.ind[i]<-mean(sales.seasmult[month==i],na.rm=TRUE)
 }
 
sales.seasmult.ind <-rep(sales.seasmult.ind,5) 
sales.seasmult.ind[c(1:6,55:60)]<-NA 
sales.seasmult.ind

# To match it back to months

tsales.seasmult.ind <- ts(sales.seasmult.ind,start=c(1990,1),frequency=12) 
tsales.seasmult.ind

# 3(b)(iv)

sales.errmult <- sales/(sales.ma12.cen*sales.seasmult.ind)
sales.errmult

# 3(b)(v)

sales.ma12.cen*sales.seasmult.ind*sales.errmult
sales

# 3(b)(vi)

# plot of the data again

plot(sales, type = "l")

# plot of the trend cycle

plot(sales.ma12.cen, type = "l")

# plot of the seasonal indices

plot(sales.seasmult.ind, type = "l")

# plot of the errors

plot(sales.errmult, type = "l")

# all on one plot - can a legend as well if you like!

plot(sales,type="l",ylim=c(0,1800)) 
lines(sales.ma12.cen,col="blue") 
lines(sales.seasmult.ind,col="red") 
lines(sales.errmult,col="darkgreen")

# 3(c)(i) 
# no different to the trend cycle calculated above

sales.ma12.cen

# 3(c)(ii)

# Removing the trend

sales.seasadd <- sales - sales.ma12.cen
sales.seasadd

# 3(c)(iii)
# Calculating seasonal indices

month<-rep(seq(1,12),5) 
sales.seasadd.ind <-rep(NA,12)

for(i in 1:12){
sales.seasadd.ind[i]<-mean(sales.seasadd[month==i],na.rm=TRUE)
 }
 
sales.seasadd.ind <-rep(sales.seasadd.ind,5) 
sales.seasadd.ind[c(1:6,55:60)]<-NA 
sales.seasadd.ind

# To match it back to months

tsales.seasadd.ind <- ts(sales.seasadd.ind,start=c(1990,1),frequency=12) 
tsales.seasadd.ind

# 3(c)(iv)

sales.erradd <- sales - sales.ma12.cen - sales.seasadd.ind
sales.erradd

sales.ma12.cen + sales.seasadd.ind + sales.erradd

sales

# 3(c)(vi)

# plot of the data again

plot(sales, type = "l")

# plot of the trend cycle

plot(sales.ma12.cen, type = "l")

# plot of the seasonal indices

plot(sales.seasadd.ind, type = "l")

# plot of the errors

plot(sales.erradd, type = "l")

# all on one plot - can a legend as well if you like!

plot(sales,type="l",ylim=c(-300,1800)) 
lines(sales.ma12.cen,col="blue") 
lines(sales.seasadd.ind,col="red") 
lines(sales.erradd,col="darkgreen")

# 3 (d)
# We can see the additive trend model provides a better fit for this data

# 3 (e)
# For the multiplicative trend

tsales.decomp.mult <- decompose(tsales,"multiplicative")
plot(tsales.decomp.mult)

# For the additive trend

tsales.decomp.add <- decompose(tsales,"additive")
plot(tsales.decomp.add)

# Question 4

q4data <- read.table("~/Documents/cardiff teaching/MAT005 Time Series/2022/OneDrive_1_01-02-2022-2/Week 2/latest/tutorial2_q4.csv", sep = ",", header = T)

qsales <- q4data$Sales

# 4(a)

tqsales <- ts(qsales, start = c(1990,1),frequency = 4)
tqsales

plot(tqsales)

# 4(b)(i)

# uncentered first

qsales.ma4.uncen <- filter(qsales, rep(1/4,4), sides = 2)
qsales.ma4.uncen

# now centered - this is the trend cycle

qsales.ma4.cen <- filter(qsales.ma4.uncen, c(1/2,1/2), sides = 1)
qsales.ma4.cen

# 4(b)(ii)

# Removing the trend

qsales.seasmult <- qsales/qsales.ma4.cen
qsales.seasmult

# 4(b)(iii)
# Calculating seasonal indices

quarter <-rep(seq(1,4),6) 
qsales.seasmult.ind <-rep(NA,4)

for(i in 1:4){
qsales.seasmult.ind[i]<-mean(qsales.seasmult[quarter == i],na.rm=TRUE)
 }
 
qsales.seasmult.ind <-rep(qsales.seasmult.ind,6) 
qsales.seasmult.ind[c(1:2,23:24)]<-NA 
qsales.seasmult.ind

# To match it back to quarters

tqsales.seasmult.ind <- ts(qsales.seasmult.ind,start=c(1990,1),frequency=4) 
tqsales.seasmult.ind

# 4(b)(iv)

qsales.errmult <- qsales/(qsales.ma4.cen*qsales.seasmult.ind)
qsales.errmult

# 3(b)(v)

qsales.ma4.cen*qsales.seasmult.ind*qsales.errmult
qsales

# 4(b)(vi)

# plot of the data again

plot(qsales, type = "l")

# plot of the trend cycle

plot(qsales.ma4.cen, type = "l")

# plot of the seasonal indices

plot(qsales.seasmult.ind, type = "l")

# plot of the errors

plot(qsales.errmult, type = "l")

# all on one plot - can a legend as well if you like!

plot(qsales,type="l",ylim=c(0,1800)) 
lines(qsales.ma4.cen,col="blue") 
lines(qsales.seasmult.ind,col="red") 
lines(qsales.errmult,col="darkgreen")

# 4(c)(i) 
# no different to the trend cycle calculated above

qsales.ma4.cen

# 4(c)(ii)

# Removing the trend

qsales.seasadd <- qsales - qsales.ma4.cen
qsales.seasadd

# 4(c)(iii)
# Calculating seasonal indices

quarter <-rep(seq(1,4),6) 
qsales.seasadd.ind <-rep(NA,4)

for(i in 1:4){
qsales.seasadd.ind[i]<-mean(qsales.seasadd[quarter==i],na.rm=TRUE)
 }
 
qsales.seasadd.ind <-rep(qsales.seasadd.ind,6) 
qsales.seasadd.ind[c(1:2,23:24)]<-NA 
qsales.seasadd.ind

# To match it back to quarters

tqsales.seasadd.ind <- ts(qsales.seasadd.ind,start=c(1990,1),frequency=4) 
tqsales.seasadd.ind

# 4(c)(iv)

qsales.erradd <- qsales - qsales.ma4.cen - qsales.seasadd.ind
qsales.erradd

qsales.ma4.cen + qsales.seasadd.ind + qsales.erradd

qsales

# 4(c)(vi)

# plot of the data again

plot(qsales, type = "l")

# plot of the trend cycle

plot(qsales.ma4.cen, type = "l")

# plot of the seasonal indices

plot(qsales.seasadd.ind, type = "l")

# plot of the errors

plot(qsales.erradd, type = "l")

# all on one plot - can a legend as well if you like!

plot(qsales,type="l",ylim=c(-300,1800)) 
lines(qsales.ma4.cen,col="blue") 
lines(qsales.seasadd.ind,col="red") 
lines(qsales.erradd,col="darkgreen")

# 3 (d)
# We can see the multiplicative trend model provides a better fit for this data

# 3 (e)
# For the multiplicative trend

tqsales.decomp.mult <- decompose(tqsales,"multiplicative")
plot(tqsales.decomp.mult)

# For the additive trend

tqsales.decomp.add <- decompose(tqsales,"additive")
plot(tqsales.decomp.add)











