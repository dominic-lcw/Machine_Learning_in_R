#Assignment 1
#Name: Leung Cheuk Wai Dominic
#SID: 1155093086 

#setwd("~/QFRM/RMSC4002/Assignment 1") #For my own use

#Part1
stock <- read.csv("hkse50.csv") #Input the stock list
#set.seed(93086) 93086 will generate the stock 1880.HK (which is already delisted)
set.seed(3086) #Set the random seed
r <- sample(1:50, size=5)
stock[r,]

stockdata <- read.csv("asg1.csv") #Input Stock Data downloaded from Yahoo

ts1 <- as.ts(stockdata$PA) #Time Series of Price
ts2 <- as.ts(stockdata$LINK)
ts3 <- as.ts(stockdata$PETRO)
ts4 <- as.ts(stockdata$CN)
ts5 <- as.ts(stockdata$CITIC)
pricemain <-cbind(ts1,ts2,ts3,ts4,ts5) #price data

#Part2
#v0 is the initial portfolio value on 31/12/2017
len <- nrow(stockdata)
v0 <- 5000 * (sum(pricemain[len,1:5]))
v0

ret1 <- (lag(ts1)-ts1)/ts1 #Return Series of the 5 stocks
ret2 <- (lag(ts2)-ts2)/ts2
ret3 <- (lag(ts3)-ts3)/ts3
ret4 <- (lag(ts4)-ts4)/ts4
ret5 <- (lag(ts5)-ts5)/ts5

#Part3
main <- cbind(ret1,ret2,ret3,ret4,ret5)
n <- nrow(main) #Length of the time series
n1 <- n-60+1 #To find the 60th observations before n (use 60days data)
u60 <- main[n1:n,] #Get the 60days data
mean <- apply(u60,2,mean) #Mean
var <- var(u60) #Variance-Covariance Matrix
C <-chol(var) #Cholesky Decomposition

portvalue <- rep(0,1000) #Set up the place to store simulated portfolio
simprice <- matrix(0,1000,5) #Set up the place to store simulated price

#Simulate 1000times
for (i in 1:1000){
  s0 <- pricemain[len,] #Every simulation starts at the latest price
#Monte Carlo Simulation
for (j in 1:10){
  z <-rnorm(5)
  v<-mean+t(C)%*%z  #Simulate with mean and SD
  s1 <-s0*(1+v)  #Calculate the simulate value with s0 as the original value
  s0 <-s1 #Update s0 with s1
}
  simprice[i,] <- s0
  portvalue[i] <- 5000*sum(s0)
}

#Part4
pnl <- portvalue - v0 #Profit and Loss by minusing the original value
head(pnl)

#Part5
min(pnl) #Find the min P&L
max(pnl) #Find the max P&L
mean(pnl) #Find the mean of P&L
sd(pnl) #S.D. of P&L
quantile(pnl,0.01) #Quantile Output
quantile(pnl,0.05) #Quantile Output

hist(pnl,breaks=20) #Histogram for reference only
