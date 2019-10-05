#RMSC 4002 Project
setwd("~/QFRM/RMSC4002/Group Project") #For my own use only
d <- read.csv("filtercleanedstock.csv") #read csv

source("cleandata.R") #clean data file
d <- cleandata(d,"main.csv") #used for normalizing the data

summary(d)

#Plotting dataset
plotd <- d #plotd is used for plotting
plotd[,3] <- plotd[,3]+1

#Box plot for the distribution
par(mfrow = c(1,8))
boxplot(d[,4],main="Operating CF")
boxplot(d[,5],main="Net Income")
boxplot(d[,6],main="Sales Gwth")
boxplot(d[,7],main="Receiv. Gwth")
boxplot(d[,8],main="Invent. Gwth")
boxplot(d[,9],main="COGS Gwth")
boxplot(d[,11],main="Gross Margin")
boxplot(d[,10],main="3YR Avg GM")

#Scatter plot to see the relationship
par(mfrow = c(1,2)) 
plot(plotd[,4],plotd[,6],pch=21,xlab="Operating Cash Flow",ylab = "Sales Growth", 
     main="Scatter Plot for OperCF and Sales Growth")
plot(plotd[,4],plotd[,5],pch=21,xlab="Operating Cash Flow",ylab = "Net Income", 
     main="Scatter Plot for OperCF and Net Income")
pairs(~d[,7]+d[,8]+d[,9],data=d,main="Scatter Matrix for Balance Sheet Related Data",
      labels=c("Receivable Growth","Inventory Growth","COGS Growth"),cex.labels=1.5)
plot(d[,11],d[,10],xlab="Gross Margin",ylab = "3Yr Average Gross Margin",
     main="Scatter Plot for Gross Margin")

set.seed(888) #
n <- nrow(d) #Get the length of the dataset
id <- sample(1:n,size=(n*0.8))  #get the 580 random index for trianing data set
d1 <- d[id,] #Save 580 data into training dataset d
d2 <- d[-id,] #Save 110 data into testing dataset d1

#ANN Part

library(nnet) #Import library nnet
#set up the ann function for repeating ANN to get the best solution
source("annfun.R")
#Initialize Results matrix to store the error
Results <- matrix(0,nrow=10,ncol=2) #initiate the results

#ANN for different hidden layer
it=2000
try=500
ann1 <- ann(d1[,4:11],d1[,3],size=1,it,F,try)
ann2 <- ann(d1[,4:11],d1[,3],size=2,it,F,try)
ann3 <- ann(d1[,4:11],d1[,3],size=3,it,F,try)
ann4 <- ann(d1[,4:11],d1[,3],size=4,it,F,try)
ann5 <- ann(d1[,4:11],d1[,3],size=5,it,F,try)
ann6 <- ann(d1[,4:11],d1[,3],size=6,it,F,try)
ann7 <- ann(d1[,4:11],d1[,3],size=7,it,F,try)
ann8 <- ann(d1[,4:11],d1[,3],size=8,it,F,try)
ann9 <- ann(d1[,4:11],d1[,3],size=9,it,F,try)
ann10 <- ann(d1[,4:11],d1[,3],size=10,it,F,try)

#Store Error in the Results Matrix
Results[1,1]<-ann1$value
Results[2,1]<-ann2$value
Results[3,1]<-ann3$value
Results[4,1]<-ann4$value
Results[5,1]<-ann5$value
Results[6,1]<-ann6$value
Results[7,1]<-ann7$value
Results[8,1]<-ann8$value
Results[9,1]<-ann9$value
Results[10,1]<-ann10$value

#Try on testing dataset and get the fitted value
pr1 <- predict(ann1,d2)
pred1 <- round(pr1)
pr2 <- predict(ann2,d2)
pred2 <- round(pr2)
pr3 <- predict(ann3,d2)
pred3 <- round(pr3)
pr4 <- predict(ann4,d2)
pred4 <- round(pr4)
pr5 <- predict(ann5,d2)
pred5 <- round(pr5)
pr6 <- predict(ann6,d2)
pred6 <- round(pr6)
pr7 <- predict(ann7,d2)
pred7 <- round(pr7)
pr8 <- predict(ann8,d2)
pred8 <- round(pr8)
pr9 <- predict(ann9,d2)
pred9 <- round(pr9)
pr10 <- predict(ann10,d2)
pred10 <- round(pr10)

#Calculate the
N <- round((n*0.2))
Results[1,2]<-sum((pred1+d2$Fraud)==1)/N
Results[2,2]<-sum((pred2+d2$Fraud)==1)/N
Results[3,2]<-sum((pred3+d2$Fraud)==1)/N
Results[4,2]<-sum((pred4+d2$Fraud)==1)/N
Results[5,2]<-sum((pred5+d2$Fraud)==1)/N
Results[6,2]<-sum((pred6+d2$Fraud)==1)/N
Results[7,2]<-sum((pred7+d2$Fraud)==1)/N
Results[8,2]<-sum((pred8+d2$Fraud)==1)/N
Results[9,2]<-sum((pred9+d2$Fraud)==1)/N
Results[10,2]<-sum((pred10+d2$Fraud)==1)/N

round(Results,4) #Print the table for error

#Hidden Layer 3 is chosen
#Classificiation table ANN3, both testing and training dataset
train <- 1*(ann3$fit>1/2) #round those fitted value, should be 0 or 1
table(train,d1$Fraud) 
table(pred3,d2$Fraud)

#Get Error rate for training data
Training <- matrix(0,nrow=10,ncol=1)
Training[1,1] <-sum((round(ann1$fitted.values)+d1$Fraud)==1)/nrow(d1)
Training[2,1] <-sum((round(ann2$fitted.values)+d1$Fraud)==1)/nrow(d1)
Training[3,1] <-sum((round(ann3$fitted.values)+d1$Fraud)==1)/nrow(d1)
Training[4,1] <-sum((round(ann4$fitted.values)+d1$Fraud)==1)/nrow(d1)
Training[5,1] <-sum((round(ann5$fitted.values)+d1$Fraud)==1)/nrow(d1)
Training[6,1] <-sum((round(ann6$fitted.values)+d1$Fraud)==1)/nrow(d1)
Training[7,1] <-sum((round(ann7$fitted.values)+d1$Fraud)==1)/nrow(d1)
Training[8,1] <-sum((round(ann8$fitted.values)+d1$Fraud)==1)/nrow(d1)
Training[9,1] <-sum((round(ann9$fitted.values)+d1$Fraud)==1)/nrow(d1)
Training[10,1] <-sum((round(ann10$fitted.values)+d1$Fraud)==1)/nrow(d1)

#Show Training Error Rate
round(Training,4)

#Plot for the error rate
par(mar = c(5, 5, 3, 5)) #Set the frame
plot(Training[,1],type="l", lty=2, lwd=2, ylim=c(0,0.3), 
xlab="Hidden Layer", ylab="Error Rate",main="Line Chart for Error and Hidden Layer Size", col="blue") #Plot Training Error Rate
lines(Results[,2],type="l", lty=1, lwd=2, col="green") #Plot Testing Error Rate
par(new = T)
plot(Results[,1], type="l", lty=3, lwd=2,xaxt="n", yaxt="n", xlab="", ylab="", col="red") #Plot Training Error
axis(side=4) #Axis for Training Error
mtext("Training Error", side=4, line=3)
legend("topleft", c("Training Error", "Training Error Rate","Testing Error Rate"), col=c("red","blue","green"),lty=c(3,2,1),cex=0.7)
#Classification tree part

library(rpart)
set.seed(64260) #Set the seed
id <- sample(1:n,size=(n*0.8))  #get the 580 random index for trianing data set
d1 <- d[id,] #Save 580 data into training dataset d
d2 <- d[-id,] #Save 110 data into testing dataset d1
par(mfrow=c(1,1)) #Define the Frame
ctree<-rpart(Fraud~CF_CASH_FROM_OPER+NET_INCOME+SALES_GROWTH+ACCOUNTS_RECEIVABLE_GROWTH+INVENTORY_GROWTH+
             COGS_YR_GROWTH+X3YR_AVG_GROSS_MARGIN+GROSS_MARGIN, data=d1,method="class",control=rpart.control(maxdepth=3))

plot(ctree, asp=1.8)
text(ctree, cex=0.7, use.n=T)
prob <- predict(ctree)
pr <- prob[,1]<0.5
table(pr,d1$Fraud)

prob2 <- predict(ctree,d2)
pr2 <- prob2[,1]<0.5
table(pr2,d2$Fraud)

d1plot <- d1
d1plot[,3] <- d1plot[,3]+1
par(mfrow=c(1,2))
plot(d1plot$NET_INCOME,d1plot$CF_CASH_FROM_OPER,pch=21,bg=c("blue","red")[d1plot$Fraud],
     xlab="Net Income", ylab="Cash Flow from Operation")
abline(v=-1.303)
plot(d1plot$NET_INCOME,d1plot$CF_CASH_FROM_OPER,pch=21,bg=c("blue","red")[d1plot$Fraud], 
     main="x-axes limit: [-20,20], y-axes limit;[-10,50]",xlim=c(-20,20),ylim=c(-10,50), xlab="Net Income", ylab="Cash Flow from Operation")
abline(v=-1.303)


d1plot2 <- d1[d1[,5]>=-1.303,]
d1plot2[,3] <- d1plot2[,3]+1
plot(d1plot2$ACCOUNTS_RECEIVABLE_GROWTH,d1plot2$CF_CASH_FROM_OPER,
     main="The group of Net Income >=1.303",xlab="Accounts Receivable Growth",ylab="Cash Flow from Operation",pch=21,bg=c("blue","red")[d1plot2$Fraud])
abline(h=-0.0252)
abline(v=26.85)
plot(d1plot2$ACCOUNTS_RECEIVABLE_GROWTH,d1plot2$CF_CASH_FROM_OPER, ylim=c(-10,30),
     main="y-axes limit:[=-10,30]",xlab="Accounts Receivable Growth", ylab="Cash Flow from Operation",pch=21,bg=c("blue","red")[d1plot2$Fraud])
abline(h=-0.0252)
abline(v=26.85)

d1plot3 <- d1[d1[,5]< (-1.303),]
# d1plot3 <- d1plot3[d1plot3[,4]<30,]
d1plot3[,3] <- d1plot3[,3]+1
plot(d1plot3$CF_CASH_FROM_OPER,d1plot3$NET_INCOME,main="The group of Net Income < 1.303",xlab="Cash Flow from Operation",ylab="Net Income",pch=21,bg=c("blue","red")[d1plot3$Fraud])
abline(v=-1.057)
abline(h = -2.27)
plot(d1plot3$CF_CASH_FROM_OPER,d1plot3$NET_INCOME,main="x-axes limit: [-5,10], y-axes limit;[-5,0]",xlim=c(-5,10),ylim=c(-5,0),xlab="Cash Flow from Operation",ylab="Net Income",pch=21,bg=c("blue","red")[d1plot3$Fraud])
abline(v=-1.057)
abline(h = -2.27)

#define frame for holding data
df1 <- data.frame(matrix(ncol = 4, nrow = 1000))
x <- c("Error_Rate", "Precision", "Recall","F1_Score")
colnames(df1) <- x

df2 <- data.frame(matrix(ncol = 4, nrow = 2))
x <- c("Error_Rate", "Precision", "Recall","F1_Score")
colnames(df2) <- x

for (i in 1:10000){
  dim(d) # display dimension
  id<-sample(1:376,300) # create random sample
  d1<-d[id,] # select training data
  d2<-d[-id,] # select testing data
  dim(d1) # display dimension of training data
  dim(d2) # display dimension of testing data
  library(rpart) # load rpart library
  ctree<-rpart(Fraud~CF_CASH_FROM_OPER+NET_INCOME+SALES_GROWTH+ACCOUNTS_RECEIVABLE_GROWTH+INVENTORY_GROWTH+COGS_YR_GROWTH+X3YR_AVG_GROSS_MARGIN+GROSS_MARGIN,data=d1,method="class",control=rpart.control(maxdepth=3)) # build a classification tree with 3 levels maximum
  
  prob<-predict(ctree) # return 2 col. of prob.
  pr<-prob[,1]<0.5 # create suitable label
  t1 <- table(pr,d1$Fraud) # classification table
  er <- (t1[1,2]+t1[2,1])/300 # Training Error Rate
  df1[i,1] <- er
  Precision <- t1[2,2]/(t1[2,2]+t1[2,1]) # Precision
  df1[i,2] <- Precision
  Recall <- t1[2,2]/(t1[2,2]+t1[1,2]) # Recall
  df1[i,3] <- Recall
  F1_Score <- 2*(Recall * Precision) / (Recall + Precision) # F1 Score
  df1[i,4] <- F1_Score
  
  prob<-predict(ctree,d2) # prediction for testing dataset
  pr<-prob[,1]<0.5 # create suitable label
  t2 <- table(pr,d2$Fraud) # classification table
  er <- (t2[1,2]+t2[2,1])/76 # Testing Error Rate
  df2[i,1] <- er
  Precision <- t2[2,2]/(t2[2,2]+t2[2,1]) # Precision
  df2[i,2] <- Precision
  Recall <- t2[2,2]/(t2[2,2]+t2[1,2]) # Recall
  df2[i,3] <- Recall
  F1_Score <- 2*(Recall * Precision) / (Recall + Precision) # F1 Score
  df2[i,4] <- F1_Score
}

#kernal density
par(mfrow=c(2,1))
den1 <- density(df1[,4])
plot(den1, main="Kernal Density for Training Dataset")
den2 <- density(df2[,4])
plot(den2, main="Kernal Density for Testing Dataset")

#Show the data
apply(df1,2,min)
apply(df1,2,max)
apply(df2,2,min)
apply(df2,2,max)
