d<-read.csv("mainversion2.csv") # read in data
names(d) # display variables
s <- d[,4:5]
s <- scale(s)
apply(s, 2, mean)
apply(s, 2, sd)
Fraud <- d[,3]
d <- cbind(s,d[,6:11],Fraud)
names(d)

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

apply(df1,2,min)
apply(df1,2,max)
apply(df2,2,min)
apply(df2,2,max)