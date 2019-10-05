#RMSC4002 HW3 
#Leung Cheuk Wai Dominic 1155093086
#setwd("~/QFRM/RMSC4002/Assignment3") #For my own use only

#1a
d <- read.csv("credit.csv")
set.seed(980209) #set seed, my birth date is 9th Feb 1998
n <- nrow(d) #Get the length of the dataset
n #display length
id <- sample(1:n,size=580)  #get the 580 random index for trianing data set
head(id) #display id
d1 <- d[id,] #Save 580 data into training dataset d
dim(d1) #check dimension 
d2 <- d[-id,] #Save 110 data into testing dataset d1
dim(d2) #check dimension

#1b
library(rpart) #import library RPART
names(d) #Show the variables of credit.csv
#Run the classification tree, added option control=rpart.control(maxdepth=3)
ctree <- rpart(Result~Age+Address+Employ+Bank+House+Save,data=d1,method="class",control=rpart.control(maxdepth = 3)) 

#1c
plot(ctree,asp=4,main="Credit") #Plot the branch of the tree
text(ctree,use.n=T,cex=0.6) #Add text to the tree
print(ctree) #Display the nodes
sum(d1["Result"]) #total of 1, accepted case
nrow(d1) - sum(d1["Result"]) #total of 0, rejected case

# For the person to be rejected, either rules below are satisfied:
#   1. If Bank<2.5 and Employ<1.27, then the person is rejected.
       # Support=(216+49)/580=0.4569, Confidence=216/265=0.8151, Capture=216/322=0.6708
#   2. If Bank<2.5 and Employ>1.27 and Save<229.5, then the person is rejected.
       # Support=(78+49)/580=0.2190, Confidence=78/127=0.6142, Capture=78/322=0.2422
#   3. Bank>2.5 and Address<1.438 and Age<24.91, then the person is rejected.
       # Support=(9+2)/580=0.0190, Confidence=9/11=0.8181, Capture=9/322=0.0280
# For the person to be accepted, either rules below are satisfied:
#   1. Bank<2.5 and Employ>1.27 and Save >229.5, then the person is accepted.
       # Support=(29+7)/580=0.0621, Confidence=29/36=0.8056, Capture=29/258=0.1124
#   2. Bank>2.5 and Address<1.438 and Age>24.91, then the person is accepted.
       # Support=(18+4)/580=0.0379, Confidence=18/22=0.8181, Capture=18/258=0.0698
#   3. Bank>2.5 and Address>1.438, then the person is accepted. 
       # Support=(111+8)/580=0.205, Confidence=111/119=0.9328, Capture=111/258=0.4302

#1d
pr <- predict(ctree) #Get probability of the sample
head(pr)
c1 <- max.col(pr) #Classify the sample with the larger probability; 1:rejected, 2:accepted
head(c1)
table(c1,d1$Result) #Display the classification table
#The Error rate = (100+19)/580=20.52%

#1e
pr2 <- predict(ctree,d2) #Get probability of testing data set
head(pr2)
c2 <- max.col(pr2) #Classify the sample with the larger probability
head(c2)
table(c2,d2$Result) #Display the classification table
#The Error rate = (21+4)/110=22.73%
# Precision=28/(28+4)=85.5%
# Recall=28/(28+21)=57.14%
# F1 Score = 2/(1/Precision+1/Recall) = 69.13%
