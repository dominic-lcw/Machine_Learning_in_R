#RMSC4002 HW3 2
#Leung Cheuk Wai Dominic 1155093086
#setwd("~/QFRM/RMSC4002/Assignment3") #For my own use only

#from 1a
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


#2a
library(nnet) #Import library nnet
#set up the ann function for repeating ANN to get the best solution
ann <- function(x,y,size,maxit,linout,try){ #Define parameter for this function 
  ann1 <- nnet(y~.,data=x,size=size,maxit=maxit,linout=linout) #the first nnet result
  
  v1 <- ann1$value # v1 stores error the first nnet
  for (i in 2:try) { #start a for loop
    ann2 <- nnet(y~., data=x,size=size,maxit=maxit,linout=linout)# get another nnet result
    if (ann2$value <v1){ #compare for a smaller error
      v1<-ann2$value #replace if the error is smaller
      ann1<-ann2 #replace the final one with new neural network
    }
  }
  ann1# return the result
}
head(d1) #show data
ann7 <- ann(d1[,1:6],d1[,7],size=7,maxit=500,linout=T,try=25) #run size=7 ann

#2b
ann8 <- ann(d1[,1:6],d1[,7],size=8,maxit=500,linout=T,try=25) #run size=8 ann
ann9 <- ann(d1[,1:6],d1[,7],size=9,maxit=500,linout=T,try=25) #run size=9 ann
ann10 <- ann(d1[,1:6],d1[,7],size=10,maxit=500,linout=T,try=25) #run size=10 ann

#2c
ann7$value #display the error
ann8$value #display the error
ann9$value #display the error
ann10$value #display the error
#ann8 has the smallest error, ann8 is chosen for this model
pred <- 1*(ann8$fit>1/2) #round those fitted value, should be 0 or 1
table(pred,d1$Result) #display the table
#The error rate = (60+39)/580=17.06%

#2d
pred2 <- predict(ann8,d2) #use predict() the use the weight of ann8 to compute the fitted value for d2
pred2 <- 1*(pred2>1/2) #round those fitted value, should be 0 or 1
table(pred2,d2$Result) #Display the result
#The error rate = (13+8)/110=19.09%
#Precision = 36/(36+8)=81.82%
#Recall = 36/(36+13)=73.47%
#F1 Score = 2/(1/Precision+1/Recall)=77.42%

#2e
# Result for testing data set
# ctree: Error rate = 22.73%. F1 score=69.13%
# ann:   Error rate = 19.09%. F1 score = 77.42%  
# Using ANN can obtain a higher F1 score and smaller error, which means we can have a more accurate classification.
# It agress with our intuition that ANN is better, because it has hidden layer to recognize and "memorize" more complex pattern.

