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