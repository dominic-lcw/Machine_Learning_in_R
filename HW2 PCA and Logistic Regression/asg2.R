#Leung Cheuk Wai Dominic 1155093086
#Assignment 2 Q2
#setwd("/Users/leung/Documents/QFRM/RMSC4002/Assignment2") #For my own use

#Q2a
data <- read.csv("Pokemon.csv") #Read the Data
names(data)
pokestat <- data[,5:10] #Read column E to column J for "HP, Attack, Defence, Special Attack, Special Defensive Strength and Speed)

pca <- princomp(pokestat,cor=T) #Do principal component analysis
names(pca) #Show things stored in PCA

pca$loadings[,1:2] #Show the two PC

pc1 <- pca$loadings[,1] #store the first PC
pc2 <- pca$loadings[,2] #store the Second PC

#Plot the Two PC
plot(pc1, ylim=c(-1,1), type="o",main="1st PC") #plot the 1st PC, y axis range:[-1,1]
plot(pc2, ylim=c(-1,1), type="o",main="2nd PC") #plot the 2nd PC, y axis range:[-1,1]


# Q2b
#For the 1st PC, we see that the loadings are all positive and their values are within the range [0.3,0.5]. 
# This PC is proportional to the average stat of the pokemon, with approximately similar weighting to those stat.
# I would interpret this PC as the quality of the pokemon, 
# because basicly there are pokemon that are stronger with higher average stat and there are weaker pokemon with lower average stat
# 
# For the 2nd PC, the loadings are positive with defensive stat(HP, Defense, SP. Def) but negative with attacking stat(Attack, Sp. Atk, Speed).
# This PC can show the difference between defensive stat and attacking stat.
# This PC can be interpretted as the measure of the agreesiveness and defensiveness of the pokemon, and it shows that a pokemon is either inclinded to the attacking side or defensive side.

# Q2c
s <- pca$sdev #Get the Standard Deviation of all PCs
pcavar <- cumsum(s^2/6) #Show the cumulative variance inherited to the PCs (Correlation matrix is used so the sum of the diagonal elements is 6)
pcavar #Display var

#As we can see the cumulative sum of the variance up to the 2nd PC is only 0.6342. 
# Conceptually, as we can only inherit 63.42% variance, it means the information obtianed is not very complete (~63%).
# Hence, if we want to give a better estimate, we need to include more PCs as we can preserve more information by including more variance of the original data.

#Aissgnment2 Q3
#3(a)
d <- read.csv("credit.csv")
names(d)

#3(b)
set.seed(93086)  #random seed, last 5-digit of my SID 
id <- sample(1:690,size=600)
d1 <- d[id,]
d2 <-d[-id,]

#3(c)
# The model selection method is backward elimination
summary(glm(Result~Age+Address+Employ+Bank+House+Save, data=d1, binomial)) #First logistic regression
summary(glm(Result~Address+Employ+Bank+House+Save, data=d1, binomial)) #Removed Age, which has the largest p-value
summary(glm(Result~Address+Employ+Bank+Save, data=d1, binomial)) #Removed House, which has the largest p-value
summary(glm(Result~Employ+Bank+Save, data=d1, binomial)) #Removed Address, p-value > 0.01 

lreg <- glm(Result~Employ+Bank+Save, data=d1, binomial) #BSave the result to #lreg
names(lreg) #display the items in lreg
pr <- (lreg$fitted.values>0.5)  #Divide the data with threshold 0.5 for the fitted value
table(pr,d1$Result) #Display the result 

#3(d)
test <- predict(lreg,d2) #Return the value for x'b
pt <- exp(test)/(1+exp(test)) #Calculate the probability from the log-odd ratio
testpr <- (pt>0.5)+1 #Prediction. 1=False, 2=True
table(testpr,d2$Result) #Display the result

#3(e)
# Training dataset:
#   Precision = (157)/(36+157) = 81.3%
#   Recall = (157)/(157+112) = 58.7%
#   F1 score = 2 / (1/Precision + 1/Recall) = 67.9%
# Testing dataset:
#   Precision = 24/(24+1) = 96%
#   Recall = 24/(24+14) = 63.2%
#   F1 score = 2 / (1/Precision + 1/Recall) = 76.2%
# Precison, Recall and F1 score for the testing dataset are higher than the training dataset. 
# The accuracy is higher in testing dataset.

#3(f)
ysort1 <- d1$Result[order(lreg$fitted.values,decreasing=T)] #sort the training dataset with the fitted value with lreg$fittedvalue
n1 = nrow(d1) #Get the length for the training dataset
perc1 <- cumsum(ysort1)/sum(ysort1) #cumulative percentage for the training dataset
pop1 <- (1:n1)/n1 # The x-coordinate
plot(pop1,perc1, type="l", col="blue",main="Lift Chart for 3(f) and 3(g)", ylab="perc", xlab="pop") #plot for the training dataset

yideal1<- c(rep(1,sum(d1$Result)),rep(0,n1-sum(d1$Result)))
perc1_ideal <- cumsum(yideal1)/sum(yideal1) #cumulative percentage for the ideal case
lines(pop1,perc1_ideal, type="l", col="red")

lines(pop1,pop1) #The reference line

#3(g)
ysort2 <- d2$Result[order(pt,decreasing=T)] #pt is the calculated probability as found in 3d
n2 = nrow(d2) #Get the length of d2
perc2 <- cumsum(ysort2)/sum(ysort2) #cumulative percentage for the testing dataset
pop2 <- (1:n2)/n2 #The x-coordinate
lines(pop2,perc2, type="l", col="green") #Plot for the testing dataset

yideal2 <- c(rep(1, sum(d2$Result)),rep(0,n2-sum(d2$Result)))#The ideal csae for the testing dataset
perc2_ideal <-cumsum(yideal2)/sum(yideal2) #cumulative precerntage for the ideal case of testing dataset
lines(pop2,perc2_ideal, type="l", col="purple") #Plot the lies of the ideal case

text(0.4,0.78,cex=0.7,"Testing") #Add label on the graph
text(0.35,0.5,cex=0.7,"Training") #Add label on the graph

sum(d1$Result)/n1 #ideal case1 
abline(v=sum(d1$Result)/n1, lty=3) #clarify ideal case1

sum(d2$Result)/n2 #ideal case2
abline(v=sum(d2$Result)/n2, lty=3) #clarify ideal case2

#3(h) comparison
# As we can see that there is more space between the ideal line with the training dataset line, so there is a higher misfit in training dataset compared to the testing dataset.
# The accuracy of the testing dataset is higher.
