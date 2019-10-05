
cleandata <- function(d,raw){
cleand <- read.csv(raw)

clean4 <- cleand[!cleand[,4]=="#N/A N/A",] #Cashflow
clean5 <- cleand[!cleand[,5]=="#N/A N/A",] #Net Income
clean6 <- cleand[!cleand[,6]=="#N/A N/A",] #Sales Growth
# clean7 <- cleand[!cleand[,7]=="#N/A N/A",] #Receivable Growth
# clean8 <- cleand[!cleand[,8]=="#N/A N/A",] #Inventory Growth
# clean9 <- cleand[!cleand[,9]=="#N/A N/A",] #COGS Growth
clean10 <- cleand[!cleand[,10]=="#N/A N/A",] #3YR Average Gross Margin
clean11 <- cleand[!cleand[,11]=="#N/A N/A",] #Gross Margin

num4 <- as.numeric(clean4[,4])
m4 <- mean(num4)
sd4 <- var(num4) ^ (1/2)
num5 <- as.numeric(clean5[,5])
m5 <- mean(num5)
sd5 <- var(num5) ^ (1/2)
num6 <- as.numeric(clean6[,6])
m6 <- mean(num6)
sd6 <- var(num6) ^ (1/2)
# num7 <- as.numeric(clean7[,7])
# m7 <- mean(num7)
# sd7 <- var(num7) ^ (1/2)
# num8 <- as.numeric(clean8[,8])
# m8 <- mean(num8)
# sd8 <- var(num8) ^ (1/2)
# num9 <- as.numeric(clean9[,9])
# m9 <- mean(num9)
# sd9 <- var(num9) ^ (1/2)
num10 <- as.numeric(clean10[,10])
m10 <- mean(num10)
sd10 <- var(num10) ^ (1/2)
num11 <- as.numeric(clean11[,11])
m11 <- mean(num11)
sd11 <- var(num11) ^ (1/2)

d[,4] <- (d[,4]-m4)/sd4
d[,5] <- (d[,5]-m5)/sd5
d[,6] <- (d[,6]-m6)/sd6
# d[,7] <- (d[,7]-m7)/sd7
# d[,8] <- (d[,8]-m8)/sd8
# d[,9] <- (d[,9]-m9)/sd9
d[,10] <- (d[,10]-m10)/sd10
d[,11] <- (d[,11]-m11)/sd11

d <-d[d[,7]<2000,]
d <-d[d[,8]<2000,]

d
}
