library(Quandl)
library(lubridate)
library(dplyr)




# Example CME_C1
# The number following C, in this case 1, indicates the contract is
# first nearest to delivery. CM8 would be the contract 8th nearest 
# to delivery





futures = read.csv("Data/Corn_FuturesMarket.csv")
nearby = data.frame(Date = mdy(futures$Date), Nearby = futures$NearbyOC)
dec = data.frame(Date = mdy(futures$Date), Nearby = futures$DecNC)

###############################################################
#NEARBY CONTRACTS

mydata1 = Quandl("CHRIS/CME_C1")


result1 <- left_join(nearby, mydata1, by = c("Date"))
result1$Settle = result1$Settle * 0.01
result1$difference = result1$Nearby - result1$Settle

#Number of rows off by over $0.01
length(which(abs(result1$difference) > 0.01))
19/2917


result1[which(abs(result1$difference) > 0.01),]

###############################################################
#???????????????????????????????????????

mydata2 = Quandl("CHRIS/CME_C2")

result2 <- left_join(nearby, mydata2, by = c("Date"))
result2$Settle = result2$Settle * 0.01
result2$difference = result2$Nearby - result2$Settle

#Number of rows off by over $0.01
length(which(abs(result2$difference) > 0.01))
2857/2917


result2 <- left_join(dec, mydata2, by = c("Date"))
result2$Settle = result2$Settle * 0.01
result2$difference = result2$Nearby - result2$Settle

#Number of rows off by over $0.01
length(which(abs(result2$difference) > 0.01))
2380/2917

###############################################################
#??????????????????????????????????????

mydata3 = Quandl("CHRIS/CME_C3")

result3 <- left_join(nearby, mydata3, by = c("Date"))
result3$Settle = result3$Settle * 0.01
result3$difference = result3$Nearby - result3$Settle

#Number of rows off by over $0.01
length(which(abs(result3$difference) > 0.01))
2856/2917


###############################################################
#??????????????????????????????????????

mydata4 = Quandl("CHRIS/CME_C4")

result4 <- left_join(nearby, mydata4, by = c("Date"))
result4$Settle = result4$Settle * 0.01
result4$difference = result4$Nearby - result4$Settle

#Number of rows off by over $0.01
length(which(abs(result4$difference) > 0.01))
2886/2917




###############################################################
#??????????????????????????????????????

mydata5 = Quandl("CHRIS/CME_C5")

result5 <- left_join(nearby, mydata5, by = c("Date"))
result5$Settle = result5$Settle * 0.01
result5$difference = result5$Nearby - result5$Settle

#Number of rows off by over $0.01
length(which(abs(result5$difference) > 0.01))
2896/2917

###############################################################
#??????????????????????????????????????

mydata6 = Quandl("CHRIS/CME_C6")

result6 <- left_join(nearby, mydata6, by = c("Date"))
result6$Settle = result6$Settle * 0.01
result6$difference = result6$Nearby - result6$Settle

#Number of rows off by over $0.01
length(which(abs(result6$difference) > 0.01))
2871/2917

###############################################################
#??????????????????????????????????????

mydata7 = Quandl("CHRIS/CME_C7")

result7 <- left_join(nearby, mydata7, by = c("Date"))
result7$Settle = result7$Settle * 0.01
result7$difference = result7$Nearby - result7$Settle

#Number of rows off by over $0.01
length(which(abs(result7$difference) > 0.01))
870/2917


###############################################################
#??????????????????????????????????????

mydata8 = Quandl("CHRIS/CME_C8")

result8 <- left_join(nearby, mydata8, by = c("Date"))
result8$Settle = result8$Settle * 0.01
result8$difference = result8$Nearby - result8$Settle

#Number of rows off by over $0.01
length(which(abs(result8$difference) > 0.01))
687/2917

###############################################################
#??????????????????????????????????????

mydata9 = Quandl("CHRIS/CME_C9")

result9 <- left_join(nearby, mydata9, by = c("Date"))
result9$Settle = result9$Settle * 0.01
result9$difference = result9$Nearby - result9$Settle

#Number of rows off by over $0.01
length(which(abs(result9$difference) > 0.01))
483/2917

###############################################################
#??????????????????????????????????????

mydata10 = Quandl("CHRIS/CME_C10")

result10 <- left_join(nearby, mydata10, by = c("Date"))
result10$Settle = result10$Settle * 0.01
result10$difference = result10$Nearby - result10$Settle

#Number of rows off by over $0.01
length(which(abs(result10$difference) > 0.01))
271/2917
















