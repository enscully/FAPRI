# Storage graphing equilibrium


library(ggplot2)
library(dplyr)
library(plyr)
library(lubridate)
library(tidyr)


# Interest Rate
interestRate = 0.055
# Monthly Commercial Storage Cost
monthlyCommCost = 0.05
# Three Month Minimum Storage Charge
TMMStorageCharge = 0.15
# Total cost of bin storage, 1st month
binStorageFirst = 0.2469625
# Total cost of bin storage(exlcuding interest), 2nd month forward
binStorageAfter = 0.0032009009009009

prices = seq(from = 7, to = 20, length.out = 420)

monthsAfterOct = seq(from = 1, to = 12, by = 1)


onFarm = data.frame()

for(j in monthsAfterOct){
  for(i in 1:length(prices)){
    
    cost = (prices[i]*((1+(interestRate/12)^monthsAfterOct[j])) + binStorageFirst + (binStorageAfter*(monthsAfterOct[j] - 1))) - prices[i]
    onFarm[i, j] = prices[i] - cost
    
  }
}

onFarm$rows = row.names(onFarm)
onFarm$priceBeforeStorage = prices


ggplot(data = onFarm, aes(x = as.numeric(row.names(onFarm)))) + 
  geom_line(aes(y = V1)) + 
  geom_line(aes(y = V2)) +
  geom_line(aes(y = V3)) +
  geom_line(aes(y = V4)) +
  geom_line(aes(y = V5)) +
  geom_line(aes(y = V6)) +
  geom_line(aes(y = V7)) +
  geom_line(aes(y = V8)) +
  geom_line(aes(y = V9)) +
  geom_line(aes(y = V10)) +
  geom_line(aes(y = V11)) +
  geom_line(aes(y = V12)) +
  labs(y = "price")



onFarmGather = onFarm %>% gather("id", "value",  1:12)



p = ggplot(data = onFarmGather, aes(x=priceBeforeStorage, y=value, group = id, colour=id)) +
  geom_line()

library(plotly)


ggplotly(p)













# COMMERCIAL

prices = seq(from = 7, to = 20, length.out = 420)

commercial = data.frame()
for(j in monthsAfterOct){
  # Commercial Storage
  for(i in 1:length(prices)) {
    # Calculate first part of storage function
    A = prices[i] * (1 + (interestRate/12)) ^ (monthsAfterOct[j])
    # Check if cost is less than three month minimum storage cost
    if((monthlyCommCost * monthsAfterOct[j]) < TMMStorageCharge) {
      B = TMMStorageCharge
    }
    else {
      B = monthlyCommCost * (monthsAfterOct[j])
    }
    
    # Compute commercial storage cost
    cost = (A + B) - prices[i]
    
    commercial[i, j] = prices[i] - cost
  }
}

commercial$rows = row.names(commercial)
commercial$priceBeforeStorage = prices


commercialGather = commercial %>% gather("id", "value",  1:12)



c = ggplot(data = commercialGather, aes(x=priceBeforeStorage, y=value, group = id, colour=id)) +
  geom_line()



ggplotly(c)


#######################################################################

Soybean_FuturesMarket = read.csv("Data/Soybean_FuturesMarket.csv", stringsAsFactors = FALSE)

marketingYear = Soybean_FuturesMarket[421:840,1:2]

dates = mdy(marketingYear$Date)
commercialGather$Dates = dates


ggplot(data = commercialGather, aes(x=Dates, y=value, group = id)) +
  geom_line() + 
  geom_line(size = 0.5, data = marketingYear, aes(x=mdy(Date), y=NearbyOC, group = 1))




ggplot(data = marketingYear, aes(x=mdy(Date), y=NearbyOC, group = 1)) + 
  geom_line(size = 0.5)




nearestPrice = vector()
for(i in 1:nrow(marketingYear)){
  nearestPrice[i] = (which(abs((commercialGather$priceBeforeStorage - marketingYear$NearbyOC[i])) == 
                             min(abs(commercialGather$priceBeforeStorage - marketingYear$NearbyOC[i]))))[1]
}




for(i in 1:length(nearestPrice)){
  rows = (which(nearestPrice[i] == commercialGather$rows))
  marketingYear[i,3:14] = commercialGather$value[rows]
}



ggplot(data = marketingYear, aes(x=mdy(Date), y=NearbyOC, group = 1)) +
  geom_line(size = 0.5, col = "green") + 
  geom_line(aes(y = V13), size = 0.25) + 
  geom_line(aes(y = V14), size = 0.25) +
  geom_line(aes(y = V3), size = 0.25) +
  geom_line(aes(y = V4), size = 0.25) +
  geom_line(aes(y = V5), size = 0.25) +
  geom_line(aes(y = V6), size = 0.25) +
  geom_line(aes(y = V7), size = 0.25) +
  geom_line(aes(y = V8), size = 0.25) +
  geom_line(aes(y = V9), size = 0.25) +
  geom_line(aes(y = V10), size = 0.25) +
  geom_line(aes(y = V11), size = 0.25) +
  geom_line(aes(y = V12), size = 0.25) +
  labs(y = "price")




########################################################################################



Soybean_FuturesMarket = read.csv("Data/Soybean_FuturesMarket.csv", stringsAsFactors = FALSE)

marketingYear = Soybean_FuturesMarket[200:619,1:2]

dates = mdy(marketingYear$Date)
onFarmGather$Dates = NA
onFarmGather$Dates = dates


ggplot(data = onFarmGather, aes(x=Dates, y=value, group = id)) +
  geom_line() + 
  geom_line(size = 0.5, data = marketingYear, aes(x=mdy(Date), y=NearbyOC, group = 1))




ggplot(data = marketingYear, aes(x=mdy(Date), y=NearbyOC, group = 1)) + 
  geom_line(size = 0.5)




nearestPrice = vector()
for(i in 1:nrow(marketingYear)){
  nearestPrice[i] = (which(abs((onFarmGather$priceBeforeStorage - marketingYear$NearbyOC[i])) == 
                             min(abs(onFarmGather$priceBeforeStorage - marketingYear$NearbyOC[i]))))[1]
}




for(i in 1:length(nearestPrice)){
  rows = (which(nearestPrice[i] == onFarmGather$rows))
  marketingYear[i,3:14] = onFarmGather$value[rows]
}



ggplot(data = marketingYear, aes(x=mdy(Date), y=NearbyOC, group = 1)) +
  geom_line(size = 0.5, col = "green") + 
  geom_line(aes(y = V13), size = 0.25) + 
  geom_line(aes(y = V14), size = 0.25) +
  geom_line(aes(y = V3), size = 0.25) +
  geom_line(aes(y = V4), size = 0.25) +
  geom_line(aes(y = V5), size = 0.25) +
  geom_line(aes(y = V6), size = 0.25) +
  geom_line(aes(y = V7), size = 0.25) +
  geom_line(aes(y = V8), size = 0.25) +
  geom_line(aes(y = V9), size = 0.25) +
  geom_line(aes(y = V10), size = 0.25) +
  geom_line(aes(y = V11), size = 0.25) +
  geom_line(aes(y = V12), size = 0.25) +
  labs(y = "price")









