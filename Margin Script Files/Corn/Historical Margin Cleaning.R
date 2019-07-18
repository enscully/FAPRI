# Corn

library(lubridate)
library(stringr)
library(dplyr)
library(plyr)
library(ggplot2)
library(readr)

futuresMarket <- read_csv("C:/Users/ensxvd/Desktop/main-model/Data/Corn_FuturesMarket.csv")

cornString1 = source("cornString1")
cornString2 = source("cornString2")
cornString3 = source("cornString3")

# string = cornString2$value

cleanTextData = function(string, string2){
  
  splitter = c("C-01",
               "C-02",
               "C-03",
               "C-04",
               "C-05",
               "C-06",
               "C-07",
               "C-08",
               "C-09",
               "C-10",
               "C-11",
               "C-12",
               "C-13",
               "C-14",
               "C-15",
               "C-16",
               "C-17",
               "C-18",
               "C-19",
               "C-20",
               "C-21",
               "C-22",
               "C-23",
               "C-24",
               "C-25",
               "C-26",
               "C-27")
  
  
  
  for (i in 1:length(splitter)){
    delim = paste("(?<=", toString(splitter[i]), ")")
    delim = str_replace_all(delim, " ", "")
    string = unlist(strsplit(string, delim, perl=TRUE))
  }
  
  stringDF = str_split(string, " ")
  
  for(i in 1:length(stringDF)){
    stringDF[[i]] = stringDF[[i]][stringDF[[i]] != ""]
    stringDF[[i]] = str_replace_all(stringDF[[i]], "[\r\n]" , "")
  }
  
  stringDFreal = rbind.fill(lapply(stringDF,function(y){as.data.frame(t(y),stringsAsFactors=FALSE)}))
  
  stringDFreal$V1 = mdy(stringDFreal$V1)
  stringDFreal$V5 = as.numeric(as.character(stringDFreal$V5))
  
  return(stringDFreal)
}

corn1 = cleanTextData(cornString1$value, F)
corn2 = cleanTextData(cornString2$value, F)
corn3 = cleanTextData(cornString3$value, F)

corn = rbind(corn1, corn2, corn3)
colnames(corn) = c("Date", "V2", "V3", "V4", "Margin", "Code")

# Remove extra dates
# corn = corn[corn$Date > ymd("2007-01-01"), ]

# data = corn[corn$Date > ymd("2007-01-01"), ]
# numberOfBushels = 5000
# p1 = 0.50
# p2 = 0.70
# p3 = 0.90

predictMargins = function(data, numberOfBushels, p1, p2, p3){
  # Date = Date
  # avg = daily average across all codes
  # NovNc = DecNC price * number of bushels in the sale
  # DecNCscaled = DecNC market price
  # adjPrice = price + margin
  # centsPerBushel = margin / number of bushels
  # Interval = dummy variable for March/ August updates
  # avgCentsPerBushel = average cents per bushel for each interval
  # averagePrice = average price per bushel for each interval
  # pricePlusMargin = price + margin in cents
  # avgPricePlusMargin = average price per bushel of the pricePlusMargin column, by interval
  # P1 = 50th percentile of pricePlusMargin
  # P2 = 70th percentile of pricePlusMargin
  # P3 = 90th percentile of pricePlusMargin
  
  data = arrange(data, Date)
  
  avgMargin = ddply(data, .(Date), summarize, avg = mean(Margin))
  
  NovMultiplied = data.frame(Date = mdy(futuresMarket$Date), DecNC = futuresMarket$DecNC * numberOfBushels)
  
  joinPriceMargins = left_join(avgMargin, NovMultiplied, rm.na = TRUE)
  
  joinPriceMargins = joinPriceMargins[complete.cases(joinPriceMargins), ]
  
  joinPriceMargins$DecNCscaled = joinPriceMargins$DecNC/numberOfBushels
  
  joinPriceMargins$adjPrice = joinPriceMargins$DecNC + joinPriceMargins$avg
  
  joinPriceMargins$centsPerBushel = joinPriceMargins$avg/numberOfBushels
  
  count = 1
  joinPriceMargins = arrange(joinPriceMargins, Date)
  joinPriceMargins$Interval = NA
  joinPriceMargins$Interval[1] = 1
  
  for(row in 2:nrow(joinPriceMargins)){
    if(month(joinPriceMargins$Date[row - 1]) == "8" && month(joinPriceMargins$Date[row]) == "9"){
      count = count + 1
    }
    else if(month(joinPriceMargins$Date[row - 1]) == "2" && month(joinPriceMargins$Date[row]) == "3"){
      count = count + 1
    }
    
    joinPriceMargins$Interval[row] = count
  }
  
  joinPriceMargins$avgCentsPerBushel = NA
  
  intervalRows = list()
  
  for(i in 1:max(joinPriceMargins$Interval)){
    intervalRows[[i]] = which(joinPriceMargins$Interval == i)
  }
  
  joinPriceMargins$avgCentsPerBushel = NA
  joinPriceMargins$averagePrice = NA
  
  for(i in 1:length(intervalRows)){
    joinPriceMargins$avgCentsPerBushel[intervalRows[[i]]] = as.numeric(tapply(joinPriceMargins$centsPerBushel, joinPriceMargins$Interval, mean)[i])
    joinPriceMargins$averagePrice[intervalRows[[i]]] = as.numeric(tapply(joinPriceMargins$DecNCscaled, joinPriceMargins$Interval, mean)[i])
  }
  
  joinPriceMargins$pricePlusMargin = joinPriceMargins$DecNCscaled + joinPriceMargins$centsPerBushel
  joinPriceMargins$avgPricePlusMargin = joinPriceMargins$averagePrice + joinPriceMargins$avgCentsPerBushel
  
  joinPriceMargins$P1 = NA
  joinPriceMargins$P2 = NA
  joinPriceMargins$P3 = NA
  
  for(i in 1:length(intervalRows)){
    joinPriceMargins$P1[intervalRows[[i]]] = sort(joinPriceMargins$pricePlusMargin[intervalRows[[i]]])[p1*length(joinPriceMargins$pricePlusMargin[intervalRows[[i]]])]
    joinPriceMargins$P2[intervalRows[[i]]] = sort(joinPriceMargins$pricePlusMargin[intervalRows[[i]]])[p2*length(joinPriceMargins$pricePlusMargin[intervalRows[[i]]])]
    joinPriceMargins$P3[intervalRows[[i]]] = sort(joinPriceMargins$pricePlusMargin[intervalRows[[i]]])[p3*length(joinPriceMargins$pricePlusMargin[intervalRows[[i]]])]
  }
  
  return(joinPriceMargins)
  
}

plotMe = predictMargins(corn[corn$Date > ymd("2007-01-01"), ], 5000, 0.50, 0.70, 0.90)

# Slow
ggplot(plotMe, aes(x = Date, y = Margin)) +
  geom_point()

# Price plus daily margin / number of bushels. Gives specific margin requirements
ggplot(plotMe, aes(x = Date, y = DecNCscaled)) +
  geom_line() +
  geom_line(y = plotMe$adjPrice/5000, linetype = "longdash", col = "red")

# Displays average margin
ggplot(plotMe, aes(x = Date, y = DecNCscaled)) +
  geom_line() +
  geom_line(y = plotMe$avgPricePlusMargin, linetype = "longdash", col = "red")

# Displays margin percentiles
ggplot(plotMe, aes(x = Date, y = DecNCscaled)) +
  geom_line() +
  geom_line(aes(color = "P1", y = P1), linetype = "longdash") +
  geom_line(aes(color = "P2", y = P2), linetype = "longdash") +
  geom_line(aes(color = "P3", y = P3), linetype = "longdash") +
  labs(x = "Date", y = "Price Per Bushel") +
  scale_colour_manual(name = '',
                      values =c("P1" = "red", "P2" = "forestgreen", "P3" = "blue"),
                      labels = c('50th Percentile','70th Percentile','90th Percentile'))


