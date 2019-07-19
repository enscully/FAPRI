# Soybeans

library(lubridate)
library(stringr)
library(dplyr)
library(plyr)
library(ggplot2)
library(readr)



futuresMarket <- read_csv("C:/Users/ensxvd/Desktop/main-model/Data/Soybean_FuturesMarket.csv")

soybeanString1 = source("Margin Script Files/Soybean/soybeanString1")
soybeanString2 = source("Margin Script Files/Soybean/soybeanString2")
soybeanString3 = source("Margin Script Files/Soybean/soybeanString3")

# string = soybeanString2$value

cleanTextData = function(string, string2){
  
  splitter = c("S-01",
               "S-02",
               "S-03",
               "S-04",
               "S-05",
               "S-06",
               "S-07",
               "S-08",
               "S-09",
               "S-10",
               "S-11",
               "S-12",
               "S-13",
               "S-14",
               "S-15",
               "S-16",
               "S-17",
               "S-18",
               "S-19",
               "S-20",
               "S-21",
               "S-22",
               "S-23",
               "S-24",
               "S-25",
               "S-26",
               "S-27")
  
  
  
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

soybean1 = cleanTextData(soybeanString1$value, F)
soybean2 = cleanTextData(soybeanString2$value, F)
soybean3 = cleanTextData(soybeanString3$value, F)

soybean = rbind(soybean1, soybean2, soybean3)
colnames(soybean) = c("Date", "V2", "V3", "V4", "Margin", "Code")

# Remove extra dates
# soybean = soybean[soybean$Date > ymd("2007-01-01"), ]

# data = soybean[soybean$Date > ymd("2007-01-01"), ]
# numberOfBushels = 5000
# p1 = 0.50
# p2 = 0.70
# p3 = 0.90

predictMargins = function(data, numberOfBushels, p1, p2, p3){
  # Date = Date
  # avg = daily average across all codes
  # NovNc = NovNC price * number of bushels in the sale
  # NovNCscaled = NovNC market price
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
  
  NovMultiplied = data.frame(Date = mdy(futuresMarket$Date), NovNC = futuresMarket$NovNC * numberOfBushels)
  
  joinPriceMargins = left_join(avgMargin, NovMultiplied, rm.na = TRUE)
  
  joinPriceMargins = joinPriceMargins[complete.cases(joinPriceMargins), ]
  
  joinPriceMargins$NovNCscaled = joinPriceMargins$NovNC/numberOfBushels
  
  joinPriceMargins$adjPrice = joinPriceMargins$NovNC + joinPriceMargins$avg
  
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
    joinPriceMargins$averagePrice[intervalRows[[i]]] = as.numeric(tapply(joinPriceMargins$NovNCscaled, joinPriceMargins$Interval, mean)[i])
  }
  
  joinPriceMargins$pricePlusMargin = joinPriceMargins$NovNCscaled + joinPriceMargins$centsPerBushel
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

plotMe = predictMargins(soybean[soybean$Date > ymd("2007-01-01"), ], 5000, 0.50, 0.70, 0.90)

# # Slow
# ggplot(plotMe, aes(x = Date, y = Margin)) +
#   geom_point()
# 
# # Price plus daily margin / number of bushels. Gives specific margin requirements
# ggplot(plotMe, aes(x = Date, y = NovNCscaled)) +
#   geom_line() +
#   geom_line(y = plotMe$adjPrice/5000, linetype = "longdash", col = "red")
# 
# # Displays average margin
# ggplot(plotMe, aes(x = Date, y = NovNCscaled)) +
#   geom_line() +
#   geom_line(y = plotMe$avgPricePlusMargin, linetype = "longdash", col = "red")
# 
# # Displays margin percentiles
# ggplot(plotMe, aes(x = Date, y = NovNCscaled)) +
#   geom_line() +
#   geom_line(aes(color = "P1", y = P1), linetype = "longdash") +
#   geom_line(aes(color = "P2", y = P2), linetype = "longdash") +
#   geom_line(aes(color = "P3", y = P3), linetype = "longdash") +
#   labs(x = "Date", y = "Price Per Bushel") +
#   scale_colour_manual(name = '',
#                       values =c("P1" = "red", "P2" = "forestgreen", "P3" = "blue"),
#                       labels = c('50th Percentile','70th Percentile','90th Percentile'))


###############################################################################################################

# Calculating what magin would have been PO Base, non-multi-year

# January
# March
# May
# July
# August
# September
# November

appObjectsSoybean = readRDS("C:/Users/ensxvd/Desktop/main-model/Application/appObjectsSoybeanBase.rds")
Soybean_CropYearObjectsBase = appObjectsSoybean[[1]]
Soybean_CropYearsBase = appObjectsSoybean[[2]]
finalizedPriceObjectSoybeanBase = appObjectsSoybean[[3]]


#data

POSales = data.frame(matrix(ncol = 7, NA))
POSales = data.frame()

# for(i in 1: length(Soybean_CropYearObjectsBase)){
#   colNames = colnames(Soybean_CropYearObjectsBase[[i]][["Sales Summary"]][,-1])
#   salesOneYear = cbind(t(Soybean_CropYearObjectsBase[[i]][["Sales Summary"]][,-1]), Date = colNames)
#   POSales = rbind(POSales, salesOneYear)
# 
# }

colNames = colnames(Soybean_CropYearObjectsBase[[1]][["Sales Summary"]][,-1])
salesOneYear = cbind(t(Soybean_CropYearObjectsBase[[1]][["Sales Summary"]][,-1]), Date = colNames)
POSales = rbind(POSales, salesOneYear)

row.names(POSales) = 1:nrow(POSales)

splitRows = which(grepl("Split1", POSales$Date) == TRUE)
POSales$Date[splitRows] = str_remove(POSales$Date[splitRows], "Split1")

colnames(POSales) = c(Soybean_CropYearObjectsBase[[1]][["Sales Summary"]][,1], "Date")

POSales$Date = mdy(POSales$Date)

marketingYear = Soybean_CropYearObjectsBase[[1]]$`Marketing Year`


novCheck = mdy(paste("11-15", toString(year(mdy(marketingYear$Date[1]))), sep="-"))
janCheck = mdy(paste("01-15", toString(year(mdy(marketingYear$Date[1]))), sep="-"))
marCheck = mdy(paste("03-15", toString(year(mdy(marketingYear$Date[1]))), sep="-"))
mayCheck = mdy(paste("05-15", toString(year(mdy(marketingYear$Date[1]))), sep="-"))
julCheck = mdy(paste("06-15", toString(year(mdy(marketingYear$Date[1]))), sep="-"))
junCheck = mdy(paste("07-15", toString(year(mdy(marketingYear$Date[1]))), sep="-"))

November = mdy(marketingYear$Date[which(abs(mdy(marketingYear$Date) - novCheck) == min(abs(mdy(marketingYear$Date) - novCheck)))])
January = mdy(marketingYear$Date[which(abs(mdy(marketingYear$Date) - janCheck) == min(abs(mdy(marketingYear$Date) - janCheck)))])
March = mdy(marketingYear$Date[which(abs(mdy(marketingYear$Date) - marCheck) == min(abs(mdy(marketingYear$Date) - marCheck)))])
May = mdy(marketingYear$Date[which(abs(mdy(marketingYear$Date) - mayCheck) == min(abs(mdy(marketingYear$Date) - mayCheck)))])
July = mdy(marketingYear$Date[which(abs(mdy(marketingYear$Date) - julCheck) == min(abs(mdy(marketingYear$Date) - julCheck)))])
August = mdy(marketingYear$Date[which(abs(mdy(marketingYear$Date) - junCheck) == min(abs(mdy(marketingYear$Date) - junCheck)))])



interval1 = interval(mdy(marketingYear$Date[1]), November - 1)
interval2 = interval(November, January - 1)
interval3 = interval(January, March - 1)
interval4 = interval(March, May - 1)
interval5 = interval(May, July - 1)
interval6 = interval(July, mdy(last(marketingYear$Date)))




marketingYear$Price[which(marketingYear$Price == max(marketingYear$Price))]













#













###############################################################################################################

# RANDOM OLD CODE

###############################################################################################################





# ggplot(soybean, aes(x = Code, y = Margin)) +
#   geom_boxplot(aes(color = Code))
# 
# # View freqency of each code
# plot(count(soybean$Code)$freq)


# # Slow
# ggplot(soybean, aes(x = Date, y = Margin)) +
#   geom_point(aes(color = Code))




# firstCode = soybean[which(soybean$Code == "S-01"),]
# secondCode = soybean[which(soybean$Code == "S-02"),]
# lastCode = soybean[which(soybean$Code == "S-10"),]
# 
# codes = rbind(firstCode, lastCode)
# 
# 
# ggplotly(ggplot(codes, aes(x = Date, y = Margin)) +
#            geom_point(aes(color = Code)))
# 
# ggplotly(ggplot(soybean, aes(x = Date, y = Margin)) +
#            geom_point(aes(color = Code)))
# 
# 
# # Find percentile ranks
# sort(firstCode$Margin)[0.50*length(firstCode$Margin)]
# sort(firstCode$Margin)[0.70*length(firstCode$Margin)]
# sort(firstCode$Margin)[0.90*length(firstCode$Margin)]
# 
# # Plot overall percentile ranks
# ggplot(firstCode, aes(x = Date, y = Margin)) +
#   geom_point(aes(color = Code)) + 
#   geom_line(y = 2300, linetype = "dashed") + 
#   geom_line(y = 2600, linetype = "dashed") + 
#   geom_line(y = 3500, linetype = "dashed")





###############################################################################################################


data = soybean
# data = firstCode
p1 = 0.50
p2 = 0.75
p3 = 0.95
numberOfBushels = 5000

graphRanks = function(data, p1, p2, p3, numberOfBushels){
  count = 1
  data = arrange(data, Date)
  data$Interval = NA
  data$Interval[1] = 1
  
  for(row in 2:nrow(data)){
    if(month(data$Date[row - 1]) == "8" && month(data$Date[row]) == "9"){
      count = count + 1
    }
    else if(month(data$Date[row - 1]) == "2" && month(data$Date[row]) == "3"){
      count = count + 1
    }
    
    data$Interval[row] = count
  }
  
  data$P1 = NA
  data$P2 = NA
  data$P3 = NA
  
  intervalRows = list()
  
  for(i in 1:max(data$Interval)){
    intervalRows[[i]] = which(data$Interval == i)
  }
  
  for(i in 1:length(intervalRows)){
    data$P1[intervalRows[[i]]] = sort(data$Margin[intervalRows[[i]]])[p1*length(data$Margin[intervalRows[[i]]])]
    data$P2[intervalRows[[i]]] = sort(data$Margin[intervalRows[[i]]])[p2*length(data$Margin[intervalRows[[i]]])]
    data$P3[intervalRows[[i]]] = sort(data$Margin[intervalRows[[i]]])[p3*length(data$Margin[intervalRows[[i]]])]
  }
  
  plotMe = data
  
  # ggplot(plotMe, aes(x = Date, y = Margin)) +
  #   geom_point() + 
  #   geom_line(y = plotMe$P1, linetype = "longdash", col = "red") + 
  #   geom_line(y = plotMe$P2, linetype = "longdash", col = "#228B22") + 
  #   geom_line(y = plotMe$P3, linetype = "longdash", col = "blue")
  
  P1avg = ddply(data, .(Date), summarize, P1avg = mean(P1))
  P2avg = ddply(data, .(Date), summarize, P2avg = mean(P2))
  P3avg = ddply(data, .(Date), summarize, P3avg = mean(P3))
  
  percentilesByDay = cbind(P1avg, P2avg = P2avg$P2avg, P3avg = P3avg$P3avg)
  
  Nov5000 = data.frame(Date = mdy(futuresMarket$Date), NovNC = futuresMarket$NovNC * numberOfBushels)
  joinPriceMargins = left_join(Nov5000,percentilesByDay)
  
  P1Adj = joinPriceMargins$NovNC + joinPriceMargins$P1avg
  P2Adj = joinPriceMargins$NovNC + joinPriceMargins$P2avg
  P3Adj = joinPriceMargins$NovNC + joinPriceMargins$P3avg
  
  joinPriceMargins$P1Adj = P1Adj
  joinPriceMargins$P2Adj = P2Adj
  joinPriceMargins$P3Adj = P3Adj
  
  ggplot(joinPriceMargins[1:250,], aes(x = Date, y = NovNC)) +
    geom_line() + 
    geom_line(y = P1Adj[1:250], linetype = "longdash", col = "red") + 
    # geom_line(y = P2Adj[1:250], linetype = "longdash", col = "#228B22") + 
    geom_line(y = P3Adj[1:250], linetype = "longdash", col = "blue")
  
  
  
}




























