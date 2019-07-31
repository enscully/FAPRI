# Soybean


library(lubridate)
library(stringr)
library(dplyr)
library(plyr)
library(ggplot2)
library(readr)
library(ggrepel)



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
  
  
  
  for (i in 1:length(splitter)) {
    delim = paste("(?<=", toString(splitter[i]), ")")
    delim = str_replace_all(delim, " ", "")
    string = unlist(strsplit(string, delim, perl = TRUE))
  }
  
  stringDF = str_split(string, " ")
  
  for (i in 1:length(stringDF)) {
    stringDF[[i]] = stringDF[[i]][stringDF[[i]] != ""]
    stringDF[[i]] = str_replace_all(stringDF[[i]], "[\r\n]" , "")
  }
  
  stringDFreal = rbind.fill(lapply(stringDF,function(y){as.data.frame(t(y),stringsAsFactors = FALSE)}))
  
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
  
  for (row in 2:nrow(joinPriceMargins)){
    if(month(joinPriceMargins$Date[row - 1]) == "8" && month(joinPriceMargins$Date[row]) == "9") {
      count = count + 1
    }
    else if (month(joinPriceMargins$Date[row - 1]) == "2" && month(joinPriceMargins$Date[row]) == "3") {
      count = count + 1
    }
    
    joinPriceMargins$Interval[row] = count
  }
  
  joinPriceMargins$avgCentsPerBushel = NA
  
  intervalRows = list()
  
  for (i in 1:max(joinPriceMargins$Interval)) {
    intervalRows[[i]] = which(joinPriceMargins$Interval == i)
  }
  
  joinPriceMargins$avgCentsPerBushel = NA
  joinPriceMargins$averagePrice = NA
  
  for (i in 1:length(intervalRows)) {
    joinPriceMargins$avgCentsPerBushel[intervalRows[[i]]] = as.numeric(tapply(joinPriceMargins$centsPerBushel, joinPriceMargins$Interval, mean)[i])
    joinPriceMargins$averagePrice[intervalRows[[i]]] = as.numeric(tapply(joinPriceMargins$NovNCscaled, joinPriceMargins$Interval, mean)[i])
  }
  
  joinPriceMargins$pricePlusMargin = joinPriceMargins$NovNCscaled + joinPriceMargins$centsPerBushel
  joinPriceMargins$avgPricePlusMargin = joinPriceMargins$averagePrice + joinPriceMargins$avgCentsPerBushel
  
  joinPriceMargins$P1 = NA
  joinPriceMargins$P2 = NA
  joinPriceMargins$P3 = NA
  
  for (i in 1:length(intervalRows)) {
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


# March
# May
# July
# September
# November

appObjectsSoybean = readRDS("C:/Users/ensxvd/Desktop/main-model/Application/appObjectsSoybeanBase.rds")
Soybean_CropYearObjectsBase = appObjectsSoybean[[1]]
Soybean_CropYearsBase = appObjectsSoybean[[2]]
finalizedPriceObjectSoybeanBase = appObjectsSoybean[[3]]
















#data

# library(ggplot2)
# library(dplyr)
# library(lubridate)
# library(stringr)

# my = marketingYear
# my1 = marketingYear1
# my2 = marketingYear2
# saleDate = POSales$originalDates[i]
# deliveryDate = POSales$Date[i]

getMaxPrices = function(my, my1, my2, saleDate, deliveryDate){
  
  if (!is.na(my) && !is.na(my1) && !is.na(my2)) {
    my1$Date = mdy(my1$Date)
    my2$Date = mdy(my2$Date)
    myFull = rbind(my, my1, my2)
  } else if (!is.na(my) && !is.na(my1)) {
    my1$Date = mdy(my1$Date)
    myFull = rbind(my, my1)
  }
  
  myFull = arrange(myFull, Date)
  salesInterval = interval(saleDate, deliveryDate)
  
  intervalRows = which(myFull$Date %within% salesInterval)
  
  maxPrice = myFull$Price[intervalRows[which(myFull$Price[intervalRows] == max(myFull$Price[intervalRows]))[1]]]
  
  return(maxPrice)
}



# i = 8
# marketingYear = Soybean_CropYearObjectsBase[[i]][["Marketing Year"]]
# marketingYear1 = NA
# marketingYear2 = NA
# salesSummary = Soybean_CropYearObjectsBase[[i]][["TS Sales Summary MY"]]
# cropYearNumber = i


# i = 9
# marketingYear = Soybean_CropYearObjectsBase[[i]][["Marketing Year"]]
# marketingYear1 = Soybean_CropYearObjectsBase[[i - 1]][["Marketing Year MY"]]
# marketingYear2 = Soybean_CropYearObjectsBase[[i - 2]][["Marketing Year MY"]]
# salesSummary = Soybean_CropYearObjectsBase[[i]][["PO Sales Summary MY"]]
# cropYearNumber = i


getMarginCosts = function(marketingYear, marketingYear1, marketingYear2, salesSummary, cropYearNumber){
  
  marketingYear = arrange(marketingYear, mdy(Date))
  
  POSales = data.frame()
  
  colNames = colnames(salesSummary[,-1])
  
  if (length(which(grepl("Split1", colNames) == TRUE)) > 0) {
    splitRows = which(grepl("Split1", colNames) == TRUE)
    colNames[splitRows] = str_remove(colNames[splitRows], "Split1")
  }
  
  colNames = mdy(colNames)
  
  originalDates = colNames
  
  for (i in 1:length(colnames(salesSummary[,-1])) ){
    if( year(colNames[i]) < year(mdy(marketingYear$Date[1]))) {
      colNames[i] = ymd(paste(year(mdy(marketingYear$Date[1])), "12-10", sep = "-"))
    }
  }
  
  salesOneYear = cbind(as.data.frame(t(salesSummary[,-1])), Date = ymd(colNames))
  POSales = rbind(POSales, salesOneYear)
  
  row.names(POSales) = 1:nrow(POSales)
  
  
  
  colnames(POSales) = c(salesSummary[,1], "Date")
  
  novCheck = mdy(paste("11-15", toString(year(mdy(marketingYear$Date[1]))), sep = "-"))
  janCheck = mdy(paste("01-15", toString(year(mdy(marketingYear$Date[1])) + 1), sep = "-"))
  marCheck = mdy(paste("03-15", toString(year(mdy(marketingYear$Date[1])) + 1), sep = "-"))
  mayCheck = mdy(paste("05-15", toString(year(mdy(marketingYear$Date[1])) + 1), sep = "-"))
  julCheck = mdy(paste("07-15", toString(year(mdy(marketingYear$Date[1])) + 1), sep = "-"))
  augCheck = mdy(paste("08-15", toString(year(mdy(marketingYear$Date[1])) + 1), sep = "-"))
  
  November = mdy(marketingYear$Date[which(abs(mdy(marketingYear$Date) - novCheck) == min(abs(mdy(marketingYear$Date) - novCheck)))])[1]
  January = mdy(marketingYear$Date[which(abs(mdy(marketingYear$Date) - janCheck) == min(abs(mdy(marketingYear$Date) - janCheck)))])[1]
  March = mdy(marketingYear$Date[which(abs(mdy(marketingYear$Date) - marCheck) == min(abs(mdy(marketingYear$Date) - marCheck)))])[1]
  May = mdy(marketingYear$Date[which(abs(mdy(marketingYear$Date) - mayCheck) == min(abs(mdy(marketingYear$Date) - mayCheck)))])[1]
  July = mdy(marketingYear$Date[which(abs(mdy(marketingYear$Date) - julCheck) == min(abs(mdy(marketingYear$Date) - julCheck)))])[1]
  August = mdy(marketingYear$Date[which(abs(mdy(marketingYear$Date) - augCheck) == min(abs(mdy(marketingYear$Date) - augCheck)))])[1]
  
  interval1 = interval(mdy(marketingYear$Date[1]), November - 1)
  interval2 = interval(November, January - 1)
  interval3 = interval(January, March - 1)
  interval4 = interval(March, May - 1)
  interval5 = interval(May, July - 1)
  interval6 = interval(July, August - 1)
  
  
  
  POSales$deliveryDate = as.Date(1:nrow(POSales), origin = Sys.Date())
  for (i in 1:nrow(POSales)) {
    if (POSales$Date[i] %within% interval1) POSales$deliveryDate[i] = November
    if (POSales$Date[i] %within% interval2) POSales$deliveryDate[i] = January
    if (POSales$Date[i] %within% interval3) POSales$deliveryDate[i] = March
    if (POSales$Date[i] %within% interval4) POSales$deliveryDate[i] = May
    if (POSales$Date[i] %within% interval5) POSales$deliveryDate[i] = July
    if (POSales$Date[i] %within% interval6) POSales$deliveryDate[i] = August
  }
  
  intervalList = list(interval1,
                      interval2,
                      interval3,
                      interval4,
                      interval5,
                      interval6)
  
  marketingYear$Date = mdy(marketingYear$Date)
  
  POSales$interval = interval(POSales$Date, (POSales$deliveryDate))
  
  
  POSales$originalDates = ymd(originalDates)
  
  POSales$Price = as.numeric(as.character(POSales$Price))
  
  POSales$maxPrice = NA
  POSales$intervalNumber = NA
  POSales$maxPriceMinusPrice = NA
  POSales$finalMarginCost = NA
  POSales$finalPriceWithMargin = NA
  POSales$Margin = NA
  POSales$intervalRow = NA
  for (i in 1:nrow(POSales)) {
    intRows = which(marketingYear$Date %within% POSales$interval[i])
    POSales$maxPrice[i] = marketingYear$Price[intRows[which(marketingYear$Price[intRows] == max(marketingYear$Price[intRows]))[1]]]
    
    POSales$Margin[i] = plotMe$avg[which(plotMe$Date == POSales$originalDates[i])]/5000
    POSales$intervalRow[i] = which(marketingYear$Date == POSales$Date[i])
    
    POSales$maxPriceMinusPrice[i] = POSales$maxPrice[i] - POSales$Price[i]
    
    POSales$finalMarginCost[i] = POSales$maxPriceMinusPrice[i] + POSales$Margin[i]
    POSales$finalPriceWithMargin[i] = POSales$finalMarginCost[i] + POSales$Price[i]
  }
  
  for (i in 1:nrow(POSales)) {
    if (POSales$Trigger[i] == "Multi-Year") {
      POSales$maxPrice[i] = getMaxPrices(marketingYear, marketingYear1, marketingYear2, POSales$originalDates[i], POSales$Date[i])[1]
      POSales$maxPriceMinusPrice[i] = POSales$maxPrice[i] - POSales$Price[i]
      
      POSales$finalMarginCost[i] = POSales$maxPriceMinusPrice[i] + POSales$Margin[i]
      POSales$finalPriceWithMargin[i] = POSales$finalMarginCost[i] + POSales$Price[i]
      
    } 
  }
  
  marginDF = data.frame(Date = POSales$originalDates,
                        Price = POSales$Price,
                        MarginCents = round((POSales$Margin), digits = 2),
                        MaxPrice = round((POSales$maxPrice), digits = 2),
                        Margin = round((POSales$finalMarginCost), digits = 2),
                        PriceWithMargin = round((POSales$finalPriceWithMargin), digits = 2),
                        PercentSold = POSales$Percentage)
  
  intervalSegments = data.frame(maxPrice = POSales$maxPrice, xstart = mdy(NA), xend = mdy(NA))
  
  for (i in 1:nrow(intervalSegments)) {
    intervalSegments$xstart[i] = ymd((POSales$originalDates[i]))
    intervalSegments$xend[i] = ymd(int_end(POSales$interval[i]))
  }
  
  plot = ggplot() +
    geom_line(data = marketingYear, aes(x = Date, y = Price), color = "black") +
    # geom_line(data = plotMe, aes(x = Date, y = NovNCscaled)) +
    geom_segment(data = intervalSegments, aes(x = ymd(xstart), xend = ymd(xend), y = maxPrice, yend = maxPrice, color = "ints"), linetype = "dashed") +
    geom_point(data = POSales, aes(x = Date, y = Price, color = "price"), size = 2) +
    geom_point(data = POSales, aes(x = Date, y = finalPriceWithMargin, color = "marginAdj"), size = 2) +
    ggtitle(paste("Crop Year", cropYearNumber)) +
    scale_color_manual("Legend",
                       values = c(ints = "black", price = "forestgreen", marginAdj = "navyblue", vert = "red"),
                       labels = c("Max Price", "Price + Margin", "Sale Price", "Margin Cost")) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) + 
    geom_segment(data = POSales, aes(x = Date, y = Price, xend = Date, yend = finalPriceWithMargin, color = "vert")) + 
    geom_text(data = POSales, aes(x = Date, label = round((POSales$finalMarginCost), digits = 2), y = (finalPriceWithMargin + Price)/2), 
              angle = 0, check_overlap = T, color = "red", size = 4, fontface = "bold")
  
  # ggplotly(plot)
  
  # plot = ggplot() +
  #   geom_point(data = POSales, aes(x = originalDates, y = finalMarginCost)) +
  #   geom_point(data = POSales, aes(x = originalDates, y = Price), color = "forestgreen")
  
  returnList = list(plot, marginDF)
  
  return(returnList)
}

#################################################################################################################
# PRICE OBJECTIVE RUN WITH MULTI YEAR SALES
#################################################################################################################

marginCosts = data.frame()
tempList = list()
marginPlot = list()
for (i in 1) {
  tempList = getMarginCosts(Soybean_CropYearObjectsBase[[i]][["Marketing Year"]], 
                            NA,
                            NA,
                            Soybean_CropYearObjectsBase[[i]][["PO Sales Summary MY"]], i)
  marginPlot[[i]] = tempList[[1]]
  # plot(marginPlot[[i]])
  marginCosts = rbind(marginCosts, cbind(cropYear = rep(Soybean_CropYearObjectsBase[[i]][["Crop Year"]], times = nrow(tempList[[2]])), tempList[[2]]))
}

for (i in 2) {
  tempList = getMarginCosts(Soybean_CropYearObjectsBase[[i]][["Marketing Year"]], 
                            Soybean_CropYearObjectsBase[[i - 1]][["Marketing Year MY"]],
                            NA,
                            Soybean_CropYearObjectsBase[[i]][["PO Sales Summary MY"]], i)
  marginPlot[[i]] = tempList[[1]]
  # plot(marginPlot[[i]])
  marginCosts = rbind(marginCosts, cbind(cropYear = rep(Soybean_CropYearObjectsBase[[i]][["Crop Year"]], times = nrow(tempList[[2]])), tempList[[2]]))
}

for (i in 3:length(Soybean_CropYearObjectsBase)) {
  tempList = getMarginCosts(Soybean_CropYearObjectsBase[[i]][["Marketing Year"]], 
                            Soybean_CropYearObjectsBase[[i - 1]][["Marketing Year MY"]],
                            Soybean_CropYearObjectsBase[[i - 2]][["Marketing Year MY"]],
                            Soybean_CropYearObjectsBase[[i]][["PO Sales Summary MY"]], i)
  marginPlot[[i]] = tempList[[1]]
  # plot(marginPlot[[i]])
  marginCosts = rbind(marginCosts, cbind(cropYear = rep(Soybean_CropYearObjectsBase[[i]][["Crop Year"]], times = nrow(tempList[[2]])), tempList[[2]]))
}

rownames(marginCosts) = 1:nrow(marginCosts)

write.csv(marginCosts, file = "marginCostsPOMY.csv",row.names = FALSE)


#################################################################################################################
# PRICE OBJECTIVE RUN WITHOUT MULTI YEAR SALES
#################################################################################################################

marginCosts = data.frame()
tempList = list()
marginPlot = list()
for (i in 1:length(Soybean_CropYearObjectsBase)) {
  tempList = getMarginCosts(Soybean_CropYearObjectsBase[[i]][["Marketing Year"]], 
                            NA,
                            NA,
                            Soybean_CropYearObjectsBase[[i]][["Sales Summary"]], i)
  marginPlot[[i]] = tempList[[1]]
  # plot(marginPlot[[i]])
  marginCosts = rbind(marginCosts, cbind(cropYear = rep(Soybean_CropYearObjectsBase[[i]][["Crop Year"]], times = nrow(tempList[[2]])), tempList[[2]]))
}

rownames(marginCosts) = 1:nrow(marginCosts)

write.csv(marginCosts, file = "marginCostsPO.csv",row.names = FALSE)




#################################################################################################################
# RUN WITH MULTI YEAR SALES
#################################################################################################################

marginCosts = data.frame()
tempList = list()
marginPlot = list()
for (i in 1) {
  tempList = getMarginCosts(Soybean_CropYearObjectsBase[[i]][["Marketing Year"]], 
                            NA,
                            NA,
                            Soybean_CropYearObjectsBase[[i]][["TS Sales Summary MY"]], i)
  marginPlot[[i]] = tempList[[1]]
  # plot(marginPlot[[i]])
  marginCosts = rbind(marginCosts, cbind(cropYear = rep(Soybean_CropYearObjectsBase[[i]][["Crop Year"]], times = nrow(tempList[[2]])), tempList[[2]]))
}

for (i in 2) {
  tempList = getMarginCosts(Soybean_CropYearObjectsBase[[i]][["Marketing Year"]], 
                            Soybean_CropYearObjectsBase[[i - 1]][["Marketing Year MY"]],
                            NA,
                            Soybean_CropYearObjectsBase[[i]][["TS Sales Summary MY"]], i)
  marginPlot[[i]] = tempList[[1]]
  # plot(marginPlot[[i]])
  marginCosts = rbind(marginCosts, cbind(cropYear = rep(Soybean_CropYearObjectsBase[[i]][["Crop Year"]], times = nrow(tempList[[2]])), tempList[[2]]))
}

for (i in 3:length(Soybean_CropYearObjectsBase)) {
  tempList = getMarginCosts(Soybean_CropYearObjectsBase[[i]][["Marketing Year"]], 
                            Soybean_CropYearObjectsBase[[i - 1]][["Marketing Year MY"]],
                            Soybean_CropYearObjectsBase[[i - 2]][["Marketing Year MY"]],
                            Soybean_CropYearObjectsBase[[i]][["TS Sales Summary MY"]], i)
  marginPlot[[i]] = tempList[[1]]
  # plot(marginPlot[[i]])
  marginCosts = rbind(marginCosts, cbind(cropYear = rep(Soybean_CropYearObjectsBase[[i]][["Crop Year"]], times = nrow(tempList[[2]])), tempList[[2]]))
}

rownames(marginCosts) = 1:nrow(marginCosts)

write.csv(marginCosts, file = "marginCostsTSMY.csv",row.names = FALSE)


#################################################################################################################
# RUN WITHOUT MULTI YEAR SALES
#################################################################################################################

marginCosts = data.frame()
tempList = list()
marginPlot = list()

for (i in 1:length(Soybean_CropYearObjectsBase)) {
  tempList = getMarginCosts(Soybean_CropYearObjectsBase[[i]][["Marketing Year"]], 
                            NA,
                            NA,
                            Soybean_CropYearObjectsBase[[i]][["TS Sales Summary"]], i)
  marginPlot[[i]] = tempList[[1]]
  # plot(marginPlot[[i]])
  marginCosts = rbind(marginCosts, cbind(cropYear = rep(Soybean_CropYearObjectsBase[[i]][["Crop Year"]], times = nrow(tempList[[2]])), tempList[[2]]))
}

rownames(marginCosts) = 1:nrow(marginCosts)

write.csv(marginCosts, file = "marginCostsTS.csv",row.names = FALSE)











#

for(i in 1: length(Soybean_CropYearObjectsBase)){
  colNames = colnames(Soybean_CropYearObjectsBase[[i]][["PO Sales Summary MY"]][,-1])
  salesOneYear = cbind(t(Soybean_CropYearObjectsBase[[i]][["PO Sales Summary MY"]][,-1]), Date = colNames)
  POSales = rbind(POSales, salesOneYear)
}

POSales$Date = mdy(POSales$Date)











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






