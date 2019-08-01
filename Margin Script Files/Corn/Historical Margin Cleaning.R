# Corn

library(lubridate)
library(stringr)
library(dplyr)
library(plyr)
library(ggplot2)
library(readr)
library(ggrepel)



futuresMarket <- read_csv("C:/Users/ensxvd/Desktop/main-model/Data/Corn_FuturesMarket.csv")

cornString1 = source("Margin Script Files/Corn/cornString1")
cornString2 = source("Margin Script Files/Corn/cornString2")
cornString3 = source("Margin Script Files/Corn/cornString3")

# string = cornString2$value

cleanTextData = function(string, string2) {
  
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
  
  for (i in 1:length(splitter)) {
    delim = paste("(?<=", toString(splitter[i]), ")")
    delim = str_replace_all(delim, " ", "")
    string = unlist(strsplit(string, delim, perl = TRUE))
  }
  
  stringDF = str_split(string, " ")
  
  for (i in 1:length(stringDF)) {
    stringDF[[i]] = stringDF[[i]][stringDF[[i]] != ""]
    stringDF[[i]] = str_replace_all(stringDF[[i]], "[\r\n]", "")
  }
  
  stringDFreal = rbind.fill(lapply(stringDF, function(y) {as.data.frame(t(y), stringsAsFactors = FALSE)}))
  
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

predictMargins = function(data, numberOfBushels, p1, p2, p3) {
  # Date = Date
  # avg = daily average across all codes
  # DecNc = DecNC price * number of bushels in the sale
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
  
  DecMultiplied = data.frame(Date = mdy(futuresMarket$Date), DecNC = futuresMarket$DecNC * numberOfBushels)
  
  joinPriceMargins = left_join(avgMargin, DecMultiplied, rm.na = TRUE)
  
  joinPriceMargins = joinPriceMargins[complete.cases(joinPriceMargins), ]
  
  joinPriceMargins$DecNCscaled = joinPriceMargins$DecNC/numberOfBushels
  
  joinPriceMargins$adjPrice = joinPriceMargins$DecNC + joinPriceMargins$avg
  
  joinPriceMargins$centsPerBushel = joinPriceMargins$avg/numberOfBushels
  
  count = 1
  joinPriceMargins = arrange(joinPriceMargins, Date)
  joinPriceMargins$Interval = NA
  joinPriceMargins$Interval[1] = 1
  
  for (row in 2:nrow(joinPriceMargins)) {
    if (month(joinPriceMargins$Date[row - 1]) == "8" && month(joinPriceMargins$Date[row]) == "9") {
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
    joinPriceMargins$averagePrice[intervalRows[[i]]] = as.numeric(tapply(joinPriceMargins$DecNCscaled, joinPriceMargins$Interval, mean)[i])
  }
  
  joinPriceMargins$pricePlusMargin = joinPriceMargins$DecNCscaled + joinPriceMargins$centsPerBushel
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

plotMe = predictMargins(corn[corn$Date > ymd("2007-01-01"), ], 5000, 0.50, 0.70, 0.90)

# # Slow
# ggplot(plotMe, aes(x = Date, y = Margin)) +
#   geom_point()
# 
# # Price plus daily margin / number of bushels. Gives specific margin requirements
# ggplot(plotMe, aes(x = Date, y = DecNCscaled)) +
#   geom_line() +
#   geom_line(y = plotMe$adjPrice/5000, linetype = "longdash", col = "red")
# 
# # Displays average margin
# ggplot(plotMe, aes(x = Date, y = DecNCscaled)) +
#   geom_line() +
#   geom_line(y = plotMe$avgPricePlusMargin, linetype = "longdash", col = "red")
# 
# # Displays margin percentiles
# ggplot(plotMe, aes(x = Date, y = DecNCscaled)) +
#   geom_line() +
#   geom_line(aes(color = "P1", y = P1), linetype = "longdash") +
#   geom_line(aes(color = "P2", y = P2), linetype = "longdash") +
#   geom_line(aes(color = "P3", y = P3), linetype = "longdash") +
#   labs(x = "Date", y = "Price Per Bushel") +
#   scale_colour_manual(name = '',
#                       values =c("P1" = "red", "P2" = "forestgreen", "P3" = "blue"),
#                       labels = c('50th Percentile','70th Percentile','90th Percentile'))


###############################################################################################################

# Calculating what margin would have been PO Base, non-multi-year

appObjectsCorn = readRDS("C:/Users/ensxvd/Desktop/main-model/Application/appObjectsCornBase.rds")
Corn_CropYearObjectsBase = appObjectsCorn[[1]]
Corn_CropYearsBase = appObjectsCorn[[2]]
finalizedPriceObjectCornBase = appObjectsCorn[[3]]
















#data

# my = marketingYear
# my1 = marketingYear1
# my2 = marketingYear2
# saleDate = POSales$originalDates[i]
# deliveryDate = POSales$Date[i]

getMaxPrices = function(my, my1, my2, saleDate, deliveryDate) {
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

# i = 1
# marketingYear = Corn_CropYearObjectsBase[[i]][["Marketing Year"]]
# marketingYear1 = NA
# marketingYear2 = NA
# salesSummary = Corn_CropYearObjectsBase[[i]][["TS Sales Summary MY"]]
# cropYearNumber = i

# i = 3
# marketingYear = Corn_CropYearObjectsBase[[i]][["Marketing Year"]]
# marketingYear1 = Corn_CropYearObjectsBase[[i - 1]][["Marketing Year MY"]]
# marketingYear2 = Corn_CropYearObjectsBase[[i - 2]][["Marketing Year MY"]]
# salesSummary = Corn_CropYearObjectsBase[[i]][["PO Sales Summary MY"]]
# cropYearNumber = i

getMarginCosts = function(marketingYear, marketingYear1, marketingYear2, salesSummary, cropYearNumber) {
  
  marketingYear = arrange(marketingYear, mdy(Date))
  
  POSales = data.frame()
  
  colNames = colnames(salesSummary[,-1])
  
  if (length(which(grepl("Split1", colNames) == TRUE)) > 0) {
    splitRows = which(grepl("Split1", colNames) == TRUE)
    colNames[splitRows] = str_remove(colNames[splitRows], "Split1")
  }
  
  colNames = mdy(colNames)
  
  originalDates = colNames
  
  for (i in 1:length(colnames(salesSummary[,-1]))) {
    if (year(colNames[i]) < year(mdy(marketingYear$Date[1]))) {
      colNames[i] = ymd(paste(year(mdy(marketingYear$Date[1])), "12-10", sep = "-"))
    }
  }
  
  salesOneYear = cbind(as.data.frame(t(salesSummary[,-1])), Date = ymd(colNames))
  POSales = rbind(POSales, salesOneYear)
  
  row.names(POSales) = 1:nrow(POSales)

  colnames(POSales) = c(salesSummary[,1], "Date")
  
  decCheck = mdy(paste("12-15", toString(year(mdy(marketingYear$Date[1]))), sep = "-"))
  marCheck = mdy(paste("03-15", toString(year(mdy(marketingYear$Date[1])) + 1), sep = "-"))
  mayCheck = mdy(paste("05-15", toString(year(mdy(marketingYear$Date[1])) + 1), sep = "-"))
  julCheck = mdy(paste("07-15", toString(year(mdy(marketingYear$Date[1])) + 1), sep = "-"))
  
  December = mdy(marketingYear$Date[which(abs(mdy(marketingYear$Date) - decCheck) == min(abs(mdy(marketingYear$Date) - decCheck)))])[1]
  March = mdy(marketingYear$Date[which(abs(mdy(marketingYear$Date) - marCheck) == min(abs(mdy(marketingYear$Date) - marCheck)))])[1]
  May = mdy(marketingYear$Date[which(abs(mdy(marketingYear$Date) - mayCheck) == min(abs(mdy(marketingYear$Date) - mayCheck)))])[1]
  July = mdy(marketingYear$Date[which(abs(mdy(marketingYear$Date) - julCheck) == min(abs(mdy(marketingYear$Date) - julCheck)))])[1]

  interval1 = interval(mdy(marketingYear$Date[1]), December - 1)
  interval2 = interval(December, March - 1)
  interval3 = interval(March, May - 1)
  interval4 = interval(May, July - 1)
  interval5 = interval(July, mdy(last(marketingYear$Date)))
  
  POSales$deliveryDate = as.Date(1:nrow(POSales), origin = Sys.Date())
  for (i in 1:nrow(POSales)) {
    if (POSales$Date[i] %within% interval1) POSales$deliveryDate[i] = December
    if (POSales$Date[i] %within% interval2) POSales$deliveryDate[i] = March
    if (POSales$Date[i] %within% interval3) POSales$deliveryDate[i] = May
    if (POSales$Date[i] %within% interval4) POSales$deliveryDate[i] = July
    if (POSales$Date[i] %within% interval5) POSales$deliveryDate[i] = last(marketingYear$Date)
  }
  
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
  
  # MY Plotting
  plot = ggplot() +
    geom_line(data = marketingYear, aes(x = Date, y = Price), color = "black") +
    geom_line(data = plotMe, aes(x = Date, y = DecNCscaled)) +
    geom_segment(data = intervalSegments, aes(x = ymd(xstart), xend = ymd(xend), y = maxPrice, yend = maxPrice, color = "ints"), linetype = "dashed") +
    geom_point(data = POSales, aes(x = originalDates, y = Price, color = "price"), size = 2) +
    geom_point(data = POSales, aes(x = originalDates, y = finalPriceWithMargin, color = "marginAdj"), size = 2) +
    ggtitle(paste("Crop Year", cropYearNumber)) +
    scale_color_manual("Legend",
                       values = c(ints = "black", price = "forestgreen", marginAdj = "navyblue", vert = "red"),
                       labels = c("Max Price", "Price + Margin", "Sale Price", "Margin Cost")) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) + 
    geom_segment(data = POSales, aes(x = originalDates, y = Price, xend = originalDates, yend = finalPriceWithMargin, color = "vert")) + 
    geom_text(data = POSales, aes(x = originalDates, label = round((POSales$finalMarginCost), digits = 2), y = (finalPriceWithMargin + Price)/2), 
              angle = 0, check_overlap = T, color = "red", size = 4, fontface = "bold")
  
  # plot = ggplot() +
  #   geom_line(data = marketingYear, aes(x = Date, y = Price), color = "black") +
  #   geom_line(data = plotMe, aes(x = Date, y = DecNCscaled)) +
  #   geom_segment(data = intervalSegments, aes(x = ymd(xstart), xend = ymd(xend), y = maxPrice, yend = maxPrice, color = "ints"), linetype = "dashed") +
  #   geom_point(data = POSales, aes(x = Date, y = Price, color = "price"), size = 2) +
  #   geom_point(data = POSales, aes(x = Date, y = finalPriceWithMargin, color = "marginAdj"), size = 2) +
  #   ggtitle(paste("Crop Year", cropYearNumber)) +
  #   scale_color_manual("Legend",
  #                      values = c(ints = "black", price = "forestgreen", marginAdj = "navyblue", vert = "red"),
  #                      labels = c("Max Price", "Price + Margin", "Sale Price", "Margin Cost")) +
  #   scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) + 
  #   geom_segment(data = POSales, aes(x = Date, y = Price, xend = Date, yend = finalPriceWithMargin, color = "vert")) + 
  #   geom_text(data = POSales, aes(x = Date, label = round((POSales$finalMarginCost), digits = 2), y = (finalPriceWithMargin + Price)/2), 
  #             angle = 0, check_overlap = T, color = "red", size = 4, fontface = "bold")
  
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
  tempList = getMarginCosts(Corn_CropYearObjectsBase[[i]][["Marketing Year"]], 
                            NA,
                            NA,
                            Corn_CropYearObjectsBase[[i]][["PO Sales Summary MY"]], i)
  marginPlot[[i]] = tempList[[1]]
  # plot(marginPlot[[i]])
  marginCosts = rbind(marginCosts, cbind(cropYear = rep(Corn_CropYearObjectsBase[[i]][["Crop Year"]], times = nrow(tempList[[2]])), tempList[[2]]))
}

for (i in 2) {
  tempList = getMarginCosts(Corn_CropYearObjectsBase[[i]][["Marketing Year"]], 
                            Corn_CropYearObjectsBase[[i - 1]][["Marketing Year MY"]],
                            NA,
                            Corn_CropYearObjectsBase[[i]][["PO Sales Summary MY"]], i)
  marginPlot[[i]] = tempList[[1]]
  # plot(marginPlot[[i]])
  marginCosts = rbind(marginCosts, cbind(cropYear = rep(Corn_CropYearObjectsBase[[i]][["Crop Year"]], times = nrow(tempList[[2]])), tempList[[2]]))
}

for (i in 3:length(Corn_CropYearObjectsBase)) {
  tempList = getMarginCosts(Corn_CropYearObjectsBase[[i]][["Marketing Year"]], 
                            Corn_CropYearObjectsBase[[i - 1]][["Marketing Year MY"]],
                            Corn_CropYearObjectsBase[[i - 2]][["Marketing Year MY"]],
                            Corn_CropYearObjectsBase[[i]][["PO Sales Summary MY"]], i)
  marginPlot[[i]] = tempList[[1]]
  # plot(marginPlot[[i]])
  marginCosts = rbind(marginCosts, cbind(cropYear = rep(Corn_CropYearObjectsBase[[i]][["Crop Year"]], times = nrow(tempList[[2]])), tempList[[2]]))
}

rownames(marginCosts) = 1:nrow(marginCosts)

write.csv(marginCosts, file = "marginCostsPOMY.csv", row.names = FALSE)

#################################################################################################################
# PRICE OBJECTIVE RUN WITHOUT MULTI YEAR SALES
#################################################################################################################

marginCosts = data.frame()
tempList = list()
marginPlot = list()
for (i in 1:length(Corn_CropYearObjectsBase)) {
  tempList = getMarginCosts(Corn_CropYearObjectsBase[[i]][["Marketing Year"]], 
                            NA,
                            NA,
                            Corn_CropYearObjectsBase[[i]][["Sales Summary"]], i)
  marginPlot[[i]] = tempList[[1]]
  # plot(marginPlot[[i]])
  marginCosts = rbind(marginCosts, cbind(cropYear = rep(Corn_CropYearObjectsBase[[i]][["Crop Year"]], times = nrow(tempList[[2]])), tempList[[2]]))
}

write.csv(marginCosts, file = "marginCostsPO.csv", row.names = FALSE)

#################################################################################################################
# RUN WITH MULTI YEAR SALES
#################################################################################################################

marginCosts = data.frame()
tempList = list()
marginPlot = list()
for (i in 1) {
  tempList = getMarginCosts(Corn_CropYearObjectsBase[[i]][["Marketing Year"]], 
                            NA,
                            NA,
                            Corn_CropYearObjectsBase[[i]][["TS Sales Summary MY"]], i)
  marginPlot[[i]] = tempList[[1]]
  # plot(marginPlot[[i]])
  marginCosts = rbind(marginCosts, cbind(cropYear = rep(Corn_CropYearObjectsBase[[i]][["Crop Year"]], times = nrow(tempList[[2]])), tempList[[2]]))
}

for (i in 2) {
  tempList = getMarginCosts(Corn_CropYearObjectsBase[[i]][["Marketing Year"]], 
                            Corn_CropYearObjectsBase[[i - 1]][["Marketing Year MY"]],
                            NA,
                            Corn_CropYearObjectsBase[[i]][["TS Sales Summary MY"]], i)
  marginPlot[[i]] = tempList[[1]]
  # plot(marginPlot[[i]])
  marginCosts = rbind(marginCosts, cbind(cropYear = rep(Corn_CropYearObjectsBase[[i]][["Crop Year"]], times = nrow(tempList[[2]])), tempList[[2]]))
}

for (i in 3:length(Corn_CropYearObjectsBase)) {
  tempList = getMarginCosts(Corn_CropYearObjectsBase[[i]][["Marketing Year"]],
                            Corn_CropYearObjectsBase[[i - 1]][["Marketing Year MY"]],
                            Corn_CropYearObjectsBase[[i - 2]][["Marketing Year MY"]],
                            Corn_CropYearObjectsBase[[i]][["TS Sales Summary MY"]], i)
  marginPlot[[i]] = tempList[[1]]
  # plot(marginPlot[[i]])
  marginCosts = rbind(marginCosts, cbind(cropYear = rep(Corn_CropYearObjectsBase[[i]][["Crop Year"]], times = nrow(tempList[[2]])), tempList[[2]]))
}

write.csv(marginCosts, file = "marginCostsTSMY.csv", row.names = FALSE)

#################################################################################################################
# RUN WITHOUT MULTI YEAR SALES
#################################################################################################################

marginCosts = data.frame()
tempList = list()
marginPlot = list()
for (i in 1:length(Corn_CropYearObjectsBase)) {
  tempList = getMarginCosts(Corn_CropYearObjectsBase[[i]][["Marketing Year"]], 
                            NA,
                            NA,
                            Corn_CropYearObjectsBase[[i]][["TS Sales Summary"]], i)
  marginPlot[[i]] = tempList[[1]]
  # plot(marginPlot[[i]])
  marginCosts = rbind(marginCosts, cbind(cropYear = rep(Corn_CropYearObjectsBase[[i]][["Crop Year"]], times = nrow(tempList[[2]])), tempList[[2]]))
}

write.csv(marginCosts, file = "marginCostsTS.csv", row.names = FALSE)












###############################################################################################################

# RANDOM OLD CODE

###############################################################################################################





# ggplot(corn, aes(x = Code, y = Margin)) +
#   geom_boxplot(aes(color = Code))
# 
# # View freqency of each code
# plot(count(corn$Code)$freq)


# # Slow
# ggplot(corn, aes(x = Date, y = Margin)) +
#   geom_point(aes(color = Code))




# firstCode = corn[which(corn$Code == "S-01"),]
# secondCode = corn[which(corn$Code == "S-02"),]
# lastCode = corn[which(corn$Code == "S-10"),]
# 
# codes = rbind(firstCode, lastCode)
# 
# 
# ggplotly(ggplot(codes, aes(x = Date, y = Margin)) +
#            geom_point(aes(color = Code)))
# 
# ggplotly(ggplot(corn, aes(x = Date, y = Margin)) +
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


# data = corn
# # data = firstCode
# p1 = 0.50
# p2 = 0.75
# p3 = 0.95
# numberOfBushels = 5000
# 
# graphRanks = function(data, p1, p2, p3, numberOfBushels) {
#   count = 1
#   data = arrange(data, Date)
#   data$Interval = NA
#   data$Interval[1] = 1
#   
#   for (row in 2:nrow(data)) {
#     if (month(data$Date[row - 1]) == "8" && month(data$Date[row]) == "9") {
#       count = count + 1
#     }
#     else if (month(data$Date[row - 1]) == "2" && month(data$Date[row]) == "3") {
#       count = count + 1
#     }
#     
#     data$Interval[row] = count
#   }
#   
#   data$P1 = NA
#   data$P2 = NA
#   data$P3 = NA
#   
#   intervalRows = list()
#   
#   for (i in 1:max(data$Interval)) {
#     intervalRows[[i]] = which(data$Interval == i)
#   }
#   
#   for (i in 1:length(intervalRows)) {
#     data$P1[intervalRows[[i]]] = sort(data$Margin[intervalRows[[i]]])[p1*length(data$Margin[intervalRows[[i]]])]
#     data$P2[intervalRows[[i]]] = sort(data$Margin[intervalRows[[i]]])[p2*length(data$Margin[intervalRows[[i]]])]
#     data$P3[intervalRows[[i]]] = sort(data$Margin[intervalRows[[i]]])[p3*length(data$Margin[intervalRows[[i]]])]
#   }
#   
#   plotMe = data
#   
#   # ggplot(plotMe, aes(x = Date, y = Margin)) +
#   #   geom_point() + 
#   #   geom_line(y = plotMe$P1, linetype = "longdash", col = "red") + 
#   #   geom_line(y = plotMe$P2, linetype = "longdash", col = "#228B22") + 
#   #   geom_line(y = plotMe$P3, linetype = "longdash", col = "blue")
#   
#   P1avg = ddply(data, .(Date), summarize, P1avg = mean(P1))
#   P2avg = ddply(data, .(Date), summarize, P2avg = mean(P2))
#   P3avg = ddply(data, .(Date), summarize, P3avg = mean(P3))
#   
#   percentilesByDay = cbind(P1avg, P2avg = P2avg$P2avg, P3avg = P3avg$P3avg)
#   
#   Dec5000 = data.frame(Date = mdy(futuresMarket$Date), DecNC = futuresMarket$DecNC * numberOfBushels)
#   joinPriceMargins = left_join(Dec5000,percentilesByDay)
#   
#   P1Adj = joinPriceMargins$DecNC + joinPriceMargins$P1avg
#   P2Adj = joinPriceMargins$DecNC + joinPriceMargins$P2avg
#   P3Adj = joinPriceMargins$DecNC + joinPriceMargins$P3avg
#   
#   joinPriceMargins$P1Adj = P1Adj
#   joinPriceMargins$P2Adj = P2Adj
#   joinPriceMargins$P3Adj = P3Adj
#   
#   ggplot(joinPriceMargins[1:250,], aes(x = Date, y = DecNC)) +
#     geom_line() + 
#     geom_line(y = P1Adj[1:250], linetype = "longdash", col = "red") + 
#     # geom_line(y = P2Adj[1:250], linetype = "longdash", col = "#228B22") + 
#     geom_line(y = P3Adj[1:250], linetype = "longdash", col = "blue")
# 
# }






