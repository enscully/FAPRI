
library(lubridate)
library(stringr)
library(dplyr)
library(plyr)
library(ggplot2)
library(readr)

futuresMarket <- read_csv("C:/Users/ensxvd/Desktop/main-model/Data/Corn_FuturesMarket.csv")
futuresMarket$Date = mdy(futuresMarket$Date)

appObjectsCorn = readRDS("C:/Users/ensxvd/Desktop/main-model/Application/appObjectsCornBase.rds")
Corn_CropYearObjectsBase = appObjectsCorn[[1]]
Corn_CropYearsBase = appObjectsCorn[[2]]
finalizedPriceObjectCornBase = appObjectsCorn[[3]]

POSales = data.frame()

for(i in 1: length(Corn_CropYearObjectsBase)){
  colNames = colnames(Corn_CropYearObjectsBase[[i]][["PO Sales Summary MY"]][,-1])
  salesOneYear = cbind(t(Corn_CropYearObjectsBase[[i]][["PO Sales Summary MY"]][,-1]), Date = colNames, cropYear = i)
  POSales = rbind(POSales, salesOneYear)
  
}

colnames(POSales) = c(Corn_CropYearObjectsBase[[1]][["PO Sales Summary MY"]][,1], "Date", "cropYear")

row.names(POSales) = 1:nrow(POSales)

splitRows = which(grepl("Split1", POSales$Date) == TRUE)
POSales$Date[splitRows] = str_remove(POSales$Date[splitRows], "Split1")

POSales$Date = mdy(POSales$Date)

POMY = POSales[which(POSales$Trigger == "Multi-Year"),]
POMY = POMY[which(POMY$Date == unique(POMY$Date)),]
POMY$Price = as.numeric(as.character(POMY$Price))

POMY <- left_join(POMY, futuresMarket, by = "Date")

years = c("2008-1-1",
          "2009-1-1",
          "2010-1-1",
          "2011-1-1",
          "2012-1-1",
          "2013-1-1",
          "2014-1-1",
          "2015-1-1",
          "2016-1-1")

dateFreq  = data.frame(table(POSales$Date))
repDates = data.frame(Date = dateFreq$Var1[dateFreq$Freq > 1], freq = dateFreq$Freq[dateFreq$Freq > 1])

repDates$Date = ymd(repDates$Date)
repDatesDF = left_join(repDates, POSales, by = "Date")

falseDates = vector()
for(i in 1:length(unique(repDatesDF$Date))){
  
  if(!any(grepl("Multi-Year", as.character(with(repDatesDF, repDatesDF[(Date ==  unique(repDatesDF$Date)[i]), ])$Trigger)))){
    falseDates = c(ymd(falseDates), ymd(unique(repDatesDF$Date)[i]))
  }
  
}

repDatesDF = repDatesDF[!repDatesDF$Date %in% falseDates, ]

repDatesDF$Price = as.numeric(as.character(repDatesDF$Price))

MYaveragesDF = aggregate(repDatesDF$Price, by=list(repDatesDF$cropYear), FUN=mean)
colnames(MYaveragesDF) = c("Year", "Average")

MYaveragesDF$Year = c("12-10-2008",
                      "12-10-2009",
                      "12-10-2010",
                      "12-10-2012",
                      "12-10-2013",
                      "12-10-2014")

MYaveragesDF$Year = mdy(MYaveragesDF$Year)

ggplot() + 
  geom_line(data = futuresMarket, aes(x = Date, y = DecNC, color = "FM"), group = 1, size = 1) + 
  geom_point(aes(x = POMY$Date, y = POMY$DecNC, color = "PO"), size = 4) + 
  geom_point(data = MYaveragesDF, aes(x = Year, y = Average, color = "avgs"), size = 4) +
  scale_x_date(breaks = scales::pretty_breaks(10)) + 
  scale_color_manual(breaks = c("FM", "PO", "avgs"), values = c("#228b22", "#1960d1", "#d94c00"), 
                     labels = c("DecNC Price", "Multi-Year Sales", "Average from Multi-Year Sales \n (Dec 10 Delivery Date)")) + 
  labs(x = "year", y = "Price", color = "", title = "Multi-Year Sales: Corn") + 
  theme(plot.title = element_text(color = "black", size = 20, face = "bold.italic", hjust = 0.5),
        axis.text.x = element_text(face="bold", color="black", size=14),
        axis.text.y = element_text(face="bold", color="black", size=14),
        axis.title.x = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y = element_text(color = "black", size = 14, face = "bold"),
        legend.title = element_text(face="bold", color="black", size=10),
        legend.text = element_text(face="bold", color="black", size=10))
