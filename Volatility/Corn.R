# Volatility
# Corn

library(lubridate)
library(ggplot2)
library(zoo)
library(plotly)
library(readr)
library(PerformanceAnalytics)

futuresMarket <- read_csv("C:/Users/ensxvd/Desktop/main-model/Data/Corn_FuturesMarket.csv")

corn = futuresMarket[,c(1,2,3)]
corn$Date = mdy(corn$Date)

corn$Interval = NA
corn$Interval[1] = 1
count = 1

for(row in 2:nrow(corn)){
  if(month(corn$Date[row - 1]) !=  month(corn$Date[row])){
    count = count + 1
  }
  
  corn$Interval[row] = count
}

intervalRows = list()

for(i in 1:max(corn$Interval)){
  intervalRows[[i]] = which(corn$Interval == i)
}

corn$volatilityDec = NA
corn$volatilityNearby = NA
corn$monthYear = NA

for(i in 1:length(intervalRows)){
  corn$volatilityDec[intervalRows[[i]]] = as.numeric(tapply(corn$DecNC, corn$Interval, sd)[i])
  corn$volatilityNearby[intervalRows[[i]]] = as.numeric(tapply(corn$NearbyOC, corn$Interval, sd)[i])
  
  corn$monthYear[intervalRows[[i]]] = paste(year(corn$Date[intervalRows[[i]]]), month(corn$Date[intervalRows[[i]]]), sep = "-")
}

decVolatility = data.frame(volatility = unique(corn$volatilityDec),Date = unique(as.yearmon(corn$monthYear)))
nearbyVolatility = data.frame(volatility = unique(corn$volatilityNearby), Date = unique(as.yearmon(corn$monthYear)))



ggplot(data = decVolatility, aes(x = as.factor(Date), y = volatility)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(x = "Month", y = "Volatility (Cents)", title = "December")


ggplot(data = nearbyVolatility, aes(x = as.factor(Date), y = volatility)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(x = "Month", y = "Volatility (Cents)", title = "Nearby")

# Line of Dec NC price over bar of volatilities
ggplot(corn)  + 
  geom_bar(aes(x=Date, y=volatilityDec*max(corn$DecNC), fill = "filler"), stat="identity", color = "#5c5c5c")+
  geom_line(aes(x=Date, y=DecNC, color = "lines"), stat="identity", size = 1.5)+
  scale_y_continuous(sec.axis = sec_axis(~./max(corn$DecNC))) + 
  labs(x = "Date", y = "Price", title = "Corn - December") + 
  scale_fill_manual("", values="#5c5c5c", labels = c("Volatility")) + 
  scale_color_manual("", values=c(lines = "red"), labels = c("Price")) + 
  theme(axis.text.x = element_text(face="bold", color="black", size=14),
        axis.text.y = element_text(face="bold", color="black", size=14),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"))

# Line of Nearby OC price over bar of volatilities
ggplot(corn)  + 
  geom_bar(aes(x=Date, y=volatilityNearby*max(corn$NearbyOC), fill = "filler"), stat="identity", color = "#5c5c5c")+
  geom_line(aes(x=Date, y=NearbyOC, color = "lines"), stat="identity", size = 1.5)+
  scale_y_continuous(sec.axis = sec_axis(~./max(corn$NearbyOC))) + 
  labs(x = "Date", y = "Price", title = "Corn - Nearby") + 
  scale_fill_manual("", values="#5c5c5c", labels = c("Volatility")) + 
  scale_color_manual("", values=c(lines = "red"), labels = c("Price")) + 
  theme(axis.text.x = element_text(face="bold", color="black", size=14),
        axis.text.y = element_text(face="bold", color="black", size=14),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"))



# cor(corn$DecNC[1:253], corn$volatilityDec[1:253])
# 
# cor(corn$NearbyOC[254:420], corn$volatilityNearby[254:420])
# 
# cor(corn$DecNC[254:505], corn$volatilityDec[254:505])




ggplotly(ggscatter(corn, x = "DecNC", y = "volatilityDec", 
                   add = "reg.line", conf.int = TRUE, 
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "December Price", ylab = "December Volatilty"))


corn$DecLower = corn$DecNC + corn$volatilityDec
corn$DecUpper = corn$DecNC - corn$volatilityDec
corn$NearbyLower = corn$NearbyOC + corn$volatilityNearby
corn$NearbyUpper = corn$NearbyOC - corn$volatilityNearby

ggplotly(ggplot(corn) + 
           geom_line(aes(x = Date, y = DecNC), size = 0.5, linetype = 1) + 
           geom_ribbon(aes(x = Date, ymin = corn$DecLower, ymax = corn$DecUpper), linetype = 2, alpha=0.5) + 
           labs(x = "Date", y = "Price", title = "Corn - December"))

ggplot(corn) + 
  geom_line(aes(x = Date, y = NearbyOC), size = 0.5, linetype = 1) + 
  geom_ribbon(aes(x = Date, ymin = corn$NearbyLower, ymax = corn$NearbyUpper), linetype = 2, alpha=0.5) + 
  labs(x = "Date", y = "Price", title = "Corn - Nearby")


cor(corn$volatilityDec, corn$volatilityNearby) # 0.6856268

cor.test(corn$volatilityDec, corn$volatilityNearby,  method="kendall")

library("ggpubr")
ggplotly(ggscatter(corn, x = "volatilityDec", y = "volatilityNearby", 
                   add = "reg.line", conf.int = TRUE, 
                   cor.coef = TRUE, cor.method = "spearman",
                   xlab = "December", ylab = "Nearby"))


# ggplot(corn) + 
#   geom_line(aes(x = Date, y = NearbyOC), size = 0.5, linetype = 1) + 
#   geom_line(aes(x = Date, y = volatilityNearby), size = 0.5, linetype = 1)

which(corn$Date == "2010-1-4")

which(corn$Date == "2010-3-10")
which(corn$Date == "2010-3-22")
which(corn$Date == "2010-6-10")
which(corn$Date == "2010-6-21")

which(corn$Date == "2010-12-31")
which(corn$Date == "2011-1-3")

which(corn$Date == "2011-3-10")
which(corn$Date == "2011-3-21")
which(corn$Date == "2011-6-10")
which(corn$Date == "2011-6-20")

which(corn$Date == "2011-12-30")

ggplot(corn[506:757,]) + 
  geom_line(aes(x = Date, y = DecNC), size = 1, linetype = 1) + 
  geom_ribbon(aes(x = Date, ymin = DecNC - volatilityDec, ymax = DecNC + volatilityDec), linetype = 2, alpha = 0.5) + 
  geom_point(aes(x = corn$Date[551], y = corn$DecNC[551]), size = 4, color = "yellow") + 
  geom_point(aes(x = corn$Date[559], y = corn$DecNC[559]), size = 4, color = "yellow") + 
  geom_point(aes(x = corn$Date[615], y = corn$DecNC[615]), size = 4, color = "yellow") + 
  geom_point(aes(x = corn$Date[622], y = corn$DecNC[622]), size = 4, color = "yellow") + 
  labs(x = "Date", y = "Price", title = "Corn - December") + 
  theme(axis.text.x = element_text(face="bold", color="black", size=14),
        axis.text.y = element_text(face="bold", color="black", size=14),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"))

ggplot(corn[757:1009,]) + 
  geom_line(aes(x = Date, y = DecNC), size = 1, linetype = 1) + 
  geom_ribbon(aes(x = Date, ymin = DecNC - volatilityDec, ymax = DecNC + volatilityDec), linetype = 2, alpha = 0.5) + 
  geom_point(aes(x = corn$Date[804], y = corn$DecNC[804]), size = 4, color = "yellow") + 
  geom_point(aes(x = corn$Date[811], y = corn$DecNC[811]), size = 4, color = "yellow") + 
  geom_point(aes(x = corn$Date[868], y = corn$DecNC[868]), size = 4, color = "yellow") + 
  geom_point(aes(x = corn$Date[874], y = corn$DecNC[874]), size = 4, color = "yellow") + 
  labs(x = "Date", y = "Price", title = "Corn - December") + 
  theme(axis.text.x = element_text(face="bold", color="black", size=14),
        axis.text.y = element_text(face="bold", color="black", size=14),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"))


ggplot(corn) + 
  geom_line(aes(x = Date, y = NearbyOC), size = 1, linetype = 1) + 
  geom_ribbon(aes(x = Date, ymin = corn$NearbyOC - corn$volatilityNearby, ymax = corn$NearbyOC + corn$volatilityNearby), linetype = 2, alpha = 0.5) + 
  labs(x = "Date", y = "Price", title = "Corn - Nearby")









################################################################################################



# By year
averageVolatility = data.frame(aggregate(corn[,5:6], list(year(corn$Date)), mean))
colnames(averageVolatility) = c("year", "DecNC", "NearbyOC")

USDA = c(3.884433052,
         3.76424926,
         6.221969157,
         6.471576426,
         6.946013339,
         4.397678115,
         3.768976721,
         3.700408337,
         3.586389526)

USDA = data.frame(Strategy = "USDA", Price = USDA, Year = averageVolatility$year[1:9])



plot1 = ggplot(data = averageVolatility[1:9,]) + 
  geom_line(aes(x = as.factor(year[1:9]), y = DecNC[1:9], color = "one"), group = 1, size = 1.5) +
  geom_line(aes(x = as.factor(year[1:9]), y = NearbyOC[1:9], color = "two"), group = 1, size = 1.5) + 
  scale_color_manual("Legend", values=c(one = "red", two = "blue"), labels = c("DecNC", "NearbyOC")) + 
  labs(x = "Year", y = "Volatility") + 
  theme(axis.text.x = element_text(face="bold", color="black", size=14),
        axis.text.y = element_text(face="bold", color="black", size=14),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"))



plot2 = ggplot(data = USDA) + 
  geom_line(aes(x = as.factor(Year), y = Price, color = "USDA"), group = 1, size = 1.5) + 
  scale_color_manual("Legend", values=c(USDA = "black"), labels = c("USDA")) + 
  labs(x = "Year", y = "Average Price") + 
  theme(axis.text.x = element_text(face="bold", color="black", size=14),
        axis.text.y = element_text(face="bold", color="black", size=14),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"))


grid.arrange(plot1, plot2, nrow=2, ncol=1, top = "Corn")


################################################################################################


# By month
averageVolatility = data.frame(aggregate(corn[,5:6], list(month(corn$Date)), mean))
colnames(averageVolatility) = c("month", "DecNC", "NearbyOC")

USDA = c(4.60,
         4.44,
         4.45,
         4.49,
         4.59,
         4.62,
         4.64,
         4.71,
         4.73,
         4.72,
         4.70,
         4.61)

USDA = data.frame(Strategy = "USDA", Price = USDA, month = averageVolatility$month)


plot1 = ggplot(data = averageVolatility) + 
  geom_line(aes(x = as.factor(month), y = DecNC, color = "one"), group = 1) +
  geom_line(aes(x = as.factor(month), y = NearbyOC, color = "two"), group = 1) + 
  scale_color_manual("Legend", values=c(one = "red", two = "blue"), labels = c("DecNC", "NearbyOC")) + 
  labs(x = "Month", y = "Volatility")

plot2 = ggplot(data = USDA) + 
  geom_line(aes(x = as.factor(month), y = Price, color = "USDA"), group = 1) + 
  scale_color_manual("Legend", values=c(USDA = "black"), labels = c("USDA")) + 
  labs(x = "Month", y = "Average Price")

grid.arrange(plot1, plot2, nrow=2, ncol=1, top = "Corn")






