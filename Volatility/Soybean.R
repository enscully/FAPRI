# Volatility
# Soybeans

library(lubridate)
library(ggplot2)
library(zoo)
library(plotly)
library(readr)
library(PerformanceAnalytics)
library(ggpubr)
library(gridExtra)

futuresMarket <- read_csv("C:/Users/ensxvd/Desktop/main-model/Data/Soybean_FuturesMarket.csv")

soybean = futuresMarket[,c(1,2,3)]
soybean$Date = mdy(soybean$Date)

soybean$Interval = NA
soybean$Interval[1] = 1
count = 1

for(row in 2:nrow(soybean)){
  if(month(soybean$Date[row - 1]) !=  month(soybean$Date[row])){
    count = count + 1
  }
  
  soybean$Interval[row] = count
}

intervalRows = list()

for(i in 1:max(soybean$Interval)){
  intervalRows[[i]] = which(soybean$Interval == i)
}

soybean$volatilityNov = NA
soybean$volatilityNearby = NA
soybean$monthYear = NA

for(i in 1:length(intervalRows)){
  soybean$volatilityNov[intervalRows[[i]]] = as.numeric(tapply(soybean$NovNC, soybean$Interval, sd)[i])
  soybean$volatilityNearby[intervalRows[[i]]] = as.numeric(tapply(soybean$NearbyOC, soybean$Interval, sd)[i])
  
  soybean$monthYear[intervalRows[[i]]] = paste(year(soybean$Date[intervalRows[[i]]]), month(soybean$Date[intervalRows[[i]]]), sep = "-")
}

novVolatility = data.frame(volatility = unique(soybean$volatilityNov),Date = unique(as.yearmon(soybean$monthYear)))
nearbyVolatility = data.frame(volatility = unique(soybean$volatilityNearby), Date = unique(as.yearmon(soybean$monthYear)))


# Plot of New crop volatilities only
ggplot(data = novVolatility, aes(x = as.factor(Date), y = volatility)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(x = "Month", y = "Volatility (Cents)", title = "November")

# Plot of old crop volatilities only
ggplot(data = nearbyVolatility, aes(x = as.factor(Date), y = volatility)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(x = "Month", y = "Volatility (Cents)", title = "Nearby")


# Line of Nov NC price over bar of volatilities
ggplot(soybean)  + 
  geom_bar(aes(x=Date, y=volatilityNov*max(soybean$NovNC), fill = "filler"), stat="identity", color = "#5c5c5c")+
  geom_line(aes(x=Date, y=NovNC, color = "lines"), stat="identity", size = 1.5)+
  scale_y_continuous(sec.axis = sec_axis(~./max(soybean$NovNC))) + 
  labs(x = "Date", y = "Price", title = "Soybean - November") + 
  scale_fill_manual("", values="#5c5c5c", labels = c("Volatility")) + 
  scale_color_manual("", values=c(lines = "red"), labels = c("Price")) + 
  theme(axis.text.x = element_text(face="bold", color="black", size=14),
        axis.text.y = element_text(face="bold", color="black", size=14),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"))

# Line of Nearby OC price over bar of volatilities
ggplot(soybean)  + 
  geom_bar(aes(x=Date, y=volatilityNearby*max(soybean$NearbyOC), fill = "filler"), stat="identity", color = "#5c5c5c")+
  geom_line(aes(x=Date, y=NearbyOC, color = "lines"), stat="identity", size = 1.5)+
  scale_y_continuous(sec.axis = sec_axis(~./max(soybean$NearbyOC))) + 
  labs(x = "Date", y = "Price", title = "Soybean - Nearby") + 
  scale_fill_manual("", values="#5c5c5c", labels = c("Volatility")) + 
  scale_color_manual("", values=c(lines = "red"), labels = c("Price")) + 
  theme(axis.text.x = element_text(face="bold", color="black", size=14),
        axis.text.y = element_text(face="bold", color="black",  size=14),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"))



# cor(soybean$NovNC[1:253], soybean$volatilityNov[1:253])
# 
# cor(soybean$NearbyOC[254:420], soybean$volatilityNearby[254:420])
# 
# cor(soybean$NovNC[254:505], soybean$volatilityNov[254:505])



# Scatter/ corralation plot
ggplotly(ggscatter(soybean, x = "NovNC", y = "volatilityNov", 
                   add = "reg.line", conf.int = TRUE, 
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "November Price", ylab = "November Volatilty"))

















soybean$NovLower = soybean$NovNC + soybean$volatilityNov
soybean$NovUpper = soybean$NovNC - soybean$volatilityNov
soybean$NearbyLower = soybean$NearbyOC + soybean$volatilityNearby
soybean$NearbyUpper = soybean$NearbyOC - soybean$volatilityNearby


# Nov NC plot with band 1 SD away
ggplotly(ggplot(soybean) + 
           geom_line(aes(x = Date, y = NovNC), size = 0.5, linetype = 1) + 
           geom_ribbon(aes(x = Date, ymin = soybean$NovLower, ymax = soybean$NovUpper), linetype = 2, alpha=0.5) + 
           labs(x = "Date", y = "Price", title = "Soybean - November"))

# Nearby OC plot with band 1 SD away
ggplot(soybean) + 
  geom_line(aes(x = Date, y = NearbyOC), size = 0.5, linetype = 1) + 
  geom_ribbon(aes(x = Date, ymin = soybean$NearbyLower, ymax = soybean$NearbyUpper), linetype = 2, alpha=0.5) + 
  labs(x = "Date", y = "Price", title = "Soybean - Nearby")



















cor(soybean$volatilityNov, soybean$volatilityNearby) # 0.7192962


# scatter/ correlation of nearby vs Nov volatilities
ggplotly(ggscatter(soybean, x = "volatilityNov", y = "volatilityNearby", 
                   add = "reg.line", conf.int = TRUE, 
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "November", ylab = "Nearby"))






# ggplot(soybean) + 
#   geom_line(aes(x = Date, y = NearbyOC), size = 0.5, linetype = 1) + 
#   geom_line(aes(x = Date, y = volatilityNearby), size = 0.5, linetype = 1)





which(soybean$Date == "2010-1-4")

which(soybean$Date == "2010-5-10")
which(soybean$Date == "2010-5-20")
which(soybean$Date == "2010-7-12")
which(soybean$Date == "2010-7-20")

which(soybean$Date == "2010-12-31")
which(soybean$Date == "2011-1-3")

which(soybean$Date == "2011-5-10")
which(soybean$Date == "2011-5-20")
which(soybean$Date == "2011-7-11")
which(soybean$Date == "2011-7-20")

which(soybean$Date == "2011-12-30")

ggplot(soybean[506:757,]) + 
  geom_line(aes(x = Date, y = NovNC), size = 1, linetype = 1) + 
  geom_ribbon(aes(x = Date, ymin = NovNC - volatilityNov, ymax = NovNC + volatilityNov), linetype = 2, alpha = 0.5) + 
  geom_point(aes(x = soybean$Date[593], y = soybean$NovNC[593]), size = 4, color = "yellow") + 
  geom_point(aes(x = soybean$Date[601], y = soybean$NovNC[601]), size = 4, color = "yellow") + 
  geom_point(aes(x = soybean$Date[636], y = soybean$NovNC[636]), size = 4, color = "yellow") + 
  geom_point(aes(x = soybean$Date[642], y = soybean$NovNC[642]), size = 4, color = "yellow") + 
  labs(x = "Date", y = "Price", title = "Soybeans - November") + 
  theme(axis.text.x = element_text(face="bold", color="black", size=14),
        axis.text.y = element_text(face="bold", color="black", size=14),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"))

ggplot(soybean[758:1009,]) + 
  geom_line(aes(x = Date, y = NovNC), size = 1, linetype = 1) + 
  geom_ribbon(aes(x = Date, ymin = NovNC - volatilityNov, ymax = NovNC + volatilityNov), linetype = 2, alpha = 0.5) + 
  geom_point(aes(x = soybean$Date[846], y = soybean$NovNC[846]), size = 4, color = "yellow") + 
  geom_point(aes(x = soybean$Date[854], y = soybean$NovNC[854]), size = 4, color = "yellow") + 
  geom_point(aes(x = soybean$Date[888], y = soybean$NovNC[888]), size = 4, color = "yellow") + 
  geom_point(aes(x = soybean$Date[895], y = soybean$NovNC[895]), size = 4, color = "yellow") + 
  labs(x = "Date", y = "Price", title = "Soybeans - November") + 
  theme(axis.text.x = element_text(face="bold", color="black", size=14),
        axis.text.y = element_text(face="bold", color="black", size=14),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"))












# Nov NC plot with band 1,2,3 SD away
ggplotly(ggplot(soybean) + 
           geom_line(aes(x = Date, y = NovNC), size = 1, linetype = 1) + 
           geom_ribbon(aes(x = Date, ymin = soybean$NovNC - soybean$volatilityNov, ymax = soybean$NovNC + soybean$volatilityNov), linetype = 2, alpha=0.5) + 
           labs(x = "Date", y = "Price", title = "Soybean - November"))




# Nearby OC plot with band 1,2,3 SD away
ggplotly(ggplot(soybean) + 
           geom_line(aes(x = Date, y = NearbyOC), size = 1, linetype = 1) + 
           geom_ribbon(aes(x = Date, ymin = soybean$NearbyOC - soybean$volatilityNearby * 3, ymax = soybean$NearbyOC + soybean$volatilityNearby * 3), linetype = 2, alpha=0.25) + 
           
           geom_line(aes(x = Date, y = soybean$NearbyOC + soybean$volatilityNearby * 2), linetype = 2, alpha=0.5, col = "blue") +
           geom_line(aes(x = Date, y = soybean$NearbyOC - soybean$volatilityNearby * 2), linetype = 2, alpha=0.5, col = "blue") +
           geom_line(aes(x = Date, y = soybean$NearbyOC + soybean$volatilityNearby * 3), linetype = 2, alpha=0.5, col = "forestgreen") +
           geom_line(aes(x = Date, y = soybean$NearbyOC - soybean$volatilityNearby * 3), linetype = 2, alpha=0.5, col = "forestgreen") +           
           
           labs(x = "Date", y = "Price", title = "Soybean - Nearby"))









################################################################################################





# By year
averageVolatility = data.frame(aggregate(soybean[,5:6], list(year(soybean$Date)), mean))
colnames(averageVolatility) = c("year", "NovNC", "NearbyOC")

USDA = c(9.875139584,
         9.82318177,
         12.63564477,
         12.84459206,
         14.88573776,
         13.18335991,
         9.901857016,
         9.170431916,
         9.943178293)

USDA = data.frame(Strategy = "USDA", Price = USDA, Year = averageVolatility$year[1:9])

plot1 = ggplot(data = averageVolatility[1:9,]) + 
  geom_line(aes(x = as.factor(year[1:9]), y = NovNC[1:9], color = "one"), group = 1, size = 1.5) +
  geom_line(aes(x = as.factor(year[1:9]), y = NearbyOC[1:9], color = "two"), group = 1, size = 1.5) + 
  scale_color_manual("Legend", values=c(one = "red", two = "blue"), labels = c("NovNC", "NearbyOC")) + 
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

grid.arrange(plot1, plot2, nrow=2, ncol=1, top = "Soybeans")




############################################################################################





# By month
averageVolatility = data.frame(aggregate(soybean[,5:6], list(month(soybean$Date)), mean))
colnames(averageVolatility) = c("month", "NovNC", "NearbyOC")

USDA = c(11.08,
         10.68,
         10.79,
         10.90,
         11.02,
         11.10,
         11.18,
         11.43,
         11.69,
         11.81,
         11.91,
         11.76)

USDA = data.frame(Strategy = "USDA", Price = USDA, month = averageVolatility$month)

plot1 = ggplot(data = averageVolatility) + 
  geom_line(aes(x = as.factor(month), y = NovNC, color = "one"), group = 1) +
  geom_line(aes(x = as.factor(month), y = NearbyOC, color = "two"), group = 1) + 
  scale_color_manual("Legend", values=c(one = "red", two = "blue"), labels = c("NovNC", "NearbyOC")) + 
  labs(x = "Month", y = "Volatility")

plot2 = ggplot(data = USDA) + 
  geom_line(aes(x = as.factor(month), y = Price, color = "USDA"), group = 1) + 
  scale_color_manual("Legend", values=c(USDA = "black"), labels = c("USDA")) + 
  labs(x = "Month", y = "Average Price")

grid.arrange(plot1, plot2, nrow=2, ncol=1, top = "Soybeans")
