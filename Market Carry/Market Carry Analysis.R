
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)
library(plotly)
library(DescTools)



################################################################################################################
# Soybeans Load/Clean Data
################################################################################################################

futuresMarket = read.csv("C:/Users/ensxvd/Desktop/main-model/Data/Soybean_FuturesMarket.csv")
Basis = read.csv("C:/Users/ensxvd/Desktop/main-model/Data/Soybean_Basis.csv")

futuresMarket$Date = mdy(futuresMarket$Date)

noMarketing = which(month(futuresMarket$Date) %in% c(9, 10, 11, 12))

futuresMarketSelect = futuresMarket[, c("Date", "NovNC", "MarNC")]
futuresMarketSelect[noMarketing, c("NovNC", "MarNC")] = NA

futuresMarketLong <- melt(futuresMarketSelect,  id.vars = 'Date', variable.name = 'series')

# plot on same grid, each series colored differently -- 
# good if the series have same scale
ggplotly(ggplot(futuresMarketLong, aes(Date,value)) + 
           geom_line(aes(colour = series)))

# Poisitive = More gains from the march contract than the november contract
futuresMarketSelect$Difference = futuresMarketSelect$MarNC - futuresMarketSelect$NovNC

# jan = futuresMarketSelect
# jan[which(month(jan$Date) != 1), ] = NA
# jan$Date = futuresMarketSelect$Date
# 
# feb = futuresMarketSelect
# feb[which(month(feb$Date) != 2), ] = NA
# feb$Date = futuresMarketSelect$Date
# 
# mar = futuresMarketSelect
# mar[which(month(mar$Date) != 3), ] = NA
# mar$Date = futuresMarketSelect$Date
# 
# may = futuresMarketSelect
# may[which(month(may$Date) != 5), ] = NA
# may$Date = futuresMarketSelect$Date

ggplot(futuresMarketSelect, aes(Date,Difference)) + 
  geom_line() + 
  # geom_line(data = may, color = "red") + 
  geom_hline(yintercept = 0) + 
  scale_x_date(date_labels = "%Y",date_breaks = "1 year")


####################################################################################
# Find Mean and Median


# 0.04351029
rawAvg = mean(futuresMarketSelect$Difference, na.rm = TRUE)

ggplot(futuresMarketSelect, aes(Date,Difference)) + 
  geom_line() + 
  geom_hline(yintercept = 0) + 
  geom_hline(yintercept = rawAvg, color = "red") + 
  scale_x_date(date_labels = "%Y",date_breaks = "1 year")

OL = which(abs(futuresMarketSelect$Difference) > 0.75)
futuresMarketSelectRMOL = futuresMarketSelect
futuresMarketSelectRMOL$Difference[OL] = NA

# 0.06139867
adjAvg = mean(futuresMarketSelectRMOL$Difference, na.rm = TRUE)

ggplot(futuresMarketSelect, aes(Date,Difference)) + 
  geom_line() + 
  geom_hline(yintercept = 0) + 
  geom_hline(aes(yintercept = rawAvg, color = "Raw Average")) + 
  geom_hline(aes(yintercept = adjAvg, color = "Adj. Average")) + 
  scale_x_date(date_labels = "%Y",date_breaks = "1 year") + 
  labs(title = "Soybeans - Averages", y = "Difference (March - Nov)", x = "Year") + 
  scale_colour_manual("", breaks = c("Raw Average", "Adj. Average"), values = c("green", "red"))


# Median Absolute Deviation
outliersMAD <- function(data, MADCutOff = 2.5, replace = NA, values = FALSE, bConstant = 1.4826, digits = 8) {
  #compute number of absolute MADs away for each value
  #formula: abs( ( x - median(x) ) )/ mad(x)
  absMADAway <- abs((data - median(data, na.rm = T))/mad(data, constant = bConstant, na.rm = T))
  #subset data that has absMADAway greater than the MADCutOff and replace them with replace
  #can also replace values other than replace
  data[absMADAway > MADCutOff] <- replace
  
  if (values == TRUE) { 
    return(round(absMADAway, digits)) #if values == TRUE, return number of mads for each value
  } else {
    return(round(data, digits)) #otherwise, return values with outliers replaced
  }
}

futuresMarketSelect$DifferenceLessOL = outliersMAD(futuresMarketSelect$Difference)

# 0.08927788
# 0.0278792 different from the adjusted average
adjAvgLessOL = mean(futuresMarketSelect$DifferenceLessOL, na.rm = TRUE)

ggplot(futuresMarketSelect) + 
  geom_line(aes(Date,Difference)) + 
  geom_line(aes(Date,DifferenceLessOL), color = "green") + 
  geom_hline(yintercept = 0) + 
  scale_x_date(date_labels = "%Y",date_breaks = "1 year")


# 0.0875
med = median(futuresMarketSelect$Difference, na.rm = TRUE)

ggplot(futuresMarketSelect) + 
  geom_line(aes(Date,Difference)) + 
  geom_hline(yintercept = med, color = "green") + 
  geom_hline(yintercept = adjAvg, color = "red") + 
  geom_hline(yintercept = 0) + 
  scale_x_date(date_labels = "%Y",date_breaks = "1 year")

# 0.08999987
adjMed = median(futuresMarketSelectRMOL$Difference, na.rm = TRUE)

ggplot(futuresMarketSelect, aes(Date,Difference)) + 
  geom_line() + 
  geom_hline(yintercept = 0) + 
  geom_hline(aes(yintercept = med, color = "Raw Median")) + 
  geom_hline(aes(yintercept = adjAvg, color = "Adj. Average")) + 
  scale_x_date(date_labels = "%Y",date_breaks = "1 year") + 
  labs(title = "Soybeans - Mean and Median", y = "Difference (March - Nov)", x = "Year") + 
  scale_colour_manual("", breaks = c("Raw Median", "Adj. Average"), values = c("green", "red"))

# 0.095
adjMedLessOL = median(futuresMarketSelect$DifferenceLessOL, na.rm = TRUE)

# ####################################################################################
# # Graphing Standard Errors
# 
# 
# stderror = sd(futuresMarketSelect$Difference, na.rm = TRUE) / 
#   sqrt(length(futuresMarketSelect$Difference[!is.na(futuresMarketSelect$Difference)]))
# 
# 
# futuresMarketSelect$ymin = futuresMarketSelect$Difference - stderror
# futuresMarketSelect$ymax = futuresMarketSelect$Difference + stderror
# 
# ggplot(futuresMarketSelect, aes(Date,Difference)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = ymin,
#                   ymax = ymax), alpha = 1, color = "green")


####################################################################################
# Graphing Standard Deviations

# 0.1956964
stdDev = sd(futuresMarketSelect$Difference, na.rm = TRUE)

ggplot(futuresMarketSelect, aes(Date,Difference)) +
  geom_line() +
  geom_hline(yintercept = adjAvg, color = "red") + 
  geom_ribbon(aes(ymin = Difference + 1 * sd(Difference, na.rm = TRUE),
                  ymax = Difference - 1 * sd(Difference, na.rm = TRUE)), alpha = 0.1, color = "gray")



####################################################################################
# Calculating cost to carry


# full carry = [(price * interest rate)/12 + (monthly storage rate)] * # of months
# Gain on march futures/full carry should be greater than 1 (100%)





interestRate = 0.055
monthlyStorageRate = 0.05
numMonths = 3
# price = 5

futuresMarketSelect$fullCarry = ((futuresMarketSelect$NovNC * interestRate)/12 + (monthlyStorageRate)) * numMonths


ggplot(data = futuresMarketSelect) + 
  geom_line(aes(x = Date, y = Difference)) + 
  geom_line(aes(x = Date, y = fullCarry), color = "red")



futuresMarketSelect$percent = (futuresMarketSelect$Difference / futuresMarketSelect$fullCarry) * 100


ggplot(data = futuresMarketSelect) + 
  geom_line(aes(x = Date, y = percent)) + 
  geom_hline(yintercept = 0) + 
  scale_x_date(date_labels = "%Y",date_breaks = "1 year") + 
  labs(title = "Soybeans - Cost to Carry", y = "Cost to Carry (Percentage)", x = "Year")

# 0.7921999% Of points are >= 100
(length(which(futuresMarketSelect$percent >= 100)) / nrow(na.omit(futuresMarketSelect))) * 100



# ####################################################################################
# # Calculating cost to carry with basis
# 
# 
# for (i in 1:nrow(futuresMarketSelect)) {
#   if (month(futuresMarketSelect$Date)[i] < 9) {  
#     futuresMarketSelect$Basis[i] = Basis$Basis[which(Basis$CropYearStart == year(futuresMarketSelect$Date)[i] - 1)]
#   }
#   else {
#     futuresMarketSelect$Basis[i] = NA
#   }
# }
# 
# futuresMarketSelect$NovNCBasis = futuresMarketSelect$NovNC - futuresMarketSelect$Basis
# futuresMarketSelect$MarNCBasis = futuresMarketSelect$MarNC - futuresMarketSelect$Basis
# 
# futuresMarketSelect$fullCarryBasis = ((futuresMarketSelect$NovNCBasis * interestRate)/12 + (monthlyStorageRate)) * numMonths
# 
# ggplot(data = futuresMarketSelect) + 
#   geom_line(aes(x = Date, y = DifferenceBasis)) + 
#   geom_line(aes(x = Date, y = fullCarryBasis), color = "red")
# 
# futuresMarketSelect$percentBasis = (futuresMarketSelect$Difference / futuresMarketSelect$fullCarryBasis) * 100
# 
# ggplot(data = futuresMarketSelect) + 
#   geom_line(aes(x = Date, y = percentBasis)) + 
#   geom_hline(yintercept = 0)


####################################################################################
# Finding minimum values





getGM = function(price) {
  GM = 4.2 * (((0.055 * price) / 12) + 0.05)
  return(GM)
}

getGMMin = function(price) {
  GM = 3 * (((0.055 * price) / 12) + 0.05)
  return(GM)
}

GMdf = data.frame(prices = seq(0, 20, by = 0.005), GM = NA)

GMdf$GM = getGM(GMdf$prices)

ggplot() + 
  geom_line(data = GMdf, aes(x = prices, y = GM))


futuresMarketSelect$GM = getGM(futuresMarketSelect$NovNC)
futuresMarketSelect$GMMin = getGMMin(futuresMarketSelect$NovNC)

ggplot(data = futuresMarketSelect) + 
  geom_line(aes(x = Date, y = Difference, color = "Difference")) +
  geom_line(aes(x = Date, y = GM, color = "140%")) + 
  geom_line(aes(x = Date, y = GMMin, color = "100%")) +
  geom_hline(yintercept = 0) + 
  scale_x_date(date_labels = "%Y",date_breaks = "1 year") + 
  labs(title = "Soybeans - Cost to Carry", y = "Difference (March - Nov)", x = "Year") + 
  scale_colour_manual("", breaks = c("Difference", "140%", "100%"), values = c("green", "red", "black"))


# fullCarryDf = data.frame(prices = seq(0, 20, by = 0.01), fullCarry = NA)
# 
# getFullCarry = function(price) {
#   fullCarry = ((price * interestRate)/12 + (monthlyStorageRate)) * numMonths
#   return(fullCarry)
# }
# 
# fullCarryDf$fullCarry = getFullCarry(fullCarryDf$prices)
# 
# fullCarryDf$percent = (GMdf$GM / fullCarryDf$fullCarry) * 100






getPrice = function(GM) {
  price = (12 / 0.055) * ((GM / 4.2) - 0.05)
  return(price)
}

priceDf = data.frame(GM = seq(0, 1, by = 0.01), price = NA)

priceDf$price = getPrice(priceDf$GM)


ggplot() + 
  geom_line(data = GMdf, aes(x = prices, y = GM)) + 
  geom_line(data = priceDf, aes(x = price, y = GM), color = "red")

####################################################################################
# Organize and aggregate data

Skew(futuresMarketSelect$Difference, na.rm = TRUE)
Kurt(futuresMarketSelect$Difference, na.rm = TRUE)


futuresMarketSelectNoYear = futuresMarketSelect



futuresMarketSelectNoYear = cbind(futuresMarket[, c("Date", "NovNC", "MarNC")], percent = futuresMarketSelect$percent)
futuresMarketSelectNoYear$Difference = futuresMarketSelectNoYear$MarNC - futuresMarketSelectNoYear$NovNC


POMarSales = c("06-05",
               "04-02",
               "06-04",
               "06-17",
               "04-21",
               "06-19",
               "07-25",
               "01-07",
               "08-23",
               "02-21",
               "07-28",
               "07-29",
               "08-15")



futuresMarketSelectNoYear$Date = format(futuresMarketSelectNoYear$Date, "%m-%d")

futuresMarketSelectTest = aggregate(futuresMarketSelectNoYear[c("Difference", "percent")], by = futuresMarketSelectNoYear["Date"], mean)
# futuresMarketSelectTest = aggregate(futuresMarketSelectNoYear[c("Difference")], by = futuresMarketSelectNoYear["Date"], mean)

futuresMarketSelectTest$ID = as.numeric(rownames(futuresMarketSelectTest))

futuresMarketSelectTest$POMarSale = "F"

for (i in 1:length(POMarSales)) {
  futuresMarketSelectTest$POMarSale[which(futuresMarketSelectTest$Date == POMarSales[i])] = "T"
}

POMar = data.frame(futuresMarketSelectTest[which(futuresMarketSelectTest$POMarSale == "T"), ])


####################################################################################
# Cross validate span choice or difference

set.seed(4)
x <- futuresMarketSelectTest$ID[1:317]
y <- futuresMarketSelectTest$Difference[1:317]
plot(x, y)
df <- data.frame(x, y)


span.seq <- seq(from = 0.15, to = 0.95, by = 0.05) #explores range of spans
k <- 10 #number of folds
set.seed(1) # replicate results
folds <- sample(x = 1:k, size = length(x), replace = TRUE)
cv.error.mtrx <- matrix(rep(x = NA, times = k * length(span.seq)), 
                        nrow = length(span.seq), ncol = k)

for (i in 1:length(span.seq)) {
  for (j in 1:k) {
    loess.fit <- loess(formula = y ~ x, data = df[folds != j, ], span = span.seq[i])
    preds <- predict(object = loess.fit, newdata = df[folds == j, ])
    cv.error.mtrx[i, j] <- mean((df$y[folds == j] - preds)^2, na.rm = TRUE)
  }
}

cv.errors <- rowMeans(cv.error.mtrx)

best.span.i <- which.min(cv.errors)
best.span.i
span.seq[best.span.i]

plot(x = span.seq, y = cv.errors, type = "l", main = "CV Plot")
points(x = span.seq, y = cv.errors, 
       pch = 20, cex = 0.75, col = "blue")
points(x = span.seq[best.span.i], y = cv.errors[best.span.i], 
       pch = 20, cex = 1, col = "red")

best.loess.fit <- loess(formula = y ~ x, data = df, 
                        span = span.seq[best.span.i])

x.seq <- seq(from = min(x), to = max(x), length = 100)

plot(x = df$x, y = df$y, main = "Best Span Plot")
lines(x = x.seq, y = predict(object = best.loess.fit, 
                             newdata = data.frame(x = x.seq)), 
      col = "red", lwd = 2)

#0.35 Best Span
####################################################################################
# Graph Difference with LOESS

ggplot(data = na.omit(futuresMarketSelectTest[1:317, c("ID", "Difference")]), aes(x = ID, y = Difference)) +
  geom_point() +
  geom_vline(xintercept = 125) + # May 5th
  geom_vline(xintercept = 235) + # Aug 24th
  # geom_vline(xintercept = 242, color = "forestgreen") + # June 14th
  geom_hline(yintercept = 0) +
  
  # geom_point(data = POMar, aes(x = ID, y = Difference), color = "red", size = 2) + 
  
  scale_x_continuous(breaks = seq(0, 317, 5), lim = c(0, 317)) +
  geom_smooth(method = "loess", se = TRUE, span = .35) + 
  annotate("text", x = 60, y = -.1, label = "Jan 1 - May 5", color = "red", size = 10) + 
  annotate("text", x = 170, y = -.1, label = "May 5 - Aug 24", color = "red", size = 10) + 
  annotate("text", x = 270, y = -.1, label = "Aug 24 - Nov 14", color = "red", size = 10)



####################################################################################
# Cross validate span choice of percent

set.seed(4)
x <- futuresMarketSelectTest$ID[1:317]
y <- futuresMarketSelectTest$percent[1:317]
plot(x, y)
df <- data.frame(x, y)


span.seq <- seq(from = 0.15, to = 0.95, by = 0.05) #explores range of spans
k <- 10 #number of folds
set.seed(1) # replicate results
folds <- sample(x = 1:k, size = length(x), replace = TRUE)
cv.error.mtrx <- matrix(rep(x = NA, times = k * length(span.seq)), 
                        nrow = length(span.seq), ncol = k)

for (i in 1:length(span.seq)) {
  for (j in 1:k) {
    loess.fit <- loess(formula = y ~ x, data = df[folds != j, ], span = span.seq[i])
    preds <- predict(object = loess.fit, newdata = df[folds == j, ])
    cv.error.mtrx[i, j] <- mean((df$y[folds == j] - preds)^2, na.rm = TRUE)
  }
}

cv.errors <- rowMeans(cv.error.mtrx)

best.span.i <- which.min(cv.errors)
best.span.i
span.seq[best.span.i]

plot(x = span.seq, y = cv.errors, type = "l", main = "CV Plot")
points(x = span.seq, y = cv.errors, 
       pch = 20, cex = 0.75, col = "blue")
points(x = span.seq[best.span.i], y = cv.errors[best.span.i], 
       pch = 20, cex = 1, col = "red")

best.loess.fit <- loess(formula = y ~ x, data = df, 
                        span = span.seq[best.span.i])

x.seq <- seq(from = min(x), to = max(x), length = 100)

plot(x = df$x, y = df$y, main = "Best Span Plot")
lines(x = x.seq, y = predict(object = best.loess.fit, 
                             newdata = data.frame(x = x.seq)), 
      col = "red", lwd = 2)

# 0.9 Best Span

####################################################################################
# Graph percent with LOESS


ggplot(data = na.omit(futuresMarketSelectTest[1:242, c("ID", "percent")]), aes(x = ID, y = percent)) +
  geom_point() +
  geom_vline(xintercept = 125) + # May 5th
  geom_vline(xintercept = 235) + # Aug 24th
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = seq(0, 242, 5), lim = c(0, 242)) +
  geom_smooth(method = "loess", se = TRUE, span = .3) + 
  annotate("text", x = 60, y = 30, label = "Jan 1 - May 5", color = "red", size = 10) + 
  annotate("text", x = 170, y = 30, label = "May 5 - Aug 24", color = "red", size = 10)

####################################################################################

# ggplot(data = na.omit(futuresMarketSelectTest), aes(x = ID, y = percent)) +
#   geom_point() +
#   geom_hline(yintercept = 0)
# 
# Skew(futuresMarketSelectTest$percent, na.rm = TRUE)
# Kurt(futuresMarketSelectTest$percent, na.rm = TRUE)
# 
# 
# futuresMarketSelectTest = na.omit(futuresMarketSelectTest)
# 
# futuresMarketSelectTest$scaled <- scale(futuresMarketSelectTest$percent)
# 
# # check that we get mean of 0 and sd of 1
# mean(futuresMarketSelectTest$scaled, na.rm = TRUE)  # faster version of apply(scaled.dat, 2, mean)
# apply(futuresMarketSelectTest$scaled, 2, sd)
# 
# ggplot(data = na.omit(futuresMarketSelectTest), aes(x = Date, y = scaled)) +
#   geom_point() +
#   geom_hline(yintercept = 0)
# 
# 
# ggplot(data = futuresMarketSelectTest, aes(x = Date, y = percent)) +
#   geom_point() +
#   geom_hline(yintercept = 0)
# 
# ggplot(data = futuresMarketSelectTest, aes(x = Date, y = percent, group = 1)) +
#   geom_point() +
#   # geom_smooth(method = "loess", se = TRUE, span = 0.35, color = "red") +
#   geom_smooth(method = "loess", se = TRUE, span = 0.5)
# 
# loessLine = loess(percent ~ ID, futuresMarketSelectTest, degree = 1, span = 0.5)
# 
# loessFit = data.frame(fits = loessLine$fitted)
# 
# 
# ggplot(data = futuresMarketSelectTest, aes(x = Date, y = percent, group = 1)) +
#   geom_point() +
#   geom_line(data = loessFit, aes(x = as.numeric(rownames(loessFit)), y = fits)) +
#   geom_smooth(method = "loess", se = TRUE, span = 0.5)



####################################################################################
# Price Objective March

POMarSales = c("2008-06-05",
               "2009-04-02",
               "2009-06-04",
               "2009-06-17",
               "2010-04-21",
               "2012-06-19",
               "2012-07-25",
               "2013-01-07",
               "2013-08-23",
               "2014-02-21",
               "2014-07-28",
               "2016-07-29",
               "2016-08-15")

PODate = NA
class(PODate) = "Date"
POPrice = NA
for (i in 1:length(POMarSales)) {
  PODate[i] = ymd(futuresMarketSelect$Date[which(futuresMarketSelect$Date == POMarSales[i])])
  POPrice[i] = futuresMarketSelect$Difference[which(futuresMarketSelect$Date == POMarSales[i])]
}

ggplot(futuresMarketSelect, aes(Date,Difference)) + 
  geom_line() + 
  geom_hline(yintercept = 0) + 
  geom_hline(aes(yintercept = adjAvg, color = "Mean")) + 
  geom_hline(aes(yintercept = med, color = "Median")) + 
  geom_point(x = PODate[1], y = POPrice[1], color = "red", size = 2) + 
  geom_point(x = PODate[2], y = POPrice[2], color = "red", size = 2) + 
  geom_point(x = PODate[3], y = POPrice[3], color = "red", size = 2) + 
  geom_point(x = PODate[4], y = POPrice[4], color = "red", size = 2) + 
  geom_point(x = PODate[5], y = POPrice[5], color = "red", size = 2) + 
  geom_point(x = PODate[6], y = POPrice[6], color = "red", size = 2) + 
  geom_point(x = PODate[7], y = POPrice[7], color = "red", size = 2) + 
  geom_point(x = PODate[8], y = POPrice[8], color = "red", size = 2) + 
  geom_point(x = PODate[9], y = POPrice[9], color = "red", size = 2) + 
  geom_point(x = PODate[10], y = POPrice[10], color = "red", size = 2) + 
  geom_point(x = PODate[11], y = POPrice[11], color = "red", size = 2) + 
  geom_point(x = PODate[12], y = POPrice[12], color = "red", size = 2) + 
  geom_point(x = PODate[13], y = POPrice[13], color = "red", size = 2) + 
  scale_x_date(date_labels = "%Y",date_breaks = "1 year") + 
  labs(title = "PO March Sales", y = "Difference (March - Nov)", x = "Year") + 
  scale_colour_manual("", breaks = c("Mean", "Median"), values = c("red", "green"))


POMarchdf = data.frame(date = PODate, diff = POPrice)

POgain = sum(POMarchdf$diff[which(POMarchdf$diff > 0)])
POloss = sum(POMarchdf$diff[which(POMarchdf$diff < 0)])

# -1.35
POgain + POloss

# Remove outlier
POgain + POloss + 1.41250069


# Assume 100,000 bushels
# 10,000 Bushell Sales
# Money gained from March sales: -175,500
length(POMarSales) * 10000 * (POgain + POloss)


POMarchdf$Storage = c(0.59,
                      0.46,
                      0.50,
                      0.50,
                      0.49,
                      0.56,
                      0.59,
                      0.56,
                      0.56,
                      0.52,
                      0.51,
                      0.48,
                      0.49)

storageCostPO = sum(POMarchdf$Storage * 10000)
# -243,600
POMarchGains = length(POMarSales) * 10000 * (POgain + POloss) - storageCostPO


####################################################################################
# Trailing Stop March


TSMarSales = c("2008-05-22",
               "2009-04-06",
               "2009-06-15",
               "2009-06-22",
               "2011-01-24",
               "2012-04-16",
               "2016-07-14",
               "2016-07-22")

TSDate = NA
class(TSDate) = "Date"
TSPrice = NA
for (i in 1:length(TSMarSales)) {
  TSDate[i] = ymd(futuresMarketSelect$Date[which(futuresMarketSelect$Date == TSMarSales[i])])
  TSPrice[i] = futuresMarketSelect$Difference[which(futuresMarketSelect$Date == TSMarSales[i])]
}

ggplot(futuresMarketSelect, aes(Date,Difference)) + 
  geom_line() + 
  geom_hline(yintercept = 0) + 
  geom_hline(aes(yintercept = adjAvg, color = "Mean")) + 
  geom_hline(aes(yintercept = med, color = "Median")) + 
  geom_point(x = TSDate[1], y = TSPrice[1], color = "red", size = 2) + 
  geom_point(x = TSDate[2], y = TSPrice[2], color = "red", size = 2) + 
  geom_point(x = TSDate[3], y = TSPrice[3], color = "red", size = 2) + 
  geom_point(x = TSDate[4], y = TSPrice[4], color = "red", size = 2) + 
  geom_point(x = TSDate[5], y = TSPrice[5], color = "red", size = 2) + 
  geom_point(x = TSDate[6], y = TSPrice[6], color = "red", size = 2) + 
  geom_point(x = TSDate[7], y = TSPrice[7], color = "red", size = 2) + 
  geom_point(x = TSDate[8], y = TSPrice[8], color = "red", size = 2) + 
  scale_x_date(date_labels = "%Y",date_breaks = "1 year") + 
  labs(title = "TS March Sales", y = "Difference (March - Nov)", x = "Year") + 
  scale_colour_manual("", breaks = c("Mean", "Median"), values = c("red", "green"))

TSMarchdf = data.frame(date = TSDate, diff = TSPrice)

TSgain = sum(TSMarchdf$diff[which(TSMarchdf$diff > 0)])
TSloss = sum(TSMarchdf$diff[which(TSMarchdf$diff < 0)])

# No loss
TSgain + TSloss

# Assume 100,000 bushels
# 10,000 Bushell Sales
# Money gained from March sales: 6,800
length(TSMarSales) * 10000 * (TSgain + TSloss)


TSMarchdf$Storage = c(0.56,
                      0.46,
                      0.50,
                      0.49,
                      0.56,
                      0.56,
                      0.50,
                      0.49)


storageCostTS = sum(TSMarchdf$Storage * 10000)
# -34,400
TSMarchGains = length(TSMarSales) * 10000 * (TSgain + TSloss) - storageCostTS


################################################################################################################
# Soybeans show up to December 1
################################################################################################################

noMarketingDec = which(month(futuresMarket$Date) %in% c(12))

futuresMarketSelectDec = futuresMarket[, c("Date", "NovNC", "MarNC")]
futuresMarketSelectDec[noMarketingDec, c("NovNC", "MarNC")] = NA

futuresMarketLong <- melt(futuresMarketSelectDec,  id.vars = 'Date', variable.name = 'series')

# plot on same grid, each series colored differently -- 
# good if the series have same scale
ggplotly(ggplot(futuresMarketLong, aes(Date,value)) + 
           geom_line(aes(colour = series)))

# Poisitive = More gains from the march contract than the november contract
futuresMarketSelectDec$Difference = futuresMarketSelectDec$MarNC - futuresMarketSelectDec$NovNC
futuresMarketSelectDec$Year = year(futuresMarketSelectDec$Date)

post = futuresMarketSelectDec
post[which(month(post$Date) == 1), ] = NA
post[which(month(post$Date) == 2), ] = NA
post[which(month(post$Date) == 3), ] = NA
post[which(month(post$Date) == 4), ] = NA
post[which(month(post$Date) == 5), ] = NA
post[which(month(post$Date) == 6), ] = NA
post[which(month(post$Date) == 7), ] = NA
post[which(month(post$Date) == 8), ] = NA
post$Date = futuresMarketSelectDec$Date

nov = futuresMarketSelectDec
nov[which(month(nov$Date) != 11), ] = NA
nov$Date = futuresMarketSelectDec$Date

ggplot(futuresMarketSelectDec, aes(Date,Difference)) + 
  geom_line() + 
  geom_line(data = post, aes(color = "Post-Harvest")) + 
  geom_hline(yintercept = 0) + 
  scale_x_date(date_labels = "%Y",date_breaks = "1 year") + 
  labs(title = "Soybeans", y = "Difference (March - Nov)", x = "Year") + 
  scale_colour_manual("", breaks = c("Post-Harvest"), values = c("red"))










