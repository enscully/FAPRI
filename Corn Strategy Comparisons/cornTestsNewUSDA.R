library(readxl)
library(ggplot2)
library(tidyr)
library(lme4)
library(lmerTest)
library(multcomp)
library(rcompanion)
library(readr)

corn <- read_excel("U:/Emily Scully/Corn Strategy Comparisons/Corn Notebook.xlsx")

colnames(corn) = c("Strategy", "cropYear", "RawAveragePrice", "PreHarvestAverage", "PostHarvestAverage", "StorageAdjustedAverage", 
                   "StorageAdjustedPostHarvestAverage")

cornWOMY = corn[1:189,]
cornMY = corn[190:378,]

rawWOMY = data.frame(cornWOMY[,c(2,3,1)])
rawMY = data.frame(cornMY[,c(2,3,1)])

rawAll = rbind(rawWOMY, rawMY)

storageWOMY = data.frame(cornWOMY[,c(2,6,1)])
storageMY = data.frame(cornMY[,c(2,6,1)])

storageAll = rbind(storageWOMY, storageMY)

########################################################################################


# USDA data
USDA = c(3.884433052,
         3.76424926,
         6.221969157,
         6.471576426,
         6.946013339,
         4.397678115,
         3.768976721,
         3.700408337,
         3.586389526)

USDA = data.frame(Strategy = "USDA", Price = USDA)


##########################################################


# Test normality

# No Storage
rawAllWide = spread(rawAll, Strategy, RawAveragePrice)
rowNames = rawAllWide[,1]
rawAllWide = rawAllWide[,-1]
rownames(rawAllWide) = rowNames


rawAllWide$USDA = USDA$RawAveragePrice
normalityRaw = data.frame(Strategy = colnames(rawAllWide), pVal = NA)
for(i in 1:ncol(rawAllWide)){
  normalityRaw$pVal[i] = shapiro.test(rawAllWide[,i])$p.value
}

# Storage

storageAllWide = spread(storageAll, Strategy, StorageAdjustedAverage)
rowNames = storageAllWide[,1]
storageAllWide = storageAllWide[,-1]
rownames(storageAllWide) = rowNames

storageAllWide$USDA = USDA$RawAveragePrice
normalityStorage = data.frame(Strategy = colnames(storageAllWide), pVal = NA)
for(i in 1:ncol(storageAllWide)){
  normalityStorage$pVal[i] = shapiro.test(storageAllWide[,i])$p.value
}


##########################################################


# Repeated measures ANOVAs without storage

USDA$cropYear = c("2008-09", "2009-10", "2010-11", "2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17")
USDA = USDA[,c(3,2,1)]
colnames(USDA) = colnames(rawAll)
rawAll = rbind(rawAll, USDA)
rawWOMY = rbind(rawWOMY, USDA)

# Only non-Multiyear WITH USDA
rpaRawWOMY <- lmer(RawAveragePrice ~ Strategy + (1 | cropYear), data = rawWOMY)
anova(rpaRawWOMY) # p = 0.2838
summary(rpaRawWOMY)

#****************************??

friedman.test(RawAveragePrice ~ Strategy|cropYear, data = rawWOMY) # p-value = 0.2617

aovModel = aov(RawAveragePrice ~ Strategy + Error(cropYear/Strategy), data = rawWOMY)
summary(aovModel) # p = 0.284

#****************************??

plot(residuals(rpaRawWOMY))
boxplot(residuals(rpaRawWOMY))
plotNormalHistogram(residuals(rpaRawWOMY))
qqnorm(residuals(rpaRawWOMY))
qqline(residuals(rpaRawWOMY), col = "steelblue", lwd = 2)

summary(glht(rpaRawWOMY, linfct=mcp(Strategy = "Tukey")), test = adjusted(type = "bonferroni")) # USDA Different from all other strategies




#non-multiyear and multiyear WITH USDA
rpaRawAll <- lmer(RawAveragePrice ~ Strategy + (1 | cropYear), data = rawAll)
anova(rpaRawAll) # p = 2.2e-16
summary(rpaRawAll)

plot(residuals(rpaRawAll))
boxplot(residuals(rpaRawAll))
plotNormalHistogram(residuals(rpaRawAll))
qqnorm(residuals(rpaRawAll))
qqline(residuals(rpaRawAll), col = "steelblue", lwd = 2)

summary(glht(rpaRawAll, linfct=mcp(Strategy = "Tukey")), test = adjusted(type = "bonferroni")) # USDA Different from all other strategies





#multiyear WITH USDA
rpaRawMY <- lmer(RawAveragePrice ~ Strategy + (1 | cropYear), data = rawAll[190:387,])
anova(rpaRawMY) # p = 2.204e-07
summary(rpaRawMY)

#****************************??

friedman.test(RawAveragePrice ~ Strategy|cropYear, data = rawAll[190:387,]) # p-value = 0.4759

aovModel = aov(RawAveragePrice ~ Strategy + Error(cropYear/Strategy), data = rawAll[190:387,])
summary(aovModel) # p = 2.2e-07

#****************************??

plot(residuals(rpaRawMY))
boxplot(residuals(rpaRawMY))
plotNormalHistogram(residuals(rpaRawMY))
qqnorm(residuals(rpaRawMY))
qqline(residuals(rpaRawMY), col = "steelblue", lwd = 2)

summary(glht(rpaRawMY, linfct=mcp(Strategy = "Tukey")), test = adjusted(type = "bonferroni")) # USDA Different from all other strategies


##########################################################


# Repeated measures ANOVAs WITH STORAGE

colnames(USDA) = colnames(storageAll)
storageAll = rbind(storageAll, USDA)
storageWOMY = rbind(storageWOMY, USDA)

# Only non-Multiyear WITH USDA
rpastorageWOMY <- lmer(StorageAdjustedAverage ~ Strategy + (1 | cropYear), data = storageWOMY)
anova(rpastorageWOMY) # p = 0.9055
summary(rpastorageWOMY)


#****************************??

friedman.test(StorageAdjustedAverage ~ Strategy|cropYear, data = storageWOMY) # p-value = 0.1032

aovModel = aov(StorageAdjustedAverage ~ Strategy + Error(cropYear/Strategy), data = storageWOMY)
summary(aovModel) # # p = 0.906

#****************************??


plot(residuals(rpastorageWOMY))
boxplot(residuals(rpastorageWOMY))
plotNormalHistogram(residuals(rpastorageWOMY))
qqnorm(residuals(rpastorageWOMY))
qqline(residuals(rpastorageWOMY), col = "steelblue", lwd = 2)

summary(glht(rpastorageWOMY, linfct = mcp(Strategy = "Tukey")), test = adjusted(type = "bonferroni"))


#non-multiyear and multiyear WITH USDA
rpastorageAll <- lmer(StorageAdjustedAverage ~ Strategy + (1 | cropYear), data = storageAll)
anova(rpastorageAll)
summary(rpastorageAll)

plot(residuals(rpastorageAll))
boxplot(residuals(rpastorageAll))
plotNormalHistogram(residuals(rpastorageAll))
qqnorm(residuals(rpastorageAll))
qqline(residuals(rpastorageAll), col = "steelblue", lwd = 2)

summary(glht(rpastorageAll, linfct = mcp(Strategy = "Tukey")), test = adjusted(type = "bonferroni"))


#multiyear WITH USDA
rpastorageMY <- lmer(StorageAdjustedAverage ~ Strategy + (1 | cropYear), data = storageAll[190:387,])
anova(rpastorageMY) # p = 3.188e-09
summary(rpastorageMY)

#****************************??

friedman.test(StorageAdjustedAverage ~ Strategy|cropYear, data = storageAll[190:387,]) # p-value = 0.08142

aovModel = aov(StorageAdjustedAverage ~ Strategy + Error(cropYear/Strategy), data = storageAll[190:387,])
summary(aovModel) # # p = 0.01686

#****************************??

plot(residuals(rpastorageMY))
boxplot(residuals(rpastorageMY))
plotNormalHistogram(residuals(rpastorageMY))
qqnorm(residuals(rpastorageMY))
qqline(residuals(rpastorageMY), col = "steelblue", lwd = 2)

summary(glht(rpastorageMY, linfct = mcp(Strategy = "Tukey")), test = adjusted(type = "bonferroni"))


#######################################################################################################


# ggplots

rawAllWide$USDA = USDA$StorageAdjustedAverage
rawAllAverages = data.frame(avg = colMeans(rawAllWide))

ggplot(rawAll, aes(factor(Strategy,levels = unique(Strategy)), RawAveragePrice), col = "black") + 
  geom_boxplot() + 
  geom_boxplot(data = USDA, aes(factor(Strategy), StorageAdjustedAverage), col = "red") + 
  geom_point(data = rawAllAverages, aes(x = row.names(rawAllAverages), y = avg), color = "navyblue") +
  geom_hline(yintercept = rawAllAverages$avg[43], color = "navyblue") + 
  theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Strategy", y = "Raw Average Price", title = "Corn: Without Storage")


storageAllWide$USDA = USDA$StorageAdjustedAverage
storageAllAverages = data.frame(avg = colMeans(storageAllWide))

ggplot(storageAll, aes(factor(Strategy,levels = unique(Strategy)), StorageAdjustedAverage), col = "black") + 
  geom_boxplot() + 
  geom_boxplot(data = USDA, aes(factor(Strategy), StorageAdjustedAverage), col = "red") + 
  geom_point(data = storageAllAverages, aes(x = row.names(storageAllAverages), y = avg), color = "navyblue") +
  geom_hline(yintercept = storageAllAverages$avg[43], color = "navyblue") + 
  theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Strategy", y = "Storage Adjusted Average", title = "Corn: With Storage")















