# Soybeans

library(readxl)
library(ggplot2)
library(tidyr)
library(lme4)
library(lmerTest)
library(multcomp)
library(rcompanion)
library(readr)

soybeans <- read_excel("U:/Emily Scully/Soybean Strategy Comparisons/Soybean Notebook.xlsx")

colnames(soybeans) = c("Strategy", "cropYear", "RawAveragePrice", "PreHarvestAverage", "PostHarvestAverage", "StorageAdjustedAverage", 
                       "StorageAdjustedPostHarvestAverage")

soybeansWOMY = soybeans[1:189,]
soybeansMY = soybeans[190:378,]

rawWOMY = data.frame(soybeansWOMY[,c(2,3,1)])
rawMY = data.frame(soybeansMY[,c(2,3,1)])

rawAll = rbind(rawWOMY, rawMY)

storageWOMY = data.frame(soybeansWOMY[,c(2,6,1)])
storageMY = data.frame(soybeansMY[,c(2,6,1)])

storageAll = rbind(storageWOMY, storageMY)

########################################################################################


# USDA data
USDA = c(9.875139584,
         9.82318177,
         12.63564477,
         12.84459206,
         14.88573776,
         13.18335991,
         9.901857016,
         9.170431916,
         9.943178293)

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
anova(rpaRawWOMY) # p = 0.000397
summary(rpaRawWOMY)

plot(residuals(rpaRawWOMY))
boxplot(residuals(rpaRawWOMY))
plotNormalHistogram(residuals(rpaRawWOMY))
qqnorm(residuals(rpaRawWOMY))
qqline(residuals(rpaRawWOMY), col = "steelblue", lwd = 2)

summary(glht(rpaRawWOMY, linfct=mcp(Strategy = "Tukey")), test = adjusted(type = "bonferroni")) # USDA Different from all other strategies

# Only Multiyear WITH USDA
rpaRawMY <- lmer(RawAveragePrice ~ Strategy + (1 | cropYear), data = rawAll[190:387,])
anova(rpaRawMY) # 6.251e-07
summary(rpaRawMY)

plot(residuals(rpaRawMY))
boxplot(residuals(rpaRawMY))
plotNormalHistogram(residuals(rpaRawMY))
qqnorm(residuals(rpaRawMY))
qqline(residuals(rpaRawMY), col = "steelblue", lwd = 2)

summary(glht(rpaRawMY, linfct=mcp(Strategy = "Tukey")), test = adjusted(type = "bonferroni")) # USDA Different from all other strategies

#non-multiyear and multiyear WITH USDA
rpaRawAll <- lmer(RawAveragePrice ~ Strategy + (1 | cropYear), data = rawAll)
anova(rpaRawAll) # p = 5.211e-13
summary(rpaRawAll)

plot(residuals(rpaRawAll))
boxplot(residuals(rpaRawAll))
plotNormalHistogram(residuals(rpaRawAll))
qqnorm(residuals(rpaRawAll))
qqline(residuals(rpaRawAll), col = "steelblue", lwd = 2)

summary(glht(rpaRawAll, linfct=mcp(Strategy = "Tukey")), test = adjusted(type = "bonferroni")) # USDA Different from all other strategies


##########################################################


# Repeated measures ANOVAs WITH STORAGE

colnames(USDA) = colnames(storageAll)
storageAll = rbind(storageAll, USDA)
storageWOMY = rbind(storageWOMY, USDA)

# Only non-Multiyear WITH USDA
rpastorageWOMY <- lmer(StorageAdjustedAverage ~ Strategy + (1 | cropYear), data = storageWOMY)
anova(rpastorageWOMY) # p = 0.02958
summary(rpastorageWOMY)

plot(residuals(rpastorageWOMY))
boxplot(residuals(rpastorageWOMY))
plotNormalHistogram(residuals(rpastorageWOMY))
qqnorm(residuals(rpastorageWOMY))
qqline(residuals(rpastorageWOMY), col = "steelblue", lwd = 2)

summary(glht(rpastorageWOMY, linfct = mcp(Strategy = "Tukey")), test = adjusted(type = "bonferroni")) # No differences in any strateies

# Only Multiyear WITH USDA
rpastorageMY <- lmer(StorageAdjustedAverage ~ Strategy + (1 | cropYear), data = storageAll[190:387,])
anova(rpastorageMY) # p = 0.003273
summary(rpastorageMY)

plot(residuals(rpastorageMY))
boxplot(residuals(rpastorageMY))
plotNormalHistogram(residuals(rpastorageMY))
qqnorm(residuals(rpastorageMY))
qqline(residuals(rpastorageMY), col = "steelblue", lwd = 2)

summary(glht(rpastorageMY, linfct = mcp(Strategy = "Tukey")), test = adjusted(type = "bonferroni"))

#non-multiyear and multiyear WITH USDA
rpastorageAll <- lmer(StorageAdjustedAverage ~ Strategy + (1 | cropYear), data = storageAll)
anova(rpastorageAll) # p = 1.016e-10
summary(rpastorageAll)

plot(residuals(rpastorageAll))
boxplot(residuals(rpastorageAll))
plotNormalHistogram(residuals(rpastorageAll))
qqnorm(residuals(rpastorageAll))
qqline(residuals(rpastorageAll), col = "steelblue", lwd = 2)

summary(glht(rpastorageAll, linfct = mcp(Strategy = "Tukey")), test = adjusted(type = "bonferroni"))


#######################################################################################################


# ggplots

rawAllWide$USDA = USDA$StorageAdjustedAverage
rawAllAverages = data.frame(avg = colMeans(rawAllWide))

ggplot(rawAll, aes(factor(Strategy,levels = unique(Strategy)), RawAveragePrice), col = "black") + 
  geom_boxplot() + 
  geom_boxplot(data = USDA, aes(factor(Strategy,levels = unique(Strategy)), StorageAdjustedAverage), col = "red") + 
  geom_point(data = rawAllAverages, aes(x = row.names(rawAllAverages), y = avg), color = "navyblue") +
  geom_hline(yintercept = rawAllAverages$avg[43], color = "navyblue") + 
  theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Strategy", y = "Raw Average Price", title = "Soybeans: Without Storage")


storageAllWide$USDA = USDA$StorageAdjustedAverage
storageAllAverages = data.frame(avg = colMeans(storageAllWide))

ggplot(storageAll, aes(factor(Strategy,levels = unique(Strategy)), StorageAdjustedAverage), col = "black") + 
  geom_boxplot() + 
  geom_boxplot(data = USDA, aes(factor(Strategy,levels = unique(Strategy)), StorageAdjustedAverage), col = "red") + 
  geom_point(data = storageAllAverages, aes(x = row.names(storageAllAverages), y = avg), color = "navyblue") +
  geom_hline(yintercept = storageAllAverages$avg[43], color = "navyblue") + 
  theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Strategy", y = "Storage Adjusted Average", title = "Soybeans: With Storage")



####################################################################################################
####################################################################################################


#Wilcox Tests

# Without Storage

rawAllWide = spread(rawAll, Strategy, RawAveragePrice)
rowNames = rawAllWide[,1]
rawAllWide = rawAllWide[,-1]
rownames(rawAllWide) = rowNames

ggplot(rawAll, aes(factor(Strategy), RawAveragePrice), col = "black") + 
  geom_boxplot() + 
  geom_boxplot(data = USDA, aes(factor(Strategy), Price), col = "red") + 
  geom_hline(yintercept = 10.10, col = "red") + 
  theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Strategy", y = "Raw Average Price")

strategyNames = colnames(rawAllWide)
wilcoxonPvalues = data.frame(strategyNames, Pvalues = NA)
for(i in 1:ncol(rawAllWide)){
  wilcoxonPvalues$Pvalues[i] = wilcox.test(rawAllWide[,i], mu = 10.10, alternative = "greater")$p.value
}

length(which(wilcoxonPvalues$Pvalues > 0.05)) # = 0


# With Storage


storageAllWide = spread(storageAll, Strategy, StorageAdjustedAverage)
rowNames = storageAllWide[,1]
storageAllWide = storageAllWide[,-1]
rownames(storageAllWide) = rowNames

ggplot(storageAll, aes(factor(Strategy), StorageAdjustedAverage), col = "black") + 
  geom_boxplot() + 
  geom_boxplot(data = USDA, aes(factor(Strategy), Price), col = "red") + 
  geom_hline(yintercept = 10.10, col = "red") + 
  theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Strategy", y = "Storage Adjusted Average")

strategyNames = colnames(storageAllWide)
StorageWilcoxonPvalues = data.frame(strategyNames, Pvalues = NA)
for(i in 1:ncol(storageAllWide)){
  StorageWilcoxonPvalues$Pvalues[i] = wilcox.test(storageAllWide[,i], mu = 10.10, alternative = "greater")$p.value
}


length(which(StorageWilcoxonPvalues$Pvalues > 0.05)) # = 2
StorageWilcoxonPvalues[which(StorageWilcoxonPvalues$Pvalues > 0.05),] # PO March & TS March

storageLessUSDA = data.frame(matrix(nrow = 9, ncol = 43, NA))
colnames(storageLessUSDA) = colnames(rawAllWide)
for(i in 1:ncol(rawAllWide)){
  storageLessUSDA[,i] = rawAllWide[,i] - USDA$StorageAdjustedAverage
}

storageLessUSDA = gather(storageLessUSDA, Strategy, difference, factor_key=TRUE)

# Another way to show the differences
ggplot(storageLessUSDA, aes(factor(Strategy), difference), col = "black") + 
  geom_boxplot() + 
  geom_hline(yintercept = 0, col = "red") + 
  theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1))















