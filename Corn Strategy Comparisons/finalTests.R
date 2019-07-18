library(readxl)
library(rcompanion)
library(ggplot2)
library(car)
library(multcompView)
library(lsmeans)
library(mvoutlier)
library(mvnormtest)
library(dplyr)
library(ggpubr)
library(lme4)
library(lmerTest)
library(lme)
library(multcomp)
library(readxl)
library(nlme)
library(gplots)
library(gridExtra)
library(DescTools)
library(lattice)
library(PMCMRplus)
library(RVAideMemoire)
library(agricolae)
library(readr)
library(readxl)


# repeatedMeasures <- read_excel("C:/Users/ensxvd/Desktop/repeatedMeasures.xlsx")
# repeatedMeasuresMY <- read_excel("C:/Users/ensxvd/Desktop/repeatedMeasuresMY.xlsx")

corn <- read_excel("U:/Emily Scully/Corn Strategy Comparisons/Corn Notebook.xlsx")

colnames(corn) = c("Strategy", "cropYear", "RawAveragePrice", "PreHarvestAverage", "PostHarvestAverage", "StorageAdjustedAverage", 
                       "StorageAdjustedPostHarvestAverage")

cornWOMY = corn[1:189,]
cornMY = corn[190:378,]

stack_df = data.frame(cornWOMY[,c(2,3,1)])
stack_dfMY = data.frame(cornMY[,c(2,3,1)])

# stack_df = data.frame(repeatedMeasures[1], stack(repeatedMeasures[2:22]))
# stack_dfMY = data.frame(repeatedMeasuresMY[1], stack(repeatedMeasuresMY[2:22]))

stack_All = rbind(stack_df, stack_dfMY)

storageWOMY = data.frame(cornWOMY[,c(2,6,1)])
storageMY = data.frame(cornMY[,c(2,6,1)])

storageAll = rbind(storageWOMY, storageMY)

########################################################################################

# Crop Years

# strategy is IV, crop year is Block, Result is DV
friedman.test(RawAveragePrice ~ cropYear|Strategy, data = stack_df) # Significant



########################################################################################

# Raw average price, without storage, without MY


histogram(~ RawAveragePrice | Strategy,
          data=stack_df,
          layout=c(1,3))

# strategy is IV, crop year is Block, Result is DV
friedman.test(RawAveragePrice ~ Strategy|cropYear, data = stack_df) # Not Significant



########################################################################################


# Raw average price, without storage, WITH MY


histogram(~ RawAveragePrice | Strategy,
          data=stack_dfMY,
          layout=c(1,22))

# strategy is IV, crop year is Block, Result is DV
friedman.test(RawAveragePrice ~ Strategy|cropYear, data = stack_dfMY) # Not Significant

########################################################################################


# Raw average price, without storage, WITH AND WITHOUT MY


histogram(~ RawAveragePrice | Strategy,
          data=stack_All,
          layout=c(1,22))

# strategy is IV, crop year is Block, Result is DV
friedman.test(RawAveragePrice ~ Strategy|cropYear, data = stack_All) # Significant

rap = stack_All$RawAveragePrice
strat = as.factor(stack_All$Strategy)
year = as.factor(stack_All$cropYear)
friedman.test(rap~strat|year)
wilcox.paired.multcomp(rap~strat|year, p.method = "bonf") # Not Significant

########################################################################################

# Storage Price, without MY

histogram(~ StorageAdjustedAverage | Strategy,
          data=storageWOMY,
          layout=c(1,3))

# strategy is IV, crop year is Block, Result is DV
friedman.test(StorageAdjustedAverage ~ Strategy|cropYear, data = storageWOMY) # Not Significant



########################################################################################



# Storage Price, WITH MY

histogram(~ StorageAdjustedAverage | Strategy,
          data=storageMY,
          layout=c(1,3))

# strategy is IV, crop year is Block, Result is DV
friedman.test(StorageAdjustedAverage ~ Strategy|cropYear, data = storageMY) # Not Significant


########################################################################################


# Storage Price, WITH AND WITHOUT MY


histogram(~ StorageAdjustedAverage | Strategy,
          data=storageAll,
          layout=c(1,3))

# strategy is IV, crop year is Block, Result is DV
friedman.test(StorageAdjustedAverage ~ Strategy|cropYear, data = storageAll) # Significant

rap1 = storageAll$StorageAdjustedAverage
strat1 = as.factor(storageAll$Strategy)
year1 = as.factor(storageAll$cropYear)
friedman.test(rap1 ~ strat1|year1)
wilcox.paired.multcomp(rap1 ~ strat1|year1, p.method = "dwsvc") # Not Significant

########################################################################################
