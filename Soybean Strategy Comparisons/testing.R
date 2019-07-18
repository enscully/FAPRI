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

repeatedMeasures <- read_excel("C:/Users/ensxvd/Desktop/repeatedMeasures.xlsx")
repeatedMeasuresMY <- read_excel("C:/Users/ensxvd/Desktop/repeatedMeasuresMY.xlsx")

soybeans <- read_excel("C:/Users/ensxvd/Desktop/Soybean Notebook.xlsx")

colnames(soybeans) = c("Strategy", "cropYear", "RawAveragePrice", "PreHarvestAverage", "PostHarvestAverage", "StorageAdjustedAverage", 
                       "StorageAdjustedPostHarvestAverage")

soybeansWOMY = soybeans[1:189,]

stack_df = data.frame(repeatedMeasures[1], stack(repeatedMeasures[2:22]))
stack_dfMY = data.frame(repeatedMeasuresMY[1], stack(repeatedMeasuresMY[2:22]))


# Plot of each crop year and strategy. Raw data
ggplot(data = stack_df, aes(x = Crop.Year, y = values, color = ind, group = ind)) + 
  geom_line() + 
  labs(x = "Crop Year", y = "Raw Average Price", color = "Strategy")



# Checks for differences between averages for each crop year
model = lme(values ~ Crop.Year, 
            random = ~1|ind,
            data=stack_df,
            method="REML")
anova(model)

par(mfrow=c(1,2))
stack_df$resi <- residuals(model)
qqnorm(stack_df$resi, main="Normal Q-Q")
qqline(stack_df$resi)
boxplot(resi ~ ind, main="Homoscedasticity", 
        xlab = "Code Categories", ylab = "Residuals", border = "white", 
        data=stack_df)
points(resi ~ ind, pch = 1, 
       main="Homoscedasticity",  data=stack_df)



# Checks for differences between averages for each strategy
model2 = lme(values ~ ind, 
             random = ~1|Crop.Year,
             data=stack_df,
             method="REML")
anova(model2)

par(mfrow=c(1,2))
stack_df$resi <- residuals(model2)
qqnorm(stack_df$resi, main="Normal Q-Q")
qqline(stack_df$resi)
boxplot(resi ~ ind, main="Homoscedasticity", 
        xlab = "Code Categories", ylab = "Residuals", border = "white", 
        data=stack_df)
points(resi ~ ind, pch = 1, 
       main="Homoscedasticity",  data=stack_df)


# Paired t-tests for each crop year
# 11/12 = 14.76
# 12/13 = 14.72
# 14/15 = 11.00
# 15/16 = 10.76
ttestCropYears = pairwise.t.test(stack_df$values, stack_df$Crop.Year, paired = T, p.adjust.method = "none")

pvalsCropYears = ttestCropYears$p.value
sigValsCropYears = which(ttestCropYears$p.value >= 0.05)



# Paired t-tests for each strategy
# POV2 = 12.14
# POV3 = 12.03
# POV5 = 12.14
ttestStrategies = pairwise.t.test(stack_df$values, stack_df$ind, paired = T, p.adjust.method = "none")

pvalsStrategies = ttestStrategies$p.value
sigValsStrategies = which(ttestStrategies$p.value <= 0.05)
# POV2 & V3 = 0.034
# POV5 & V3 = 0.034



# GLM fitting
fit = glm(values ~ ind + Crop.Year, data = stack_df)
summary(fit)
summary(aov(fit))





########################################################################################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Raw average price, without storage, without MY


histogram(~ values | ind,
          data=stack_df,
          layout=c(1,3))

# strategy is IV, crop year is Block, Result is DV
friedman.test(values ~ ind|Crop.Year, data = stack_df)

# There was NOT a significant difference in values among strategies

row.names(repeatedMeasures) = repeatedMeasures$`Crop Year`
rowNames = row.names(repeatedMeasures)
repeatedMeasures = repeatedMeasures[,2:22]
row.names(repeatedMeasures) = rowNames

KendallW(t(repeatedMeasures), 
         correct=TRUE, 
         test=TRUE)




#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
########################################################################################





########################################################################################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Raw average price, without storage, WITH MY


histogram(~ values | ind,
          data=stack_dfMY,
          layout=c(1,22))

# strategy is IV, crop year is Block, Result is DV
friedman.test(values ~ ind|Crop.Year, data = stack_dfMY)


# There was NOT a significant difference in values among strategies

row.names(repeatedMeasuresMY) = repeatedMeasuresMY$`Crop Year`
rowNames = row.names(repeatedMeasuresMY)
repeatedMeasuresMY = repeatedMeasuresMY[,2:22]
row.names(repeatedMeasuresMY) = rowNames

KendallW(t(repeatedMeasuresMY), 
         correct=TRUE, 
         test=TRUE)




#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
########################################################################################













# Non parametric test for checking differences between crop years
friedman.test(values ~ Crop.Year|ind, data = stack_df)
# Non parametric test for checking differences between strategies
friedman.test(values ~ ind|Crop.Year, data = stack_df)






#######################################################################
# Testing Price - Storage



repeatedMeasuresStorage <- read_excel("C:/Users/ensxvd/Desktop/avgPriceWithStorage.xlsx")

soybeans <- read_excel("C:/Users/ensxvd/Desktop/Soybean Notebook.xlsx")

colnames(soybeans) = c("Strategy", "cropYear", "RawAveragePrice", "PreHarvestAverage", "PostHarvestAverage", "StorageAdjustedAverage", 
                       "StorageAdjustedPostHarvestAverage")

soybeansWOMY = soybeans[1:189,]

stack_df = data.frame(repeatedMeasuresStorage[1], stack(repeatedMeasuresStorage[2:22]))


# Plot of each crop year and strategy. Raw data
ggplot(data = stack_df, aes(x = Crop.Year, y = values, color = ind, group = ind)) + 
  geom_line()



# Checks for differences between averages for each crop year
model = lme(values ~ Crop.Year, 
            random = ~1|ind,
            data=stack_df,
            method="REML")
anova(model)

par(mfrow=c(1,2))
stack_df$resi <- residuals(model)
qqnorm(stack_df$resi, main="Normal Q-Q")
qqline(stack_df$resi)
boxplot(resi ~ ind, main="Homoscedasticity", 
        xlab = "Code Categories", ylab = "Residuals", border = "white", 
        data=stack_df)
points(resi ~ ind, pch = 1, 
       main="Homoscedasticity",  data=stack_df)



# Checks for differences between averages for each strategy
model2 = lme(values ~ ind, 
             random = ~1|Crop.Year,
             data=stack_df,
             method="REML")
anova(model2)

par(mfrow=c(1,2))
stack_df$resi <- residuals(model2)
qqnorm(stack_df$resi, main="Normal Q-Q")
qqline(stack_df$resi)
boxplot(resi ~ ind, main="Homoscedasticity", 
        xlab = "Code Categories", ylab = "Residuals", border = "white", 
        data=stack_df)
points(resi ~ ind, pch = 1, 
       main="Homoscedasticity",  data=stack_df)


# Paired t-tests for each crop year
# 11/12 = 
# 12/13 = 
# 14/15 = 
# 15/16 = 
ttestCropYears = pairwise.t.test(stack_df$values, stack_df$Crop.Year, paired = T, p.adjust.method = "none")

pvalsCropYears = ttestCropYears$p.value
sigValsCropYears = which(ttestCropYears$p.value >= 0.05)



# Paired t-tests for each strategy
# POV2 = 11.79
# POV3 = 11.68
# POV5 = 11.79
# TS March = 11.36
# TSV3Base = 11.84
# TSV3V2 = 11.85
# TSV3V4 = 11.84
# TSV3V5 = 11.85
ttestStrategies = pairwise.t.test(stack_df$values, stack_df$ind, paired = T, p.adjust.method = "none")

pvalsStrategies = ttestStrategies$p.value
sigValsStrategies = which(ttestStrategies$p.value <= 0.05)
# POV2 & V3 = 0.03575369
# POV5 & V3 = 0.03575369
# TS March & TSV3Base = 0.03002043
# TS March & TSV3V2 = 0.02997491
# TS March & TSV3V4 = 0.03002043
# TS March & TSV3V5 = 0.02997491


# GLM fitting
fit = glm(values ~ ind + Crop.Year, data = stack_df)
summary(fit)
summary(aov(fit))















