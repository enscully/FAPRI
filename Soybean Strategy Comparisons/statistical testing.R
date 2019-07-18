library(readxl)
library(rcompanion)
library(ggplot2)
library(car)
library(multcompView)
library(lsmeans)
library(mvoutlier)
library(mvnormtest)

soybeans <- read_excel("C:/Users/ensxvd/Desktop/Soybean Notebook.xlsx")

colnames(soybeans) = c("Strategy", "cropYear", "RawAveragePrice", "PreHarvestAverage", "PostHarvestAverage", "StorageAdjustedAverage", 
                       "StorageAdjustedPostHarvestAverage")

soybeansWOMY = soybeans[1:189,]

boxplot(RawAveragePrice ~ Strategy,
        data = soybeansWOMY)



Sum = groupwiseMean(RawAveragePrice ~ Strategy,
                    data = soybeansWOMY,
                    conf   = 0.95,
                    digits = 3,
                    traditional = FALSE,
                    percentile  = TRUE)





ggplot(Sum, aes(x = Strategy, y = Mean)) +
  geom_errorbar(aes(ymin = Percentile.lower, ymax = Percentile.upper), width = 0.05,  size  = 0.5) +
  geom_point(shape = 15, size  = 4) +
  theme_bw() +
  theme(axis.title   = element_text(face  = "bold")) +
  ylab("Mean Price")


model = lm(RawAveragePrice ~ Strategy,
           data = soybeansWOMY)


summary(model)



Anova(model, type = "II")   ### Type II sum of squares

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model), residuals(model))



marginal = lsmeans(model, ~ Strategy)

pairs(marginal, adjust="tukey")



###############################################################################


model = lm(RawAveragePrice ~ Strategy + cropYear,
           data = soybeansWOMY)

summary(model)

Anova(model, type = "II")

x = residuals(model)

plotNormalHistogram(x)

plot(fitted(model), residuals(model))

plot(model)

marginal = lsmeans(model, ~ cropYear)

pairs(marginal, adjust="tukey")





#############################################################################


library(dplyr)
library(ggpubr)


group_by(soybeansWOMY, Strategy) %>%
  summarise(
    count = n(),
    mean = mean(RawAveragePrice, na.rm = TRUE),
    sd = sd(RawAveragePrice, na.rm = TRUE),
    median = median(RawAveragePrice, na.rm = TRUE),
    IQR = IQR(RawAveragePrice, na.rm = TRUE)
  )


ggboxplot(soybeansWOMY, x = "Strategy", y = "RawAveragePrice", 
          color = "Strategy",
          ylab = "RawAveragePrice", xlab = "Strategy")


ggboxplot(soybeansWOMY, x = "cropYear", y = "RawAveragePrice", 
          color = "cropYear",
          ylab = "RawAveragePrice", xlab = "cropYear")



ggline(soybeansWOMY, x = "Strategy", y = "RawAveragePrice", 
       add = c("mean_se", "jitter"),
       ylab = "RawAveragePrice", xlab = "Treatment")



kruskal.test(RawAveragePrice ~ Strategy, data = soybeansWOMY)



res.aov <- aov(RawAveragePrice ~ Strategy, data = soybeansWOMY)
summary(res.aov)

#####################################################

model = lm(RawAveragePrice ~ cropYear,
           data = soybeansWOMY)

summary(model)

Anova(model, type = "II")



###############################################################


groupAverages = read_excel("C:/Users/ensxvd/Desktop/groupMeasn.xlsx")

colnames(groupAverages) = c("Strategy", "avgPrice")


res.aov <- aov(avgPrice ~ Strategy, data = groupAverages)
summary(res.aov)



#############################################################


PO = soybeansWOMY$RawAveragePrice[which(soybeansWOMY$Strategy == "PO Base")]



TSMarBase = soybeansWOMY$RawAveragePrice[which(soybeansWOMY$Strategy == "TS March Baselines")]



TSV3V2 = soybeansWOMY$RawAveragePrice[which(soybeansWOMY$Strategy == "TS V3V2")]


t.test(PO, mu = 11.41, alternative = "greater")

t.test(TSMarBase, mu = 11.41, alternative = "greater")

t.test(TSV3V2, mu = 11.41, alternative = "greater")











##################################################################

library(readxl)
repeatedMeasures <- read_excel("C:/Users/ensxvd/Desktop/repeatedMeasures.xlsx")



lm(RawAveragePrice ~ cropYear + Error(Strategy/cropYear), data = soybeansWOMY)





aovModel = aov(RawAveragePrice ~ Strategy + Error(cropYear/Strategy), data = soybeansWOMY)
summary(aovModel)







####################################################################




demo1  <- read.csv("https://stats.idre.ucla.edu/stat/data/demo1.csv")
## Convert variables to factor
demo1 <- within(demo1, {
  group <- factor(group)
  time <- factor(time)
  id <- factor(id)
})

par(cex = .6)

with(demo1, interaction.plot(time, group, pulse,
                             ylim = c(5, 20), lty= c(1, 12), lwd = 3,
                             ylab = "mean of pulse", xlab = "time", trace.label = "group"))

demo1.aov <- aov(pulse ~ group * time + Error(id), data = demo1)
summary(demo1.aov)







###################################################################################

stack_df = data.frame(repeatedMeasures[1], stack(repeatedMeasures[2:22]))

library(lme4)
library(lmerTest)
library(lme)
library(multcomp)

rmaModel = lmer(values ~ ind + (1|Crop.Year), data = stack_df)
anova(rmaModel)



rmaModel2 = lmer(values ~ ind + (1|Crop.Year), data = stack_df, REML=TRUE)
anova(rmaModel2)

plot(rmaModel2)

plot(residuals(rmaModel2))


par(mfrow=c(1,2)) #add room for the rotated labels
stack_df$resi <- residuals(rmaModel2)
qqnorm(stack_df$resi, main="Normal Q-Q") # A quantile normal plot - good for checking normality
qqline(stack_df$resi)
boxplot(resi ~ ind, main="Homoscedasticity", 
        xlab = "Code Categories", ylab = "Residuals", border = "white", 
        data=stack_df)
points(resi ~ ind, pch = 1, 
       main="Homoscedasticity",  data=stack_df)



ggplot(data = stack_df, aes(x = Crop.Year, y = values, color = ind, group = ind)) + 
  geom_line()







##################################################################################################





# Testing for differences between crop years


stripchart(values ~ Crop.Year, vertical = TRUE, pch = 1, data = stack_df)

fitCropYear <- lmer(values ~ (1 | Crop.Year), data = stack_df)

summary(fitCropYear)

plot(fitCropYear)


par(mfrow = c(1, 2))
qqnorm(ranef(fitCropYear)$Crop.Year[, "(Intercept)"], main = "Random effects")
qqnorm(resid(fitCropYear), main = "Residuals")








###################################################################################################



# Testing for differences between strategies within crop years




with(stack_df, interaction.plot(x.factor = cropYear, trace.factor = Strategy, response = RawAveragePrice))



fit.stack_df <- lmer(RawAveragePrice ~ (1 | cropYear) + (1 | Strategy), data = stack_df)
summary(fit.stack_df)

fit.stack_df <- lmer(RawAveragePrice ~ Strategy + (1 | cropYear), data = stack_df[1:189,])
summary(fit.stack_df)

fit.stack_dfAll <- lmer(RawAveragePrice ~ Strategy + (1 | cropYear), data = stack_df)
summary(fit.stack_dfAll)

qqnorm(residuals(fit.stack_df))
qqline(residuals(fit.stack_df), col = "steelblue", lwd = 2)

fit.lm <- lm(RawAveragePrice ~ cropYear + Strategy, data = stack_df)
summary(fit.lm)





################################################################################################################3






library(nlme)

model = lme(values ~ Crop.Year, 
            random = ~1|ind,
            data=stack_df,
            method="REML")


anova(model)



model2 = lme(values ~ ind, 
             random = ~1|Crop.Year,
             data=stack_df,
             method="REML")


anova(model2)



summary(glht(model2,linfct=mcp(ind="Tukey")))






#####################################################################################


library(ez)
output = ezANOVA(stack_df,
                 dv = values,
                 wid = ind,
                 within = as.factor(Crop.Year),
                 detailed = TRUE,
                 type = 3)



########################################################################################



# model <- aov(wm$iq ~ wm$condition + Error(wm$subject / wm$condition))



# anova model
model <- aov(values ~ Crop.Year + Error(ind / Crop.Year), data = stack_df)

# summary model
summary(model)

with(stack_df, pairwise.t.test(values, Crop.Year, paired = T))

with(stack_df, pairwise.t.test(values, Crop.Year, paired = TRUE, p.adjust.method = 'bonferroni'))

cond_0809 <- subset(stack_df, Crop.Year == '2008-09')$values
cond_0910 <- subset(stack_df, Crop.Year == '2009-10')$values

t.test(cond_0809, cond_0910, paired = TRUE)








# anova model
model2 <- aov(values ~ ind + Error(Crop.Year / ind), data = stack_df)

# summary model
summary(model2)

with(stack_df, pairwise.t.test(values, ind, paired = T))

with(stack_df, pairwise.t.test(values, ind, paired = TRUE, p.adjust.method = 'bonferroni'))

POBase <- subset(stack_df, ind == 'Price Objective Base')$values
POV2 <- subset(stack_df, ind == 'Price Objective V2')$values
POV3 <- subset(stack_df, ind == 'Price Objective V3')$values
POV4 <- subset(stack_df, ind == 'Price Objective V3')$values
POV5 <- subset(stack_df, ind == 'Price Objective V5')$values
POMar <- subset(stack_df, ind == 'Price Objective March')$values
POMarBaselines <- subset(stack_df, ind == 'Price Objective MarchBaselines')$values
TS <- subset(stack_df, ind == 'Trailing Stop Base')$values
TSV2 <- subset(stack_df, ind == 'Trailing Stop V2')$values
TSV3 <- subset(stack_df, ind == 'Trailing Stop V3')$values
TSV4 <- subset(stack_df, ind == 'Trailing Stop V4')$values
TSV5 <- subset(stack_df, ind == 'Trailing Stop V5')$values
TSV3Base <- subset(stack_df, ind == 'Trailing Stop V3Base')$values
TSV3V2 <- subset(stack_df, ind == 'Trailing Stop V3V2')$values
TSV3V3 <- subset(stack_df, ind == 'Trailing Stop V3V3')$values
TSV3V4 <- subset(stack_df, ind == 'Trailing Stop V3V4')$values
TSV3V5 <- subset(stack_df, ind == 'Trailing Stop V3V5')$values
TSMar <- subset(stack_df, ind == 'Trailing Stop March')$values
TSMarBaselines <- subset(stack_df, ind == 'Trailing Stop MarchBaselines')$values
SSBase <- subset(stack_df, ind == 'Seasonal Sales Base')$values
SSMarBaselines <- subset(stack_df, ind == 'Seasonal Sales MarchBaselines')$values

t.test(POBase, POV2, paired = TRUE)
t.test(POBase, POV3, paired = TRUE)
t.test(POBase, POV4, paired = TRUE)
t.test(POBase, POV5, paired = TRUE)
t.test(POBase, POMar, paired = TRUE)
t.test(POBase, POMarBaselines, paired = TRUE)






###############################################################################################

model = lme(values ~ Crop.Year, 
            random = ~1|ind,
            data=stack_df,
            method="REML")


anova(model)



model2 = lme(values ~ ind, 
             random = ~1|Crop.Year,
             data=stack_df,
             method="REML")


anova(model2)






pairwise.t.test(stack_df$values, stack_df$Crop.Year, paired = T, p.adjust.method = "none")
pairwise.t.test(stack_df$values, stack_df$ind, paired = T, p.adjust.method = "none")


fit = aov(values ~ Crop.Year, data = stack_df)

fit.tukey <- glht(model2, linfct = mcp(ind = "Tukey"))
summary(fit.tukey)

confint(glht(fit.tukey))


plot(confint(glht(fit.tukey)))


##################################################################################

# GLM fitted model gives same results as repeated measures ANOVA

fit = glm(values ~ ind + Crop.Year, data = stack_df)
summary(fit)
summary(aov(fit))

fittedValues = fit$fitted.values
fittedValues$cropYear = stack_df$Crop.Year
fittedValues$actual = stack_df$values

ggplot(data = stack_df, aes(x = row.names(stack_df), y = values, group = ind, col = ind)) + 
  geom_point(data = fittedValues, aes(x = row.names(fittedValues), y = V1))


ggplot() + 
  geom_point(data = fittedValues, aes(x = cropYear, y = V1), col = "red") + 
  geom_point(data = fittedValues, aes(x = cropYear, y = actual), col = "blue", shape = 0)


ggplot(data = stack_df, aes(x = Crop.Year, y = values, color = ind, group = ind)) + 
  geom_line()


##################################################################################

# Prediction modeling

library(caret)

set.seed(123)
train_ind <- sample(seq_len(nrow(stack_df)), size = nrow(stack_df)*0.75)

eightYears = stack_df[train_ind,]
oneYear = stack_df[-train_ind,]

fit2 = glm(values ~ ind + Crop.Year, data = eightYears)
summary(fit2)
summary(aov(fit2))

predictedValues = predict(fit2, newdata = oneYear, xlev = as.factor(stack_df$Crop.Year))

mean(table(oneYear$values, predictedValues))

postResample(predictedValues, oneYear$values)

fittedValues2 = data.frame("V1" = fit2$fitted.values)
fittedValues2$cropYear = eightYears$Crop.Year
fittedValues2$actual = eightYears$values

ggplot(data = eightYears, aes(x = row.names(eightYears), y = values, group = cropYear, col = cropYear)) + 
  geom_point(data = fittedValues2, aes(x = row.names(fittedValues2), y = V1))

predictedValues = data.frame("V1" = predictedValues)

oneYear$values = predictedValues$V1
oneYear$code = "test"
eightYears$code = "train"

new = data.frame(rbind(eightYears, oneYear))

ggplot() + 
  geom_point(data = new, aes(x = Crop.Year, y = values, col = code)) + 
  geom_point(data = fittedValues2, aes(x = cropYear, y = actual), col = "green", shape = 0)

ggplot() + 
  geom_point(data = fittedValues2, aes(x = cropYear, y = V1), col = "red") + 
  geom_point(data = fittedValues2, aes(x = cropYear, y = actual), col = "blue", shape = 0)
 
ggplot(data = eightYears, aes(x = Crop.Year, y = values, color = ind, group = ind)) + 
  geom_line()




#########################################################################################################



# Friedman Tests



########################################################################################

# Crop Years

# strategy is IV, crop year is Block, Result is DV
friedman.test(RawAveragePrice ~ cropYear|Strategy, data = stack_df) # Significant

rap = stack_df$RawAveragePrice
strat = as.factor(stack_df$Strategy)
year = as.factor(stack_df$cropYear)
friedman.test(rap ~ year|strat)
wilcox.paired.multcomp(rap ~ year|strat, p.method = "bonf") # Significant
# 11/12 - 12/13
# 14/15 - 15/16


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
friedman.test(RawAveragePrice ~ Strategy|cropYear, data = stack_All) # Not Significant


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

rap = storageAll$StorageAdjustedAverage
strat = as.factor(storageAll$Strategy)
year = as.factor(storageAll$cropYear)
friedman.test(rap ~ strat|year)
wilcox.paired.multcomp(rap ~ strat|year, p.method = "bonf") # Not Significant




##################################################################################################


USDA$cropYear = c("2008-09", "2009-10", "2010-11", "2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17")
USDA = USDA[,c(3,2,1)]
colnames(USDA) = colnames(rawAll)
rawAll = rbind(rawAll, USDA)
rawWOMY = rbind(rawWOMY, USDA)

friedman.test(RawAveragePrice ~ Strategy|cropYear, data = rawWOMY)

rap = rawWOMY$RawAveragePrice
strat = as.factor(rawWOMY$Strategy)
year = as.factor(rawWOMY$cropYear)
friedman.test(rap ~ strat|year)
wilcox.paired.multcomp(rap ~ strat|year, p.method = "bonf") # Not Significant



















