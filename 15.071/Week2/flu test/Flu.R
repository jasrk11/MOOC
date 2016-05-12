rm(list=ls())



FluTrain <- read.csv("FluTrain.csv", stringsAsFactors = F)
FluTest <- read.csv("FluTest.csv", stringsAsFactors = F)
summary(FluTrain)

FluTrain[which(FluTrain$ILI == max(FluTrain$ILI)), ]
FluTrain[which(FluTrain$Queries == max(FluTrain$Queries)), ]

hist(FluTrain$ILI)
plot(log(FluTrain$ILI), FluTrain$Queries)
plot(FluTrain$Queries, log(FluTrain$ILI))

FluTrend1 <- lm(log(ILI) ~ Queries, data = FluTrain)
summary(FluTrend1)
# Residuals:
#     Min       1Q   Median       3Q      Max 
# -0.76003 -0.19696 -0.01657  0.18685  1.06450 
# 
# Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
#     (Intercept)      -0.49934    0.03041  -16.42   <2e-16 ***
#     FluTrain$Queries  2.96129    0.09312   31.80   <2e-16 ***
#     ---
#     Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.2995 on 415 degrees of freedom
# Multiple R-squared:  0.709,	Adjusted R-squared:  0.7083 
# F-statistic:  1011 on 1 and 415 DF,  p-value: < 2.2e-16

cor(log(FluTrain$ILI), FluTrain$Queries)^2



PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
FluTest$Week
(FluTest$ILI[11] - PredTest1[11])/FluTest$ILI[11]


sqrt(mean((PredTest1-FluTest$ILI)^2))


library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)

#plot(FluTrain$ILILag2, FluTrain$ILI)
plot(log(FluTrain$ILI), log(FluTrain$ILILag2))

FluTrend2 <- lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)
# 
# Residuals:
#     Min       1Q   Median       3Q      Max 
# -0.52209 -0.11082 -0.01819  0.08143  0.76785 
# 
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
#     (Intercept)  -0.24064    0.01953  -12.32   <2e-16 ***
#     Queries       1.25578    0.07910   15.88   <2e-16 ***
#     log(ILILag2)  0.65569    0.02251   29.14   <2e-16 ***
#     ---
#     Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.1703 on 412 degrees of freedom
# (2 observations deleted due to missingness)
# Multiple R-squared:  0.9063,	Adjusted R-squared:  0.9059 
# F-statistic:  1993 on 2 and 412 DF,  p-value: < 2.2e-16



ILILag2_test <- lag(zoo(FluTest$ILI), -2, na.pad = T)
FluTest$ILILag2 <- coredata(ILILag2_test)

FluTest$ILILag2[1] <- FluTrain$ILI[length(FluTrain$ILI)-1]
FluTest$ILILag2[2] <- FluTrain$ILI[length(FluTrain$ILI)]


PredTest2 <- exp(predict(FluTrend2, newdata = FluTest))
sqrt(mean((PredTest2 - FluTest$ILI)^2))
