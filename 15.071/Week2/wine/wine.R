rm(list=ls())
setwd("/home/musigma/MOOC/15.071/Week2/wine/")

wine <- read.csv("wine.csv", header = T, stringsAsFactors = F)

model1 <- lm(Price ~ WinterRain+HarvestRain, data = wine)
summary(model1)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.0933 -0.3222 -0.1012  0.3871  1.1877 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  7.865e+00  6.616e-01  11.888 4.76e-11 ***
# WinterRain  -9.848e-05  9.007e-04  -0.109  0.91392    
# HarvestRain -4.971e-03  1.601e-03  -3.105  0.00516 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.5611 on 22 degrees of freedom
# Multiple R-squared:  0.3177,	Adjusted R-squared:  0.2557 
# F-statistic: 5.122 on 2 and 22 DF,  p-value: 0.01492

cor(wine$WinterRain, wine$HarvestRain)
# -0.2754409