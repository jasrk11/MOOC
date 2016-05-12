rm(list=ls())

pisaTrain <- read.csv("pisa2009Train.csv")
pisaTest <- read.csv("pisa2009Test.csv")

tapply(pisaTrain$readingScore, pisaTrain$male, mean)
#       0        1 
# 512.9406 483.5325 


lapply(pisaTrain, mean)

pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

summary(pisaTrain$grade)
is.factor(pisaTrain$raceeth)


pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")
summary(pisaTrain$raceeth)

lmScore <- lm(readingScore ~., data = pisaTrain)
summary(lmScore)
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -247.44  -48.86    1.86   49.77  217.18 
# 
# Coefficients:
#                                                     Estimate Std. Error t value Pr(>|t|)    
#     (Intercept)                                   143.766333  33.841226   4.248 2.24e-05 ***
#     grade                                          29.542707   2.937399  10.057  < 2e-16 ***
#     male                                          -14.521653   3.155926  -4.601 4.42e-06 ***
#     raceethAmerican Indian/Alaska Native          -67.277327  16.786935  -4.008 6.32e-05 ***
#     raceethAsian                                   -4.110325   9.220071  -0.446  0.65578    
#     raceethBlack                                  -67.012347   5.460883 -12.271  < 2e-16 ***
#     raceethHispanic                               -38.975486   5.177743  -7.528 7.29e-14 ***
#     raceethMore than one race                     -16.922522   8.496268  -1.992  0.04651 *  
#     raceethNative Hawaiian/Other Pacific Islander  -5.101601  17.005696  -0.300  0.76421    
#     preschool                                      -4.463670   3.486055  -1.280  0.20052    
#     expectBachelors                                55.267080   4.293893  12.871  < 2e-16 ***
#     motherHS                                        6.058774   6.091423   0.995  0.32001    
#     motherBachelors                                12.638068   3.861457   3.273  0.00108 ** 
#     motherWork                                     -2.809101   3.521827  -0.798  0.42517    
#     fatherHS                                        4.018214   5.579269   0.720  0.47147    
#     fatherBachelors                                16.929755   3.995253   4.237 2.35e-05 ***
#     fatherWork                                      5.842798   4.395978   1.329  0.18393    
#     selfBornUS                                     -3.806278   7.323718  -0.520  0.60331    
#     motherBornUS                                   -8.798153   6.587621  -1.336  0.18182    
#     fatherBornUS                                    4.306994   6.263875   0.688  0.49178    
#     englishAtHome                                   8.035685   6.859492   1.171  0.24153    
#     computerForSchoolwork                          22.500232   5.702562   3.946 8.19e-05 ***
#     read30MinsADay                                 34.871924   3.408447  10.231  < 2e-16 ***
#     minutesPerWeekEnglish                           0.012788   0.010712   1.194  0.23264    
#     studentsInEnglish                              -0.286631   0.227819  -1.258  0.20846    
#     schoolHasLibrary                               12.215085   9.264884   1.318  0.18749    
#     publicSchool                                  -16.857475   6.725614  -2.506  0.01226 *  
#     urban                                          -0.110132   3.962724  -0.028  0.97783    
#     schoolSize                                      0.006540   0.002197   2.977  0.00294 ** 
#     ---
#     Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 73.81 on 2385 degrees of freedom
# (1249 observations deleted due to missingness)
# Multiple R-squared:  0.3251,	Adjusted R-squared:  0.3172 
# F-statistic: 41.04 on 28 and 2385 DF,  p-value: < 2.2e-16


## Problem 4.1 - Predicting on unseen data

predTest <- predict(lmScore, newdata = pisaTest)
summary(predTest)
