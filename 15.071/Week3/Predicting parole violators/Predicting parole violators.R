rm(list = ls())


# Problem 1.1 - Loading the Dataset 
parole <- read.csv("parole.csv")
str(parole)
# 'data.frame':	675 obs. of  9 variables:
# $ male             : int  1 0 1 1 1 1 1 0 0 1 ...
# $ race             : int  1 1 2 1 2 2 1 1 1 2 ...
# $ age              : num  33.2 39.7 29.5 22.4 21.6 46.7 31 24.6 32.6 29.1 ...
# $ state            : int  1 1 1 1 1 1 1 1 1 1 ...
# $ time.served      : num  5.5 5.4 5.6 5.7 5.4 6 6 4.8 4.5 4.7 ...
# $ max.sentence     : int  18 12 12 18 12 18 18 12 13 12 ...
# $ multiple.offenses: int  0 0 0 0 0 0 0 0 0 0 ...
# $ crime            : int  4 3 3 1 1 4 3 1 3 2 ...
# $ violator         : int  0 0 0 0 0 0 0 0 0 0 ...

# Number of parolees in the dataset violated the terms of their parole
nrow(subset(parole, violator == 1))
# 78

# Problem 2.2 - Preparing the Dataset 
parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)
summary(parole$crime)
str(parole$crime)
table(parole$crime)


# Problem 3.1 - Splitting into a Training and Testing Set 
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)


# Problem 4.1 - Building a Logistic Regression Model 
ParlViol1 <- glm(violator ~., data = train, family = binomial)
summary(ParlViol1)
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.7041  -0.4236  -0.2719  -0.1690   2.8375  
# 
# Coefficients:
#                     Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -4.2411574  1.2938852  -3.278  0.00105 ** 
# male               0.3869904  0.4379613   0.884  0.37690    
# race               0.8867192  0.3950660   2.244  0.02480 *  
# age               -0.0001756  0.0160852  -0.011  0.99129    
# state2             0.4433007  0.4816619   0.920  0.35739    
# state3             0.8349797  0.5562704   1.501  0.13335    
# state4            -3.3967878  0.6115860  -5.554 2.79e-08 ***
# time.served       -0.1238867  0.1204230  -1.029  0.30359    
# max.sentence       0.0802954  0.0553747   1.450  0.14705    
# multiple.offenses  1.6119919  0.3853050   4.184 2.87e-05 ***
# crime2             0.6837143  0.5003550   1.366  0.17180    
# crime3            -0.2781054  0.4328356  -0.643  0.52054    
# crime4            -0.0117627  0.5713035  -0.021  0.98357    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 340.04  on 472  degrees of freedom
# Residual deviance: 251.48  on 460  degrees of freedom
# AIC: 277.48
# 
# Number of Fisher Scoring iterations: 6

# Problem 4.3 - Building a Logistic Regression Model 
# what are the odds this individual is a violator
exp(-4.2411574+0.3869904+0.8867192-0.0001756*50-0.1238867*3+0.0802954*12+0.6837143)
# 0.1825687
# what is the probability this individual is a violator
1/(1+exp(-(-4.2411574+0.3869904+0.8867192-0.0001756*50-0.1238867*3+0.0802954*12+0.6837143)))
# 0.1543832

# Problem 5.1 - Evaluating the Model on the Testing Set 
testPredict <- predict(ParlViol1, newdata=test, type="response")
# maximum predicted probability of a violation
max(testPredict)
# 0.9072791

table(test$violator, testPredict>=0.5)
#   FALSE TRUE
# 0   167   12
# 1    11   12

# Sensitivity
12/(11+12)
# 0.5217391

# Specificity
167/(167+12)
# 0.9329609

# Accuracy
(167+12)/(167+12+11+12)
# 0.8861386

# Accuracy of baseline model
(167+12)/(167+12+11+12)
# 0.8861386       # WTH!!

library(ROCR)
ROCRpred <- prediction(testPredict, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)
# 0.8945834

# Describe the meaning of AUC in this context.
# The probability the model can correctly differentiate between a randomly 
# selected parole violator and a randomly selected parole non-violator

# Problem 6.1 - Identifying Bias in Observational Data 
# We should use a dataset tracking a group of parolees from the start of 
# their parole until either they violated parole or they completed their term. 