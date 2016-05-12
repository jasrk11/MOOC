rm(list = ls())
setwd("/home/musigma/MOOC/15.071/Week3/Predicting loan repayment/")

loans <- read.csv("loans.csv", header = T, stringsAsFactors = F)

# Problem 1.1 - Preparing the Dataset 
table(loans$not.fully.paid)
1533/(8045+1533)

# Problem 1.2 - Preparing the Dataset 
summary(loans)

# Problem 1.4 - Preparing the Dataset 
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed
# We predicted missing variable values using the available independent 
# variables for each observation

loans <- read.csv("loans_imputed.csv", header = T, stringsAsFactors = F)
# Problem 2.1 - Prediction Models 
library(caTools)
set.seed(144)
split = sample.split(loans$not.fully.paid, SplitRatio=0.7)
train = subset(loans, split==TRUE)
test = subset(loans, split==FALSE)

LoanModel1 = glm(not.fully.paid~., data=train, family="binomial")
summary(LoanModel1)
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.2049  -0.6205  -0.4951  -0.3606   2.6397  
# 
# Coefficients:
#                             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                9.187e+00  1.554e+00   5.910 3.42e-09 ***
# credit.policy             -3.368e-01  1.011e-01  -3.332 0.000861 ***
# purposecredit_card        -6.141e-01  1.344e-01  -4.568 4.93e-06 ***
# purposedebt_consolidation -3.212e-01  9.183e-02  -3.498 0.000469 ***
# purposeeducational         1.347e-01  1.753e-01   0.768 0.442201    
# purposehome_improvement    1.727e-01  1.480e-01   1.167 0.243135    
# purposemajor_purchase     -4.830e-01  2.009e-01  -2.404 0.016203 *  
# purposesmall_business      4.120e-01  1.419e-01   2.905 0.003678 ** 
# int.rate                   6.110e-01  2.085e+00   0.293 0.769446    
# installment                1.275e-03  2.092e-04   6.093 1.11e-09 ***
# log.annual.inc            -4.337e-01  7.148e-02  -6.067 1.30e-09 ***
# dti                        4.638e-03  5.502e-03   0.843 0.399288    
# fico                      -9.317e-03  1.710e-03  -5.448 5.08e-08 ***
# days.with.cr.line          2.371e-06  1.588e-05   0.149 0.881343    
# revol.bal                  3.085e-06  1.168e-06   2.641 0.008273 ** 
# revol.util                 1.839e-03  1.535e-03   1.199 0.230722    
# inq.last.6mths             8.437e-02  1.600e-02   5.275 1.33e-07 ***
# delinq.2yrs               -8.320e-02  6.561e-02  -1.268 0.204762    
# pub.rec                    3.300e-01  1.139e-01   2.898 0.003756 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 5896.6  on 6704  degrees of freedom
# Residual deviance: 5485.2  on 6686  degrees of freedom
# AIC: 5523.2
# 
# Number of Fisher Scoring iterations: 5

# Problem 2.2 - Prediction Models 
8.951e-03 * 10
exp(8.951e-03 * 10)

# Problem 2.3 - Prediction Models 
predicted.risk <- predict(LoanModel1, newdata = test, type = "response")
test$not.fully.paid.predicted <- predicted.risk 

table(test$not.fully.paid, predicted.risk >= 0.5)
#   FALSE TRUE
# 0  2400   13
# 1   457    3
# Accuracy
(2400+3)/(2400+3+13+457)

# Baseline model accuracy
(2400+13)/(2400+13+457+3)


# Problem 2.4 - Prediction Models 
# Use the ROCR package to compute the test set AUC.
library(ROCR)
ROCRpred <- prediction(predicted.risk, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

#  Problem 3.1 - A "Smart Baseline" 
LoanModel2 = glm(not.fully.paid~int.rate, data=train, family="binomial")
summary(LoanModel2)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.0547  -0.6271  -0.5442  -0.4361   2.2914  
# 
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -3.6726     0.1688  -21.76   <2e-16 ***
# int.rate     15.9214     1.2702   12.54   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 5896.6  on 6704  degrees of freedom
# Residual deviance: 5734.8  on 6703  degrees of freedom
# AIC: 5738.8
# 
# Number of Fisher Scoring iterations: 4

# Problem 3.2 - A "Smart Baseline" 
predicted.risk_binomial <- predict(LoanModel2, newdata = test, type = "response")
max(predicted.risk_binomial)
table(test$not.fully.paid, predicted.risk_binomial >= 0.5)
table(predicted.risk_binomial>=0.5)

# Problem 4.1 - Computing the Profitability of an Investment 
10*exp(0.18)
# 11.97217

# Problem 5.1 - A Simple Investment Strategy 
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1

max(test$profit)

# Problem 6.1 - An Investment Strategy Based on Risk 
highInterest <- subset(test, int.rate >= 0.15)
mean(highInterest$profit)
table(highInterest$not.fully.paid)
110/(437)

# Problem 6.2 - An Investment Strategy Based on Risk 
cutoff = sort(highInterest$not.fully.paid.predicted, decreasing=FALSE)[100]
selectedLoans <- subset(highInterest, not.fully.paid.predicted <= cutoff)
sum(selectedLoans$profit)
# 31.27825
table(selectedLoans$not.fully.paid)
#  0  1 
# 81 19 

