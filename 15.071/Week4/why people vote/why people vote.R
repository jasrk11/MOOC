# Read in the data
rm(list=ls())
setwd("/home/musigma/MOOC/15.071/Week4/why people vote/")
gerber = read.csv("gerber.csv")

# proportion of people in this dataset voted in this election
nrow(subset(gerber, voting == 1))/nrow(gerber)
# 0.3158996

# Which of the four "treatment groups" had the largest percentage of people who actually voted (voting = 1)?
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$neighbors, mean) ##
tapply(gerber$voting, gerber$self, mean)

logistic <- glm(voting ~ hawthorne + civicduty + neighbors + self, data = gerber, family = "binomial")
summary(logistic)
trainPredict = predict(logistic, type = "response")
table(gerber$voting, trainPredict >= 0.3)
#   FALSE   TRUE
# 0 134513 100875
# 1  56730  51966
# Accuracy-
(134513+51966)/(134513+51966+100875+56730)

table(gerber$voting, trainPredict >= 0.5)
#   FALSE
# 0 235388
# 1 108696
235388/(108696+235388)

# AUC
library(ROCR)
pred = prediction(trainPredict, gerber$voting)
AUC = as.numeric(performance(pred, "auc")@y.values)
# 0.5308461

# Build a regression tree
library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)
# There are no splits in the tree, because none of the 
# variables make a big enough effect to be split on.

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

# Make a new tree that includes the "sex" variable, again with cp = 0.0. Notice that sex appears as a 
# split that is of secondary importance to the treatment group.
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)

# Problem 3.1 - Interaction Terms
CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)
CARTmodel5 = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTmodel4, digits = 6)
abs(0.296638-0.34)
# 0.043362

prp(CARTmodel5, digits = 6)
abs(0.2904558-0.3341757)
abs(0.3027947-0.3458183)

# Going back to logistic regression now, create a model using "sex" and "control". 
# Interpret the coefficient for "sex":
LogModelSex = (glm(voting ~ control + sex, data=gerber, family = binomial))
summary(LogModelSex)
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -0.635538   0.006511 -97.616  < 2e-16 ***
# control     -0.200142   0.007364 -27.179  < 2e-16 ***
# sex         -0.055791   0.007343  -7.597 3.02e-14 ***

# Problem 3.4 - Interaction Terms 
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogModelSex, newdata=Possibilities, type="response")
#     1         2         3         4 
# 0.3462559 0.3024455 0.3337375 0.2908065 
abs(0.2908065-0.290456)

# We're going to add a new term to our logistic regression now, 
# that is the combination of the "sex" and "control" variables - so 
# if this new variable is 1, that means the person is a woman AND in the control group.

LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)
predict(LogModel2, newdata=Possibilities, type="response")
#     1         2         3         4 
# 0.3458183 0.3027947 0.3341757 0.2904558 
abs(0.290456-0.2904558)

# We should not use all possible interaction terms in a logistic regression model 
# due to overfitting. Even in this simple problem, we have four treatment groups 
# and two values for sex. If we have an interaction term for every treatment 
# variable with sex, we will double the number of variables. In smaller data sets, 
# this could quickly lead to overfitting.