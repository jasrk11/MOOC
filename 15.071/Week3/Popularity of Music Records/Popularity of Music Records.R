# Week 3 - Logistic Regression
# Assignment 3


rm(list=ls())
setwd('/home/musigma/MOOC/15.071/Week3')

songs<-read.csv("songs.csv", header = T, stringsAsFactors = F)

# Problem 1.1 - Understanding the Data 

nrow(subset(songs, year == 2010))
# 373

table(songs$year)
# 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 
# 328  196  186  324  198  258  178  329  380  357  363  282  518  434  479  392  479  622 
# 2008 2009 2010 
# 415  483  373 

# Problem 1.2 - Understanding the Data 
MichaelJackson = subset(songs, artistname == "Michael Jackson")
nrow(MichaelJackson)
# 18

# Problem 1.3 - Understanding the Data 
MichaelJackson_top10 <- subset(MichaelJackson, Top10 == 1)

# Problem 1.4 - Understanding the Data 
table(songs$timesignature)
# 0    1    3    4    5    7 
# 10  143  503 6787  112   19 

# Problem 1.5 - Understanding the Data 
songs[which(songs$tempo == max(songs$tempo)),"songtitle"]
# "Wanna Be Startin' Somethin'"

# Problem 2.1 - Creating Our Prediction Model 
## Splitting our data
SongsTrain <- subset(songs, year <= 2009)
SongsTest <- subset(songs, year == 2010)
nrow(SongsTrain)
# 7201

# Problem 2.2 - Creating our Prediction Model 
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[,!(names(SongsTrain) %in% nonvars)]
SongsTest = SongsTest[,!(names(SongsTest) %in% nonvars)]

SongsLog1 = glm(Top10 ~., data = SongsTrain, family = binomial)

summary(SongsLog1)
# AIC: 4827.2

# Problem 3.1 - Beware of Multicollinearity Issues! 
cor(SongsTrain$loudness, SongsTrain$energy)
# 0.7399067

# Problem 3.2 - Beware of Multicollinearity Issues! 
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

# Although the coefficient for "energy" is positive, but the significance is low

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.0983  -0.5607  -0.3602  -0.1902   3.3107  
# 
# Coefficients:
#                           Estimate Std. Error z value Pr(>|z|)    
# (Intercept)              -2.241e+00  7.465e-01  -3.002 0.002686 ** 
# timesignature             1.625e-01  8.734e-02   1.860 0.062873 .  
# timesignature_confidence  6.885e-01  1.924e-01   3.578 0.000346 ***
# tempo                     5.521e-04  1.665e-03   0.332 0.740226    
# tempo_confidence          5.497e-01  1.407e-01   3.906 9.40e-05 ***
# key                       1.740e-02  1.026e-02   1.697 0.089740 .  
# key_confidence            2.954e-01  1.394e-01   2.118 0.034163 *  
# energy                    1.813e-01  2.608e-01   0.695 0.486991    
# pitch                    -5.150e+01  6.857e+00  -7.511 5.87e-14 ***
# timbre_0_min              2.479e-02  4.240e-03   5.847 5.01e-09 ***
# timbre_0_max             -1.007e-01  1.178e-02  -8.551  < 2e-16 ***
# timbre_1_min              7.143e-03  7.710e-04   9.265  < 2e-16 ***
# timbre_1_max             -7.830e-04  7.064e-04  -1.108 0.267650    
# timbre_2_min             -1.579e-03  1.109e-03  -1.424 0.154531    
# timbre_2_max              3.889e-04  8.964e-04   0.434 0.664427    
# timbre_3_min              6.500e-04  5.949e-04   1.093 0.274524    
# timbre_3_max             -2.462e-03  5.674e-04  -4.339 1.43e-05 ***
# timbre_4_min              9.115e-03  1.952e-03   4.670 3.02e-06 ***
# timbre_4_max              6.306e-03  1.532e-03   4.115 3.87e-05 ***
# timbre_5_min             -5.641e-03  1.255e-03  -4.495 6.95e-06 ***
# timbre_5_max              6.937e-04  7.807e-04   0.889 0.374256    
# timbre_6_min             -1.612e-02  2.235e-03  -7.214 5.45e-13 ***
# timbre_6_max              3.814e-03  2.157e-03   1.768 0.076982 .  
# timbre_7_min             -5.102e-03  1.755e-03  -2.907 0.003644 ** 
# timbre_7_max             -3.158e-03  1.811e-03  -1.744 0.081090 .  
# timbre_8_min              4.488e-03  2.810e-03   1.597 0.110254    
# timbre_8_max              6.423e-03  2.950e-03   2.177 0.029497 *  
# timbre_9_min             -4.282e-04  2.955e-03  -0.145 0.884792    
# timbre_9_max              3.525e-03  2.377e-03   1.483 0.138017    
# timbre_10_min             2.993e-03  1.804e-03   1.660 0.097004 .  
# timbre_10_max             7.367e-03  1.731e-03   4.255 2.09e-05 ***
# timbre_11_min            -2.837e-02  3.630e-03  -7.815 5.48e-15 ***
# timbre_11_max             1.829e-02  3.341e-03   5.476 4.34e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 6017.5  on 7200  degrees of freedom
# Residual deviance: 4871.8  on 7168  degrees of freedom
# AIC: 4937.8
# 
# Number of Fisher Scoring iterations: 6


# Problem 3.3 - Beware of Multicollinearity Issues! 
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.9182  -0.5417  -0.3481  -0.1874   3.4171  
# 
# Coefficients:
#                            Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               1.196e+01  1.714e+00   6.977 3.01e-12 ***
# timesignature             1.151e-01  8.726e-02   1.319 0.187183    
# timesignature_confidence  7.143e-01  1.946e-01   3.670 0.000242 ***
# loudness                  2.306e-01  2.528e-02   9.120  < 2e-16 ***
# tempo                    -6.460e-04  1.665e-03  -0.388 0.698107    
# tempo_confidence          3.841e-01  1.398e-01   2.747 0.006019 ** 
# key                       1.649e-02  1.035e-02   1.593 0.111056    
# key_confidence            3.394e-01  1.409e-01   2.409 0.015984 *  
# pitch                    -5.328e+01  6.733e+00  -7.914 2.49e-15 ***
# timbre_0_min              2.205e-02  4.239e-03   5.200 1.99e-07 ***
# timbre_0_max             -3.105e-01  2.537e-02 -12.240  < 2e-16 ***
# timbre_1_min              5.416e-03  7.643e-04   7.086 1.38e-12 ***
# timbre_1_max             -5.115e-04  7.110e-04  -0.719 0.471928    
# timbre_2_min             -2.254e-03  1.120e-03  -2.012 0.044190 *  
# timbre_2_max              4.119e-04  9.020e-04   0.457 0.647915    
# timbre_3_min              3.179e-04  5.869e-04   0.542 0.588083    
# timbre_3_max             -2.964e-03  5.758e-04  -5.147 2.64e-07 ***
# timbre_4_min              1.105e-02  1.978e-03   5.585 2.34e-08 ***
# timbre_4_max              6.467e-03  1.541e-03   4.196 2.72e-05 ***
# timbre_5_min             -5.135e-03  1.269e-03  -4.046 5.21e-05 ***
# timbre_5_max              2.979e-04  7.855e-04   0.379 0.704526    
# timbre_6_min             -1.784e-02  2.246e-03  -7.945 1.94e-15 ***
# timbre_6_max              3.447e-03  2.182e-03   1.580 0.114203    
# timbre_7_min             -5.128e-03  1.768e-03  -2.900 0.003733 ** 
# timbre_7_max             -3.394e-03  1.820e-03  -1.865 0.062208 .  
# timbre_8_min              3.686e-03  2.833e-03   1.301 0.193229    
# timbre_8_max              4.658e-03  2.988e-03   1.559 0.119022    
# timbre_9_min             -9.318e-05  2.957e-03  -0.032 0.974859    
# timbre_9_max              1.342e-03  2.424e-03   0.554 0.579900    
# timbre_10_min             4.050e-03  1.827e-03   2.217 0.026637 *  
# timbre_10_max             5.793e-03  1.759e-03   3.294 0.000988 ***
# timbre_11_min            -2.638e-02  3.683e-03  -7.162 7.96e-13 ***
# timbre_11_max             1.984e-02  3.365e-03   5.896 3.74e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 6017.5  on 7200  degrees of freedom
# Residual deviance: 4782.7  on 7168  degrees of freedom
# AIC: 4848.7
# 
# Number of Fisher Scoring iterations: 6


testPredict = predict(SongsLog3, newdata=SongsTest, type="response")
table(SongsTest$Top10, testPredict >= 0.45)
#   FALSE TRUE
# 0   309    5
# 1    40   19
(309+19)/(309+19+5+40)
# 0.8793566

# Problem 4.2 - Validating Our Model 
# accuracy of the baseline model be on the test set?
table(SongsTest$Top10)

314/(314+59)

# Problem 4.4 - Validating Our Model 
# Sensitivity
19/(40+19)
# specificity
309/(309+5)

# Model 3 has a very high specificity, meaning that it favors specificity 
# over sensitivity. While Model 3 only captures less than half of the Top 10 songs, 
# it still can offer a competitive edge, since it is very conservative in its predictions.