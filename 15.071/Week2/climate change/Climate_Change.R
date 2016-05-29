rm(list=ls())


df <- read.csv("climate_change.csv")

colnames(df)
# [1] "Year"     "Month"    "MEI"      "CO2"      "CH4"      "N2O"      "CFC.11"   "CFC.12"   "TSI"      "Aerosols"
# [11] "Temp"    

# train & Test

train <- subset(df, Year <= 2006)
test <- subset(df, Year > 2006)

lm_1 <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = train)
summary(lm_1)
# 
# Call:
#     lm(formula = Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + 
#            TSI + Aerosols, data = train)
# 
# Residuals:
#     Min       1Q   Median       3Q      Max 
# -0.25888 -0.05913 -0.00082  0.05649  0.32433 
# 
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
#     (Intercept) -1.246e+02  1.989e+01  -6.265 1.43e-09 ***
#     MEI          6.421e-02  6.470e-03   9.923  < 2e-16 ***
#     CO2          6.457e-03  2.285e-03   2.826  0.00505 ** 
#     CH4          1.240e-04  5.158e-04   0.240  0.81015    
#     N2O         -1.653e-02  8.565e-03  -1.930  0.05467 .  
#     CFC.11      -6.631e-03  1.626e-03  -4.078 5.96e-05 ***
#     CFC.12       3.808e-03  1.014e-03   3.757  0.00021 ***
#     TSI          9.314e-02  1.475e-02   6.313 1.10e-09 ***
#     Aerosols    -1.538e+00  2.133e-01  -7.210 5.41e-12 ***
#     ---
#     Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.09171 on 275 degrees of freedom
# Multiple R-squared:  0.7509,	Adjusted R-squared:  0.7436 
# F-statistic: 103.6 on 8 and 275 DF,  p-value: < 2.2e-16

## Finding Correlation:

cor(train)
#               Year         Month           MEI         CO2         CH4         N2O      CFC.11        CFC.12
# Year      1.00000000 -0.0279419602 -0.0369876842  0.98274939  0.91565945  0.99384523  0.56910643  0.8970116635
# Month    -0.02794196  1.0000000000  0.0008846905 -0.10673246  0.01856866  0.01363153 -0.01311122  0.0006751102
# MEI      -0.03698768  0.0008846905  1.0000000000 -0.04114717 -0.03341930 -0.05081978  0.06900044  0.0082855443
# CO2       0.98274939 -0.1067324607 -0.0411471651  1.00000000  0.87727963  0.97671982  0.51405975  0.8526896272
# CH4       0.91565945  0.0185686624 -0.0334193014  0.87727963  1.00000000  0.89983864  0.77990402  0.9636162478
# N2O       0.99384523  0.0136315303 -0.0508197755  0.97671982  0.89983864  1.00000000  0.52247732  0.8679307757
# CFC.11    0.56910643 -0.0131112236  0.0690004387  0.51405975  0.77990402  0.52247732  1.00000000  0.8689851828
# CFC.12    0.89701166  0.0006751102  0.0082855443  0.85268963  0.96361625  0.86793078  0.86898518  1.0000000000
# TSI       0.17030201 -0.0346061935 -0.1544919227  0.17742893  0.24552844  0.19975668  0.27204596  0.2553028138
# Aerosols -0.34524670  0.0148895406  0.3402377871 -0.35615480 -0.26780919 -0.33705457 -0.04392120 -0.2251312440
# Temp      0.78679714 -0.0998567411  0.1724707512  0.78852921  0.70325502  0.77863893  0.40771029  0.6875575483

#               TSI    Aerosols        Temp
# Year      0.17030201 -0.34524670  0.78679714
# Month    -0.03460619  0.01488954 -0.09985674
# MEI      -0.15449192  0.34023779  0.17247075
# CO2       0.17742893 -0.35615480  0.78852921
# CH4       0.24552844 -0.26780919  0.70325502
# N2O       0.19975668 -0.33705457  0.77863893
# CFC.11    0.27204596 -0.04392120  0.40771029
# CFC.12    0.25530281 -0.22513124  0.68755755
# TSI       1.00000000  0.05211651  0.24338269
# Aerosols  0.05211651  1.00000000 -0.38491375
# Temp      0.24338269 -0.38491375  1.00000000



### Given that the correlations are so high, let us focus on the N2O variable 
### and build a model with only MEI, TSI, Aerosols and N2O as independent variables. 
### Remember to use the training set to build the model.

lm_2 <- lm(Temp ~ MEI + N2O + TSI + Aerosols, data = train)
summary(lm_2)

# Residuals:
#     Min       1Q   Median       3Q      Max 
# -0.27916 -0.05975 -0.00595  0.05672  0.34195 
# 
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
#     (Intercept) -1.162e+02  2.022e+01  -5.747 2.37e-08 ***
#     MEI          6.419e-02  6.652e-03   9.649  < 2e-16 ***
#     N2O          2.532e-02  1.311e-03  19.307  < 2e-16 ***
#     TSI          7.949e-02  1.487e-02   5.344 1.89e-07 ***
#     Aerosols    -1.702e+00  2.180e-01  -7.806 1.19e-13 ***
#     ---
#     Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.09547 on 279 degrees of freedom
# Multiple R-squared:  0.7261,	Adjusted R-squared:  0.7222 
# F-statistic: 184.9 on 4 and 279 DF,  p-value: < 2.2e-16


### Using the step function

lm_3 <- step(lm_1)
summary(lm_3)

# Residuals:
#     Min       1Q   Median       3Q      Max 
# -0.25770 -0.05994 -0.00104  0.05588  0.32203 
# 
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
#     (Intercept) -1.245e+02  1.985e+01  -6.273 1.37e-09 ***
#     MEI          6.407e-02  6.434e-03   9.958  < 2e-16 ***
#     CO2          6.402e-03  2.269e-03   2.821 0.005129 ** 
#     N2O         -1.602e-02  8.287e-03  -1.933 0.054234 .  
#     CFC.11      -6.609e-03  1.621e-03  -4.078 5.95e-05 ***
#     CFC.12       3.868e-03  9.812e-04   3.942 0.000103 ***
#     TSI          9.312e-02  1.473e-02   6.322 1.04e-09 ***
#     Aerosols    -1.540e+00  2.126e-01  -7.244 4.36e-12 ***
#     ---
#     Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.09155 on 276 degrees of freedom
# Multiple R-squared:  0.7508,	Adjusted R-squared:  0.7445 
# F-statistic: 118.8 on 7 and 276 DF,  p-value: < 2.2e-16


### Testing the lk_3 model on test data
predict_lm_3 <- predict(lm_3, newdata = test)
summary(predict_lm_3)


