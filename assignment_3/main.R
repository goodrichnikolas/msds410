#1. Observations are the combination of variables and residuals
#so there is (x1,x2,x3,x4) and 67 residuals so the answer plus the intercept
#is 72.

#2 Null Hypothesis: The coefficient for the variable x1 is not significant.
# Ho:Beta1 = 0
#Alternative hypothesis: The coefficient for the variable x1 is significant.
#Ha: B1 != 0

#3 The t-statistic value for Beta1 is 5.327. We get this by t = Beta1/SE(Beta1) = 5.327.
# The p-value is < 1.258 x 10**-6 which allows us to reject the null hypothesis. This means
# that the variable is statistically significant. 

#4 The multiple R squared's formula = R**2 = SSR/SST where SSR is the sum of squares of the regression and
# SST is the total sum of squares. So plugging in the values we get 0.7713 because SSR = 2216, and SST = 2756.37.

#5. The adjusted R square's formula = adj.R**2 = 1 - (1-R**2)*(n-1)/(n-p-1), so swapping in values of R**2
#= 0.7713 which gives us the value 0.7577. The adjusted R-squared is different from the R-squared because
# because it takes the number of variables into account. The adjusted R-squared is lower than the R-squared
# because adding more variables to the model will always increase the R-squared value, but it may not necessarily
# increase the accuracy of the model.

#6 The null hypothesis for the overall F-Test
# H0 = B1 = B2 = B3 = B4 = 0
# The alternative hypothesis
# Ha = At lease one Bi != 0 for i = 1,2,3,4

#7 The mean square of the model is 531.50 and the mean square of the residuals is 9.41.
#The formula for the F-statistic is F = MSR/MSE = 531.50/9.41 = 56.48.
#The p-value is way below the required 0.05 so we reject the null hypothesis. This means that there is
# a very high significance for the model.
#8 Model 1 nests model 2 because all of model 1's variables are in model 2.
#9 H0 = B5 = B6 = 0
#  Ha = At least one Bi != 0 for i = 5,6
# This states that if at least one variable in model 2 is significant then model 2 is better than model 1.
# Otherwise, model 1 is better than model 2.
#10 The formula to calculate a nested f-test is 
# F = (RSS1 - RSS2)/(p2 - p1)/(RSS2/(n-p2-1))
#Plugging in 

#10 F = (630.36-572.61/6-4)/(572.61/(72-6-1)) = 3.28
#The hypothesis test is H0 = B5 = B6 = 0
#The corresponding p-value is approximately 0.044, which is less than 0.05 so we reject the null hypothesis.
#This means that model 2 is better than model 1, with the two additional variables.

r_problem = (630.36-572.61)/(6-4)/(572.61/(72-6-1))
library(readxl)

data = read_excel("./ames_housing_data.xlsx")




#Model 3
#11 
#1. Zoning
#2. BldgType
#3. HouseStyle
#4. OverallQual
#5. OverallCond
#6. YearBuilt
#7. Exterior1
#8. ExterQual
#9. ExterCond
#10. Foundation
#11. TotalBsmtSF
#12. Heating
#13. CentralAir
#14. FirstFlrSF
#15. GrLivArea
#1. Zoning
#2. BldgType
#3. HouseStyle
#4. OverallQual
#5. OverallCond
#16. FullBath
#17. HalfBath
#18. YrSold
#19. SaleType
#20. SaleCondition
#21. SalePrice

#11 Out of the original 20 variables, I decided to keep the 10 continous variables
# TotalBsmtSF, FirstFlrSF, GrLivArea, FullBath, HalfBath, YrSold, OverallQual, OverallCond, YearBuilt,
#and SalePrice
#If I were to divide them into two groups based on their type I would group
# GrLivArea, TotalBsmtSF, FirstFlrSF, FullBath, HalfBath together as these have to do with space
# The rest are the condition of the space provided. Either one by themselves won't necessarily be a good
# predictor of the price as large bad houses exist and small nice houses exist, but traditionally square
# footage is incredibly important, no matter the quality.

#12 I picked the living area set to run a multiple regression model on to prdict my Y variable SalePrice

size_variables = c("GrLivArea", "TotalBsmtSF", "FirstFlrSF", "FullBath", "HalfBath", "SalePrice")
size_data = data[,size_variables]
#Dropped NA
size_data = na.omit(size_data)

model_3 = lm(SalePrice ~ GrLivArea + TotalBsmtSF + FirstFlrSF + FullBath + HalfBath, data = size_data)
summary(model_3)
#anova
anova(model_3)

#a: Null Hypothesis: For any given variable in size_variables, the coefficient is not significant.
# Ha: For any given variable in size_variables, the coefficient is significant.
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -35152.145   3241.329 -10.845  < 2e-16 ***
#  GrLivArea       49.172      3.098  15.872  < 2e-16 ***
#  TotalBsmtSF     64.290      3.322  19.354  < 2e-16 ***
#  FirstFlrSF      23.929      4.359   5.489 4.38e-08 ***
#  FullBath     23937.968   2068.224  11.574  < 2e-16 ***
#  HalfBath     24674.383   2216.902  11.130  < 2e-16 ***
#P-values: 
# We see that the P-Values for each variable are well under 0.05 so we reject the null hypothesis
#b: The null hypothesis for the omnibus Overall F-test is
# H0 = B1 = B2 = B3 = B4 = B5 = B6 = 0
# The alternative hypothesis
# Ha = At lease one Bi != 0 for i = 1,2,3,4,5,6
#The F-statisic value from summary(model_3) is 1094 and the p-value is 2.2e-16 which shows an 
#extremely high significance for the model.

#Model 4
#13
total_set = c("GrLivArea", "TotalBsmtSF", "FirstFlrSF", "FullBath", "HalfBath", "SalePrice", "OverallQual", "OverallCond", "YearBuilt", "YrSold")
total_data = data[,total_set]
#Dropped NA
total_data = na.omit(total_data)

model_4 = lm(SalePrice ~ GrLivArea + TotalBsmtSF + FirstFlrSF + FullBath + HalfBath + OverallQual + OverallCond + YearBuilt + YrSold, data = total_data)
summary(model_4)
#anova
anova(model_4)
#Call:
#  lm(formula = SalePrice ~ GrLivArea + TotalBsmtSF + FirstFlrSF + 
#       FullBath + HalfBath + OverallQual + OverallCond + YearBuilt + 
#       YrSold, data = total_data)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-543222  -18802   -2147   14580  263797 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  5.232e+04  1.043e+06   0.050  0.95998    
#GrLivArea    5.763e+01  2.788e+00  20.673  < 2e-16 ***
#  TotalBsmtSF  2.300e+01  2.784e+00   8.264  < 2e-16 ***
#  FirstFlrSF   1.964e+01  3.450e+00   5.694 1.37e-08 ***
#  FullBath    -5.864e+03  1.894e+03  -3.095  0.00198 ** 
#  HalfBath    -2.038e+02  1.944e+03  -0.105  0.91651    
#OverallQual  2.138e+04  7.693e+02  27.783  < 2e-16 ***
#  OverallCond  6.012e+03  6.794e+02   8.849  < 2e-16 ***
#  YearBuilt    5.634e+02  3.582e+01  15.731  < 2e-16 ***
#  YrSold      -6.326e+02  5.178e+02  -1.222  0.22191    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 36840 on 2919 degrees of freedom
#Multiple R-squared:  0.788,	Adjusted R-squared:  0.7873 
#F-statistic:  1205 on 9 and 2919 DF,  p-value: < 2.2e-16

#> #anova
#  > anova(model_4)
# Analysis of Variance Table
# 
# Response: SalePrice
# Df     Sum Sq    Mean Sq   F value    Pr(>F)    
# GrLivArea      1 9.3284e+12 9.3284e+12 6874.9232 < 2.2e-16 ***
#   TotalBsmtSF    1 2.3558e+12 2.3558e+12 1736.1521 < 2.2e-16 ***
#   FirstFlrSF     1 2.2490e+09 2.2490e+09    1.6575 0.1980419    
# FullBath       1 2.1270e+11 2.1270e+11  156.7576 < 2.2e-16 ***
#   HalfBath       1 2.7578e+11 2.7578e+11  203.2481 < 2.2e-16 ***
#   OverallQual    1 2.1876e+12 2.1876e+12 1612.2210 < 2.2e-16 ***
#   OverallCond    1 2.0430e+10 2.0430e+10   15.0565 0.0001066 ***
#   YearBuilt      1 3.3646e+11 3.3646e+11  247.9680 < 2.2e-16 ***
#   YrSold         1 2.0253e+09 2.0253e+09    1.4926 0.2219062    
# Residuals   2919 3.9607e+12 1.3569e+09                        
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#a: Null Hypothesis: For any given variable in total_set, the coefficient is not significant.
# Ha: For any given variable in total_set, the coefficient is significant.
#b The null hypothesis for the omnibus Overall F-test is
# H0 = B1 = B2 = B3 = B4 = B5 = B6 = B7 = B8 = B9 = 0
# The alternative hypothesis
# Ha = At lease one Bi != 0 for i = 1,2,3,4,5,6,7,8,9
# The F-statistic value from summary(model_4) is 1205 and all p-values presented are well below except
# for YrSold and FirstFlrSF. This shows an extremely high significance for the model, but for those two
# we cannot reject the null hypothesis.

#Nested Model
#The nested F-test hypothesis for model 3 and model are
# H0 = B7 = B8 = B9 = 0
# Ha = At least one Bi != 0 for i = 7,8,9

#Model for the nested F-test
nested_model = lm(SalePrice ~ GrLivArea + TotalBsmtSF + FirstFlrSF + FullBath + HalfBath + OverallQual + OverallCond, data = total_data)
summary(nested_model)
anova(nested_model)
# Call:
#   lm(formula = SalePrice ~ GrLivArea + TotalBsmtSF + FirstFlrSF + 
#        FullBath + HalfBath + OverallQual + OverallCond, data = total_data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -527352  -20342    -773   17578  272642 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.289e+05  5.279e+03 -24.423  < 2e-16 ***
#   GrLivArea    3.699e+01  2.556e+00  14.475  < 2e-16 ***
#   TotalBsmtSF  2.809e+01  2.879e+00   9.756  < 2e-16 ***
#   FirstFlrSF   2.751e+01  3.554e+00   7.739 1.37e-14 ***
#   FullBath     6.380e+03  1.796e+03   3.553 0.000387 ***
#   HalfBath     1.174e+04  1.861e+03   6.308 3.26e-10 ***
#   OverallQual  2.699e+04  7.103e+02  38.004  < 2e-16 ***
#   OverallCond  2.490e+03  6.684e+02   3.726 0.000198 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 38360 on 2921 degrees of freedom
# Multiple R-squared:  0.7699,	Adjusted R-squared:  0.7693 
# F-statistic:  1396 on 7 and 2921 DF,  p-value: < 2.2e-16
# 
# > anova(nested_model)
# Analysis of Variance Table
# 
# Response: SalePrice
# Df     Sum Sq    Mean Sq   F value    Pr(>F)    
# GrLivArea      1 9.3284e+12 9.3284e+12 6337.9832 < 2.2e-16 ***
#   TotalBsmtSF    1 2.3558e+12 2.3558e+12 1600.5565 < 2.2e-16 ***
#   FirstFlrSF     1 2.2490e+09 2.2490e+09    1.5281 0.2165037    
# FullBath       1 2.1270e+11 2.1270e+11  144.5146 < 2.2e-16 ***
#   HalfBath       1 2.7578e+11 2.7578e+11  187.3741 < 2.2e-16 ***
#   OverallQual    1 2.1876e+12 2.1876e+12 1486.3046 < 2.2e-16 ***
#   OverallCond    1 2.0430e+10 2.0430e+10   13.8805 0.0001985 ***
#   Residuals   2921 4.2992e+12 1.4718e+09                        
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#14
#The F-statistic value from summary(nested_model) is 1396 and the p-value is 2.2e-16 which shows an
#extremely high significance for the model. This means that the model with the additional variables is
#better than the model without the additional variables.
#Even in the updated nested model, FirstFlrSF and YrSold are not significant.
