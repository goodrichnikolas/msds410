#Import 
library(readxl)
library(ggplot2)
library(gridExtra)
#USStates
data = read_excel("USStates.xlsx")

#Output the column names
colnames(data)
#1
#Let's start with traditional EDA before we attempt to tackle the problem. We are trying to find explanatory and response variables.
#Explanatory variables are the variables that we think will explain the response variable, or are independent.
#Whereas the response variable is the variable that we are trying to predict, or is dependent.
#We don't necessarily have a goal yet on what we want to predict, hence the EDA.

str(df)

#Create a table of the top 5 rows
head(data,5)

#A brief look shows us that the USStates dataset provides description information about the 50 states in the US.
#We have categorical variables such as State and region, and numeric ones such as population and HouseholdIncome.
#The rest appear to be percentages. We can surmise that the school variables are the percent of the population
#That have completed a certain level of education. There are many health related such as Smokers, HeavyDrinkers.
#And finally the last important is Insured.
#It's obvious these statistics are meant to draft up insurance policies.

#Let's look at the summary of the data

summary(data)

#Since this data is already the average for each state, we don't see any crazy outliers, but had this been individuals, we would
#see larger extremes. 

both = c("HouseholdIncome", "HighSchool", "College", "Smokers", "PhysicalActivity", "Obese")

explanatory = c("Population", "NoneWhite", "Region", "HeavyDrinkers", "TwoParents", NA)

response = c( "Insured", NA,NA, NA, NA, NA)

#Create a table for both, explanatory, and response variables, just the names within
variable_table = data.frame(both, explanatory, response)

#The categories that describe characteristics such as income, school, whether some is a smoker, etc. are all explanatory variables, but
#at the same time they could be useful for responses, such as for the insurance company, to decide whether someone is not
#reporting they are a smoker.
#It's unlikely that the company would want to use the other characteristics to guess whether someone was not white, or in a certain
#region or state. So I decided that they would be used as input to guess other characteristics.
#I only had one that was only a response, because it seems to be the goal to find out what percentage of the population
#either needs or haves insurance already for them to guess.

#2

#The population of interest is unique here. On a surface level one would assume that the population would be Americans living in
#these states. That's only partly true. Like I mentioned above, these are the averages. If it were individuals, we would see
#more extreme outliers. This isn't the case. The population of interest is the average of each state. Any sort of supervised model
#will only ingest these averages which is a culmination of larger populations within each state.

#3
#We move on to HouseholdIncome as our response variable, Y.

#Non-demographic variables
non_demographic_vars = c("HighSchool", "College", "Smokers", "PhysicalActivity", "Obese",
                         "NonWhite", "HeavyDrinkers", "TwoParents", "Insured", "HouseholdIncome")
#Create scatter plots against HouseholdIncome


#Plot list
plot_list = list()

for (i in non_demographic_vars){
  p = ggplot(data, aes_string(x = i, y = "HouseholdIncome")) + geom_point() + geom_smooth(method = "lm")
  plot_list[[i]] = p
  
}

#Arrange all plots into one image
grid.arrange(grobs = plot_list, ncol = 3)

#There are a few surprises amongst the list. I think it goes without saying that those with college degrees earn more
#on average. There is a strong correlation between College and HouseholdIncome. It also goes without saying that
#those that have more money tend to have more insurance. People who do not smoke tend to make more, but the rest
#are not so obvious.

#4 Now we move on to Pearson Product Moment correlations for non-demographic variables.
#Here is a table of the R values.

corr_dataframe = data.frame(non_demographic_vars, NA)

correlations = cor(data[,non_demographic_vars])

#Our table confirms that at 4 of the variables show a strong positive or negative correlation. Smoking and obesity
#Have strong linearity for less income, while insured and college show strong correlation for income.
#I think these are grounds for using multi-variable linear regression because both the numbers and the visual aids
#make it clear they could be great predictors of income.

#5
#Fit the model to College

model1 = lm(HouseholdIncome ~ College, data = data)
summary(model1)
#As I said above, college had the biggest correlation with income. Building a model with just college as the predictor
#lets us see if the best is good enough. If the most correlated variable cannot perform, then it's going to be a waste
#of time to continue with the rest.
#Having a look we see the minimum residual is -7.3 and the maximum is 23.5 which means that the largest
#underprediction is 7 units below the true value and largest is 23 units above the true value.
#The medium is -2.2 so it's slightly negative and is going to be a little biased towards underpredicting.
#The prediction equation is the Y-intercept plus the slope multiplied by the predictor, in this case, College
#Y = 23.0664 + 0.9801 * College
#For each unit, we increase by 0.98 multiplied by college.
#The Adjusted R-squared is 0.459 which means that 45.9% of the variation in income is explained by college.

#6
# The null hypothesis is that the slope is 0, or that there is no relationship between college and income.
# We can reject this because the slope is 0.98 and the p-value is 1.2e-09 which is significantly smaller.
# The alternative hypothesis is that there is a relationship between college and income.
# Looking at the p-value, we can reject the null hypothesis and accept the alternative hypothesis.
# The null hypotheses for the omnibus or the overall model is that the model is not significant.
# Whereas the alternative hypothesis is that the model is significant.
# Here is the ANOVA table of the F-test.
anova(model1)
#Response: HouseholdIncome
#Df Sum Sq Mean Sq F value    Pr(>F)    
#College    1 1739.4 1739.36  42.572 3.941e-08 ***
#  Residuals 48 1961.1   40.86                      
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#The Anova function outputs the F-statistic and the p-value. The p-value is 3.941e-08 which is significantly smaller
#than 0.05. We can reject the null hypothesis and accept the alternative hypothesis that the model is significant.
#The F-statistic is 42.572 which is significantly larger than 1. We can reject the null hypothesis and accept the
#alternative hypothesis that the model is significant.

#7
data$Y_hat = predict(model1, data)
data$residuals = data$HouseholdIncome - data$Y_hat
#Sum of Square Residuals
SSR = sum(data$residuals^2)

#Sum of Squares Total
Y_bar = mean(data$HouseholdIncome)
SST = sum((data$HouseholdIncome - Y_bar)^2)

#Sum of squares due to regression
SSR_ = sum((data$Y_hat - Y_bar)^2)

#Calculate the statistic (SSR/SST)
statistic = SSR / SST

print(paste("Sum of Squares Residuals: ", SSR))
print(paste("Sum of Squares Total: ", SST))
print(paste("Sum of Squares Regression: ", SSR_))
print(paste("F-statistic: ", statistic))


#[1] "Sum of Squares Residuals:  1961.12951169001"

#[1] "Sum of Squares Total:  3700.48829208"

#[1] "Sum of Squares Regression:  1739.35878038999"

#[1] "F-statistic:  0.529965063228907"


#8.#Viewing the difference between my manual calculation and the Anova table, we see that the F-statistic is not the same.
#The Anova table is 42.572 and my calculation is 0.529. I'm not sure why this is the case. I'm assuming it's because
#I'm using the sum of squares of the residuals instead of the sum of squares of the regression.
#data$Y_hat = predict(model1, data)
#data$residuals = data$HouseholdIncome - data$Y_hat
#Sum of Square Residuals
#SSR = sum(data$residuals^2)

#Sum of Squares Total
#Y_bar = mean(data$HouseholdIncome)
#SST = sum((data$HouseholdIncome - Y_bar)^2)

#Sum of squares due to regression
#SSR_ = sum((data$Y_hat - Y_bar)^2)

#Calculate the statistic (SSR/SST)
#statistic = SSR / SST

#I am a bit confused over the differences between a lot of these.

#Now we standardize the residuals for Model 1, without using the lm()
#Standardized residuals
data$standardized_residuals = rstandard(model1)
#plotting it using GGPlot's histogram
ggplot(data, aes(x = standardized_residuals)) + geom_histogram(binwidth = 0.5)
#Now a scatterplot
ggplot(data, aes(x = College, y = standardized_residuals)) + geom_point() + geom_smooth(method = "lm")
#Looking at both the histogram and the scatterplot, we see that the residuals are not normally distributed. There is
#no common pattern in the scatterplot, but the histogram is skewed to the right. This means that the residuals are
#not normally distributed either. We would expect a normal distribution pattern in the center.
#9
#Now we compare householdincome to TwoParents for our other variable.
model2 = lm(HouseholdIncome ~ TwoParents, data = data)
summary(model2)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-13.402  -5.355  -1.649   2.938  21.718 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   0.6845    14.0071   0.049 0.961226    
#TwoParents    0.8028     0.2131   3.767 0.000452 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 7.714 on 48 degrees of freedom
#Multiple R-squared:  0.2281,	Adjusted R-squared:  0.2121 
#F-statistic: 14.19 on 1 and 48 DF,  p-value: 0.000452

#Model2 compared to model1 has a lower Adjusted R-squared value of 0.2121 compared to 0.459. This means that
#the model is not as good as model1. The p-value is 0.000452 which is significantly smaller than our null-hypothesis
#but even just by looking at at our plots from earlier, it was obvious that household income would be better
#judged using college. Our correlation also corooborates this from earlier.
#10
#Finally we pick one more linear regression model for HighSchool.
model3 = lm(HouseholdIncome ~ HighSchool, data = data)
summary(model3)
#Making a scatterplot
ggplot(data, aes(x = HighSchool, y = HouseholdIncome)) + geom_point() + geom_smooth(method = "lm")
#Histogram
ggplot(data, aes(x = standardized_residuals)) + geom_histogram(binwidth = 0.5)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-13.302  -5.647  -2.430   5.687  20.037 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept) -54.3476    32.5588  -1.669  0.10159   
#HighSchool    1.2050     0.3643   3.308  0.00179 **
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 7.924 on 48 degrees of freedom
#Multiple R-squared:  0.1856,	Adjusted R-squared:  0.1687 
#F-statistic: 10.94 on 1 and 48 DF,  p-value: 0.001788
#The residuals are skewed to the right, but the scatterplot shows a linear relationship between HighSchool and
#HouseholdIncome. The Adjusted R-squared is 0.1687 which is lower than the other two models. The p-value is
#0.001788 which is significantly smaller than 0.05. We can reject the null hypothesis and accept the alternative
#hypothesis that the model is significant.
  
#Summary
#I've learned quite a bit about how to reliably create basic linear regression models.
#I've learned how to use the lm() function to create a model and how to use the predict() function to predict future
#values to add to the original dataframe.
#One part that I struggle with is the sums of squares. I'm not sure why my manual calculation is different from the
#Anova table. I'm assuming it's because I'm using the sum of squares of the residuals instead of the sum of squares
#however I need to understand how they change as some of their values were very close, but not exactly the same.
#It could be that the built in R functions are doing something in addition to the basic formulas you would find
#in general.
