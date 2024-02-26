library(readxl)
library(dplyr)
library(randomForest)
library(vip)
library(caret)
library(MASS)
library(car)
# install.packages(MASS)
data <- read_excel("ames_housing_data.xlsx")

#Ive decided to create a new sample population, since I've learned a lot, and I know how to
#prepare the data. This also gives me a chance to practice the whole process again.


#My drop requirements, changed a bit so I will reiterate that they are single family homes
#That are ranches. This means, 1 floor. 1 Family, no pools, no misc features.
#No fences
#Finding the categorical variables
# Check the structure of your data to identify potential categorical columns


data_filtered <- data %>%
  filter(BldgType == "1Fam", HouseStyle %in% c("1Story"),
         PoolArea == 0)

str(data) 

# Use sapply to go through each column and check its class
categorical_vars <- sapply(data, function(x) is.factor(x) | is.character(x))

# Get names of categorical variables
categorical_vars_names <- names(data)[categorical_vars]

summary_stats_by_cat <- function(data, cat_var) {
  data %>%
    group_by(!!sym(cat_var)) %>%
    summarise(
      Mean = mean(SalePrice, na.rm = TRUE),
      Median = median(SalePrice, na.rm = TRUE),
      StdDev = sd(SalePrice, na.rm = TRUE),
      Min = min(SalePrice, na.rm = TRUE),
      Max = max(SalePrice, na.rm = TRUE),
      DiffBetweenMaxMin = max(SalePrice, na.rm = TRUE) - min(SalePrice, na.rm = TRUE),
      Count = n()
    )
}

categorical_vars_names <- names(data_filtered)[sapply(data_filtered, function(x) is.factor(x) | is.character(x))]

for(cat_var in categorical_vars_names) {
  cat_stats <- summary_stats_by_cat(data_filtered, cat_var)
  print(cat_stats) # Or save the stats to a list or file for later use
}



selected_cat_vars <- c("LotConfig", "Neighborhood", "Condition1", "Condition2", "GarageType",
                       "GarageArea", "GarageCond", "PavedDrive", "BsmtFinType1", "BsmtExposure",
                       "TotalSqftCalc", "QualityIndex") 
                       
selected_ord_vars <- c("ExterQual","ExterCond","KitchenQual", "FireplaceQu", "GarageQual", "BsmtQual",
                       "BsmtCond")

selected_num_vars <- c("LotFrontage", "LotArea", "OverallQual", "OverallCond", "YearBuilt", "YearRemodel",
                       "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", "FirstFlrSF",
                       "LowQualFinSF", "GrLivArea", "BsmtFullBath", "BsmtHalfBath", "FullBath",
                       "HalfBath", "BedroomAbvGr", "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces", "GarageYrBlt",
                       "GarageCars", "WoodDeckSF", "OpenPorchSF", "EnclosedPorch", "ThreeSsnPorch", "ScreenPorch",
                      "MoSold", "YrSold", "SalePrice")

#Recode ord vars to 0 being lowest
for (var in selected_ord_vars) {
  data_filtered <- data_filtered %>%
    mutate(!!sym(var) := forcats::fct_recode(!!sym(var),
                                             "0" = "NA", # Convert character "NA" to a factor level if necessary
                                             "5" = "Ex",
                                             "4" = "Gd",
                                             "3" = "TA",
                                             "2" = "Fa",
                                             "1" = "Po"))
}


#Only select integer columns from data_filtered and those in cat and ord vars
selected_vars <- c(selected_cat_vars, selected_ord_vars, selected_num_vars)

data_filtered <- data_filtered %>%
  select(SalePrice, selected_vars)

#One Hot Encoding
data_ohe <- dummyVars(" ~ .", data = data_filtered[, selected_vars]) %>%
  predict(data_filtered[, selected_vars])

# Assignment Tasks
# For the tasks in this assignment, the response variable will be:  SALEPRICE (Y).   
# The remaining variables will be considered potential explanatory variables (X’s).  
# (1)	The Predictive Modeling Framework
# A defining feature of predictive modeling is assessing model performance out-of-sample. 
# We will use uniform random number to split the sample into a 70/30 train/test split.
# With a train/test split we now have two data sets: one for in-sample model
# development and one for out-of-sample model assessment.  

set.seed(123)
# Assuming data_ohe is a data frame and exists
my.data <- data_ohe # Make a copy of data_ohe

#Make sure my.data is a dataframe
my.data <- as.data.frame(my.data)

my.data$u <- runif(n=dim(my.data)[1],min=0,max=1);

# Define these two variables for later use;
my.data$QualityIndex <- my.data$OverallQual*my.data$OverallCond;
my.data$TotalSqftCalc <- my.data$BsmtFinSF1+my.data$BsmtFinSF2+my.data$GrLivArea;

# Create train/test split;
train.df <- subset(my.data, u<0.70);
test.df  <- subset(my.data, u>=0.70);

# Check your data split. The sum of the parts should equal the whole.
# Do your totals add up?
x <- dim(my.data)[1]
y <- dim(train.df)[1]
z <- dim(test.df)[1]
a <- dim(train.df)[1]+dim(test.df)[1]


# Create a named vector or data frame for better readability
observations <- c("Original Data" = x, "Training Data" = y, "Test Data" = z, "Total (Train + Test)" = a)

# Convert the named vector to a data frame for a nicer printout, if desired
observations_df <- as.data.frame(t(observations))  # Transpose to get a column-wise layout
colnames(observations_df) <- "Observation Count"

print(observations_df)

#2
clean.data <- my.data[complete.cases(my.data),]




upper.model <- lm(SalePrice ~ ., data = clean.data)
lower.model <- lm(SalePrice ~ 1, data = clean.data)
junk.model <- lm(SalePrice ~ ., data = my.data)

# Forward Selection
forward.lm <- stepAIC(lower.model, scope=list(lower=~1, upper=formula(upper.model)), direction="forward")
summary(forward.lm)

# Backward Elimination
backward.lm <- stepAIC(upper.model, scope=list(lower=~1, upper=formula(upper.model)), direction="backward")
summary(backward.lm)

# Stepwise Selection
stepwise.lm <- stepAIC(lower.model, scope=list(lower=~1, upper=formula(upper.model)), direction="both")
summary(stepwise.lm)

#Summary junkmodel
summary(junk.model)

#VIF values
vif.values.forward <- vif(forward.lm)
vif.values.backward <- vif(backward.lm)
vif.values.stepwise <- vif(stepwise.lm)

sorted_vif_values_forward <- sort(vif.values.forward, decreasing = TRUE)
sorted_vif_values_backward <- sort(vif.values.backward, decreasing = TRUE)
sorted_vif_values_stepwise <- sort(vif.values.stepwise, decreasing = TRUE)

#Vifs are well within acceptable ranges

calculate_model_metrics <- function(model, data) {
  predictions <- predict(model, newdata = data)
  residuals <- data$SalePrice - predictions
  mse <- mean(residuals^2)
  mae <- mean(abs(residuals))
  adjusted_r_squared <- summary(model)$adj.r.squared
  aic_val <- AIC(model)
  bic_val <- BIC(model)
  
  return(list(
    AdjustedR2 = adjusted_r_squared,
    AIC = aic_val,
    BIC = bic_val,
    MSE = mse,
    MAE = mae
  ))
}

metrics_forward <- calculate_model_metrics(forward.lm, train.df)
metrics_backward <- calculate_model_metrics(backward.lm, train.df)
metrics_stepwise <- calculate_model_metrics(stepwise.lm, train.df)
metrics_junk <- calculate_model_metrics(junk.model, train.df)

# Print the metrics
print("Forward Selection Metrics")
print(metrics_forward)
print("Backward Elimination Metrics")
print(metrics_backward)
print("Stepwise Selection Metrics")
print(metrics_stepwise)

# Function to calculate MSE and MAE
calculate_test_metrics <- function(model, test_data) {
  predictions <- predict(model, newdata = test_data)
  residuals <- test_data$SalePrice - predictions
  mse <- mean(residuals^2)
  mae <- mean(abs(residuals))
  return(list(MSE = mse, MAE = mae))
}

# Calculate metrics for each model using test.df
metrics_forward_test <- calculate_test_metrics(forward.lm, test.df)
metrics_backward_test <- calculate_test_metrics(backward.lm, test.df)
metrics_stepwise_test <- calculate_test_metrics(stepwise.lm, test.df)
metrics_junk_test <- calculate_test_metrics(junk.model, test.df)

# Print the metrics for comparison
print(list("Forward Selection Test Metrics" = metrics_forward_test,
           "Backward Elimination Test Metrics" = metrics_backward_test,
           "Stepwise Selection Test Metrics" = metrics_stepwise_test,
           "Junk Model Test Metrics" = metrics_junk_test))

