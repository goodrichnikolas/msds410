#1

# Load necessary libraries
library(readxl)
library(dplyr)
library(MASS)
library(car)
library(Metrics)
# install.packages("Metrics")
# Import the dataset
ames_data <- read_excel("ames_housing_data.xlsx")
ames_data <- na.omit(ames_data)

ames_data<- ames_data %>%
  filter(BldgType == "1Fam", HouseStyle %in% c("1Story"),
         PoolArea == 0)

set.seed(123)

# Summary
str(ames_data)
head(ames_data)

#Adding uniform random number column
#Add a uniform random number column
ames_data$u <- runif(n = nrow(ames_data), min = 0, max = 1)

# Add two new variables
ames_data$QualityIndex <- ames_data$OverallQual * ames_data$OverallCond
ames_data$TotalSqftCalc <- ames_data$BsmtFinSF1 + ames_data$BsmtFinSF2 + ames_data$GrLivArea

#Test and train

train.df <- subset(ames_data, u < 0.70)
test.df <- subset(ames_data, u >= 0.70)

# Verify the split
cat("Total observations:", nrow(ames_data), "\n")
cat("Training set observations:", nrow(train.df), "\n")
cat("Test set observations:", nrow(test.df), "\n")
cat("Sum of train and test observations:", nrow(train.df) + nrow(test.df), "\n")





selected_num_vars <- c("LotArea", "OverallQual","YearBuilt", 
                       "MasVnrArea","Fireplaces",
                       "GarageCars", "TotalSqftCalc", "QualityIndex")


# Apply log transformation safely
for(var in selected_num_vars) {
  # Check if there are non-positive values
  if(any(train.clean[[var]] <= 0)) {
    # Handle zero or negative values before transformation
    train.clean[[var]] <- log(train.clean[[var]] + 1) # Adding 1 to avoid log(0)
    test.clean[[var]] <- log(test.clean[[var]] + 1)
  } else {
    # Apply log transformation directly
    train.clean[[var]] <- log(train.clean[[var]])
    test.clean[[var]] <- log(test.clean[[var]])
  }
}



#turn the selected into a table for easier viewing 11 x 2
print(all)

#Create a new dataframe with only the selected variables
train.clean <- train.df[, selected_num_vars]
test.clean <- test.df[, selected_num_vars]

#Add SalePrice back in
train.clean$SalePrice <- train.df$SalePrice
test.clean$SalePrice <- test.df$SalePrice

#Check for infinite values


#Apply log to all the selected numerical variables


#Add SalePrice


#Upper and lower models
upper.model <- lm(SalePrice ~ ., data = train.clean) #This is the full model
lower.model <- lm(SalePrice ~ 1, data = train.clean) #This is only the Y-intercept model
initial.model <- lm(SalePrice ~ TotalSqftCalc,data = train.clean)
junk.model <- lm(SalePrice ~ 1, data = train.df)

summary(upper.model)

#Forward Selection
forward.model <- stepAIC(lower.model, scope = list(upper = formula(upper.model), lower = ~1), direction = "forward")
forward.model <- stepAIC(object=lower.model,scope=list(upper=formula(upper.model),lower= ~1), direction=c('forward'))

forward.model.junk <- stepAIC(junk.model, scope = list(upper = formula(upper.model), lower = ~1), direction = "forward")
# Backward Elimination
backward.model <- stepAIC(upper.model, direction = "backward")
backward.model.junk <- stepAIC(junk.model, direction = "backward")
# Stepwise Selection
stepwise.model <- stepAIC(initial.model, scope = list(upper = formula(upper.model), lower = ~1), direction = "both")
stepwise.model.junk <- stepAIC(initial.model, scope = list(upper = formula(upper.model), lower = ~1), direction = "both")


#Check for multicollinearity
summary(forward.model)


# Compute VIF for the forward selection model
vif_forward <- vif(forward.model)
print(sort(vif_forward, decreasing = TRUE))

# Compute VIF for the backward elimination model
vif_backward <- vif(backward.model)
print(sort(vif_backward, decreasing = TRUE))

# Compute VIF for the stepwise selection model
vif_stepwise <- vif(stepwise.model)
print(sort(vif_stepwise, decreasing = TRUE))





# Generate predictions on the test data for each model
predictions_forward <- predict(forward.model, newdata = test.df)
predictions_backward <- predict(backward.model, newdata = test.df)
predictions_stepwise <- predict(stepwise.model, newdata = test.df)
predictions_junk <- predict(junk.model, newdata = test.df)


calculate_errors <- function(actual, predicted, model) {
  ARS <- summary(model)$adj.r.squared
  AIC <- AIC(model)
  BIC <- BIC(model)
  mse <- mean((actual - predicted)^2)
  mae <- mae(actual, predicted)
  return(list(ARS = ARS, AIC = AIC, BIC = BIC, MSE = mse, MAE = mae))
}

# Actual sale prices from the test data
actual_prices <- test.df$SalePrice

# Compute errors
errors_forward <- calculate_errors(actual_prices, predictions_forward, forward.model)
errors_backward <- calculate_errors(actual_prices, predictions_backward, backward.model)
errors_stepwise <- calculate_errors(actual_prices, predictions_stepwise, stepwise.model)
errors_junk <- calculate_errors(actual_prices, predictions_junk, junk.model)

# Output the errors for each model
print("Forward Selection Errors:")
print(errors_forward)
print("Backward Elimination Errors:")
print(errors_backward)
print("Stepwise Selection Errors:")
print(errors_stepwise)
print("Arbitrary 'Junk' Model Errors:")
print(errors_junk)

#In sample vs out of sample
forward.test <- predict(forward.model, newdata = test.df)
forward.train <- predict(forward.model, newdata = train.df)

#Print them out
cat("Forward Selection Model In-Sample R-squared: ", summary(forward.model)$r.squared, "\n")
cat("Forward Selection Model Out-of-Sample R-squared: ", R2(forward.test, test.df$SalePrice), "\n")


# Function to calculate prediction grades
calculate_prediction_grades <- function(actual, predicted) {
  pct_error <- abs(actual - predicted) / actual
  prediction_grade <- ifelse(pct_error <= 0.10, 'Grade 1: [0, 0.10]',
                             ifelse(pct_error <= 0.15, 'Grade 2: (0.10, 0.15]',
                                    ifelse(pct_error <= 0.25, 'Grade 3: (0.15, 0.25]',
                                           'Grade 4: (0.25+]')))
  return(prediction_grade)
}


test_prediction_grade_forward <- calculate_prediction_grades(test.df$SalePrice, predictions_forward)
test_prediction_grade_backward <- calculate_prediction_grades(test.df$SalePrice, predictions_backward)
test_prediction_grade_stepwise <- calculate_prediction_grades(test.df$SalePrice, predictions_stepwise)
test_prediction_grade_junk <- calculate_prediction_grades(test.df$SalePrice, predictions_junk)

#Out of Sample



# Tabulate and normalize grades for each model
normalize_table <- function(grades) {
  table(grades) / sum(table(grades))
}

# Print normalized distributions
cat("Forward Selection Test Prediction Grades:\n")
print(normalize_table(test_prediction_grade_forward))

cat("\nBackward Elimination Test Prediction Grades:\n")
print(normalize_table(test_prediction_grade_backward))

cat("\nStepwise Selection Test Prediction Grades:\n")
print(normalize_table(test_prediction_grade_stepwise))

cat("\n'Junk' Model Test Prediction Grades:\n")
print(normalize_table(test_prediction_grade_junk))

###########
#Refining
###########

summary(stepwise.model)

#Check for heteroskedasticity
# Plot all residuals
plot(stepwise.model, which = 1)


