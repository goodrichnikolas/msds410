#Load XLS
library(readxl)
library(ggplot2) # For data visualization
library(dplyr) # For data manipulation
library(GGally) # For pairwise plots
library(tidyr) # For handling missing values visualization
library(plyr)
library(fastDummies)
library(patchwork)
#install.packages('fastDummies')
#Load data.xls in current folder
data <- read_excel("NutritionStudy.xls")


#Recode Smoke to 1 if yes, 0 if no
data$Smoke <- ifelse(data$Smoke == "Yes", 1, 0)

#Gender male = 0 female = 1
data$Gender <- ifelse(data$Gender == "Male", 1, 0)

#VitamanUse get all unique variables
unique(data$VitaminUse)
#"Regular"    "Occasional" "No" 

#Create Dummies for VitaminUse
data <- dummy_cols(data, select_columns = "VitaminUse")
#Create Dummies for PriorSmoke
data <- dummy_cols(data, select_columns = "PriorSmoke")

#Drop the original columns
#data <- data[, !(names(data) %in% c("VitaminUse", "PriorSmoke"))]

#If Alcohol = 0 None, if 0-10 moderate, if >= 10 great
data$alcohol_usage <- ifelse(data$Alcohol == 0, "None", ifelse(data$Alcohol <= 10, "Moderate", "Great"))

#Now dummies
data <- dummy_cols(data, select_columns = "alcohol_usage")

#Drop Alcohol
#data <- data[, !(names(data) %in% c("Alcohol", "alcohol_usage"))]

#Descriptive statistics by PriorSmoke

# Descriptive statistics by PRIORSMOKE, mean Cholesterol
priorSmoke_stats <- data %>%
  group_by(PriorSmoke)

#Summarizing the meand and SD of Cholesterol
priorSmoke_stats <- priorSmoke_stats %>%
  summarise(meanCholesterol = mean(Cholesterol, na.rm = TRUE),
            sdCholesterol = sd(Cholesterol, na.rm = TRUE))
#Anova test
anova_test <- aov(Cholesterol ~ PriorSmoke, data = data)
summary(anova_test)

#2
#Predicting Cholesterol as our Y variable using PriorSmoke_1, PriorSmoke_2, PriorSmoke_3

model1 <- lm(Cholesterol ~ PriorSmoke_1 + PriorSmoke_2, data = data)
summary(model1)

#3

model2 <- lm(Cholesterol ~ PriorSmoke_1 + PriorSmoke_2 + Fat, data = data)
summary(model2)

#Diagnostic plots
par(mfrow=c(2,2))
plot(model2)

#Influence measures
influence_measures <- influence.measures(model2)

#4


#Predicted values for Y
data$predicted_values <- predict(model2)


par(mfrow=c(1,2))
#Scatterplot
scatterplot_ <- ggplot(data, aes(x = Fat, y = predicted_values, color = PriorSmoke)) +
  geom_point() +
  labs(title = "Predicted Values for Y by Fat", x = "Fat", y = "Predicted Values for Y")

#Add line
lineplot_ <- ggplot(data, aes(x = Fat, y = Cholesterol, color = PriorSmoke)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Actual Values for Y by Fat", x = "Fat", y = "Actual Values for Y")

#Adding the two together
scatterplot_ + lineplot_

#5

#Create new product variables
data$PriorSmoke_1_Fat <- data$PriorSmoke_1 * data$Fat
data$PriorSmoke_2_Fat <- data$PriorSmoke_2 * data$Fat

#Unequal slopes model
model3 <- lm(Cholesterol ~ PriorSmoke_1 + PriorSmoke_2 + Fat + PriorSmoke_1_Fat + PriorSmoke_2_Fat, data = data)

#Fit
summary(model3)

#diagnostic plots
par(mfrow=c(2,2))
plot(model3)

#6
#Use Model 3 to obtain predicted values.   Plot the predicted values
#for CHOLESTEROL (Y) by FAT(X).  Discuss what you see in this graph.

data_predicted_values <- predict(model3)
data$predicted_values_m3 <- data_predicted_values

#Plot
scatterplot_m3 <- ggplot(data, aes(x = Fat, y = predicted_values_m3, color = PriorSmoke)) +
  geom_point() +
  labs(title = "Predicted Values for Y by Fat", x = "Fat", y = "Predicted Values for Y")

#Add line
lineplot_m3 <- ggplot(data, aes(x = Fat, y = Cholesterol, color = PriorSmoke)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Actual Values for Y by Fat", x = "Fat", y = "Actual Values for Y")

scatterplot_m3
lineplot_m3

scatterplot_m3 + lineplot_m3

#7

#Nested F-test

nested_f_test <- anova(model2, model3)
print(nested_f_test)

#8
#Lets do the same thing with alcohol usage


alcohol_usage_stats <- data %>%
  group_by(alcohol_usage_Great, alcohol_usage_Moderate) %>%
  summarise(meanCholesterol = mean(Cholesterol, na.rm = TRUE),
            sdCholesterol = sd(Cholesterol, na.rm = TRUE))

# Basic Model - Predicting Cholesterol using alcohol usage levels
model_alcohol1 <- lm(Cholesterol ~ alcohol_usage_Great + alcohol_usage_Moderate, data = data)
summary(model_alcohol1)

# Extended Model - Adding another variable (e.g., Fat) to the model
model_alcohol2 <- lm(Cholesterol ~ alcohol_usage_Great + alcohol_usage_Moderate + Fat, data = data)
summary(model_alcohol2)


#Interaction terms
data$alcohol_usage_Great_Fat <- data$alcohol_usage_Great * data$Fat
data$alcohol_usage_Moderate_Fat <- data$alcohol_usage_Moderate * data$Fat

model_alcohol3 <- lm(Cholesterol ~ alcohol_usage_Great + alcohol_usage_Moderate + Fat + alcohol_usage_Great_Fat + alcohol_usage_Moderate_Fat, data = data)
summary(model_alcohol3)

# Diagnostic plots for the extended model with interactions
par(mfrow=c(2,2))
plot(model_alcohol3)

#predicted values
data$predicted_values_alcohol <- predict(model_alcohol3)

#plot predicted vs actual
scatterplot_alcohol <- ggplot(data, aes(x = Fat, y = predicted_values_alcohol, color = alcohol_usage_Great)) +
  geom_point() +
  labs(title = "Predicted Values for Y by Fat", x = "Fat", y = "Predicted Values for Y")

#actual
lineplot_alcohol <- ggplot(data, aes(x = Fat, y = Cholesterol, color = alcohol_usage_Great)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Actual Values for Y by Fat", x = "Fat", y = "Actual Values for Y")

scatterplot_alcohol + lineplot_alcohol



nested_f_test_alcohol <- anova(model_alcohol2, model_alcohol3)
print(nested_f_test_alcohol)
