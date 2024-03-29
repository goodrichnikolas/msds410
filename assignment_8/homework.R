# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(caTools)
library(dplyr)
library(MASS)
# Read the dataset
data <- read_excel("wine.xlsx")

####################
#EDA
####################

#Drop column INDEX
data <- data[,-1]

#Which columns contain negative numbers
negatives <- sapply(data, function(x) any(x < 0))



#Missing values for each column
missing_values <- colSums(is.na(data))
print(missing_values)



# Define the list of variables for boxplots
var_list <- c("Purchase", "Cases", "STARS", "FixedAcidity", "VolatileAcidity", 
              "CitricAcid", "ResidualSugar", "Chlorides", "FreeSulfurDioxide", 
              "TotalSulfurDioxide", "Density", "pH", "Sulphates", "Alcohol", 
              "LabelAppeal", "AcidIndex")
var_list_ <- c("FixedAcidity", "VolatileAcidity", "CitricAcid", "ResidualSugar",
              "Chlorides", "FreeSulfurDioxide", "TotalSulfurDioxide", "Density",
              "pH", "Sulphates", "Alcohol")

# Initialize an empty list to store plots
plot_list <- list()

# Generate a boxplot for each variable and store it in the list
for (i in var_list_) {
  plot_list[[i]] <- ggplot(data, aes(x = 1, y = .data[[i]])) +
    geom_boxplot() +
    labs(title = i, y = i, x = "") +
    theme_minimal()
}

# Combine all plots into a single plot with patchwork
combined_plot <- wrap_plots(plot_list, ncol = 4) # Adjust ncol as needed for layout

# Print the combined plot
print(combined_plot)


#Distributions
# Initialize an empty list to store plots
plot_list <- list()

# Generate a density plot for each variable and store it in the list
for (i in var_list_) {
  plot_list[[i]] <- ggplot(data, aes(x = .data[[i]])) +
    geom_density(fill = "blue", alpha = 0.5) + # Adjust color and transparency as desired
    labs(title = i, x = i, y = "Density") +
    theme_minimal()
}

# Combine all plots into a single plot with patchwork
combined_plot <- wrap_plots(plot_list, ncol = 3) # Adjust ncol as needed for layout

# Print the combined plot
print(combined_plot)

#Distributions are leptokurtic with many outliers

#Correlation Analysis
numeric_data <- data[, sapply(data, is.numeric)] # Select only numeric columns
corr_matrix <- cor(numeric_data, use = "complete.obs") # Exclude NA values for correlation calculation
corrplot(corr_matrix, method = "circle")

#######################
#Data Preprocessing
#######################

#Remove STARS
data <- data[,-3]

#Remove rows that contain negative

#Only select those with complete data
data <- na.omit(data)

#Cube root to address leptokurtic distributions
# Apply cube root transformation to variables
#data_transformed <- data %>%
#  mutate(across(c(FixedAcidity, VolatileAcidity, CitricAcid, ResidualSugar, Chlorides, FreeSulfurDioxide, TotalSulfurDioxide, Density, pH, Sulphates, Alcohol), ~ sign(.) * abs(.)^(1/3)))

#Verify trans
# Generate density plots for the transformed variables
#plot_list_transformed <- list()
#for (i in var_list_) {
#  plot_list_transformed[[i]] <- ggplot(data_transformed, aes(x = .data[[i]])) +
#    geom_density(fill = "blue", alpha = 0.5) + # Adjust color and transparency as desired
#    labs(title = paste("Transformed", i), x = i, y = "Density") +
#    theme_minimal()
#}

# Combine all transformed plots into a single plot with patchwork
#combined_plot_transformed <- wrap_plots(plot_list_transformed, ncol = 3) # Adjust ncol as needed for layout

# Print the combined transformed plot
#print(combined_plot_transformed)

#Cubed Root didn't work

#Apply log instead
# data_transformed <- data %>%
#   mutate(across(c(FixedAcidity, VolatileAcidity, CitricAcid, ResidualSugar, Chlorides, FreeSulfurDioxide, TotalSulfurDioxide, Density, pH, Sulphates, Alcohol), ~ log(. + 1)))
# 
# plot_list_transformed <- list()
# 
# # Generate density plots for the transformed variables
# for (i in var_list_) {
#   plot_list_transformed[[i]] <- ggplot(data_transformed, aes(x = .data[[i]])) +
#     geom_density(fill = "blue", alpha = 0.5) + # Adjust color and transparency as desired
#     labs(title = paste("Transformed", i), x = i, y = "Density") +
#     theme_minimal()
# }
# 
# # Combine all transformed plots into a single plot with patchwork
# combined_plot_transformed <- wrap_plots(plot_list_transformed, ncol = 3) # Adjust ncol as needed for layout
# print(combined_plot_transformed)

#We can't use Box Cox because too many variables are negative

#Square root transformation
# Apply square root transformation to variables
data_transformed <- data %>%
  mutate(across(c(FixedAcidity, VolatileAcidity, CitricAcid, ResidualSugar, Chlorides, FreeSulfurDioxide, TotalSulfurDioxide, Density, pH, Sulphates, Alcohol), ~ sign(.) * abs(.)^(1/2)))

# Verify transformation
# Generate density plots for the transformed variables
# plot_list_transformed <- list()
# 
# for (i in var_list_) {
#   plot_list_transformed[[i]] <- ggplot(data_transformed, aes(x = .data[[i]])) +
#     geom_density(fill = "blue", alpha = 0.5) + # Adjust color and transparency as desired
#     labs(title = paste("Transformed", i), x = i, y = "Density") +
#     theme_minimal()
# }
# 
# # Combine all transformed plots into a single plot with patchwork
# combined_plot_transformed <- wrap_plots(plot_list_transformed, ncol = 3) # Adjust ncol as needed for layout
# print(combined_plot_transformed)


# Separate the predictors from the response variable
predictors <- data %>% select(-Purchase) # Exclude the target variable
response <- data$Purchase # Only the target variable

# Split data into training and testing set (70% train, 30% test)
set.seed(123) # For reproducibility
split <- sample.split(response, SplitRatio = 0.7) # Split based on the response variable
train_pred <- predictors[split, ] # Training predictors
test_pred <- predictors[!split, ] # Testing predictors
train_resp <- response[split] # Training response
test_resp <- response[!split] # Testing response

# Verifying the split
cat("Training set rows:", nrow(train_pred), "\n")
cat("Testing set rows:", nrow(test_pred), "\n")

