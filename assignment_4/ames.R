# Load necessary libraries
library(readxl)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(knitr)
library(corrplot)
library(leaps)
# Load data from the Excel file
ames_data <- read_excel("ames_housing_data.xlsx")

#Create Drop Conditions
# Apply the conditions
ames_data_filtered <- ames_data %>%
  filter(LotArea > 0,
         HouseStyle == "1Story",
         BldgType == "1Fam",
         TotalBsmtSF > 0,
         GarageArea > 0)

# Create histograms for each variable
histogram_LotArea <- ggplot(ames_data_filtered, aes(x = LotArea)) + 
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal() +
  ggtitle("LotArea by Square Footage")

histogram_FirstFlrSF <- ggplot(ames_data_filtered, aes(x = FirstFlrSF)) + 
  geom_histogram(bins = 30, fill = "green", color = "black") +
  theme_minimal() +
  ggtitle("FirstFlrSF by Square Footage")

histogram_GarageArea <- ggplot(ames_data_filtered, aes(x = GarageArea)) + 
  geom_histogram(bins = 30, fill = "red", color = "black") +
  theme_minimal() +
  ggtitle("GarageArea by Square Footage")

histogram_TotalBsmtSF <- ggplot(ames_data_filtered, aes(x = TotalBsmtSF)) + 
  geom_histogram(bins = 30, fill = "orange", color = "black") +
  theme_minimal() +
  ggtitle("TotalBsmtSF by Square Footage")

# Combine the histograms into one image
combined_histograms <- grid.arrange(histogram_LotArea, histogram_FirstFlrSF, histogram_GarageArea, histogram_TotalBsmtSF, ncol = 2)

#Save the image
ggsave("combined_histograms.png", combined_histograms)

# Calculate summary statistics
summary_stats <- ames_data_filtered %>%
  summarise(mean_LotArea = mean(LotArea),
            median_LotArea = median(LotArea),
            min_LotArea = min(LotArea),
            max_LotArea = max(LotArea),
            sd_LotArea = sd(LotArea),
            
            mean_FirstFlrSF = mean(FirstFlrSF),
            median_FirstFlrSF = median(FirstFlrSF),
            min_FirstFlrSF = min(FirstFlrSF),
            max_FirstFlrSF = max(FirstFlrSF),
            sd_FirstFlrSF = sd(FirstFlrSF),
            
            mean_GarageArea = mean(GarageArea),
            median_GarageArea = median(GarageArea),
            min_GarageArea = min(GarageArea),
            max_GarageArea = max(GarageArea),
            sd_GarageArea = sd(GarageArea),
            
            mean_TotalBsmtSF = mean(TotalBsmtSF),
            median_TotalBsmtSF = median(TotalBsmtSF),
            min_TotalBsmtSF = min(TotalBsmtSF),
            max_TotalBsmtSF = max(TotalBsmtSF),
            sd_TotalBsmtSF = sd(TotalBsmtSF))

# Create a data frame with the summary statistics
summary_stats_df <- data.frame(
  Statistic = c('Mean', 'Median', 'Min', 'Max', 'SD'),
  LotArea = c(11308.17, 9991, 2887, 215245, 8660.32),
  FirstFlrSF = c(1325.80, 1266, 407, 5095, 406.37),
  GarageArea = c(507.45, 484, 100, 1390, 201.47),
  TotalBsmtSF = c(1256.78, 1188, 105, 5095, 419.88)
)

# Display the table
print(summary_stats_df)


write.csv(summary_stats, "summary_stats.csv")

#Creating a table of the summary stats
summary_stats_table <- as.data.frame(t(summary_stats))

#Lets boil it down to the essentials now

selected_vars <- c("LotArea", "TotalBsmtSF", "GarageArea", "FirstFlrSF", "TotRmsAbvGrd",
                   "FullBath", "HalfBath", "Neighborhood", "SalePrice")

ames_data_w_saleprice <- ames_data_filtered %>%
  select(selected_vars)

#Numeric only
ames_data_numeric <- ames_data_w_saleprice %>%
  select_if(is.numeric)

#Correlation table
correlation_table <- cor(ames_data_numeric)

#Turn correlation table into a heatmap
# Using corrplot to create a heatmap with numbers
corrplot(correlation_table, method = "color", type = "upper", 
         order = "hclust", addCoef.col = "black", 
         tl.col = "black", tl.srt = 45, number.cex = .7, 
         cl.lim = c(-1, 1))
#Scatterplots
# Create scatterplots for each variable against SalePrice
scatterplot_LotArea <- ggplot(ames_data_filtered, aes(x = LotArea, y = SalePrice)) + 
  geom_point() +
  theme_minimal() +
  ggtitle("SalePrice vs LotArea")

scatterplot_TotalBsmtSF <- ggplot(ames_data_filtered, aes(x = TotalBsmtSF, y = SalePrice)) + 
  geom_point() +
  theme_minimal() +
  ggtitle("SalePrice vs TotalBsmtSF")

scatterplot_GarageArea <- ggplot(ames_data_filtered, aes(x = GarageArea, y = SalePrice)) + 
  geom_point() +
  theme_minimal() +
  ggtitle("SalePrice vs GarageArea")

scatterplot_FirstFlrSF <- ggplot(ames_data_filtered, aes(x = FirstFlrSF, y = SalePrice)) + 
  geom_point() +
  theme_minimal() +
  ggtitle("SalePrice vs FirstFlrSF")

# Combine the scatterplots into one image using gridExtra
combined_scatterplots <- grid.arrange(scatterplot_LotArea, scatterplot_TotalBsmtSF, 
                                      scatterplot_GarageArea, scatterplot_FirstFlrSF, ncol = 2)

# Save the combined scatterplot image
ggsave("combined_scatterplots.png", combined_scatterplots, width = 12, height = 8)

# Create variable SqFtPerDollar
ames_data_w_saleprice <- ames_data_w_saleprice %>%
  mutate(SqFtPerDollar = SalePrice / (LotArea + TotalBsmtSF + GarageArea + FirstFlrSF))

#1
Model_1 <- lm(SalePrice ~ FirstFlrSF, data = ames_data_w_saleprice)
summary(Model_1)
#a
ggplot(ames_data_w_saleprice, aes(x = FirstFlrSF, y = SalePrice)) +
  geom_point() +  # Add points for scatterplot
  geom_smooth(method = "lm", col = "red") +  # Add linear regression line
  theme_minimal() +
  ggtitle("SalePrice vs FirstFlrSF with Regression Line") +
  xlab("First Floor Square Footage") +
  ylab("Sale Price")

#c Anova table
anova(Model_1)

#e
# Calculate predicted values
ames_data_w_saleprice$predicted_values <- predict(Model_1)
std_res <- rstandard(Model_1)


# Calculate standardized residuals
ames_data_w_saleprice$std_res <- rstandard(Model_1)

# Plot histogram of standardized residuals
ggplot(ames_data_w_saleprice, aes(x = std_res)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", color = "black") +
  geom_density(col = "red") + # Overlay density plot for visual comparison to normal distribution
  theme_minimal() +
  ggtitle("Histogram of Standardized Residuals") +
  xlab("Standardized Residuals") +
  ylab("Density")

# Plot scatterplot of standardized residuals by predicted values
ggplot(ames_data_w_saleprice, aes(x = predicted_values, y = std_res)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  ggtitle("Standardized Residuals by Predicted Values") +
  xlab("Predicted Values (Y_hat)") +
  ylab("Standardized Residuals")

#f
ames_data_w_saleprice$leverage <- hatvalues(Model_1)
ames_data_w_saleprice$cooks_distance <- cooks.distance(Model_1)

# Plot Leverage vs. Standardized Residuals Squared
ggplot(ames_data_w_saleprice, aes(x = leverage, y = std_res^2)) +
  geom_point() +
  geom_hline(yintercept = c(1, 3), linetype = "dashed", color = "red") + # Reference lines for residuals
  theme_minimal() +
  ggtitle("Leverage vs. Standardized Residuals Squared") +
  xlab("Leverage") +
  ylab("Standardized Residuals Squared")

# Plot Cook's Distance
ggplot(ames_data_w_saleprice, aes(x = seq_along(cooks_distance), y = cooks_distance)) +
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  ggtitle("Observation by Cook's Distance") +
  xlab("Observation") +
  ylab("Cook's Distance")

#2 Now we create a model with FirstFlrSF and OverallQual
selected_vars_2 <- c("SalePrice", "FirstFlrSF", "OverallQual")
ames_data_w_saleprice_2 <- ames_data_filtered %>%
  select(selected_vars_2)

Model_2 <- lm(SalePrice ~ FirstFlrSF + OverallQual, data = ames_data_w_saleprice_2)

#a
summary(Model_2)
#Prediction equation
#SalePrice =  -103.464 + 0.107*FirstFlrSF +  3.948*OverallQual

#c
#Anova
anova(Model_2)

#e
# Calculate predicted values
ames_data_w_saleprice_2$predicted_values <- predict(Model_2)
std_res <- rstandard(Model_2)

# Calculate standardized residuals
ames_data_w_saleprice_2$std_res <- rstandard(Model_2)

#PLot histogram of standardized residuals
ggplot(ames_data_w_saleprice_2, aes(x = std_res)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", color = "black") +
  geom_density(col = "red") + # Overlay density plot for visual comparison to normal distribution
  theme_minimal() +
  ggtitle("Histogram of Standardized Residuals") +
  xlab("Standardized Residuals") +
  ylab("Density")

# Plot scatterplot of standardized residuals by predicted values
ggplot(ames_data_w_saleprice_2, aes(x = predicted_values, y = std_res)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  ggtitle("Standardized Residuals by Predicted Values") +
  xlab("Predicted Values (Y_hat)") +
  ylab("Standardized Residuals")

#3
#Now let's do 3 by adding Neighborhood
selected_vars_3 <- c("SalePrice", "FirstFlrSF", "OverallQual", "Neighborhood")
ames_data_w_saleprice_3 <- ames_data_filtered %>%
  select(selected_vars_3)

Model_3 <- lm(SalePrice ~ FirstFlrSF + OverallQual + Neighborhood, data = ames_data_w_saleprice_3)
summary(Model_3)

#c
#Anova
anova(Model_3)

#Check underlying assumptions
#e
# Calculate predicted values
ames_data_w_saleprice_3$predicted_values <- predict(Model_3)
std_res <- rstandard(Model_3)

# Calculate standardized residuals
ames_data_w_saleprice_3$std_res <- rstandard(Model_3)

#PLot histogram of standardized residuals
ggplot(ames_data_w_saleprice_3, aes(x = std_res)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", color = "black") +
  geom_density(col = "red") + # Overlay density plot for visual comparison to normal distribution
  theme_minimal() +
  ggtitle("Histogram of Standardized Residuals") +
  xlab("Standardized Residuals") +
  ylab("Density")

# Plot scatterplot of standardized residuals by predicted values
ggplot(ames_data_w_saleprice_3, aes(x = predicted_values, y = std_res)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  ggtitle("Standardized Residuals by Predicted Values") +
  xlab("Predicted Values (Y_hat)") +
  ylab("Standardized Residuals")

#4
# Model 4: Using the natural log of SalePrice as the response variable
Model_4 <- lm(log(SalePrice) ~ FirstFlrSF + OverallQual + Neighborhood, data = ames_data_w_saleprice_3)
summary(Model_4)

#5
# Leverage
leverage4 <- hatvalues(Model_4)

# Cook's Distance
cooksD4 <- cooks.distance(Model_4)

# Standardized Residuals
stdResiduals4 <- rstandard(Model_4)

# Combine into a data frame
diagnostics4 <- data.frame(leverage = leverage4, cooksD = cooksD4, stdResiduals = stdResiduals4)

# Set a threshold for influential points (common choices, may adjust based on data)
highLeverageThreshold <- 2 * mean(leverage4)
highCooksDThreshold <- 4 / length(cooksD4)
highStdResidualsThreshold <- 2  # Typically, above 2 or below -2 considered high

# Identify rows meeting any of the criteria
influentialPoints <- which(diagnostics4$leverage > highLeverageThreshold |
                             diagnostics4$cooksD > highCooksDThreshold |
                             abs(diagnostics4$stdResiduals) > highStdResidualsThreshold)

# Display number of influential points identified
length(influentialPoints)

# Remove influential points
ames_data_cleaned <- ames_data_w_saleprice_3[-influentialPoints, ]

# Refit Model 4 without the influential points
Model_4_Cleaned <- lm(log(SalePrice) ~ FirstFlrSF + OverallQual + Neighborhood, data = ames_data_cleaned)
summary(Model_4_Cleaned)

#6
#Using leaps let's find the best model
ames_data_numeric <- ames_data %>%
  select(where(is.numeric)) %>%
  select(-SalePrice)  
full_model_formula <- as.formula(paste("SalePrice ~ .", sep = ""))

best_subset <- regsubsets(full_model_formula, data = ames_data, 
                          nbest = 1,          
                          nvmax = 4,
                          method = "exhaustive",
                          really.big = TRUE)
best_models_summary <- summary(best_subset)
