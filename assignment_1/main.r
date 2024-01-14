# Load necessary libraries
library(readxl)
library(ggplot2)
library(knitr)

# Loading the data into a dataframe
mydata <- read_excel("C:/Users/Nikolas/Desktop/410/msds410/assignment_1/class files/ames_housing_data.xlsx")

# Columns to keep
columns_to_keep <- c("Zoning", "BldgType", "HouseStyle", "OverallQual",
    "OverallCond", "YearBuilt", "Exterior1",  "ExterQual",
    "ExterCond", "TotalBsmtSF", "Heating", "CentralAir",
    "FirstFlrSF",  "GrLivArea", "FullBath", "HalfBath",
    "YrSold", "SaleType", "SaleCondition", "SalePrice")

refined_mydata <- mydata[columns_to_keep]

# Drop rows that contain NA
refined_mydata <- na.omit(refined_mydata)

