"""
MSDS 410 Assignment 1 - EDA
Section 1: Sample Definition
Provide the waterfall of your drop conditions with counts.
Section 2: Data Quality Check
Provide a table listing out your twenty variables.
Provide the data quality results with discussion for your twenty selected variables.
Section 3: Initial Exploratory Data Analysis
Provide the EDA results with discussion for your ten variables.
Section 4: Exploratory Data Analysis for Modeling
Provide the EDA results with discussion for your three variables.
Section 5: Summary/Conclusions
"""

import pandas as pd

#Loading the data into a dataframe
df_original = pd.read_excel('./assignment_1/class files/ames_housing_data.xlsx')

#Dropping columns that don't have to do with a single family home, 1f
columns_to_keep = ['Zoning', 'BldgType', 'HouseStyle', 'OverallQual',
       'OverallCond', 'YearBuilt', 'Exterior1',  'ExterQual',
       'ExterCond', 'TotalBsmtSF', 'Heating', 'CentralAir',
       'FirstFlrSF',  'GrLivArea',       'FullBath', 'HalfBath',  
       'YrSold', 'SaleType', 'SaleCondition', 'SalePrice']

df = df_original[columns_to_keep]


#Only select House Style = 1Story
df = df[df['HouseStyle'] == '1Story']

#Check for datatypes
print(df.dtypes)

#For each column, ensure each value is the correct type

for column in df.columns:
    print(column)
    print(df[column].unique())
    print('\n')
    

#Only select rows that do not have null values
df_no_na = df.dropna()

#Print out the number of rows removed
print('Number of rows removed: ', len(df) - len(df_no_na))

#Check on Correlation

print(df_no_na.corr())