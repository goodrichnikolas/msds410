library(readxl)
library(dplyr)
library(ggplot2)
library(pheatmap)
library(gridExtra)
#install.packages("pheatmap")
#install.packages(c("ggplot2", "gridExtra"))


#1
#Loading the excel
icu_data <- read_excel("icu.xlsx")
#Remove ID
icu_data <- select(icu_data, -ID)
#All variables except the target STA
icu_data_no_STA <- select(icu_data, -STA)

head(icu_data)

#Ensure all columns are integers
icu_data <- icu_data %>% mutate_all(as.integer)
#Summary
summary(icu_data)

#Structure
str(icu_data)
#Distribution of the target variable STA
table(icu_data$STA)
40 / 160
#Check for correlation against STA
cor(icu_data$STA, icu_data[,-1])

#Bar plots list
bar_plot_variables <- c("AGE", "SEX", "SYS", "HRA", "RACE", "LOC")

#Bar plots for distribution
bar_plots <- lapply(bar_plot_variables, function(x) {
  ggplot(icu_data, aes_string(x)) +
    geom_bar() +
    labs(title = x)
})

#Plot each to see the distribution
grid.arrange(grobs = bar_plots, ncol = 2)

#2
#Apply drop conditions only those where TYP == 1
icu_data <- filter(icu_data, TYP == 1)
contingency_table <- table(icu_data$SEX, icu_data$STA)


dimnames(contingency_table) <- list(Gender = c("Male", "Female"), 
                                    Status = c("Lived", "Died"))

print(contingency_table)




total_males <- sum(contingency_table[1,])
total_females <- sum(contingency_table[2,])

prob_survival_male <- contingency_table["Male", "Lived"] / total_males
prob_survival_female <- contingency_table["Female", "Lived"] / total_females
total_survival <- sum(contingency_table[, "Lived"]) / sum(contingency_table)

cat("Probability of survival - Males:", prob_survival_male, "\n")
cat("Probability of survival - Females:", prob_survival_female, "\n")
cat("Total probability of survival:", total_survival, "\n")


#3
#############################################


# Create a 2x2 contingency table for Type of Admission (TYP) and Status (STA)
contingency_table_TYP_STA <- table(icu_data$TYP, icu_data$STA)
dimnames(contingency_table_TYP_STA) <- list(Type_of_Admission = c("Elective", "Emergency"), 
                                            Status = c("Lived", "Died"))
print("Contingency Table for Type of Admission vs Status:")
print(contingency_table_TYP_STA)

# Calculate probabilities of survival 
total_elective <- sum(contingency_table_TYP_STA["Elective",])
total_emergency <- sum(contingency_table_TYP_STA["Emergency",])

prob_survival_elective <- contingency_table_TYP_STA["Elective", "Lived"] / total_elective
prob_survival_emergency <- contingency_table_TYP_STA["Emergency", "Lived"] / total_emergency

cat("Probability of survival - Elective Admission:", prob_survival_elective, "\n")
cat("Probability of survival - Emergency Admission:", prob_survival_emergency, "\n")

# Calculate odds of survival
odds_survival_elective <- contingency_table_TYP_STA["Elective", "Lived"] / contingency_table_TYP_STA["Elective", "Died"]
odds_survival_emergency <- contingency_table_TYP_STA["Emergency", "Lived"] / contingency_table_TYP_STA["Emergency", "Died"]

cat("Odds of survival - Elective Admission:", odds_survival_elective, "\n")
cat("Odds of survival - Emergency Admission:", odds_survival_emergency, "\n")

# Calculate the odds ratio
odds_ratio <- (contingency_table_TYP_STA["Elective", "Lived"] / contingency_table_TYP_STA["Elective", "Died"]) / 
              (contingency_table_TYP_STA["Emergency", "Lived"] / contingency_table_TYP_STA["Emergency", "Died"])

cat("Odds Ratio of survival comparing Elective to Emergency Admissions:", odds_ratio, "\n")
#b
##################
ggplot(icu_data, aes(x = AGE, y = STA)) +
  geom_jitter(width = 0.2, height = 0.05, alpha = 0.5) + 
  labs(x = "Age", y = "Status (0=Lived, 1=Died)", title = "Scatterplot of STA by AGE") +
  theme_minimal()
#c
######################
icu_data$AGE_CAT <- cut(icu_data$AGE, 
                        breaks=c(14, 24, 34, 44, 54, 64, 74, 84, 94, Inf), 
                        labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                        right=TRUE)

# Check the newly created AGE_CAT variable
table(icu_data$AGE_CAT)

# Calculate the proportion (mean) of STA for each AGE_CAT
age_cat_proportion <- icu_data %>%
  group_by(AGE_CAT) %>%
  summarise(ProportionOfSurvival = mean(STA, na.rm=TRUE))

# Plotting the proportions with respect to AGE_CAT
ggplot(age_cat_proportion, aes(x = AGE_CAT, y = ProportionOfSurvival)) +
  geom_bar(stat="identity", fill="skyblue") +
  scale_y_continuous(labels=scales::percent_format()) +
  labs(title = "Age Category vs. Proportion of Death",
       x = "Age Category",
       y = "Proportion of Death") +
  theme_minimal()

#d
#########################
#fit a logistic regression model
logistic_model <- glm(STA ~ AGE, data = icu_data, family = "binomial")
summary(logistic_model)

#f
#############################
aic_value <- AIC(logistic_model)
cat("AIC value for the logistic model:", aic_value, "\n")
n <- nobs(logistic_model)
k <- length(coefficients(logistic_model))
loglike_val <- logLik(logistic_model)
bic_value <- -2*loglike_val + k*log(n)
cat("BIC value for the logistic model:", bic_value, "\n")
#deviation
deviance_val <- deviance(logistic_model)
cat("Deviance value for the logistic model:", deviance_val, "\n")

#g
################################
# Predicting the logit values using the fitted logistic regression model
icu_data$logit_values <- predict(logistic_model, type = "link")

# Creating a scatterplot of the predicted logit values by AGE
ggplot(icu_data, aes(x = AGE, y = logit_values)) +
  geom_point(alpha = 0.6) + 
  labs(title = "Scatterplot of Predicted Logit Values by AGE",
       x = "Age",
       y = "Predicted Logit Values") +
  theme_minimal()

#h
#############################
# Compute the probabilities of survival from the logits
icu_data$predicted_probabilities <- plogis(icu_data$logit_values)

# Plotting the sigmoid curve of predicted probabilities by AGE
ggplot(icu_data, aes(x = AGE, y = predicted_probabilities)) +
  geom_point(alpha = 0.6) + 
  labs(title = "Sigmoid Curve of Predicted Probabilities by AGE",
       x = "Age",
       y = "Predicted Probabilities") +
  theme_minimal()
