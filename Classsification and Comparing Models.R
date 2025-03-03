library(caret)
library(randomForest)
library(dplyr)
library(tidyr)

# Load dataset
Finance_Data <- read.csv("C:/Users/jorge/OneDrive/Business Analytics/Advanced Data Analytics/Data Set/loan_default_data_set.csv")
summary(Finance_Data)

#PART 1

## Do EDA and report you findings.
# 1. Find the shape of the dataset - rows and columns <- There are a total of 2000 rows and 21 columns in this dataset
dim(Finance_Data)

# 2. Datatypes <- There are 12 integers, 8 numeric, and 1 factor variable in this dataset.
table(sapply(Finance_Data, class))

# 3. Plot the variables; Plot any two variables in the data and describe your graphs.
hist(Finance_Data$tot_balance, main="Histogram of Total Balance", xlab="Total Balance", col="blue", border="black") #Histo of TOT Bal.
hist(Finance_Data$rep_income, main="Histogram of Reported Income", xlab="Reported Income", col="blue", border="black") #Histo of Rep. Income

# 4. Missing values
sum(is.na(Finance_Data)) #There are 3,517 missing values in this dataset.
colSums(is.na(Finance_Data)) # Missing values by column; 
                             # There are 1958 missing values in pct_card_over_50_uti, 1559 missing values in rep_income

Finance_Data_clean <- Finance_Data %>% mutate(across(everything(), ~replace_na(., 0)))

                                              
# 5. Duplicates
sum(duplicated(Finance_Data)) # There are no duplicates in this dataset


# 6. Which education level is underrepresented in the data?
table(Finance_Data_clean$rep_education) # The education level of "Graduate" and "Other" is underrepresented in the data.

# 7. Are the classes in the default status variable (“Def_Ind”) balanced in the data? If they are not balanced, suggest ways to correct this imbalance.
table(Finance_Data_clean$Def_ind)

#The classes are not balance. To correct this imbalance, we can use the SMOTE technique.

# 8. How would you describe the distribution of “rep_income”? Is it skewed or approximately normal?
library(e1071)
rep_income_skewness <- skewness(Finance_Data_clean$rep_income)
rep_income_skewness

#The skewness of the rep_income variable is -1.4418, meaning that it is a negatively skewed distribution.

# 9. Group default status (“Def_Ind”) by education level (“rep_education”). Which education level is more likely to default on loans?
table(Finance_Data_clean$rep_education, Finance_Data_clean$Def_ind)

# Calculating Default Rate
college__default_rate <- 100 * (1177/(10960+1177))
college__default_rate # 9.70%

graduate_default_rate <- 100 * (197/(2209+197))
graduate_default_rate # 8.19%

highschool_default_rate <- 100 * (615/(615+4699))
highschool_default_rate # 11.57%

other_default_rate <- 100 * (11/(131+11))
other_default_rate # 7.75%

#After calculating the default rate for each education level,we can see that highschool has the highest default rate to default on loans. 

# Does anything else stand out? 
# What stands out most to me, is that for each edudcation level, there are different amounts of observations. This may play a crucial role in determining the default rate for each education level.

##PART 2

# A. Separate your data into training and testing sets. Fit the model, report, and interpret the accuracy, precision, and recall of the model 
# Split dataset
set.seed(42)
trainIndex <- createDataPartition(Finance_Data_clean$Def_ind, p=0.8, list=FALSE)
train <- Finance_Data_clean[trainIndex, ]
test <- Finance_Data_clean[-trainIndex, ]

## Train and evaluate KNN
knn_model <- train(Def_ind ~ ., data=train, method='knn', tuneLength=5) # Fit KNN model
pred_knn <- predict(knn_model, test)

#Factoring predknn
pred_knn <- as.factor(pred_knn)

conf_matrix <- table(Predicted = pred_knn, Actual = test$Def_ind)
print(conf_matrix)
head(conf_matrix)

##interpret the accuracy, precision, and recall of the model
# Accuracy
accuracy <- sum(diag(conf_matrix))/sum(conf_matrix) * 100
accuracy # .025%

# Precsion
precision <- conf_matrix[2,2]/sum(conf_matrix[,2]) * 100
precision #.248%

# Recall
recall <- conf_matrix[2,2] / (conf_matrix[2,2] + conf_matrix[1,2]) * 100
recall #50%

# Plot a ROC/AUC curve KNN Model
install.packages("pROC")
library(pROC)

# Probability predictions for the positive class (default)
prob_predictions <- predict(knn_model, test)

# Compute ROC curve
roc_curve <- roc(test$Def_ind, prob_predictions)

# Plot ROC curve
plot(roc_curve, main = "ROC Curve")

# Calculate AUC = 0.6369
auc_value <- auc(roc_curve)
print(auc_value) 


# Which features in the data are the most important for predicting default status (“Def_Ind”) 


## B. Train and evaluate Decision Tree

dt_model <- train(Def_ind ~ ., data=train, method='rpart') # Fit Decision Tree Model
pred_dt <- predict(dt_model, test)

pred_dt <- as.factor(pred_dt)

conf_matrix_dt <- table(Predicted = pred_dt, Actual = test$Def_ind)
print(conf_matrix_dt)

##interpret the accuracy, precision, and recall of the model

# Accuracy
accuracy_dt <- sum(diag(conf_matrix_dt))/sum(conf_matrix_dt) * 100
accuracy_dt # 87.725%
# Precision
precision_dt <- conf_matrix_dt[2,2]/sum(conf_matrix_dt[,2]) * 100
precision_dt # 15.842%

# Recall
recall_dt <- conf_matrix_dt[2,2] / (conf_matrix_dt[2,2] + conf_matrix_dt[1,2]) * 100
recall_dt #19.162%


#Plot a ROC/AUC curve 
# Probability predictions for the positive class (default)
prob_predictions_dt <- predict(dt_model, test)

# Compute ROC curve
roc_curve_dt <- roc(test$Def_ind, prob_predictions_dt)

# Plot ROC curve
plot(roc_curve_dt, main = "ROC Curve")

# Calculate AUC = 0.6459
auc_value <- auc(roc_curve_dt)
print(auc_value) 

#Which features in the data are the most important for predicting default status (“Def_Ind”) 
#The feature in the dataset that is most important for predicting default status is the "Def_ind" variable.
#The numnber of observations also played a role in showing which education level was more likely to default on loans.


#Which model performs better at classification? Why?
#The Decision Tree model performs better at classification. The Decision Tree model has a higher accuracy, precision, and recall rate than the KNN model. 
#The Decision Tree model also has a higher AUC value than the KNN model, meaning that it distinguishes between classes better, indicating
#a better performance.The Decision Tree is consistently better than the KNN model in all metrics. As seen in the data above.


