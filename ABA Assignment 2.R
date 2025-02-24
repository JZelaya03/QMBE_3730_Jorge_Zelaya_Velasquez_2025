# Load libraries
#install.packages("caTools")
library(tidyverse)
library(caTools) # Splitting the data set
#install.packages("moments")
library(moments)


# https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/mtcars 
# We'll use the mtcars dataset and predict whether a car is automatic (am = 0) or manual (am = 1).

# Load dataset
data(admit)

View(admit) #View data set.

#Exploratory Data Analysis. 
# 1. Find the shape of the dataset - rows and columns <- there are 280 rows and 4 columns.
# 2. Datatypes <- There are 3 numeric variables and 1 factor variable.
# 3. Plot the variables <- There are no outliers.
# 4. Missing values <- There are no missing values.
# 5. Duplicates <- There are duplicates, but do not need to be removed from the dataset. 

#Classification models
#Check the class distribution <- Row and columns are balanced. However, there is an imbalance between the number of people admitted and not admitted.

#1. Are the classes (admit/don't admit) balanced in the data set? -- No, there is an imbalance between the number of people admitted and not admitted.

#2. How would you describe the distribution of GRE scores? Is it skewed or approximately normal?
gre_skewness <- skewness(admit$gre)
gre_skewness # GRE scores is slightly left skewed, but it is very close to zero, which could suggest there is an approximately normal distribution.


# Convert 'admit' to a factor (categorical variable)
admit$admit <- as.factor(admit$admit)

# View structure of dataset
str(admit)

# Summary statistics
summary(admit)

#Check for class balance
table(admit$admit) #there is no balance admit ~ 68% (0) and 32% (1)

class_proportions <- prop.table(table(admit$admit))
class_proportions #there is no balance admit ~68.3% (0) and 31.7% (1)


# Split the data into training and testing sets.
# Set seed for reproducibility
set.seed(1)

# Split the dataset into training (70%) and testing (30%)
split <- sample.split(admit$admit, SplitRatio = 0.7)

# Create training and testing sets
# https://search.r-project.org/CRAN/refmans/caTools/html/sample.split.html
train_data <- subset(admit, split == TRUE)
test_data <- subset(admit, split == FALSE)

# Check dataset dimensions
dim(train_data) #280 rows and 4 columns
dim(test_data) #120 rows and 4 columns


#Fit the Logistic Regression Model

# Train the logistic regression model
log_model <- glm(admit ~ gre + gpa + rank, data = train_data, family = binomial) # Binomial Distribution, Y variable is binary
# https://www.datacamp.com/doc/r/glm 

# Display model summary
summary(log_model) 
#in a model, the variable with the highest absolute coefficient, the more important the variable is. It also needs to be significant.
# --Which variable in the data is the most important for predicting admission status? -- rank is the most important variable in predicting admission status.
# --This is mostly because it has the highest absolute coefficient and is highly significant (meaning it is less than the p-value .01 being at .000923)

# glm() fits a generalized linear model.
# admit ~ gre + gpa + rank means we predict admit based on gre, gpa, and rank.
# family = binomial specifies logistic regression (since it models probabilities).
# summary(log_model) displays coefficients, significance levels, and model fit statistics.

# Predict probabilities on the test dataset
pred_probs <- predict(log_model, test_data, type = "response")
pred_probs
# Convert probabilities to binary predictions (threshold = 0.5)
pred_classes <- ifelse(pred_probs > 0.5, 1, 0) #this needs to happen in order to become a classification model

# Convert to factor for comparison
pred_classes <- as.factor(pred_classes)

# Display predictions
head(pred_probs)
head(pred_classes)

### Print predictions and true y values as dataframe
do.call(rbind, Map(data.frame, predicted_classes=pred_classes, admit=test_data$admit)) #this is to compare the predicted values with the actual values

#Evaluate model performance

# Create confusion matrix
conf_matrix <- table(Predicted = pred_classes, Actual = test_data$admit)
conf_matrix 

# Compute accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy

# Print results
print(conf_matrix)
print(paste("Accuracy:", round(accuracy, 4)))


# Visualizing predictions vs. actuals
ggplot(test_data, aes(x = gpa, y = as.numeric(as.character(admit)), color = as.factor(pred_classes))) +
  geom_point(size = 3) +
  labs(title = "Predicted vs Actual Admissions",
       x = "GPA",
       y = "Admissions (0 = Do Not Admit, 1 = Admit)") +
  scale_color_manual(values = c("red", "blue"), name = "Prediction")

##Fit the model, report, and interpret the accuracy, precision and recall of the model.

#The accuracy of this model is 0.717, which means that the model correctly predicted 71.7% of the observations in the test dataset.

# The model has a precision of 0.643, which means that 64.3% of the predicted positive cases were correct. 
precision <- conf_matrix[2,2] / (conf_matrix[2,2] + conf_matrix[2,1])
precision
# The model has a recall of .237, which means that 23.7% of the actual positive cases were correctly predicted.
recall <- conf_matrix[2,2] / (conf_matrix[2,2] + conf_matrix[1,2])
recall

