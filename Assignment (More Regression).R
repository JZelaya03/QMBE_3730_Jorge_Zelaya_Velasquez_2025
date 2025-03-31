# Script Name: Assignment (more regression)
# 
#   Purpose:More Regression
#   Author(s): Jorge Zelaya Velasquez
#   Date Created: 02/16/2025
#   
#   Notes: 
#   
#

--------------------------------------------------------------------------------
  
# Liberary and packages
  
library(tidyverse)
library(dplyr)
library(caret)
library(randomForest)
library(readxl)

--------------------------------------------------------------------------------
  setwd("C:/Users/jorge/OneDrive/Business Analytics/Advanced Data Analytics")
getwd()

--------------------------------------------------------------------------------

# Load the data
wages <- read_excel("Data Set/wages.xlsx") 
#Contains data on hourly wage, age,and education of 80 workers at a single
#manufacturing company.
View(wages)

#1a. Plot Wage against Age and evaluate whether a linear or quadratic model
# better capture of the relationship.
plot(wages$Age, wages$Wage, main = "Wage vs Age", xlab = "Age", ylab = "Wage")

# The line of code below fits a linear model to the data and plots it on the
# scatter plot
linear_model <- lm(Wage ~ Age, data = wages)
abline(linear_model, col = "red")

#This line of code fits a quadratic model to the data and plots it
#on the scatter plot
quadratic_model <- lm(Wage ~ poly(Age, 2), data = wages)
lines(wages$Age, predict(quadratic_model), col = "blue")

# Looking at the plot, looks like the quadratic model captures the relationship 
# between wage and age the best, since it fits withing the line of best fit as 
# well as being the closest to the points.


#1b. Estimate a multiple regression model of Wage using Age and Education as 
# independent (X) variables; assume a standard linear relationship 
# between Wage and Age.

#The line of code below fits a multiple regression model to the data, as well as
# summarizes the results of the model using a linear relationship.
multiple_regression_model <- lm(Wage ~ Age + Educ, data = wages)
summary(multiple_regression_model)

#1c. Estimate another multiple regression model of Wage using Age and Education 
# as independent (X) variables; this time fit Age using a quadratic relationship
multiple_regression_model_2 <- lm(Wage ~ poly(Age, 2) + Educ, data = wages)
summary(multiple_regression_model_2)

# Verify your choice from part a. by comparing the distribution of residuals 
# and the goodness of fit between the models in parts b and c.

# Looking at the residuals of the linear and quadratic models, we can see that
# with the linear model there is an average deviation of observed wages from
# the predicted values at 4.678. When looking at the quadratic model, the
# residual standard error is 3.123, showing a more tight fir compared to the
# linear model. This is also demonstrated on the coefficients when looking at
# the p values which were all statistically significant for the quadratic model
# and only shown for Education in the linear model.This backs up my choice of
# choosing the quadratic model.

#1d. Use the appropriate model to predict hourly wages for someone with 16 
# years of education and age equal 30, 50, or 70.

#Lines of code below predict hourly wages for someone with 16 years of education
prediction_data <- data.frame(Age = c(30, 50, 70), Educ = rep(16, 3))
predicted_wages <- predict(multiple_regression_model_2, newdata = prediction_data)

print(predicted_wages)
# Using the line of code above, we can see that the predicted hourly wage for
# someone with 16 years of education and at age 30 is predicted to earn 
# $25.85/hr, at age 50 is predicted to earn $31.54/hr, and at age 70 they are
# predicted to earn $26.56/hr.

#1e. According to the model, at what age will someone with 16 years of 
# education attain the highest wages?

# Using the model, we can see that at age 50 someone will earn 31.54/hr, which
# is the highest attainable wage for someone with 16 years of education.


#2. The AnnArbor.xlsx Download AnnArbor.xlsxfile contains data on a portion of 
# the rental market in Ann Arbor, MI. The data includes the 
# monthly rent, number of bedrooms, number of bathrooms, and the square 
# footage of 40 rental properties.
AnnArbor <- read_excel("Data Set/AnnArbor.xlsx") 
summary(AnnArbor)
view(AnnArbor)

#2a.Plot Rent against each of the three predictor variables and evaluate whether 
# the relationship is best captured by a line or a curve. Identify variables 
# that may benefit from a log-transformation.

#I) The line of code below plots Rent against each of the three 
# predictor variables

#IA. Plotting Rent against Bedrooms
plot(AnnArbor$Beds, 
     AnnArbor$Rent, 
     main = "Rent vs Bedrooms", 
     xlab = "Bedrooms", 
     ylab = "Rent")

#Line of best fit for Rent vs Bedrooms
abline(lm(AnnArbor$Rent ~ AnnArbor$Beds), col = "red")

#Quadratic model for Rent vs Bedrooms
quadratic_model_beds <- lm(Rent ~ poly(Beds, 2), data = AnnArbor)
lines(AnnArbor$Beds, predict(quadratic_model_beds), col = "blue")

#IB. Plotting Rent against Bathrooms
plot(AnnArbor$Baths, 
     AnnArbor$Rent, 
     main = "Rent vs Bathrooms", 
     xlab = "Bathrooms", 
     ylab = "Rent")

#Line of best fit for Rent vs Bathrooms
abline(lm(AnnArbor$Rent ~ AnnArbor$Baths), col = "red")

#Quadratic model for Rent vs Bathrooms
quadratic_model_baths <- lm(Rent ~ poly(Baths, 2), data = AnnArbor)
lines(AnnArbor$Baths, predict(quadratic_model_baths), col = "blue")


#IC. Plotting Rent against SqFt
plot(AnnArbor$Sqft, 
     AnnArbor$Rent, 
     main = "Rent vs SqFt", 
     xlab = "SqFt", 
     ylab = "Rent")

#Line of best fit for Rent vs SqFt
abline(lm(AnnArbor$Rent ~ AnnArbor$Sqft), col = "red")

#Quadratic model for Rent vs SqFt
quadratic_model_sqft <- lm(Rent ~ poly(Sqft, 2), data = AnnArbor)
lines(AnnArbor$Sqft, predict(quadratic_model_sqft), col = "blue")

#II) Evaluating whether the relationship is best captured by a line or a curve.
# Looking at the plots, it seems that the relationship between Rent and Beds
# and Rent and Baths is best captured by a line. The relationship between
# Rent and SqFt is best captured by a curve.

#III) Identifying variables that may benefit from a log-transformation.
# The variable that may benefit from a log-transformation is SqFt, since the
# relationship between Rent and SqFt is best captured by a curve.


#2b. Estimate a multiple regression model (with any appropriate 
# log-transformations) to predict rent for a 1,600-square-foot rental 
# with 3 bedrooms and 2 bathrooms.

#The line of code below log-transforms the Sqft variable
AnnArbor$Sqft_log <- log(AnnArbor$Sqft)

#The code below creates a multiple regression model for the AnnArbor data.
multiple_regression_model_AnnArbor <- lm(Rent ~ Beds + Baths + Sqft, 
                                         data = AnnArbor)

summary(multiple_regression_model_AnnArbor)

# Using the line of code below, we can predict the rent for a 1,600-square-foot
# rental with 3 bedrooms and 2 bathrooms.
predicted_rent <- predict(multiple_regression_model_AnnArbor, 
                          newdata = 
                            data.frame(Beds = 3, Baths = 2, Sqft = 1600))

print(predicted_rent)

# The predicted rent for a 1,600-square-foot rental with 3 bedrooms and
# 2 bathrooms is $1491.69.

