# Script Name: ABA Assignment 0
# 
#   Purpose:Analyzing Netflix Titles
#   Author(s): Jorge Zelaya Velasquez
#   Date Created: 01/31/2025
#   
#   Notes: 
#   
#

--------------------------------------------------------------------------------
  #liberary and packages
  
install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")

library(readr)
library(dplyr)
library(tidyr)
library(stringr)


--------------------------------------------------------------------------------
setwd("C:/Users/jorge/OneDrive/Business Analytics/Advanced Data Analytics")
getwd()
  
  
#Load Data
Netflix_Data <- read_csv("netflix_titles.csv") #Read the data

summary(Netflix_Data) #Summary of the data

New_Netflix_Data <- Netflix_Data %>%
                      mutate_if(is.numeric, ~replace(., is.na(.), mean(., na.rm = TRUE))) #Replace missing values with the mean)

New_Netflix_Data_v2 <- distinct(New_Netflix_Data) #Remove duplicates

summary(New_Netflix_Data_v2) #Summary of the cleaned data


#Data Analysis
head(New_Netflix_Data_v2) #First 6 rows of the data


#Changing Data Types as Numeric
New_Netflix_Data_v2 <- New_Netflix_Data_v2 %>%
                        mutate(release_year = as.numeric(release_year),
                               duration = as.numeric(duration)) #Duration data type turns all data into NA in the duration column; need to remove "min" from the data.

#Inspecting Data
unique(New_Netflix_Data_v2$duration) #Unique values of duration column
New_Netflix_Data_v2$duration <- as.numeric(str_replace_all(New_Netflix_Data_v2$duration, " min", "")) #How to remove "min" from duration column

#Structure of Data, making sure the changes were made
str(New_Netflix_Data_v2) 


#Regression Model Of Data
regression_model <- lm(duration ~ release_year, data = New_Netflix_Data_v2) #Regression model

summary(regression_model) #Summary of the regression model) #Summary of regression model

#The regression model is showing the relationship between the release year and the duration of the movie. The model shows that for every year the movie is released, the movie length decreases 0.60287 minutes. 
 
  
