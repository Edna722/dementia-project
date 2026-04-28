# Install the packages required 
install.packages("dplyr")
install.packages("readxl")
install.packages("ggplot2")
install.packages("naniar")


# Libraies
library(dplyr)
library(readxl)
library(ggplot2)
library(naniar)
library(visdat)



### Data Munging
# Load the dataset 
data <- read_excel("C:/Users/wanji/Desktop/ICT 583/ICT583 s1 2026 dataset.xlsx")
str (data)

# Display the first 15 rows of the data 
head (data)

#Display the last rows of the data 
tail (data)


# Understanding the data : Check the data 
glimpse (data)

# Composition of data:In-depth understanding of data 
summary(data)

# Check for data types 
sapply(data, class)

# Check for unique values 
sapply(data, function(x) length(unique(x)))

# Identify numerical and categorical values 
#Numerical values 

# Data cleaning 

# Identify missing values 
data %>%
  
  is.na () %>%
  
  colSums ()

# Visualize missing data values 
vis_miss (data, warn_large_data = FALSE)

# Missing values data patterns :Finding the intersection of the missing values 
gg_miss_upset(data) 
# Heatmap to represent the missing values 

# Create a missing value variable 

# Percentage threshold of missing values : >70%

# Using Median

  
  