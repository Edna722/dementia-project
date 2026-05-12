# Install the packages required 
install.packages("dplyr")
install.packages("readxl")
install.packages("ggplot2")
install.packages("naniar")
install.packages("e1071")
install.packages("heatmaply")
install.packages("tidyverse")



# Libraies
library(dplyr)
library(readxl)
library(ggplot2)
library(naniar)
library(visdat)
library(e1071)
library(heatmaply)
library(tidyverse)

## Data Munging
# Load the dataset 
data <- read_excel("C:/Users/wanji/Desktop/ICT 583/ICT583 s1 2026 dataset.xlsx")

str (data)

# Display the first 15 rows of the data 
head (data)

# Display the last rows of the data 
tail (data)

# Understanding the data : Check the data 
glimpse (data)

# Composition of data:In-depth understanding of data 
summary (data)

# Check for unique values 
sapply (data, function(x) length(unique(x)))


# Statistics 

# In depth statistics 
data %>%
  select(where (is.numeric)) %>%
  summary()

# In-depth stats summary for age, body_weight, body_height, waist
data %>%
  summarise(
    mean_age = mean (Age, na.rm = TRUE),
    Median_age = median (Age,na.rm = TRUE),
    sd_age = sd (Age, na.rm = TRUE),
    min_age = min (Age, na.rm = TRUE),
    max_age = max (Age, na.rm = TRUE)
  )

# Numeric variables statistics 
key_vars <-data %>%
  select (Age, body_weight, body_height, waist)

# Descriptive Statistics 
desc_stats <- key_vars%>%
  summarise (across(
    everything (),
    list(
      mean = ~ mean (., na.rm = TRUE),
      median = ~ median (.,na.rm = TRUE),
      sd = ~ sd (.,na.rm = TRUE),
      min = ~ min (., na.rm = TRUE),
      max = ~ max (., na.rm = TRUE),
      Q1 = ~ quantile (., 0.25,  na.rm = TRUE),
      Q3 = ~ quantile (., 0.75, na.rm = TRUE)
    ),
    
    .names = "{.col}_{.fn}"
  ))

desc_stats


# Impute Categorical data 
data <- data %>%
  mutate(
    Education_ID = as.factor (Education_ID),
    Mobility = as.factor (Mobility),
    MMSE_class = as.factor (MMSE_class),
    Hyperlipidaemia = as.factor (Hyperlipidaemia)
  )


# Identify missing values 
data %>%
  
  is.na () %>%
  
  colSums ()

# Visualize missing data values percentage threshold 
missing_percent <- colSums(is.na(data)) / nrow(data) * 100
missing_percent

# Handling the missing values for Numerical and Categorical variables 

# Impute for numerical variables
data <- data %>%
  mutate (across(
    where (is.numeric),
    ~ ifelse (is.na (.), median (., na.rm = TRUE),.)
  ))


# Impute for Categorical variables 
data <- data %>%
  mutate(across(
    where(is.factor),
    ~ ifelse (is.na(.) , get_mode(.), .)
    
  ))


# Histogram Analysis 
plot_histogram <- function (var){
  ggplot(data, aes (x = .data[[var]])) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black") +
    labs (title = paste ("Distribution" , var), x = var, y = "Count") +
    theme_minimal()
}

# Plot for Age
plot_histogram("Age")

#Plot for body_weight
plot_histogram("body_weight")

# plot for height
plot_histogram("body_height")

# Plot for waist
plot_histogram("waist")

# Checking for Outliers 
plot_boxplot <- function(var){
  ggplot(data, aes(y = .data[[var]])) +
           geom_boxplot(fill = "steelblue") +
           labs (title = paste("Boxplot", var), y = var)+
           theme_minimal()
}

#   Plot for Age 
plot_boxplot("Age")

# Plot for body_weight
plot_boxplot("body_weight")

#Plot for body_height
plot_boxplot("body_height")

# Plot for waist
plot_boxplot("waist")

# Heatmap for missing values 
heatmaply_na(data)

# Checking for skewness
skewness_values <- key_vars %>%
  summarise(across (everything(), ~ skewness (., narm = TRUE)))
skewness_values

# Check for outliers 
detect_outliers <- function(x) {
  Q1 <- quantile(x,0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.25, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  
  x[x < lower] <- lower
  x[x > upper] <- upper
  
  return(x)
  
}
data <- data %>%
  mutate(across(c(Age, body_weight, body_weight, waist), cap_outliers))

# Missing values data patterns :Finding the intersection of the missing values 
gg_miss_upset( x = data) 
# Heatmap to represent the missing values 

# Create a missing value variable 

# Percentage threshold of missing values : >70%

# Using Median

  
  