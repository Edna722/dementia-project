# Install the packages required 
install.packages("dplyr")
install.packages("readxl")
install.packages("ggplot2")
install.packages("naniar")
install.packages("e1071")



# Libraies
library(dplyr)
library(readxl)
library(ggplot2)
library(naniar)
library(visdat)
library(e1071)



# Data Munging
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
summary (data)

# Check for data types 
sapply (data, class)

# Check for unique values 
sapply (data, function(x) length(unique(x)))

# Identify numerical and categorical values 
# Impute Categorical data 
data <- data %>%
  mutate(
    Education_ID = as.factor (Education_ID),
    Mobility = as.factor (Mobility),
    MMSE_class = as.factor (MMSE_class),
    Hyperlipidaemia = as.factor (Hyperlipidaemia)
  )

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
           geom_boxplot(fill = "orange") +
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

# Checking for skewness
skewness_values <- key_vars %>%
  summarise(across (everything(), ~ skewness (., narm = TRUE)))
skewness_values

# Check for outliers 

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

  
  