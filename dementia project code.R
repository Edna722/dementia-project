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
dsesc_stats <- key_vars%>%
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

# Plotting
plot_histogram("Age")
plot_histogram("body_weight")
plot_histogram("body_height")
plot_histogram("waist")


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

  
  