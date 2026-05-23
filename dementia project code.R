# Install the packages required 
install.packages("dplyr")
install.packages("readxl")
install.packages("ggplot2")
install.packages("naniar")
install.packages("e1071")
install.packages("heatmaply")
install.packages("tidyverse")
install.packages("naniar")
install.packages("missMethods")
install.packages("corrplot")
install.packages("caret")



# Libraies
library(dplyr)
library(readxl)
library(ggplot2)
library(naniar)
library(visdat)
library(e1071)
library(heatmaply)
library(tidyverse)
library(missMethods)
library(corrplot)
library(caret)

## Data Munging
# Load the dataset 
data =  read_excel("C:/Users/wanji/Desktop/ICT 583/ICT583 s1 2026 dataset.xlsx")

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

# Function for categorical method imputation 

get_mode <- function(x) {
  x <- x [!is.na(x)]
  ux <- unique(x)
  ux [which.max (tabulate (match (x, ux)))]
}

# Impute for Categorical variables 

data <- data %>%
  mutate(across(
    where(is.factor),
    ~ {
      mode_val <- get_mode(.)
      factor(ifelse(is.na(.), as.character(mode_val), as.character(.)), levels = levels(.))
    }
  ))

# Checking if there are missing values 

data %>%
  
  is.na () %>%
  
  colSums ()

# Histogram Analysis 

plot_histogram <- function (var){
  ggplot(data, aes (x = .data[[var]])) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black") +
    labs (title = paste ("Distribution" , var), x = var, y = "Count") +
    theme_minimal()
}

# Plot for Age

plot_histogram("Age")

# Plot for body_weight

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

# Plot for body_height

plot_boxplot("body_height")

# Plot for waist
plot_boxplot("waist")

# Checking for skewness

skewness_values <- key_vars %>%
  summarise(across (everything(), ~ skewness (., na.rm = TRUE)))
skewness_values

# Correlation Matrix

cor_matrix <- data %>%
  select ( where ( is.numeric)) %>%
  cor()

cor_matrix

# Visualization for the correlation Matrix plot

corrplot(cor_matrix, method = "color", tl.cex = 0.8)


# Check for outliers 

detect_outliers <- function(x) {
  Q1 <- quantile(x,0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  
  x[x < lower] <- lower
  x[x > upper] <- upper
  
  return(x)
  
}

data <- data %>%
  mutate(across(c(Age, body_weight, waist), detect_outliers))


# Check Target Distribution

table (data$MMSE_class)

prop.table(table(data$MMSE_class)) * 100

# Feature Analysis

# Numerical vs Target
ggplot (data, aes (x = MMSE_class, y = Age)) +
  
  geom_boxplot (fill = "steelblue") +
  
  theme_minimal()

ggplot (data, aes (x = MMSE_class, y = body_weight))+
  
  geom_boxplot(fill = "orange") +
  
  theme_minimal()

# Categorical vs Target Variables 

table (data $ Mobility, data$MMSE_class)

prop.table (table (data$Mobility, data$MMSE_class), 2)

# Remove unwanted variable 

data <- data %>%
  select (-ID)

# Define Features and Target
x <- data %>%
  select (-MMSE_class)

y <- data $MMSE_class

# Encode Categorical Variables 

# Dummy Encoding 

dummies <- dummyVars(MMSE_class ~ ., data = data)

x <- predict(dummies, newdata = data)

x <- as.data.frame(x)





# Train/ Test Split: Split the dataset for testing and training needed for the machine Learning models

# Split to < 70-30 > 70% for training and 30 % for testing 









  