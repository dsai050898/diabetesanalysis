# Import required libraries 
library(tidyverse)
library(formattable)

# constructing a data frame in R studio after importing the cleansed diabetes data set
diabetes_df <- read.csv("C:/Users/dsai0/OneDrive/Desktop/DATA SC ASSignment/RAW DATA/diabetes.csv")


# Checking the last five rows of the data frame
tail(diabetes_df)

# Checking the first five rows of the data frame
head(diabetes_df)

# Checking the number of rows of the data frame
nrow(diabetes_df)

# Checking the number of columns of the data frame
ncol(diabetes_df)


sum(is.na(diabetes_df))
write.csv(diabetes_df, "C:/Users/dsai0/OneDrive/Desktop/DATA SC ASSignment/Clean data/diabetes_cleaned.csv")
