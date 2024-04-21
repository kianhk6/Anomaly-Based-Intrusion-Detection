getwd()
# # Adjust the file path and options as necessary
# setwd("C:/Users/kian/anamolydetection/")

DataDf <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", na.strings = "?")
# DataDf <- read.table("household_power_consumption.txt", header = T, sep = ",")
# I think you need to install packages before here 

library(ggplot2)
library(ggbiplot)
library(dplyr)
library(ggpubr)
library(zoo)
library(tidyverse)
library(repr)
library(data.table)
library(ggplot2)
library(dplyr)
library(pracma)
library(lubridate)
library(depmixS4)
library(MASS)
library(factoextra)

# Applying linear interpolation for each column  

df <- DataDf # Create a copy of the data to store interpolated values
#interpolate missing values in each column
df <- df %>%
  mutate(Global_active_power = na.approx(Global_active_power))
df <- df %>%
  mutate(Global_reactive_power = na.approx(Global_reactive_power))
df <- df %>%
  mutate(Voltage  = na.approx(Voltage ))
df <- df %>%
  mutate(Global_intensity = na.approx(Global_intensity))
df <- df %>%
  mutate(Sub_metering_1  = na.approx(Sub_metering_1 ))
df <- df %>%
  mutate(Sub_metering_2  = na.approx(Sub_metering_2 ))
df <- df %>%
  mutate(Sub_metering_3  = na.approx(Sub_metering_3 ))

# format the date
df$weekday <- wday(dmy(df$Date))

# convert the time format so that we can comapre it to our window
dataTime <- as.ITime(df$Time, format = "%H:%M:%S")
df$time2 <- as.ITime(df$Time, format = "%H:%M:%S")

# Set a specific time window from 3:59pm to 7:59pm
window_start = as.ITime("15:59:59")
window_end = as.ITime("19:59:59")

# Extract the same time window for each week of the dataset:
wensday_data <- df %>% filter(time2 > window_start & time2 < window_end & weekday == 3)
head(wensday_data)

# Copying 'wensday_data' to a new variable for scaling operations
wensday_data_scaled <- wensday_data

# Defining the columns that are numeric being scaled
numeric_columns <- c("Global_active_power", "Global_reactive_power", "Voltage", 
                     "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")

# Applying standardization (Z-score normalization) to the specified numeric columns
wensday_data_scaled <- wensday_data_scaled %>%
  mutate(across(all_of(numeric_columns), scale))

# Displaying the first few rows of the scaled data (standardization)
head(wensday_data_scaled)

####################### Sampling ######################

# Ensure the dataset is not NULL or empty
if(is.null(wensday_data_scaled) || nrow(wensday_data_scaled) == 0) {
  stop("The dataset is NULL or empty.")
}

# Calculate the size for training data (e.g., 80% of the dataset)
train_size <- floor(0.80 * nrow(wensday_data_scaled))

# Ensure that both training and testing datasets will have at least 1 row
if (train_size == 0 || train_size == nrow(wensday_data_scaled)) {
  stop("The dataset is too small to be split into training and testing sets.")
}

# Setting a seed for reproducibility
set.seed(233)

# Sample the indices for training data
train_indices <- df[1:train_size,]

print(train_indices)

# Create training dataset
train_data <- df[train_indices, ]

# Checking the dimensions of the train data
print(dim(train_data))
train_data[,3:9] <- scale(train_data[,3:9], center = TRUE, scale = TRUE)
data_sample <- train_data[,3:9]

#PCA
pca_result <- prcomp(data_sample)
summary(pca_result)
#shows negative values for PCA 
print(pca_result)
#So we take the absolute value of the PCA
print("Absolute Load Scores:")
print(abs(pca_result$rotation))
load_scores <- as.data.frame(abs(pca_result$rotation))
print(load_scores)

d <- pca_result$sdev^2
df <- data.frame(PC = 1:length(d), yvar = d/sum(d))
scree_plt <- ggplot(data = df, aes(x = PC, y = yvar)) +geom_bar(stat = "identity")+
  scale_x_continuous(breaks = df$PC) +
  labs(
    title = "Train PCA Variance",
    x = "PC #",
    y = "Proportion of Explained Variance"
  ) 
print(scree_plt)  

library(factoextra)
#get_eig() (or get_eigenvalue()): 
#returns a data.frame containing 3 columns: 
#the eigenvalues, the percentage of variance 
#and the cumulative percentage of variance 
#retained by each dimension.
get_eig(pca_result)
#fviz_eig() (or fviz_screeplot()): 
#returns a ggplot2
fviz_screeplot(pca_result, addlabels = TRUE)
fviz_eig(pca_result,addlabels = TRUE )

# Now we graph the resulting PCA
#This gives us a blob can't see the vectors 
ggbiplot(pca_result, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE)


#lets take a sample of the data and show the vectors 
data <- data_sample[sample(nrow(data_sample), 400),]
pca_result <- prcomp(data)
load_scores <- as.data.frame(abs(pca_result$rotation))
print(load_scores)
ggbiplot(pca_result, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE)