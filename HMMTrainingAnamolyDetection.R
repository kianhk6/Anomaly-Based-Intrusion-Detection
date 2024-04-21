getwd()
# Adjust the file path and options as necessary
# setwd("C:/Users/kian/anamolydetection")

DataDf <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", na.strings = "?")
#DataDf <- read.table("household_power_consumption.txt", header = T, sep = ",")
# I think you need to install packages before here 

library(ggplot2)
library(dplyr)
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

# Applying linear interpolation for each column to fill in missing values

df <- DataDf # Create a copy of the data to store interpolated values
# interpolate missing values in each column
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
# view updated data frame
# checking and comparing the filled in values
df[6841,]
DataDf[6841,]

# selecting the time frame

# format the date
df$weekday <- wday(dmy(df$Date))

# convert the time format so that we can comapre it to our window
dataTime <- as.ITime(df$Time, format = "%H:%M:%S")
df$time2 <- as.ITime(df$Time, format = "%H:%M:%S")

# Set a specific time window from 3:59pm to 7:59pm
window_start = as.ITime("15:59:59")
window_end = as.ITime("19:59:59")

# Extract the same time window for each week of the dataset:
wensday_data <- df %>% filter(time2 > window_start & time2 < window_end & weekday == 4)
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

# Random Sampling

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
train_indices <- sample(seq_len(nrow(wensday_data_scaled)), size = train_size)

# Create training and testing datasets
train_data <- wensday_data_scaled[train_indices, ]
test_data <- wensday_data_scaled[-train_indices, ]

# Checking the dimensions of the training and testing data
print(dim(train_data))
print(dim(test_data))

# Training HMM with Gaussian distribution for countinous data

# due to compute power tried this range

states_range <- c(4, 5,6,7,8)

# List to store the fitted models
state_nums <- vector()
likelihood_values <- vector()
bic_values <- vector() 
hmm_models <- vector()
nTimesTrain <- aggregate(Date ~ Time, train_data, FUN = length)$Date

results <- data.frame(State = integer(), LogLikelihood = numeric(), BIC = numeric())

# Loop to train models for each number of states
for (state in states_range) {
  print(state) 
  hmm_model <- depmix(response = list(train_data$Global_intensity ~ 1, 
                                      train_data$Global_active_power ~ 1), 
                      data = train_data, 
                      nstates = state,
                      ntimes = nTimesTrain,
                      family = list(gaussian(), gaussian()))
  
  fitted_model <- fit(hmm_model)
  hmm_models <- append(hmm_models, fitted_model)
  likelihood_values <- append(likelihood_values, logLik(fitted_model))
  bic_values <- append(bic_values, BIC(fitted_model))
  state_nums <- append(state_nums, state)
}

print(length(hmm_models))

plot_data <- data.frame(State = state_nums, LogLikelihood = likelihood_values, BIC = bic_values)

# Preparing the plot with ggplot2, setting up the aesthetics
visualization <- ggplot(plot_data, aes(x = State)) + 
  # Adding points and lines for Log Likelihood with customized colors
  geom_point(aes(y = LogLikelihood, color = "Value of Log Likelihood")) + 
  geom_line(aes(y = LogLikelihood, color = "Value of Log Likelihood"), size = 0.8) + 
  # Adding points and lines for BIC with different colors
  geom_point(aes(y = BIC, color = "Bayesian Information Criterion (BIC)")) + 
  geom_line(aes(y = BIC, color = "Bayesian Information Criterion (BIC)"), size = 0.8) + 
  # Adjusting scales and colors
  scale_color_manual(values = c("Value of Log Likelihood" = "#2307bd", "Bayesian Information Criterion (BIC)" = "#5fae34")) + 
  scale_x_continuous(breaks = seq(4, 8, by = 1)) +
  # Customizing the labels and titles
  labs(title = "Comparison of BIC & Log Likelihood Across Various States", 
       x = "States Count", 
       y = "Metrics Values", 
       color = "Metrics") + 
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold")
  )

# Displaying the plot
print(visualization)

# testing 
test_likelihoods <- vector()

nTimesTest <- aggregate(Date ~ Time, test_data, FUN = length)$Date

for (i in seq_along(hmm_models)) {
  print(i)
  model <- hmm_models[[i]]
  modified_model <- depmix(response = list(test_data$Global_intensity ~ 1,
                                           test_data$Global_active_power ~ 1),
                           data = test_data, 
                           nstates = model@nstates, 
                           ntimes = nTimesTest, 
                           family = list(gaussian(), gaussian()))
  model_params <- getpars(model)
  modified_model <- setpars(modified_model, model_params)
  test_likelihoods[i] <- forwardbackward(modified_model, 
                                         test_window, return.all = FALSE,
                                         useC = FALSE)$logLik
}

normalized_train <- likelihood_values / nrow(scaled_train_data)
normalized_test <- test_likelihoods / nrow(test_data)
print(normalized_test)

# normalized data plotting
normalized_plot_data <- data.frame(State = state_nums, TrainLL = normalized_train, BIC = bic_values, TestLL = normalized_test)

test_train_plot <- ggplot(normalized_plot_data, aes(State)) +
  geom_point(aes(y = TrainLL, color = "Train LogLik")) +
  geom_line(aes(y = TrainLL, color = "Train LogLik"), size=1) +
  geom_point(aes(y = TestLL, color = "Test LogLik")) +
  geom_line(aes(y = TestLL, color = "Test LogLik"), size=1) +
  scale_x_continuous(breaks = 7:12) +
  scale_color_manual(values = c("Test LogLik" = "#E7298A", "Train LogLik" = "#7570B3")) +
  labs(
    title = "Normalized LogLik Train Test vs N states",
    x = "Number of States", 
    y = "Normalized LogLik") +
  theme(
    plot.title = element_text(face="bold", size = 16),
    plot.subtitle = element_text(face="bold.italic"),
    axis.title = element_text(face="bold", size = 14),
    axis.text = element_text(size = 10),
    legend.title = element_text(face="bold", size = 12),
    legend.position = "top"
  )

# Print the plots
print(test_train_plot)

chosen_model = hmm_models[[4]] #8 states
saveRDS(hmm_models[[4]], file = "model_8.rds")

# Anamoly detection

m8 <- readRDS("model_8.rds")

# training analysis

train_loglik <- vector()
train_subset <- train_data

#calculate # of pts in subset
nTimesSubset <- aggregate(Date ~ Time, train_subset, FUN = length)$Date

#create modified model using depmix function
modified_model <- depmix(response = list(train_subset$Global_intensity ~ 1,
                                         train_subset$Global_active_power ~ 1),
                         data = train_data,
                         nstates = m8@nstates,
                         ntimes = nTimesSubset,
                         family = list(gaussian(), gaussian()))


#set params of modified model to match optimal model
model_params <- getpars(m8)
modified_model <- setpars(modified_model, model_params)

#calc logli of test subset during modified model
train_loglik <- forwardbackward(modified_model, 
                                train_subset, 
                                return.all = FALSE, 
                                useC = FALSE)$logLik

train_loglik
norm_train_loglik <- train_loglik / nrow(train_data)

norm_train_loglik

# data cleansing for testing analysis

#partition test data into 10 equal size subsets representing consecutive weeks 
rows_per_split <- nrow(test_data) %/% 10

#create list to store split data frames 
split_test_data <- vector("list", length = 10)

#loop thru and split data 
for (i in 1:9) {
  split_test_data[[i]] <- test_data[((i-1) * rows_per_split + 1):(i * rows_per_split), ]
}

split_test_data[[10]] <- test_data[((9 * rows_per_split) + 1):nrow(test_data), ]

#access the dataframes 
split_test_data[[1]]
split_test_data[[10]]

#compute logli for each subset using best model to determine max deviation from training log lik val

# testing analysis 

#need to iterate over 10 data frames, with 8 states, and n times

test_likelihoods <- vector(length = 10)

#iterate over each subset (10 times)

for (i in 1:10) {
  #get subset test data 
  test_subset <- split_test_data[[i]]
  
  #calculate # of pts in subset
  nTimesSubset <- aggregate(Date ~ Time, test_subset, FUN = length)$Date
  
  #create modified model using depmix function
  modified_model <- depmix(response = list(test_subset$Global_intensity ~ 1,
                                           test_subset$Global_active_power ~ 1),
                           data = test_subset,
                           nstates = m8@nstates,
                           ntimes = nTimesSubset,
                           family = list(gaussian(), gaussian()))
  
  
  #set params of modified model to match optimal model
  model_params <- getpars(m8)
  modified_model <- setpars(modified_model, model_params)
  
  #calc logli of test subset during modified model
  test_likelihoods[i] <- forwardbackward(modified_model, 
                                         test_subset, 
                                         return.all = FALSE, 
                                         useC = FALSE)$logLik
  
}

test_likelihoods

#normalize them
normalized_test_loglik <- test_likelihoods
for (i in 1:10) {
  normalized_test_loglik[i] <- test_likelihoods[i] / nrow(split_test_data[[i]]) 
  
}

normalized_test_loglik



#compare the test and the train -> loop thru and find the difference

anom <- vector(length = 10)

for (i in 1:10) {
  anom[i] <- abs(normalized_test_loglik[i] - norm_train_loglik)
}

anom

cat("maximum deviation", max(anom)) 

# multinomial distribution for discretized data as we recieved positive loglikelihood

# Applying standardization (Z-score normalization) to the specified numeric columns
wensday_data_scaled <- wensday_data_scaled %>%
  mutate(across(all_of(numeric_columns), scale))

# Displaying the first few rows of the scaled data (standardization)
head(wensday_data_scaled)

# Discretization sampling 

# Bin the Global_intensity
wensday_data_scaled$Global_intensity_binned <- cut(wensday_data_scaled$Global_intensity, breaks = 5)

# Bin the Global_active_power
wensday_data_scaled$Global_active_power_binned <- cut(wensday_data_scaled$Global_active_power, breaks = 5)


train_data <- wensday_data_scaled[trainIndex, ]
test_data <- wensday_data_scaled[-trainIndex, ]

# Determine the range for binning

global_intensity_range <- range(c(train_data$Global_intensity, test_data$Global_intensity))

# Making sure range of categories are the same for both test and training samples

Define the number of bins or explicitly define the breaks
bins_intensity = seq(from = floor(global_intensity_range[1]), 
                     to = ceiling(global_intensity_range[2]), 
                     by = 0.8)

# Apply binning
train_data$Global_intensity <- cut(train_data$Global_intensity, breaks = bins_intensity, include.lowest = TRUE, labels = FALSE)
train_data$Global_active_power <- cut(train_data$Global_active_power, breaks = bins_active_power, include.lowest = TRUE, labels = FALSE)

test_data$Global_intensity <- cut(test_data$Global_intensity, breaks = bins_intensity, include.lowest = TRUE, labels = FALSE)
test_data$Global_active_power <- cut(test_data$Global_active_power, breaks = bins_active_power, include.lowest = TRUE, labels = FALSE)


summary(unique(train_data$Global_intensity))
cat("test_data - Number of categories in Global_intensity:",sort(unique(train_data$Global_intensity)), "\n")
cat("test_data - Number of categories in Global_intensity:", sort(unique(test_data$Global_intensity)), "\n")

cat("test_data - Number of categories in Global_intensity:",sort(unique(test_data$Global_active_power)), "\n")
cat("test_data - Number of categories in Global_intensity:", sort(unique(test_data$Global_active_power)), "\n")

nTimesTest <- aggregate(Date ~ Time, test_data, FUN = length)$Date


# Training

states_range <- c(1,2)

List to store the fitted models
state_nums <- vector()
likelihood_values <- vector()
bic_values <- vector() 
hmm_models <- vector()
nTimesTrain <- aggregate(Date ~ Time, train_data, FUN = length)$Date

results <- data.frame(State = integer(), LogLikelihood = numeric(), BIC = numeric())

# Loop to train models for each number of states
for (state in states_range) {
  print(state) 
  hmm_model <- depmix(response = list(train_data$Global_intensity ~ 1, 
                                      train_data$Global_active_power ~ 1), 
                      data = train_data, 
                      nstates = state,
                      ntimes = nTimesTrain,
                      family = list(multinomial(), multinomial())) # This is an example for multinomial/categorical variables.


  fitted_model <- fit(hmm_model)
  hmm_models <- append(hmm_models, fitted_model)
  likelihood_values <- append(likelihood_values, logLik(fitted_model))
  print(logLik(fitted_model))
  bic_values <- append(bic_values, BIC(fitted_model))
  state_nums <- append(state_nums, state)
}

print(length(hmm_models))

plot_data <- data.frame(State = state_nums, LogLikelihood = likelihood_values, BIC = bic_values)

# Preparing the plot with ggplot2, setting up the aesthetics
visualization <- ggplot(plot_data, aes(x = State)) + 
  # Adding points and lines for Log Likelihood with customized colors
  geom_point(aes(y = LogLikelihood, color = "Value of Log Likelihood")) + 
  geom_line(aes(y = LogLikelihood, color = "Value of Log Likelihood"), size = 0.8) + 
  # Adding points and lines for BIC with different colors
  geom_point(aes(y = BIC, color = "Bayesian Information Criterion (BIC)")) + 
  geom_line(aes(y = BIC, color = "Bayesian Information Criterion (BIC)"), size = 0.8) + 
  # Adjusting scales and colors
  scale_color_manual(values = c("Value of Log Likelihood" = "#2307bd", "Bayesian Information Criterion (BIC)" = "#5fae34")) + 
  scale_x_continuous(breaks = seq(4, 8, by = 1)) +
  # Customizing the labels and titles
  labs(title = "Comparison of BIC & Log Likelihood Across Various States", 
       x = "States Count", 
       y = "Metrics Values", 
       color = "Metrics") + 
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold")
  )

# Displaying the plot
print(visualization)

Testing
test_likelihoods <- vector()

# Iterate over each fitted model
for (i in seq_along(hmm_models)) {
  model <- hmm_models[[i]]

  # Extract parameters from the trained model
  model_params <- length(getpars(model))

  # Check the length of parameters
  cat("Length of model_params:", (model_params), "\n")

  # Initialize the modified model but do not set parameters yet
  modified_model <- depmix(response = list(test_data$Global_intensity ~ 1,
                                           test_data$Global_active_power ~ 1),
                           data = test_data, 
                           nstates = model@nstates, 
                           ntimes = nTimesTest, 
                           family =  list(multinomial(), multinomial()))

  # Check expected number of parameters for the modified model
  expected_params_length <- length(getpars(modified_model))
  cat("Expected parameters length for modified model:", expected_params_length, "\n")

  # Only proceed if the lengths match

  modified_model <- setpars(modified_model, model_params)


  test_likelihoods[i] <- forwardbackward(modified_model, 
                                         test_window, return.all = FALSE,
                                         useC = FALSE)$logLik
}


normalized_train <- likelihood_values / nrow(train_data)
normalized_test <- test_likelihoods / nrow(test_data)
print(normalized_test)

# Normalized data plotting
normalized_plot_data <- data.frame(State = state_nums, TrainLL = normalized_train, BIC = bic_values, TestLL = normalized_test)

test_train_plot <- ggplot(normalized_plot_data, aes(State)) +
  geom_point(aes(y = TrainLL, color = "Train LogLik")) +
  geom_line(aes(y = TrainLL, color = "Train LogLik"), size=1) +
  geom_point(aes(y = TestLL, color = "Test LogLik")) +
  geom_line(aes(y = TestLL, color = "Test LogLik"), size=1) +
  scale_x_continuous(breaks = 7:12) +
  scale_color_manual(values = c("Test LogLik" = "#E7298A", "Train LogLik" = "#7570B3")) +
  labs(
    title = "Normalized LogLik Train Test vs N states",
    x = "Number of States", 
    y = "Normalized LogLik") +
  theme(
    plot.title = element_text(face="bold", size = 16),
    plot.subtitle = element_text(face="bold.italic"),
    axis.title = element_text(face="bold", size = 14),
    axis.text = element_text(size = 10),
    legend.title = element_text(face="bold", size = 12),
    legend.position = "top"
  )

# Print the plots
print(test_train_plot)

chosen_model = hmm_models[[1]]

# anamoly detection for discretized data

m8 <- hmm_models[[1]]

train_loglik <- vector()
train_subset <- train_data

#calculate # of pts in subset
nTimesSubset <- aggregate(Date ~ Time, train_subset, FUN = length)$Date

#create modified model using depmix function
modified_model <- depmix(response = list(train_subset$Global_intensity ~ 1,
                                         train_subset$Global_active_power ~ 1),
                         data = train_data,
                         nstates = m8@nstates,
                         ntimes = nTimesSubset,
                         family = list(multinomial(), multinomial()))


#set params of modified model to match optimal model
model_params <- getpars(m8)
modified_model <- setpars(modified_model, model_params)

#calc logli of test subset during modified model
train_loglik <- forwardbackward(modified_model, 
                                train_subset, 
                                return.all = FALSE, 
                                useC = FALSE)$logLik

train_loglik
norm_train_loglik <- train_loglik / nrow(train_data)

norm_train_loglik

#partition test data into 10 equal size subsets representing consecutive weeks 
rows_per_split <- nrow(test_data) %/% 10

#create list to store split data frames 
split_test_data <- vector("list", length = 10)

#loop thru and split data 
for (i in 1:9) {
  split_test_data[[i]] <- test_data[((i-1) * rows_per_split + 1):(i * rows_per_split), ]
}
#this for remaining 
split_test_data[[10]] <- test_data[((9 * rows_per_split) + 1):nrow(test_data), ]


#access the dataframes 
split_test_data[[1]]
split_test_data[[10]]

test_likelihoods <- vector(length = 10)

for (i in 1:10) {
  # get subset test data 
  test_subset <- split_test_data[[i]]

  # calculate # of pts in subset
  nTimesSubset <- aggregate(Date ~ Time, test_subset, FUN = length)$Date

  # create modified model using depmix function
  modified_model <- depmix(response = list(test_subset$Global_intensity ~ 1,
                                           test_subset$Global_active_power ~ 1),
                           data = test_subset,
                           nstates = m8@nstates,
                           ntimes = nTimesSubset,
                           family = list(multinomial(), multinomial()))


  # set params of modified model to match optimal model
  model_params <- getpars(m8)
  modified_model <- setpars(modified_model, model_params)

  # calc logli of test subset during modified model
  test_likelihoods[i] <- forwardbackward(modified_model, 
                                         test_subset, 
                                         return.all = FALSE, 
                                         useC = FALSE)$logLik

}

test_likelihoods

# normalize them
normalized_test_loglik <- test_likelihoods
for (i in 1:10) {
  normalized_test_loglik[i] <- test_likelihoods[i] / nrow(split_test_data[[i]]) 

}

normalized_test_loglik

# compare the test and the train -> loop thru and find the difference

anom <- vector(length = 10)

for (i in 1:10) {
  anom[i] <- abs(normalized_test_loglik[i] - norm_train_loglik)
}

anom

cat("maximum deviation", max(anom)) 