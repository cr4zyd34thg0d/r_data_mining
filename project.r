install.packages("readr") # Install the readr package
library(readr) # Load the readr package
install.packages("forecast")
###############################################################
######Data Preprocessing (Descriptive Method - Clustering):####
###############################################################
# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(cluster)
library(caret)
library(forecast)
library(ggplot2)
library(e1071)
library(rpart)
library(rpart.plot)
library(rpart)
library(rpart.plot)

# Load the data
data <- read_csv("projectdataset.csv", col_types = cols(
  City = col_character(),
  Price = col_double(),
  Day = col_character(),
  `Room Type` = col_character(),
  `Shared Room` = col_logical(),
  `Private Room` = col_logical(),
  `Person Capacity` = col_double(),
  Superhost = col_logical(),
  `Multiple Rooms` = col_double(),
  Business = col_double(),
  `Cleanliness Rating` = col_double(),
  `Guest Satisfaction` = col_double(),
  Bedrooms = col_double(),
  `City Center (km)` = col_double(),
  `Metro Distance (km)` = col_double(),
  `Attraction Index` = col_double(),
  `Normalised Attraction Index` = col_double(),
  `Restraunt Index` = col_double()
))

# Convert data types
data$City <- as.factor(data$City)
data$Day <- as.factor(data$Day)
data$`Room Type` <- as.factor(data$`Room Type`)
data$`Shared Room` <- as.logical(data$`Shared Room`)
data$`Private Room` <- as.logical(data$`Private Room`)
data$Price <- as.numeric(data$Price)
data$`Cleanliness Rating` <- as.numeric(data$`Cleanliness Rating`)

# Perform K-means clustering
k <- 3  # Number of clusters
set.seed(123)  # Set seed for reproducibility
data_cluster <- data[, c("Price", "Person Capacity", "Cleanliness Rating", "Bedrooms", "City Center (km)", "Attraction Index", "Restraunt Index")]  # Select relevant columns for clustering
kmeans_model <- kmeans(scale(data_cluster), centers = k, nstart = 10)  # Perform K-means clustering

# Add cluster labels to original data
data$Cluster <- as.factor(kmeans_model$cluster)

# Scatter plot of Price vs. City Center (km) with cluster labels
ggplot(data, aes(x = Price, y = `City Center (km)`, color = Cluster)) +
  geom_point() +
  labs(title = "K-means Clustering of Accommodation Data",
       x = "Price",
       y = "City Center (km)") +
  theme_minimal()

#############################################
###############Classification################
#############################################

# Create a copy of the original data
data_classification <- data

# Convert the Cluster column to a factor
data_classification$Cluster <- as.factor(data_classification$Cluster)

# Split the data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(data_classification), nrow(data_classification) * 0.8) # 80% for training
train_data <- data_classification[train_indices, ]
test_data <- data_classification[-train_indices, ]

# Train a decision tree model
tree_model <- rpart(Cluster ~ Price + `Person Capacity` + `Cleanliness Rating` + Bedrooms + `City Center (km)` + `Attraction Index` + `Restraunt Index`, data = train_data, method = "class")

# Visualize the decision tree
rpart.plot(tree_model, box.palette = "auto", shadow.col = "gray", nn = TRUE)

# Make predictions on test data
predictions <- predict(tree_model, test_data, type = "class")

# Calculate accuracy
accuracy <- sum(predictions == test_data$Cluster) / nrow(test_data)
cat("Accuracy:", round(accuracy * 100, 2), "%")
######################################################
###############Forecasting############################
######################################################

# Create a copy of the original data
data_forecast <- data

# Convert satisfaction rating into categorical levels
data_forecast$Satisfaction_Level <- ifelse(data_forecast$`Guest Satisfaction` >= 97, "High Satisfaction",
                                          ifelse(data_forecast$`Guest Satisfaction` >= 80, "Satisfied", "Not Satisfied"))

# Convert the satisfaction level column to a factor
data_forecast$Satisfaction_Level <- as.factor(data_forecast$Satisfaction_Level)

# Split the data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(data_forecast), nrow(data_forecast) * 0.8) # 80% for training
train_data <- data_forecast[train_indices, ]
test_data <- data_forecast[-train_indices, ]

# Train a decision tree model
tree_model <- rpart(Satisfaction_Level ~ `Person Capacity` + `Cleanliness Rating` + `Restraunt Index`, 
                   data = train_data, method = "class")

# Visualize the decision tree
rpart.plot(tree_model, box.palette = "auto", shadow.col = "gray", nn = TRUE)

# Make predictions on test data
predictions <- predict(tree_model, test_data, type = "class")

# Calculate accuracy
accuracy <- sum(predictions == test_data$Satisfaction_Level) / nrow(test_data)
cat("Accuracy:", round(accuracy * 100, 2), "%")