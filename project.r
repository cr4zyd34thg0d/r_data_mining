######Data Preprocessing (Descriptive Method - Clustering):
# Load required libraries
library(dplyr)
library(tidyr)
library(readr)

# Load the data
data <- read_csv("europearibnbinfo.csv")

# Perform data cleaning - handle missing values
data <- na.omit(data)

# Perform data cleaning - correct data types if needed

# Perform data cleaning - handle outliers or inconsistencies if needed

# Perform data cleaning - other data preprocessing steps if needed


######Data Exploration and Visualization (Descriptive Method - Clustering):
# Conduct exploratory data analysis (EDA)
summary(data)  # Summary statistics of the data
head(data)     # Preview of the first few rows of data
str(data)      # Structure of the data

# Visualize the data using various charts and graphs
# For example, create bar charts, scatter plots, or heatmaps to explore patterns and distribution in the data



######Clustering (Descriptive Method - Clustering):
# Use clustering techniques, such as k-means or hierarchical clustering, to group the data
# Based on location, room type, host versus super host, and other relevant variables

# Example using k-means clustering
library(cluster)

# Select relevant variables for clustering
cluster_vars <- data %>%
  select(location, room_type, host_vs_superhost)

# Scale the variables if needed
cluster_vars_scaled <- scale(cluster_vars)

# Perform k-means clustering
k <- 3  # Number of clusters
kmeans_model <- kmeans(cluster_vars_scaled, centers = k, nstart = 10)
cluster_assignments <- kmeans_model$cluster

# Add cluster assignments to the original data
data$cluster <- cluster_assignments



######Feature Engineering (Predictive Method - Classification):
# Create relevant features or variables that can be used in the predictive models
# For example, calculate average ratings, total number of reviews, or any other relevant metrics

# Example of creating average rating feature
data <- data %>%
  group_by(cluster) %>%
  mutate(avg_rating = mean(rating, na.rm = TRUE))



######Classification (Predictive Method - Classification):
# Build classification models, such as logistic regression, decision trees, or random forests
# To predict the likelihood of high satisfaction ratings based on the clusters created in the previous step

# Example using logistic regression
library(glmnet)

# Select relevant variables for the classification model
classification_vars <- data %>%
  select(avg_rating, location, room_type, host_vs_superhost)

# Prepare the data for modeling
classification_vars <- as.data.frame(classification_vars)
classification_vars <- mutate_all(classification_vars, as.factor)

# Split the data into training and testing sets
set.seed(123)
train_index <- sample(nrow(classification_vars), nrow(classification_vars) * 0.8)
train_data <- classification_vars[train_index, ]
test_data <- classification_vars[-train_index, ]

# Build the logistic regression model
logit_model <- glmnet(avg_rating ~ location + room_type + host_vs_superhost,
                      data = train_data,
                      family = "binomial")

# Predict the likelihood of high satisfaction ratings on the test data
predictions <- predict(logit_model, newx = test_data, type = "response")
predicted_classes <- ifelse(predictions > 0.5, "High_Satisfaction", "Not_High_Satisfaction")

# Evaluate the performance of