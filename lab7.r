########################################Q1###########################################
# Load required libraries
library(tree)

# Load the data
df <- read.csv("affairs.csv")

# Convert if_affair to a factor variable
df$if_affair <- as.factor(df$if_affair)

# Split the data into training and testing sets
set.seed(123) # Set a seed for reproducibility
train_indices <- sample(1:nrow(df), nrow(df)*0.7) # 70% for training
train_data <- df[train_indices, ] # Subset the training data
test_data <- df[-train_indices, ] # Subset the testing data

# Fit a decision tree model
tree_model <- tree(if_affair ~ age + yearsmarried + religiousness + rating, data = train_data) # Fit the decision tree model
summary(tree_model) # Print the summary of the tree model

# Plot the decision tree
plot(tree_model) # Plot the decision tree
text(tree_model, pretty = 0) # Add text labels to the plot

# Make predictions on the test data using the decision tree model
tree_pred <- predict(tree_model, newdata = test_data, type = "class") # Predict class labels (0 or 1)

# Calculate accuracy of the decision tree model
tree_acc <- sum(tree_pred == test_data$if_affair) / nrow(test_data) # Calculate accuracy
cat("Accuracy of Decision Tree:", tree_acc, "\n") # Print accuracy

########################################Q1###########################################
########################################Q2###########################################

# Find the most useful attribute in prediction from the Decision Tree model
var_importance <- tree_model$splits # Extract variable importance scores
most_useful_attribute <- names(var_importance)[which.max(var_importance)] # Find variable with highest importance
cat("Most Useful Attribute in Prediction:", most_useful_attribute, "\n") # Print the most useful attribute

# Create a confusion matrix for the Decision Tree model's predictions on the test data
confusion <- table(tree_pred, test_data$if_affair)

# Calculate Precision and Recall for the "Yes" class (positive class)
precision <- confusion[2, 2] / (confusion[2, 2] + confusion[1, 2]) # True Positive / (True Positive + False Positive)
recall <- confusion[2, 2] / (confusion[2, 2] + confusion[2, 1]) # True Positive / (True Positive + False Negative)

cat("Precision (for 'Yes' class):", precision, "\n") # Print Precision
cat("Recall (for 'Yes' class):", recall, "\n") # Print Recall
########################################Q2###########################################

########################################Q3###########################################
# Load required libraries
library(tree)
library(pROC)

# Load the data
df <- read.csv("affairs.csv")

# Convert if_affair to a factor variable
df$if_affair <- as.factor(df$if_affair)

# Split the data into training and testing sets
set.seed(123) # Set a seed for reproducibility
train_indices <- sample(1:nrow(df), nrow(df)*0.7) # 70% for training
train_data <- df[train_indices, ] # Subset the training data
test_data <- df[-train_indices, ] # Subset the testing data

# Fit a logistic regression model using the tree package
logit_model <- tree(if_affair ~ age + yearsmarried + religiousness + rating, data = train_data, 
                    control = tree.control(nobs = nrow(train_data), minsize = 1)) # Fit the logistic regression model
logit_probs <- predict(logit_model, newdata = test_data, type = "vector") # Predict probabilities
logit_probs <- logit_probs[, 2] # Extract predicted probabilities for the positive class ("Yes")

# Calculate AUC-ROC of the logistic regression model
logit_roc <- roc(response = test_data$if_affair, predictor = logit_probs)

# Plot ROC curve
plot(logit_roc, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate",
     col = "blue", lwd = 2, print.auc = TRUE, auc.polygon = TRUE, max.auc.polygon = TRUE)
legend("bottomright", legend = paste("AUC =", round(auc(logit_roc), 2)), col = "blue", lwd = 2, bty = "n")


########################################Q3###########################################

########################################Q4###########################################
TEXT
########################################Q4###########################################