# Step 1: Load the data set and subset columns
carseats <- read.csv("carseats.csv")
carseats <- subset(carseats, select = c("Advertising", "Price", "ShelveLoc", "Age", "Sales"))

# Step 2: Convert categorical variables to factors
carseats$Advertising <- factor(carseats$Advertising, levels = c("Low", "High"))
carseats$Price <- factor(carseats$Price, levels = c("Low", "High"))
carseats$ShelveLoc <- factor(carseats$ShelveLoc, levels = c("Bad", "Medium", "Good"))
carseats$Age <- factor(carseats$Age, levels = c("Young", "Old"))

# Step 3: Split the data set into training and testing sets
set.seed(123) # for reproducibility
train_index <- sample(nrow(carseats), 0.75 * nrow(carseats))
train_data <- carseats[train_index, ]
test_data <- carseats[-train_index, ]

# Step 4: Train the decision tree model
library(rpart)
tree_model <- rpart(Sales ~ ., data = train_data, method = "class")

# Step 5: Plot the decision tree and extract the two most important variables
library(rpart.plot)
rpart.plot(tree_model, main = "Decision Tree for Car Seat Sales")

# Extract variable importance
var_imp <- varImp(tree_model)
head(var_imp)