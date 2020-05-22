# Import the libraries
library(mlbench)  # For the dataset
library(mice)  # For handling missing values
library(caret)  # For training the model
library(e1071)  # For implementing the Naive Bayes algorithm

# Import the data
data(BreastCancer)

# Get the structure of the data
str(BreastCancer)

# Get the levels of the target 'Class'
levels(BreastCancer$Class)

# Get the summary of the data
summary(BreastCancer)

# Here, we don't need the 'Id' and 'Class' column
data_cleaned <- mice(BreastCancer[,2:10], print = FALSE)

# Add the 'Class' column to the cleaned data
BreastCancer <- cbind(BreastCancer[,11, drop = FALSE], mice::complete(data_cleaned, 1))

# Check the summary for the cleaned data.
summary(BreastCancer)

# For the sake of reproducibility, we set a random seed number
set.seed(365)

# Perform a random split
train_index <- createDataPartition(BreastCancer$Class, p = 0.7, list = FALSE)
train_set <- BreastCancer[train_index,]
test_set <- BreastCancer[-train_index,]

# Remove the target 'Class' in the test sets
test_set_predict <- test_set[2:10]

# Check the dimension of our train and test sets
dim(train_set)
dim(test_set_predict)

# Build and fit the model
model <- naiveBayes(Class ~ ., data = train_set)

# Make the predictions
preds <- predict(model, newdata = test_set_predict)

# Create a confusion matrix to evaluate the model
conf_matrix <- table(preds, test_set$Class)
confusionMatrix(conf_matrix)