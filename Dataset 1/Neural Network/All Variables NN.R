# Load necessary libraries
library(tidyverse)
library(caret)
library(neuralnet)

# Specify the path to your dataset
file_path <- "/Users/justinong/Library/Mobile Documents/com~apple~CloudDocs/NTUY2/SEM2/BC2407/group proj/ACTUAL PROJ/Dataset/Phishing_Legitimate_full.csv"

# Load the data
data <- read.csv(file_path)

# Remove the 'id' column
data <- select(data, -id,-HttpsInHostname)

# Convert binary variables to numeric. Assuming all except the last are predictors and last is the target.
categorical_vars <- names(data)[1:(ncol(data)-1)] # Adjust based on your dataset structure
data[categorical_vars] <- lapply(data[categorical_vars], function(x) as.numeric(as.character(x)))

# Ensure the CLASS_LABEL variable (assuming it's the last column) is converted to numeric if it's not already.
data$CLASS_LABEL <- as.numeric(as.character(data$CLASS_LABEL))

# Split the data into training and test sets
set.seed(123) # For reproducibility
index <- createDataPartition(data$CLASS_LABEL, p = 0.8, list = FALSE)
train_data <- data[index, ]
test_data <- data[-index, ]

# Define the neural network formula
# Exclude the CLASS_LABEL from the predictors
input_vars <- setdiff(names(train_data), "CLASS_LABEL")
nn_formula <- as.formula(paste("CLASS_LABEL ~", paste(input_vars, collapse = " + ")))

# Train the neural network
nn_model <- neuralnet(nn_formula, 
                      data = train_data, 
                      hidden = c(25), # This can be adjusted based on your needs
                      threshold = 0.01,
                      rep = 1,
                      linear.output = FALSE,
                      lifesign = 'minimal')

# Predictions on the test set
test_pred <- compute(nn_model, test_data[,1:(ncol(test_data)-1)])
predicted_classes <- ifelse(test_pred$net.result > 0.5, 1, 0)

# Evaluation
actual_classes <- test_data$CLASS_LABEL
confusionMatrix <- table(Predicted = predicted_classes, Actual = actual_classes)
accuracy <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
print(confusionMatrix)
print(paste("Accuracy:", accuracy))

#Plot Model

plot(nn_model)
