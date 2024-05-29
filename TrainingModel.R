# Load the required packages
library(caret)

# Load dataset
laptop_data <- read.csv("data/Laptop_price.csv", colClasses = c(
  Brand = "factor",
  Processor_Speed = "numeric",
  RAM_Size = "factor",
  Storage_Capacity = "factor",
  Screen_Size = "numeric",
  Weight = "numeric",
  Price = "numeric"
))

# Data Splitting
set.seed(123) # for reproducibility
train_index <- createDataPartition(laptop_data$Price, p = 0.8, list = FALSE)
train_data <- laptop_data[train_index, ]
test_data <- laptop_data[-train_index, ]

dim(train_data)
dim(test_data)

# Bootstrapping
bootstrapped_means <- replicate(1000, mean(sample(train_data$Price, replace = TRUE)))
bootstrapped_sd <- sd(bootstrapped_means)
print(paste("Bootstrapped Standard Deviation of means:", bootstrapped_sd))

# Cross-validation
set.seed(123) # for reproducibility
fit_control <- trainControl(method = "cv", number = 5)
model <- train(Price ~ ., data = train_data, method = "lm", trControl = fit_control)
print(summary(model))
print(model)

# Load the required packages
library(caret)
library(rpart)
library(randomForest)

# Data Splitting
set.seed(123) # for reproducibility
train_index <- createDataPartition(laptop_data$Price, p = 0.8, list = FALSE)
train_data <- laptop_data[train_index, ]
test_data <- laptop_data[-train_index, ]

# Linear Regression (lm)
lm_model <- lm(Price ~ ., data = train_data)
print(summary(lm_model))

# Decision Tree
tree_model <- rpart(Price ~ ., data = train_data)
print(tree_model)

