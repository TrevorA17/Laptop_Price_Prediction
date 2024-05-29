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

# Check for NA values in the dataset
na_count <- sum(is.na(laptop_data))
print(paste("Number of NA values in the dataset:", na_count))

# Check for NULL values in the dataset
null_count <- sum(is.null(laptop_data))
print(paste("Number of NULL values in the dataset:", null_count))

# Check for missing values in specific columns
missing_values_brand <- sum(is.na(laptop_data$Brand))
print(paste("Number of missing values in Brand column:", missing_values_brand))

missing_values_price <- sum(is.na(laptop_data$Price))
print(paste("Number of missing values in Price column:", missing_values_price))
