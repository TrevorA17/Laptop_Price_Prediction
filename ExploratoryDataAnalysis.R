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

# Display the structure of the dataset
str(laptop_data)

# View the first few rows of the dataset
head(laptop_data)

# View the dataset in a separate viewer window
View(laptop_data)
