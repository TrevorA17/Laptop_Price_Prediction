# Load the required packages
library(dplyr)
library(psych)

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

# Define a function to calculate mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# Measures of Frequency
# Frequency table for Brand
brand_freq <- table(laptop_data$Brand)
print("Frequency table for Brand:")
print(brand_freq)

# Measures of Central Tendency
# Mean, Median, and Mode for Price
price_mean <- mean(laptop_data$Price)
price_median <- median(laptop_data$Price)
price_mode <- Mode(laptop_data$Price)
print("Measures of Central Tendency for Price:")
print(paste("Mean:", price_mean))
print(paste("Median:", price_median))
print(paste("Mode:", price_mode))

# Measures of Distribution
# Standard Deviation and Range for Screen Size
screen_sd <- sd(laptop_data$Screen_Size)
screen_range <- range(laptop_data$Screen_Size)
print("Measures of Distribution for Screen Size:")
print(paste("Standard Deviation:", screen_sd))
print(paste("Range:", screen_range))

# Measures of Relationship
# Correlation between Processor Speed and Price
processor_price_corr <- cor(laptop_data$Processor_Speed, laptop_data$Price)
print("Correlation between Processor Speed and Price:")
print(processor_price_corr)

# Correlation between RAM Size and Price
# Since RAM Size is currently a factor, we need to convert it to numeric first
laptop_data$RAM_Size <- as.numeric(as.character(laptop_data$RAM_Size))
ram_price_corr <- cor(laptop_data$RAM_Size, laptop_data$Price)
print("Correlation between RAM Size and Price:")
print(ram_price_corr)
