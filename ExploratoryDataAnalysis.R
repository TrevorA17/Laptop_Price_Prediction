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

# Load the required packages
library(dplyr)

# Convert RAM_Size and Storage_Capacity to factors
laptop_data$RAM_Size <- as.factor(laptop_data$RAM_Size)
laptop_data$Storage_Capacity <- as.factor(laptop_data$Storage_Capacity)

# Perform ANOVA on Brand
anova_brand <- aov(Price ~ Brand, data = laptop_data)
print(summary(anova_brand))

# Post-hoc tests for pairwise comparisons
pairwise_tukey <- TukeyHSD(anova_brand)
print(pairwise_tukey)

library(ggplot2)
# Univariate Plots
# Histogram for Price
histogram_price <- ggplot(laptop_data, aes(x = Price)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 20) +
  labs(title = "Histogram of Laptop Prices", x = "Price", y = "Frequency")
print(histogram_price)

# Boxplot for Screen Size
boxplot_screen_size <- ggplot(laptop_data, aes(x = "", y = Screen_Size)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot of Screen Size", x = "", y = "Screen Size")
print(boxplot_screen_size)

# Multivariate Plot
# Scatter plot of Price vs. Processor Speed
scatter_price_processor <- ggplot(laptop_data, aes(x = Processor_Speed, y = Price)) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot of Price vs. Processor Speed", x = "Processor Speed", y = "Price")
print(scatter_price_processor)
