# Saving the Linear Regression model
saveRDS(lm_model, "./models/saved_lm_model.rds")

# Load the saved model
loaded_lm_model <- readRDS("./models/saved_lm_model.rds")

# Prepare new data for prediction
new_data <- data.frame(
  Brand = factor("Asus"),
  Processor_Speed = as.numeric(3.0),
  RAM_Size = factor("8"),
  Storage_Capacity = factor("512"),
  Screen_Size = as.numeric(15.6),
  Weight = as.numeric(3.5)
)

# Use the loaded model to make predictions
predictions_loaded_model <- predict(loaded_lm_model, newdata = new_data)

# Print predictions
print(predictions_loaded_model)
