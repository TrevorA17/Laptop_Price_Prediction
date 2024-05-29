# Load the saved linear regression model
loaded_lm_model <- readRDS("./models/saved_lm_model.rds")

#* @apiTitle Laptop Price Prediction API
#* @apiDescription Used to predict laptop prices.

#* @param Brand Brand of the laptop
#* @param Processor_Speed Processor speed of the laptop
#* @param RAM_Size RAM size of the laptop
#* @param Storage_Capacity Storage capacity of the laptop
#* @param Screen_Size Screen size of the laptop
#* @param Weight Weight of the laptop

#* @post /predict_laptop_price
predict_laptop_price <- function(Brand, Processor_Speed, RAM_Size, 
                                 Storage_Capacity, Screen_Size, Weight) {
  
  # Create a data frame using the arguments
  to_be_predicted <- data.frame(
    Brand = as.factor(Brand),
    Processor_Speed = as.numeric(Processor_Speed),
    RAM_Size = as.factor(RAM_Size),
    Storage_Capacity = as.factor(Storage_Capacity),
    Screen_Size = as.numeric(Screen_Size),
    Weight = as.numeric(Weight)
  )
  
  # Use the loaded model to make predictions
  prediction <- predict(loaded_lm_model, newdata = to_be_predicted)
  
  # Return the prediction
  return(prediction)
}
