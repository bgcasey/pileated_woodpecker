# ---
# title: "5_spatial_predictions"
# author: "Brendan Casey"
# created: "2024-06-20"
# description: >
#   This script generates spatial predictions from a list of 
#   bootstrapped models and prediction grid of model covariates. It 
#   outputs mean and standard deviation predictive rasters.
# ---


# 1. Setup ----
## Load packages ----
library(terra)  # For working with rasters

## 1.2 Load prediction_grid ----
prediction_grid <- rast(
  "0_data/manual/predictor/raster_mosaics/cov_all.tif")

## 1.3 Load bootstrapped models ----
load("3_output/model_results/bootstrap_models.rData") 

# 2. Generate spatial predictions ----
# 2.1 Define function to make spatial predictions ----
# This function generates spatial predictions from a list of models
# and a prediction grid. It calculates and returns the mean and 
# standard deviation of the predictions across all models.
#
# Args:
#   models_list: A list of models to make predictions from.
#   prediction_grid: A SpatRaster object with covariates matching 
#                    those used in the models.
#
# Returns:
#   A list containing two SpatRaster objects:
#     - mean_raster: The mean of the predictions across all models.
#     - sd_raster: The standard deviation of the predictions across
#                  all models.

make_spatial_pred <- function(models_list, prediction_grid) {
  # Initialize list for prediction rasters
  prediction_rasters <- list()
  
  # Loop through each model
  for (i in seq_along(models_list)) {
    model <- models_list[[i]]
    
    # Generate predictions
    prediction <- predict(prediction_grid, model, 
                          type = "response")
    
    # Store prediction raster
    prediction_rasters[[paste("prediction", i, sep = "_")]] <- 
      prediction
  }
  
  # Combine predictions into SpatRaster
  prediction_stack <- rast(prediction_rasters)
  
  # Calculate mean and sd across layers
  mean_raster <- app(prediction_stack, mean, na.rm = TRUE)
  sd_raster <- app(prediction_stack, sd, na.rm = TRUE)
  
  # Return mean and sd rasters
  return(list(mean_raster = mean_raster, sd_raster = sd_raster))
}

# Example usage:
# spatial_predictions <- make_spatial_pred(bootstrap_models$models, 
#                                          prediction_grid
#                                          )

# 2.2 Apply function ----
spatial_pred <- make_spatial_pred(bootstrap_models$models, 
                                  prediction_grid
                                  )

## 2.3 Save spatial predictions ---- 
writeRaster(spatial_pred$mean_raster, 
            "3_output/spatial_predictions/mean_prediction.tif", 
            overwrite=TRUE)

writeRaster(spatial_pred$sd_raster,
            "3_output/spatial_predictions/sd_prediction.tif", 
            overwrite=TRUE)

