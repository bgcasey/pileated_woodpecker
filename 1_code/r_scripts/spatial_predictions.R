# ---
# title: "5_spatial_predictions"
# author: "Brendan Casey"
# created: "2024-06-20"
# description: >
#   This script generates spatial predictions from a list of 
#   bootstrapped models and a prediction grid of model 
#   covariates. It outputs mean and standard deviation 
#   predictive rasters.
# ---

# 1. Setup ----
## 1.1 Load packages ----
library(terra)    # For working with rasters
library(parallel) # For parallel processing
library(dismo)    # For species distribution modeling

## 1.2 Load focal rasters ----
focal_image_500 <- rast(
  "0_data/manual/predictor/gee/focal_image_500.tif"
)
focal_image_0 <- rast(
  "0_data/manual/predictor/gee/focal_image_0.tif"
)
scanfi_focal_500 <- rast(
  "0_data/manual/predictor/scanfi/scanfi_focal_500.tif"
)

## 1.3 Load bootstrapped models ----
load("3_output/models/s2_vars_noOff/bootstrap_models.rData")

# 2. Prepare prediction grid ----
## 2.1 Combine focal rasters ----
pred_raster <- c(
  scanfi_focal_500, focal_image_500, focal_image_0
)

## 2.2 Keep layers that match model covariates ----
# Extract covariate names from the first model in 
# bootstrap_models
model_covariates <- bootstrap_models$models$model_1$var.names

# Identify the names in model_covariates that are not in 
# pred_raster
pred_raster_names <- names(pred_raster)
missing_in_pred_raster <- setdiff(model_covariates, 
                                  pred_raster_names)

# Subset the pred_raster to keep only the layers that match 
# the model covariates
pred_raster <- subset(pred_raster, model_covariates)

## 2.3 Create cropped raster for testing ----
### 2.3.1 Dummy AOI ----
source("1_code/r_scripts/functions/utils.R")
aoi <- create_buffered_area(lon = -113.578, lat = 55.266, 
                            buffer_dist = 10)
aoi <- st_transform(aoi, crs(pred_raster))
pred_raster <- crop(pred_raster, aoi)

# 3. Clean environment ----
# Clean environment to avoid memory issues.
# Remove all objects except pred_raster and bootstrap_models
rm(list = setdiff(ls(), c("pred_raster", "bootstrap_models")))

# Run garbage collection to free up memory
gc()

# 4. Generate spatial predictions ----
## 4.1 Define function to make spatial predictions ----

#' Generate Spatial Predictions from Models
#'
#' This function generates spatial predictions from a list of models 
#' and a prediction grid. It calculates and returns the mean and 
#' standard deviation of the predictions across all models.
#'
#' @param models_list list. A list of models to make predictions from.
#' @param prediction_grid SpatRaster. A SpatRaster object with 
#' covariates matching those used in the models.
#' @param n_cores integer. The number of cores to use for parallel 
#' processing.
#' @param output_dir character. The directory to save the prediction 
#' rasters. Default is "output".
#' @return list. A list containing two SpatRaster objects:
#' \item{mean_raster}{The mean of the predictions across all models.}
#' \item{sd_raster}{The standard deviation of the predictions across 
#' all models.}
#' 
#' @example 
#' # Example usage of the function
#' models_list <- list(model1, model2, model3) # Replace with actual 
#' models
#' prediction_grid <- rast("path_to_prediction_grid.tif") # Replace 
#' with actual SpatRaster
#' result <- make_spatial_pred(models_list, prediction_grid, 
#' n_cores = 4)
#' print(result$mean_raster)
#' print(result$sd_raster)
make_spatial_pred <- function(models_list, prediction_grid, n_cores, 
                              output_dir = "output") {
  # Step 1: Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Step 2: Define the function to run a single prediction
  run_prediction <- function(i) {
    tryCatch(
      {
        model <- models_list[[i]]
        
        # Generate predictions
        prediction <- predict(prediction_grid, model, 
                              type = "response",
                              n.trees = model$gbm.call$best.trees,
                              na.rm = TRUE)
        
        # Define file path
        file_path <- file.path(output_dir, 
                               paste("prediction", i, ".tif", 
                                     sep = ""))
        
        # Save prediction raster to TIFF file
        writeRaster(prediction, file_path, overwrite = TRUE)
        
        # Print completion message
        print(paste("Completed and saved raster", i))
        
        # Trigger garbage collection
        gc()
        
        return(TRUE)
      },
      error = function(e) {
        # Log the error message
        message(paste("Error in model", i, ":", e$message))
        return(FALSE)
      }
    )
  }
  
  # Step 3: Process all models in parallel
  results <- mclapply(seq_along(models_list), run_prediction, 
                      mc.cores = n_cores)
  
  # Step 4: Check for any errors in the results
  if (any(!unlist(results))) {
    message("Some models encountered errors.")
  }
  
  # Step 5: Calculate mean and standard deviation of the predictions
  prediction_files <- list.files(output_dir, 
                                 pattern = "prediction.*\\.tif$", 
                                 full.names = TRUE)
  prediction_stack <- rast(prediction_files)
  mean_raster <- mean(prediction_stack, na.rm = TRUE)
  sd_raster <- stdev(prediction_stack, na.rm = TRUE)
  
  return(list(mean_raster = mean_raster, sd_raster = sd_raster))
}

## 4.2 Apply function to bootstrapped BRTs ----
output_dir <- "3_output/spatial_predictions/s2_vars_noOff"

spatial_pred <- make_spatial_pred(
  bootstrap_models$models[],
  pred_raster,
  n_cores = 10, # Reduce the number of cores
  output_dir = paste0(output_dir, "/raw")
)

# 5. Create summary rasters ----

## 5.1 List prediction rasters ----
raster_files <- list.files(output_dir,
                           pattern = "\\.tif$",
                           full.names = TRUE
)

prediction_rasters <- lapply(raster_files, rast)

# Combine predictions into SpatRaster
prediction_stack <- rast(prediction_rasters)

# Trigger garbage collection
gc()

## 5.2 Calculate mean raster ----
mean_raster <- app(prediction_stack, mean, na.rm = TRUE)
writeRaster(mean_raster, 
            paste0(output_dir, "mean_raster.tif"), 
            overwrite = TRUE)

## 5.3 Calculate sd raster ----
cv_raster <- app(prediction_stack, sd, na.rm = TRUE)
writeRaster(sd_raster, 
            paste0(output_dir, "mean_raster.tif"), 
            overwrite = TRUE)

