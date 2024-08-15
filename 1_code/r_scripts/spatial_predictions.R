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
library(terra) # For working with rasters
library(parallel) # For parallel processing
library(dismo) # For species distribution modeling
library(gbm) # for processing gbm models
library(sf)

aoi <- st_read("0_data/external/alberta/Alberta.shp")

## 1.2 Load prediction grid----
pred_raster <- rast("0_data/manual/predictor/pred_raster.tif")

## 1.3 Load bootstrapped models ----
load("3_output/models/s2_vars_noOff/bootstrap_models.rData")

## 1.4 Load custom functions ----
source("1_code/r_scripts/functions/replace_outliers.R")
source("1_code/r_scripts/functions/make_spatial_pred.R")


# 2. Filter prediction grid ----
## 2.1 Keep layers that match model covariates ----
# Extract covariate names from the first model in
# bootstrap_models
model_covariates <- bootstrap_models$models$model_1$var.names
model_covariates <- brt_3$var.names

# Identify the names in model_covariates that are not in
# pred_raster
pred_raster_names <- names(pred_raster)
missing_in_pred_raster <- setdiff(
  model_covariates,
  pred_raster_names
)

# Subset the pred_raster to keep only the layers that match
# the model covariates
pred_raster <- subset(chunks[[6]], model_covariates)

## 2.2 Create cropped raster for testing ----
##  Dummy AOI 
# source("1_code/r_scripts/functions/utils.R")
# aoi <- create_buffered_area(lon = -113.578, lat = 55.266,
#                             buffer_dist = 10)
# aoi <- st_transform(aoi, crs(pred_raster))
# pred_raster <- crop(pred_raster, aoi)

# 3. Clean environment ----
# Clean environment to avoid memory issues.
# Remove all objects except pred_raster and bootstrap_models
rm(list = setdiff(ls(), c("pred_raster", "bootstrap_models")))

# Run garbage collection to free up memory
gc()

# 4. Generate spatial predictions ----
output_dir <- "3_output/spatial_predictions/s2_vars_noOff"

spatial_pred <- make_spatial_pred(
  bootstrap_models$models[6:100],
  pred_raster,
  n_cores = 3, # Reduce the number of cores
  output_dir = output_dir
)

# 5. Clean Prediction Rasters ----

## 5.1 Mask pixels outside of AOI ----
aoi <- st_transform(aoi, crs(spatial_pred))
spatial_pred <- mask(spatial_pred, aoi)

## 5.2 Mask permanent water pixels ----
LC <- rast("0_data/manual/predictor/focal_rasters/COPERNICUS_LC.tif")

# Create a mask using pixels from "LC" where values equal 220
mask <- ifel(LC$`water-permanent-coverfraction` != 100, 1, NA)

# Mask the "spatial_pred" raster using the created mask
spatial_pred <- mask(spatial_pred, mask)

## 5.3 Replace outlier pixels ----
n_sd_value <- 3

spatial_pred <- lapply(spatial_pred, function(raster) {
  replace_outliers(raster, n_sd = n_sd_value)
})

## 5.4 Save ----
writeRaster(spatial_pred$mean_raster,
            paste0(output_dir, "/mean_raster.tif"),
            overwrite = TRUE
)

writeRaster(spatial_pred$sd_raster,
            paste0(output_dir, "/sd_raster.tif"),
            overwrite = TRUE
)






###//////////////////////////////////////////////////////

# Testing ----

pred_raster<-rast("0_data/manual/predictor/pred_raster_chunks/pred_raster_chunks5.tif")

# Predict the log-odds (link) values
p_piwo <- predict(pred_raster, brt_3,
                  type = "link",
                  n.trees = brt_3$gbm.call$best.trees,
                  na.rm = TRUE)

# Define the offset (log of survey intensity of 12)
offset <- log(20)

# Add the offset to the predicted log-odds values
p_piwo_with_offset <- p_piwo + offset

# Define the logistic function to convert log-odds to probabilities
logistic_function <- function(log_odds) {
  exp(log_odds) / (1 + exp(log_odds))
}

# Apply the logistic function to the raster to get probabilities
probability_raster <- app(p_piwo_with_offset, logistic_function)

# Apply the logistic function to the raster to get probabilities
probability_raster <- app(prediction, logistic_function)



# Replace outlier values ----
# Load the raster
raster <- probability_raster





# Save the cleaned raster
writeRaster(raster_cleaned, "path/to/your/cleaned_raster.tif", overwrite = TRUE)
#



# Rescale the probabilities to a 0-1 range ----
raster <- raster_cleaned


raster_stats <- global(raster, c("min", "max"), na.rm = TRUE)
min_value <- raster_stats$min
max_value <- raster_stats$max

rescale_function <- function(value) {
  (value - min_value) / (max_value - min_value)
}


