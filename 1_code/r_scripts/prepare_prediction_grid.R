# 1. Setup ----
## 1.1 Load packages ----
library(terra) # For working with rasters
library(parallel) # For parallel processing
library(dismo) # For species distribution modeling
library(gbm) # for processing gbm models
library(sf)

aoi <- st_read("0_data/external/alberta/Alberta.shp")

## 1.2 Load prediction grid----

# Define file paths
file_paths <- c(
  "0_data/manual/predictor/gee/focal_image_500.tif",
  "0_data/manual/predictor/gee/focal_image_0.tif",
  "0_data/manual/predictor/scanfi/scanfi_focal_500.tif",
  "0_data/manual/predictor/nat_raster.tif"
)

# Load rasters into a list
pred_raster <- rast(lapply(file_paths, rast))

## Create latitdude and longitude raster----
### Create lat and lon values for each cell ----
pred_raster <- project(pred_raster, "EPSG:3978", method = "near")

aoi_tr <- st_transform(aoi, st_crs(pred_raster))

x <- init(pred_raster, 'x')
names(x)<-"x_3978"
x <- mask(x, aoi_tr)

y <- init(pred_raster, 'y')
names(y)<-"y_3978"
y <- mask(y, aoi_tr)

# Create a year raster where every cell equals 2022
year_raster <- rast(pred_raster$nrname)
values(year_raster) <- 2022
names(year_raster) <- "year"
year_raster <- mask(year_raster, aoi_tr)


# Create a survey effort raster where every cell equals 3
survey_effort_raster <- rast(pred_raster$nrname)
values(survey_effort_raster) <- 3
names(survey_effort_raster) <- "survey_effort"
survey_effort_raster <- mask(survey_effort_raster, aoi_tr)


pred_raster<-c(pred_raster, x, y, 
               survey_effort_raster, year_raster)
# clean environment
rm(list = setdiff(ls(), "pred_raster"))

writeRaster(pred_raster, 
            filename = "0_data/manual/predictor/pred_raster.tif", 
            overwrite = TRUE)



## Divide raster into smaller chunks ----
# Define the number of tiles you want in each dimension
n_rows <- 3
n_cols <- 2

# Get the extent of the raster
ext <- ext(pred_raster)

# Calculate the width and height of each tile
x_step <- (ext[2] - ext[1]) / n_cols
y_step <- (ext[4] - ext[3]) / n_rows

# Initialize a list to store the chunks
chunks <- list()

# Loop through rows and columns to create the chunks
for (i in 1:n_rows) {
  for (j in 1:n_cols) {
    # Define the extent for the current tile
    x_min <- ext[1] + (j-1) * x_step
    x_max <- ext[1] + j * x_step
    y_min <- ext[3] + (i-1) * y_step
    y_max <- ext[3] + i * y_step
    
    # Create the extent for the current chunk
    chunk_ext <- ext(x_min, x_max, y_min, y_max)
    
    # Crop the raster to the current extent
    chunk <- crop(pred_raster, chunk_ext)
    
    # Store the chunk in the list
    chunks[[length(chunks) + 1]] <- chunk
  }
}

# View the chunks
chunks

# Write each chunk to a .tif file
for (i in 1:length(chunks)) {
  file_name <- paste0("0_data/manual/predictor/pred_raster_chunks/", i, ".tif")
  writeRaster(chunks[[i]], filename = file_name, overwrite = TRUE)
}



## resample to 1000m
r <- pred_raster

# Define the target resolution (1000 meters)
target_res <- 5000

# Create a template raster with the desired resolution
template <- rast(ext(r), res = target_res, crs = crs(r))

# Identify numeric and categorical layers based on data type
# List of categorical layers
categorical_layers <- c(
  "nfiLandCover_mode_500", "nrname", "nsrname","survey_effort", "year"
)

# Separate numeric and categorical layers
numeric_layers <- setdiff(names(r), categorical_layers)

# Resample numeric layers using bilinear interpolation
resampled_numeric <- lapply(numeric_layers, function(layer) {
  resample(r[[layer]], template, method = "bilinear")
})

# Resample categorical layers using mode interpolation
resampled_categorical <- lapply(categorical_layers, function(layer) {
  resample(r[[layer]], template, method = "mode")
})

# Combine the resampled layers
resampled_layers <- c(resampled_numeric, resampled_categorical)
resampled_raster <- rast(resampled_layers)

# Save the resampled raster if needed
writeRaster(resampled_raster, filename = "resampled_raster.tif", 
            format = "GTiff", overwrite = TRUE)


