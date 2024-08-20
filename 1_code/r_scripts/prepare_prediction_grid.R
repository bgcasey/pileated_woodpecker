# ---
# title: "Prepare prediction grid"
# author: "Brendan Casey"
# created: "2024-08-01"
# description:
#   "This script prepares a prediction grid for gbm model predictions.
#   It includes steps to load and process raster covariates,
#   create additional raster layers for fixed/nonraster covariates,
#   and to divide the final prediction grid into smaller chunks.
#   The final output is a .tif file corresponding to an area of
#   interest and a chunked version saved as seperate .tif files."
# ---

# 1. Setup ----

## 1.1 Load packages ----
# Load necessary packages
library(terra) # For working with rasters
library(sf) # For handling spatial vector data

## 1.2 Import data ----
# Load the area of interest (AOI) shapefile
aoi <- st_read("0_data/external/alberta/Alberta.shp")

# Define file paths for the prediction grid rasters
file_paths <- c(
  "0_data/manual/predictor/gee/focal_image_500.tif",
  "0_data/manual/predictor/gee/focal_image_0.tif",
  "0_data/manual/predictor/scanfi/scanfi_focal_500.tif",
  "0_data/manual/predictor/nat_raster.tif"
)

# list the rasters
pred_raster <- rast(lapply(file_paths, rast))

# 2. Create year and survey effort layers ----
# Create a year raster where every cell equals 2022
year_raster <- rast(pred_raster$nrname)
values(year_raster) <- 2022
names(year_raster) <- "year"
aoi <- st_transform(aoi, crs(pred_raster[[1]]))
year_raster <- mask(year_raster, aoi)

# Create a survey effort raster where every cell equals 3
survey_effort_raster <- rast(pred_raster$nrname)
values(survey_effort_raster) <- 3
names(survey_effort_raster) <- "survey_effort"
survey_effort_raster <- mask(survey_effort_raster, aoi)

# 3. Create single prediction grid ----
# Combine all rasters
pred_raster <- c(pred_raster, survey_effort_raster, year_raster)

# Clean environment
rm(list = setdiff(ls(), "pred_raster"))
gc()

# Save the combined raster
writeRaster(pred_raster,
  filename = "0_data/manual/predictor/pred_raster.tif",
  overwrite = TRUE
)

# 4. Divide Raster into Smaller Chunks ----
# This section divides the raster into smaller chunks for easier
# processing and analysis.

## Load custom functions ----
source("1_code/r_scripts/functions/split_raster.R")

## Set number of tiles
n_rows <- 3
n_cols <- 2

## Split raster
chunks <- split_raster(pred_raster, n_rows, n_cols)

## Save
for (i in 1:length(chunks)) {
  file_name <- paste0(
    "0_data/manual/predictor/pred_raster_chunks/",
    i, ".tif"
  )
  writeRaster(chunks[[i]], filename = file_name, overwrite = TRUE)
}
