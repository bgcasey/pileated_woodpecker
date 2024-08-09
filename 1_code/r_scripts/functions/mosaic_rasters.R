# ---
# title: "Mosaic rasters downloaded from earth engine"
# author: "Brendan Casey"
# created: "2024-08-05"
# description:
#   This script creates a mosaic of raster files downloaded from
#   Google Earth Engine. It includes steps to list directories,
#   load raster files, and generate a mean mosaic of these rasters.
# ---

# 1. Setup ----

## 1.1 Load packages ----
library(terra) # processing rasters
library(tidyverse) # data manipulation and visualization
library(sf) # handling spatial vector data

## 1.2 Import data ----
aoi <- st_read("0_data/external/alberta/Alberta.shp")

# 2. Mosaic rasters ----
# This section creates a mosaic of raster files from a list of .tif
# files in a directory.

## 2.1 List directories ----
path_name <- paste0(
  "/Users/brendancasey/Library/CloudStorage/",
  "GoogleDrive-bgcasey@ualberta.ca/My Drive/",
  "gee_exports/"
)

fl <- list.files(
  path_name,
  pattern = "focal_image_500.*\\.tif$",
  recursive = TRUE,
  full.names = TRUE
)

## 2.2 Load and mosaic rasters ----
rasters <- terra::sprc(fl)
m <- terra::mosaic(rasters, fun = "mean")

## 2.3 Crop to aoi ----
aoi_tr <- st_transform(aoi, st_crs(m))
m <- mask(m, aoi_tr)

## 2.3 Save as .tif ----
writeRaster(
  m,
  filename = "0_data/manual/predictor/gee/focal_image_500.tif",
  overwrite = TRUE
)
