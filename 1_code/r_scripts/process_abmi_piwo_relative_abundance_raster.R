# ---
# title: "Process ABMI PIWO Relative Abundance Raster"
# author: "Brendan Casey"
# created: "2024-08-02"
# description: >
#   This script scales the relative abundance values of the current
#   (2021) band of the ABMI PIWO raster. It exports a scaled raster
#   file.
# ---

# 1. Setup ----

## 1.1 Load packages ----
library(terra) # for handling raster data

## 1.2 Import data ----
raster <- rast("0_data/external/dryocopus_pileatus.tif")

# 2. Extract and Save Current Band ----
# This section extracts the "Current" band from the raster.

## 2.1 Extract "Current" band ----
current_band <- raster[["Current"]]

## 2.2 Save "Current" band ----
writeRaster(current_band, "3_output/rasters/abmi_piwo_2021.tif")

# 3. Normalize and Scale Values ----
# This section normalizes the values of the "Current" band to the
# range [0, 1] and then scales them to the range [0, 100].

## 3.1 Normalize values to [0, 1] ----
min_value <- min(values(current_band), na.rm = TRUE)
max_value <- max(values(current_band), na.rm = TRUE)
normalized_band <- (current_band - min_value) / (max_value - min_value)

## 3.2 Scale normalized values to [0, 100] ----
scaled_band <- normalized_band * 100

## 3.3 Print the scaled_band object to verify ----
print(scaled_band)

## 3.4 Save scaled raster ----
writeRaster(scaled_band, "3_output/rasters/abmi_piwo_scaled_2021.tif")
