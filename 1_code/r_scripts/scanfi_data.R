# ---
# title: "Process SCANFI data"
# author: "Brendan Casey"
# created: "2024-04-23"
# description: 
#   "Code to process multi-year SCANFI data. It includes steps to 
#   import and combine raster files, extract raster values to point 
#   locations, and generate focal statostics. The final output is a 
#   multiband raster focal raster for 2020 and a dataframe of 
#   SCANFRI data summarized to point count locations."
# ---

# 1. Setup ----
# This section sets up the environment for the analysis.
# It includes steps to load packages, custom functions, study 
# area, and point locations.

## 1.1 Load packages ----
library(terra) # for raster data manipulation
library(sf)    # for spatial data manipulation

## 1.2 Load custom functions ----
source("1_code/r_scripts/functions/utils.R") 

## 1.3 Load study area ----
aoi <- st_read("0_data/external/alberta/Alberta.shp")

## 1.4 Load point locations ----
load("0_data/manual/spatial/ss_xy_4326.rData")

## 1.5 Dummy data for testing ----
# This subsection creates dummy data for testing purposes.
# It uses predefined coordinates and random generation for points.
# It includes steps to create a dummy area of interest and random 
# points. The subsection produces dummy spatial data for testing.

### 1.5.1 Dummy aoi ----
# aoi <- create_buffered_area(lon = -113.578, lat = 55.266, 
#                             buffer_dist = 10)

### 1.5.2 Dummy points ----
# Define the bounding box coordinates
# xmin <- -113.77187
# ymin <- 55.15541
# xmax <- -113.38443
# ymax <- 55.37665

# Generate 10 random points within the bounding box
# set.seed(123)  # For reproducibility
# points_df <- data.frame(
#   loc_id = 1:10,  # Add an id column
#   x = runif(10, xmin, xmax),
#   y = runif(10, ymin, ymax)
# )

# Convert the data.frame to an sf object
# xy_locations <- st_as_sf(points_df, coords = c("x", "y"), crs = 4326)

# 2. Create multiband raster ----
# This section creates a single multiband raster from multiple raster 
# files. It includes steps to list raster files, combine them, and 
# rename layers. The section produces a multiband raster ready for 
# further analysis.

## 2.1 List raster files ----
fl <- list.files(path = "/Users/brendancasey/Desktop/scanfi", 
                 pattern = '*.tif$', recursive = TRUE, 
                 full.names = TRUE)

## 2.2 Combine into a single multiband raster ----
scanfi <- rast(lapply(fl, rast))

## 2.3 Rename layers ----
new_names <- names(scanfi) %>%
  gsub("SCANFI_|_SW_2020_v1|att_|sps_|_v0|_S", "", .)
names(scanfi) <- new_names

# 3. Extract SCANFI data to point locations ----
# This section extracts raster values to point locations.
# The section uses the SCANFI multiband raster and point count 
# locations. It includes steps to filter raster layers, extract values 
# using different summary functions, and combine the extracted data. 
# The section produces a dataframe of raster values summarized to 
# point locations.

## 3.1 SCANFI: continuous layers ----
filtered_layers <- scanfi[[!grepl("nfiLandCover", names(scanfi))]]
xy_c_mean_500 <- extract_raster_values(xy_locations = ss_xy_4326,
                                       raster = filtered_layers,
                                       buffer = 500,
                                       fun = mean)

## 3.2 SCANFI: discrete layers ----
filtered_layers <- scanfi[[grepl("nfiLandCover", names(scanfi))]]

# Define the mode function
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

xy_lc_mode_500 <- extract_raster_values(xy_locations = ss_xy_4326,
                                        raster = filtered_layers,
                                        buffer = 500,
                                        fun = mode)

xy_lc_first <- extract_raster_values(xy_locations = ss_xy_4326,
                                     raster = filtered_layers)

## 3.3 Combine extracted data ----
xy_scanfi <- xy_c_mean_500 %>%
  left_join(xy_lc_mode_500) %>%
  left_join(xy_lc_first) %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .))) %>%
  select(1, order(names(.)[2:ncol(.)]) + 1)

save(xy_scanfi, file = "0_data/manual/predictor/xy_scanfi.rData")

# 4. Create raster for predictions ----
# This section creates a raster for model predictions using focal 
# statistics. The section uses the SCANFI multiband raster and an sf 
# object of the area of interest. It includes steps to filter raster 
# layers, clip the raster to the study area, and calculate focal 
# statistics. This section produces a 2020 focal raster.

## 4.1 Focal statistics setup ----

### 4.1.1 Filter to only 2020 layers ----
scanfi_2020 <- scanfi[[grepl("2020", names(scanfi))]]

### 4.1.2 Clip to study area ----
aoi_tr <- st_transform(aoi, st_crs(scanfi_2020))
scanfi_2020_ab <- crop(scanfi_2020, aoi_tr)
# Mask the raster to remove areas outside the polygon
scanfi_2020_ab <- mask(scanfi_2020_ab, aoi_tr)

writeRaster(scanfi_2020_ab, 
            file = "0_data/manual/predictor/scanfi_2020_ab.tif",
            overwrite = TRUE)

### 4.1.3 Clean global environment and load Alberta SCANFI
# Clear the global environment except for functions
rm(list = setdiff(ls(), lsf.str()))
# Trigger garbage collection
gc()

scanfi_2020_ab <- rast("0_data/manual/predictor/scanfi_2020_ab.tif")

### 4.1.4 Subset rasters ----

#### 4.1.4.1 Numeric rasters ----
scanfi_2020_ab_num <- scanfi_2020_ab[[!grepl("nfiLandCover", 
                                             names(scanfi_2020_ab))]]

#### 4.1.4.2 Categorical rasters ----
scanfi_2020_ab_cat <- scanfi_2020_ab[[grepl("nfiLandCover", 
                                            names(scanfi_2020_ab))]]

# Clean global environment
rm(scanfi_2020_ab)
gc()

## 4.2 Focal statistics ----
f_500_mean <- calculate_focal_stat(raster_input = scanfi_2020_ab_num, 
                                   window_size_meters = 500, 
                                   fun = "mean")

writeRaster(f_500_mean, 
            file = "2_pipeline/tmp/f_500_mean.tif",
            overwrite = TRUE)

# Clean global environment
rm(scanfi_2020_ab)
gc()

f_500_mode <- calculate_focal_stat(raster_input = scanfi_2020_ab_cat, 
                                   window_size_meters = 500, 
                                   fun = "modal")

## 4.3 Stack all ----
scanfi_2020_focal <- c(f_500_mean, f_500_mode)

## 4.4 Sort layers alphabetically ----
layer_names <- names(scanfi_2020_focal)
layer_names_sorted <- sort(layer_names)
scanfi_2020_focal <- scanfi_2020_focal[[layer_names_sorted]]

writeRaster(scanfi_2020_focal, 
            file = "0_data/manual/predictor/scanfi_2020_focal.tif",
            overwrite = TRUE)

