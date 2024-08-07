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
library(sf) # for spatial data manipulation
# library(devtools) # get functions from github repo

# Load the function from the GitHub URL
## 1.2 Load custom functions ----
source("1_code/r_scripts/functions/utils.R")
# source_url(paste0(
#   "https://raw.githubusercontent.com/",
#   "bgcasey/r_functions/main/calculate_focal_statistics.R"
# ))

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
fl <- list.files(
  path = "/Users/brendancasey/Desktop/scanfi",
  pattern = "*.tif$", recursive = TRUE,
  full.names = TRUE
)

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
xy_c_mean_500 <- extract_raster_values(
  xy_locations = ss_xy_4326,
  raster = filtered_layers,
  buffer = 500,
  fun = mean
)

## 3.2 SCANFI: discrete layers ----
filtered_layers <- scanfi[[grepl("nfiLandCover", names(scanfi))]]

# Define the mode function
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

xy_lc_mode_500 <- extract_raster_values(
  xy_locations = ss_xy_4326,
  raster = filtered_layers,
  buffer = 500,
  fun = mode
)

xy_lc_first <- extract_raster_values(
  xy_locations = ss_xy_4326,
  raster = filtered_layers
)

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

writeRaster(scanfi_2020_ab,
  file = "0_data/manual/predictor/scanfi/scanfi_2020_ab.tif",
  overwrite = TRUE
)

### 4.1.3 Clean global environment and load Alberta SCANFI
# Clear the global environment except for functions
rm(list = setdiff(ls(), lsf.str()))
# Trigger garbage collection
gc()

scanfi_2020_ab <- rast("0_data/manual/predictor/scanfi/scanfi_2020_ab.tif")

## 4.2 Focal statistics ----

### 4.2.1 Percent broadleaf ----
prcB <- scanfi_2020_ab$prcB_2020
names(prcB) <- "prcB"

prcB_mean_500 <- calculate_focal_stat(
  raster_input = prcB,
  window_size_meters = 500,
  fun = "mean"
)

writeRaster(prcB_mean_500,
  file = "0_data/manual/predictor/scanfi/prcB_mean_500.tif",
  overwrite = TRUE
)

rm(prcB_mean_500)
gc()

### 4.2.2 Height ----
height <- scanfi_2020_ab$height_2020
names(height) <- "height"

height_mean_500 <- calculate_focal_stat(
  raster_input = height_2020,
  window_size_meters = 500,
  fun = "mean"
)

writeRaster(height_mean_500,
  file = "0_data/manual/predictor/scanfi/height_mean_500.tif",
  overwrite = TRUE
)

rm(height_mean_500)
rm(height_2020)
gc()

### 4.2.3 Biomass ----
biomass <- scanfi_2020_ab$biomass_2020
names(biomass) <- "biomass"

biomass_mean_500 <- calculate_focal_stat(
  raster_input = biomass,
  window_size_meters = 500,
  fun = "mean"
)

writeRaster(biomass_mean_500,
  file = "0_data/manual/predictor/scanfi/biomass_mean_500.tif",
  overwrite = TRUE
)

rm(biomass_mean_500)
rm(biomass)
gc()

### 4.2.4 Closure ----
closure <- scanfi_2020_ab$closure_2020
names(closure) <- "closure"

closure_mean_500 <- calculate_focal_stat(
  raster_input = closure,
  window_size_meters = 500,
  fun = "mean"
)

writeRaster(closure_mean_500,
  file = "0_data/manual/predictor/scanfi/closure_mean_500.tif",
  overwrite = TRUE
)

rm(closure_mean_500)
rm(closure_2020)
gc()

### 4.2.5 NFI Land Cover ----
nfiLandCover <- scanfi_2020_ab$nfiLandCover_2020
names(nfiLandCover) <- "nfiLandCover"

nfiLandCover_mode_500 <- calculate_focal_stat(
  raster_input = nfiLandCover,
  window_size_meters = 500,
  fun = "modal"
)

writeRaster(nfiLandCover_mode_500,
  file = "0_data/manual/predictor/scanfi/nfiLandCover_mode_500.tif",
  overwrite = TRUE
)

rm(nfiLandCover_mode_500)
rm(nfiLandCover)
gc()


















## 4.3 Stack all ----
nfiLandCover_mode_500 <- rast("2_pipeline/tmp/f_500_mode.tif")
closure_mean_500 <- rast("closure_2020_500_mean.tif")
biomass_mean_500 <- rast("biomass_2020_500_mean.tif")
height_mean_500 <- rast("height_2020_500_mean.tif")
prcB_mean_500 <- rast("2_pipeline/tmp/prcB_2020_500_mean.tif")
scanfi_2020_focal <- c(f_500_mode, closure_mean_500, biomass_mean_500, height_mean_500, prcB_mean_500)


# Rename the layers to match the object names
names(closure_mean_500) <- "closure_mean_500"
names(biomass_mean_500) <- "biomass_mean_500"
names(height_mean_500) <- "height_mean_500"
names(prcB_mean_500) <- "prcB_mean_500"
names(nfiLandCover_mode_500) <- "nfiLandCover_mode_500"


nfiLandCover_mode_500 <- f_500_mode
# Print the new names to verify

names(nfiLandCover_mode_500) <- "nfiLandCover_mode_500"

print(names(closure_mean_500))
print(names(biomass_mean_500))
print(names(height_mean_500))
print(names(prcB_mean_500))


## 4.4 Sort layers alphabetically ----
layer_names <- names(scanfi_2020_focal)
layer_names_sorted <- sort(layer_names)
scanfi_2020_focal <- scanfi_2020_focal[[layer_names_sorted]]

# Get the current names
current_names <- names(scanfi_2020_focal)

# Remove "_2020" from the names
new_names <- gsub("_2020", "", current_names)

# Assign the new names to the raster layers
names(scanfi_2020_focal) <- new_names



# Remove the raster objects
rm(f_500_mode, closure_mean_500, biomass_mean_500, height_mean_500, prcB_mean_500)

# Optionally, run garbage collection to free up memory
gc()


aoi_tr <- st_transform(aoi, st_crs(scanfi_2020_focal))
# Mask the raster to remove areas outside the polygon
scanfi_2020_focal <- mask(scanfi_2020_focal, aoi_tr)

writeRaster(scanfi_2020_focal,
  file = "0_data/manual/predictor/scanfi_2020_focal.tif",
  overwrite = TRUE
)


writeRaster(scanfi_2020_focal,
  "0_data/manual/predictor/scanfi_2020_focal.tif",
  overwrite = TRUE,
  wopt = list(datatype = "FLT4S", progress = 1, memfrac = 0.5)
)
