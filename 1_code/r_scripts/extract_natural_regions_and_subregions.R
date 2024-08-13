# ---
# title: "Extract Natural Regions and Subregions"
# author: "Brendan Casey"
# created: "2024-08-12"
# description:
#   "This script extracts natural regions and subregions classes to
#   points. The final output is a cleaned dataframe saved as an .RData
#   file."
# ---

# 1. Setup ----

## 1.1 Load packages ----
library(sf) # for handling spatial data
library(tidyverse) # for data manipulation and visualization
library(snakecase) # for converting strings to snake case
library(terra)

## 1.2 Import data ----

### 1.2.1 Load Natural regions and subregions ----
nat <- st_read(
  paste0(
    "~/Library/CloudStorage/GoogleDrive-bgcasey@ualberta.ca/",
    "My Drive/3_Resources/data/spatial/Alberta_Landcover_Data/",
    "GOA Products/Natural_Regions_Subregions_of_Alberta/",
    "Natural_Regions_Subregions_of_Alberta.shp"
  )
)
# Format
nat <- nat %>%
  mutate(
    nrname = to_snake_case(NRNAME),
    nsrname = to_snake_case(NSRNAME),
    nsrcode = NSRCODE
  ) %>%
  select(nrname, nsrname, nsrcode)

#### 1.2.2 Load point locations ----
load("0_data/manual/spatial/ss_xy_4326.rData")

# 2. Transform and Extract Data ----
## 2.1 Ensure same CRS ----
ss_xy_4326 <- st_transform(ss_xy_4326, st_crs(nat))

## 2.2 Extract data to points ----
xy_nat_region <- st_intersection(ss_xy_4326, nat)

# 3. Clean and Format Data ----
## 3.1 Convert to snake case and drop geometry ----
xy_nat_region <- xy_nat_region %>%
  st_drop_geometry() %>%
  select(location, nrname, nsrname, nsrcode) %>%
  mutate(
    nrname = as.factor(nrname),
    nsrname = as.factor(nsrname),
    nsrcode = as.factor(nsrcode)
  )

## 3.2 Save cleaned dataframe ----
save(xy_nat_region,
  file = "0_data/manual/predictor/xy_nat_region.rData"
)

# 4. Rasterize Data ----
## 4.1 Create raster template ----
# Define the extent and resolution for the raster
raster_template <- rast(ext(nat), 
                          res = 100, 
                          crs = st_crs(nat)$proj4string
                          )

## 4.2 Rasterize the spatial data ----
# Rasterize the 'nat' spatial object
nat_r_raster <- rasterize(nat, raster_template, field = "NRNAME")
snake_case_values <- setNames(to_snake_case(unique_values), unique_values)

nat_sr_raster <- rasterize(nat, raster_template, field = "NSRNAME")

## 4.3 Save Raster Data ----
# This section saves the rasterized data to a file. The section uses
# the rasterized data from the previous step. It includes steps to 
# save the raster data to a specified file path. The section produces
# a raster file containing the rasterized spatial data.

## 3.1 Save rasterized data ----
writeRaster(nat_raster, 
            filename = "0_data/manual/predictor/nat_raster.tif", 
            format = "GTiff", 
            overwrite = TRUE)


