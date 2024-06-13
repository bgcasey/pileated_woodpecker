# ---
# title: "Create spatial object with point count locations"
# author: "Brendan Casey"
# created: "2024-02-06"
# description: "This script generates shapefiles for point count 
# locations "
# ---

# 1. Setup ----

## 1.1 Load packages ----
library(sf) # for handling and analyzing spatial data
library(tidyverse) # data manipulation and visualization

## 1.2 Import data ----
# Load the cleaned Wildtrax data
load("0_data/manual/response/wildtrax_cleaned_piwo_2024-06-13.rData")

# 2. Get XY coordinates ----

## 2.1 Create spatial object ----
# Generate a spatial object with unique point count locations
ss_xy <- wildtrax_cleaned_piwo %>%
  ungroup() %>%
  dplyr::select(location, lon, lat) %>%
  distinct()

# Convert the data to a spatial object with CRS set to 
# EPSG:4326 (WGS84)
ss_xy_4326 <- ss_xy %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

## 2.2 Transform coordinates ----
# Convert the coordinates to meters (EPSG:3978)
ss_xy_3978 <- ss_xy_4326 %>%
  st_transform(crs = 3978) %>%
  dplyr::mutate(x_3978 = sf::st_coordinates(.)[,1],
                y_3978 = sf::st_coordinates(.)[,2]) %>%
  dplyr::select(-c(lon, lat))

# 3. Save ----
save(ss_xy_4326, file = "0_data/manual/spatial/ss_xy_4326.rData")
st_write(ss_xy_4326, "0_data/manual/spatial/ss_xy_4326.shp", 
         append = FALSE)

save(ss_xy_3978, file = "0_data/manual/spatial/ss_xy_3978.rData")
st_write(ss_xy_3978, "0_data/manual/spatial/ss_xy_3978.shp", 
         append = FALSE)
