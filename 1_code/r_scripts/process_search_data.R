# ---
# title: "Search Areas Data Processing"
# author: "Brendan Casey"
# created: "2024-11-19"
# inputs: 
#   - "0_data/external/Simran_Bains/Website_Files_Sept122024/"
#   - "0_data/external/Simran_Bains/Website_Files_Nov182024/"
# outputs: 
#   - "3_output/shapefiles/piwo_searched_YYYYMMDD.shp"
# notes: 
#   "This script processes and standardizes woodpecker search area 
#   data. It saves the output as a shapefile."
# ---

# 1. Setup ----

## 1.1 Load packages ----
library(sf)          # Spatial data handling (version: 1.0-16)
library(tidyverse)   # Data manipulation (2.0.0)

## 1.2 Load data ----
sa_sept_wood <- st_read(
  "0_data/external/Simran_Bains/Website_Files_Sept122024//WOODGroundTruthing_searchareas.shp"
)
sa_spring <- read_csv(
  "0_data/external/Simran_Bains/Website_Files_Sept122024/SpringSearchAreasByLab.csv"
)
sa_nov_random <- st_read(
  "0_data/external/Simran_Bains/Website_Files_Nov182024//Fall_RandomSearchAreas.shp"
)
sa_nov_sys <- st_read(
  "0_data/external/Simran_Bains/Website_Files_Nov182024//Fall_SystemicSearchAreas.shp"
)

# 2. Standardize data ----
# This section standardizes the columns of the input data for consistency.

sa_sept_wood <- sa_sept_wood %>%
  as.data.frame() %>%
  select(Location, Latitude = lat, Longitude = long)

sa_spring <- sa_spring %>%
  select(Location = Location_name, Latitude = Lat, Longitude = Long)

sa_nov_random <- sa_nov_random %>%
  as.data.frame() %>%
  select(Location = Point, Latitude, Longitude) %>%
  mutate(Location = as.character(Location))

sa_nov_sys <- sa_nov_sys %>%
  as.data.frame() %>%
  select(Location = Name, Latitude = Lat, Longitude = Long)

# 3. Combine data ----
# Combine all standardized search area data into a single data frame.

search_areas_all <- bind_rows(
  sa_sept_wood, sa_spring, sa_nov_random, sa_nov_sys
) %>%
  distinct()

# 4. Save as shapefile ----
# Convert the combined data into an sf object and save it as a shapefile.

search_areas_sf <- st_as_sf(
  search_areas_all, 
  coords = c("Longitude", "Latitude"), 
  crs = 4326
)

st_write(
  search_areas_sf,
  paste0(
    "3_output/shapefiles/piwo_searched_", 
    format(Sys.Date(), "%Y%m%d"), ".shp"
  ),
  append = TRUE
)


