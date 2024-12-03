# ---
# title: "Cavity Data Standardization and Precessing"
# author: "Brendan"
# created: "2024-11-19"
# inputs: 
#   - "0_data/external/Simran_Bains/Website_Files_Sept122024/"
#   - "0_data/external/Simran_Bains/Website_Files_Nov182024/"
# outputs: 
#   - "3_output/shapefiles/cavities_all_YYYYMMDD.shp"
# notes: 
#   "This script standardizes and processes cavity data.
#   It produces a consolidated shapefile for use in GEE."
# ---

# 1. Setup ----

## 1.1 Load packages ----
library(sf)          # Spatial data handling (version: 1.0-16)
library(tidyverse)   # Data manipulation (2.0.0)

# 2. Process September 2024 Data ----

## 2.1 Read and process PWIO cavities ----
shapefile_directory <- 
  "0_data/external/Simran_Bains/Website_Files_Sept122024/"
shapefiles <- list.files(
  path = shapefile_directory, pattern = "\\.shp$", full.names = TRUE
)
sf_list <- lapply(shapefiles, st_read)
names(sf_list) <- basename(tools::file_path_sans_ext(shapefiles))
df_list <- lapply(sf_list, as.data.frame)

# Standardize columns
names(df_list$Spring2024cavities) <- c(
  "Location", "Latitude", "Longitude", "geometry"
)
names(df_list$RedDeer_PIWO_Cavities) <- c(
  "Location", "Latitude", "Longitude", "geometry"
)
df_list$Cavitiesfrom2023 <- df_list$Cavitiesfrom2023 %>%
  dplyr::select(
    Location = location, Latitude = latitude, Longitude = longitude
  )

# Combine selected data frames
piwo_cavities_1 <- bind_rows(
  df_list$Spring2024cavities,
  df_list$EdmontonPIWOcavities,
  df_list$RedDeer_PIWO_Cavities,
  df_list$Winter2024_PIWOcavities,
  df_list$Cavitiesfrom2023
)

piwo_cavities_2 <- piwo_cavities_1 %>%
  mutate(
    diameter = "> 10.2",
    d_class = 4,
    color = "980043",
    size = 5
  ) %>%
  dplyr::select(-geometry)

## 2.2 Process small cavities ----
Spring2024_SmallCavities_epicollect <- 
  df_list$Spring2024_SmallCavities_epicollect %>%
  select(Location = title, Latitude = lat_14_Loc, 
         Longitude = long_14_Lo, diameter = F17_Estima, geometry)%>%
  mutate(diameter = str_extract(diameter, 
                                "\\d+\\.\\d+ to \\d+\\.\\d+ cm")) %>%
  mutate(diameter = str_replace(diameter, " to ", " - "),
         diameter = str_replace_all(diameter, " cm|cm", ""))%>%
  mutate(d_class = case_when(
    diameter == "2.5 - 5.0" ~ "1",
    diameter == "5.0 - 7.6" ~ "2",
    diameter == "7.6 - 10.2" ~ "3",
    str_detect(diameter, "> 10.2") ~ "4", 
    TRUE ~ NA_character_ 
  )) %>%
  ## add columns for size and color for visualizing in GEE
  mutate(color = case_when(
    diameter == "2.5 - 5.0" ~ 'd7b5d8',
    diameter == "5.0 - 7.6" ~ 'df65b0',
    diameter == "7.6 - 10.2" ~ 'dd1c77',
    str_detect(diameter, "> 10.2") ~ '980043', 
    TRUE ~ NA_character_ 
  ))%>%
  mutate(size = as.numeric(case_when(
    diameter == "2.5 - 5.0" ~ '2',
    diameter == "5.0 - 7.6" ~ '3',
    diameter == "7.6 - 10.2" ~ '4',
    str_detect(diameter, "> 10.2") ~ '5', 
    TRUE ~ NA_character_ 
  )))%>%
  mutate(d_class = as.numeric(d_class))%>%
  mutate(
    Longitude = ifelse(Longitude > 0, -Longitude, Longitude),
    Latitude = abs(Latitude)
  ) %>%
  dplyr::select(-geometry)  %>%
  na.omit()

## 2.3 Combine PIWO and small cavities ----
cavities_sept <- bind_rows(
  Spring2024_SmallCavities_epicollect, piwo_cavities_2
) %>%
  rename(site = Location)

rm(list = setdiff(ls(), "cavities_sept"))

# 3. Process November 2024 Data ----

## 3.1 Read and process data ----
shapefile_directory <- 
  "0_data/external/Simran_Bains/Website_Files_Nov182024/"
shapefiles <- list.files(
  path = shapefile_directory, pattern = "\\.shp$", full.names = TRUE
)
sf_list <- lapply(shapefiles, st_read)
names(sf_list) <- basename(tools::file_path_sans_ext(shapefiles))
df_list <- lapply(sf_list, as.data.frame)

# Standardize columns
df_list$Fall_IncidentalCavities <- df_list$Fall_IncidentalCavities %>%
  dplyr::select(-geometry, diameter = Estimated_) %>%
  mutate(site = paste0("fall_incidental_", row_number()))

df_list$Fall_RandomCavities <- df_list$Fall_RandomCavities %>%
  dplyr::select(
    -c(Point, Cavity_num, geometry), diameter = Esitmated_
  ) %>%
  mutate(site = paste0("fall_random_", row_number()))

df_list$Fall_SystemicCavities <- df_list$Fall_SystemicCavities %>%
  dplyr::select(
    -c(Cavity_tre, geometry), Longitude = Longitude_, 
    diameter = Estimated_
  ) %>%
  mutate(site = paste0("fall_systemic_", row_number()))



## 3.2 Combine all processed data frames ----

## Combine all processed data frames ----
cavities_nov <- bind_rows(
  df_list$Fall_IncidentalCavities,
  df_list$Fall_RandomCavities,
  df_list$Fall_SystemicCavities
)

cavities_nov <- cavities_nov %>% 
  mutate(
    diameter = gsub(".*\\(([^)]+)\\).*", "\\1", diameter),
    diameter = gsub(" to ", " - ", diameter),
    diameter = gsub(" cm", "", diameter)
  ) %>%
  mutate(d_class = case_when(
    diameter == "2.5 - 5.0" ~ "1",
    diameter == "5.0 - 7.6" ~ "2",
    diameter == "7.6 - 10.2" ~ "3",
    str_detect(diameter, "> 10.2") ~ "4", 
    TRUE ~ NA_character_ 
  )) %>%
  ## add columns for size and color for visualizing in GEE
  mutate(color = case_when(
    diameter == "2.5 - 5.0" ~ 'd7b5d8',
    diameter == "5.0 - 7.6" ~ 'df65b0',
    diameter == "7.6 - 10.2" ~ 'dd1c77',
    str_detect(diameter, "> 10.2") ~ '980043', 
    TRUE ~ NA_character_ 
  ))%>%
  mutate(size = as.numeric(case_when(
    diameter == "2.5 - 5.0" ~ '2',
    diameter == "5.0 - 7.6" ~ '3',
    diameter == "7.6 - 10.2" ~ '4',
    str_detect(diameter, "> 10.2") ~ '5', 
    TRUE ~ NA_character_ 
  ))) %>%
  mutate(d_class = as.numeric(d_class)) %>%
  mutate(
    Longitude = ifelse(Longitude > 0, -Longitude, Longitude),
    Latitude = abs(Latitude)) %>%  
  na.omit()

rm(list = setdiff(ls(), c("cavities_sept", "cavities_nov")))

# 4. Combine All Data ----
# Combine processed September and November data

cavities_all <- bind_rows(cavities_sept, cavities_nov)

cavities_all_sf <- st_as_sf(
  cavities_all, coords = c("Longitude", "Latitude"), crs = 4326
)

# 5. Save Output ----
# Save the consolidated shapefile for further use.

st_write(
  cavities_all_sf, 
  paste0(
    "3_output/shapefiles/cavities_all_", 
    format(Sys.Date(), "%Y%m%d"), ".shp"
  ),
  append = TRUE
)

