# ---
# title: "PIWO Nest Cavities"
# author: "Brendan Casey"
# created: "2024-05-23"
# description: >
#   Code to extract fieldwork locations and known PIWO cavity locations 
#   from Epicollect.
# ---

# 1. Setup ----
## 1.1 Load packages ----
library(tidyverse)  # for data manipulation
library(sf)         # for spatial data
library(httr)       # for HTTP requests (Epicollect)
library(jsonlite)   # if needing JSON format

## 1.2 Epicollect login credentials ----
config <- "1_code/r_scripts/.epilogin.R"
source(config)

## 1.3 Source functions ----
source("1_code/r_scripts/functions/get_epicollect_data.R")
source("1_code/r_scripts/functions/misc_functions.R")

# 2. Import data ----
## 2.1 Get planned locations ----
ss_plan_points <- st_read(paste0("0_data/external/Simran_Bains/",
                                 "fieldwork_2024/sample_locations_v4/",
                                 "sample_locations_v4.shp"))
ss_plan_square <- st_read(paste0("0_data/external/Simran_Bains/",
                                 "fieldwork_2024/sample_locations_v4/",
                                 "sample_squares_v4.shp"))
## 2.2 Survey notes ----
load("0_data/manual/response/wood_field_notes_2024.RData")

# 3. Get data from Epicollect ----
## 3.1 BU nest cavities ----
### Enter Epicollect credentials
cID <- Sys.getenv("cID_cavity")  # client ID
secret <- Sys.getenv("secret_cavity")  # client secret

# The following arguments can be found via the API tab in the 
# Epicollect form dashboard. 
proj_slug <- "bu-piwo-cavity-survey"      # project slug
form_ref <- paste0("d9574500b12d4784925e688e9d4e8ad6",
                   "_6501e829c9424")      # form reference
branch_ref_1 <- paste0("d9574500b12d4784925e688e9d4e8ad6",
                       "_6501e829c9424",
                       "_6530962eb8a46")  # branch reference

cavities <- get_epi_data(cID, secret, proj_slug, form_ref, 
                         branch_ref_1)

## 3.2 BU PIWO cavity survey photos ----
### Enter Epicollect credentials
cID <- Sys.getenv("cID_photo")  # client ID
secret <- Sys.getenv("secret_photo")  # client secret

# The following arguments can be found via the API tab in the 
# Epicollect form dashboard. 
proj_slug <- "bu-piwo-cavity-survey-photos"  # project slug
form_ref <- paste0("916196c9db8345dfad511f18283b9f0b",
                   "_65f45a58f2e6f")  # form reference
branch_ref_1 <- paste0("916196c9db8345dfad511f18283b9f0b_",
                       "65f45a58f2e6f",
                       "_65f45b0ae8946")  # branch reference

survey_photos <- get_epi_data(cID, secret, proj_slug, form_ref, 
                              branch_ref_1)

## 3.3 YEG Pileated Woodpecker Cavity Survey ----
### Enter Epicollect credentials
cID <- Sys.getenv("cID_yeg")  # client ID
secret <- Sys.getenv("secret_yeg")  # client secret

# The following arguments can be found via the API tab in the 
# Epicollect form dashboard. 
proj_slug <- "yeg-pileated-woodpecker-cavity-survey"  # project slug
form_ref <- paste0("3a2c7dc2f9ec4290b719921f8d78a484",
                   "_6501e829c9424")  # form reference
branch_ref_1 <- paste0("3a2c7dc2f9ec4290b719921f8d78a484",
                       "_6501e829c9424",
                       "_65f45b0ae8946")  # branch reference

yeg_cavities <- get_epi_data(cID, secret, proj_slug, form_ref, 
                             branch_ref_1)

# 4. Field work locations ----
## 4.1 Searched locations
epi_ss <- as.data.frame(survey_photos$ct1$data$entries) %>%
  select(c(ss = '2_What_is_the_name_o')) %>%
  mutate(ss = toupper(ss)) %>%
  distinct()

# 4.2 Get searched locations
searched <- ss_plan_points %>%
  semi_join(field_notes, by = c('Name' = 'ss'))

# 4.3 Convert points to 1 ha squares
points <- st_transform(piwo_searched, crs = 3348)
radius <- 50  # radius in meters
squares <- points_to_squares(points, radius)

## 4.4 Save the data ----
st_write(piwo_searched, 
         "3_output/shapefiles/piwo_searched.shp", append = FALSE)

# 5. Known cavity locations ----
bu_cav <- cavities$ct1$data$entries %>%
  dplyr::select(c(location = "6_Location_Name_plea",
                  date = "3_Date",
                  lat = "7_Latitude_6_decimal",
                  lon = "8_Longitude_6_decima"))

yeg_cav <- yeg_cavities$ct1$data$entries %>%
  dplyr::select(c(location = "6_Location_Name_plea",
                  date = "3_Date",
                  lat = "7_Latitude_6_decimal",
                  lon = "8_Longitude_6_decima"))

# 5.1 Combine the two data frames
combined_df <- rbind(bu_cav, yeg_cav)

# 5.2 Make all longitudes negative
combined_df$lon <- abs(combined_df$lon) * -1

# 5.3 Convert to an sf object
combined_sf <- st_as_sf(combined_df, coords = c("lon", "lat"), 
                        crs = 4326)

# 5.4 Save the data
st_write(combined_sf, "3_output/shapefiles/piwo_cavities.shp")

