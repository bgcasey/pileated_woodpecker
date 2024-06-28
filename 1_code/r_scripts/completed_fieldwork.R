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
# library(httr)       # for HTTP requests (Epicollect)
# library(jsonlite)   # if needing JSON format
# 
# ## 1.2 Epicollect login credentials ----
# config <- "1_code/r_scripts/.epilogin.R"
# source(config)
# 
# ## 1.3 Source functions ----
# source("1_code/r_scripts/functions/get_epicollect_data.R")
# # source("1_code/r_scripts/functions/misc_functions.R")
# 
# # 2. Import data ----
# ## 2.1 Get planned locations ----
# ss_plan_points <- st_read(paste0("0_data/external/Simran_Bains/",
#                                  "fieldwork_2024/sample_locations_v4/",
#                                  "sample_locations_v4.shp"))
# ss_plan_square <- st_read(paste0("0_data/external/Simran_Bains/",
#                                  "fieldwork_2024/sample_locations_v4/",
#                                  "sample_squares_v4.shp"))
# ## 2.2 Survey notes ----
# load("0_data/manual/response/wood_field_notes_2024.RData")
# 
# # 3. Get data from Epicollect ----
# 
# ## 3.2 BU PIWO cavity survey photos ----
# ### Enter Epicollect credentials
# cID <- Sys.getenv("cID_photo")  # client ID
# secret <- Sys.getenv("secret_photo")  # client secret
# 
# # The following arguments can be found via the API tab in the
# # Epicollect form dashboard.
# proj_slug <- "bu-piwo-cavity-survey-photos"  # project slug
# form_ref <- paste0("916196c9db8345dfad511f18283b9f0b",
#                    "_65f45a58f2e6f")  # form reference
# branch_ref_1 <- paste0("916196c9db8345dfad511f18283b9f0b_",
#                        "65f45a58f2e6f",
#                        "_65f45b0ae8946")  # branch reference
# 
# survey_photos <- get_epi_data(cID, secret, proj_slug, form_ref,
#                               branch_ref_1)
# 
# ## 4.1 Searched locations
# epi_ss <- as.data.frame(survey_photos$ct1$data$entries) %>%
#   select(c(ss = '2_What_is_the_name_o')) %>%
#   mutate(ss = toupper(ss)) %>%
#   distinct()
# 
# 
## BU 2024 woodpecker cavity survey ----
cID <- Sys.getenv("cID_survey2024")  # client ID
secret <- Sys.getenv("secret_survey2024")  # client secret

# The following arguments can be found via the API tab in the
# Epicollect form dashboard.
proj_slug <- "bu-2024-woodpecker-cavity-survey"  # project slug
form_ref <- paste0("4cfa4a5dbf5c4559899a9cbf89a4e923",
                   "_66418cffc00ea")  # form reference
branch_ref_1 <- paste0("4cfa4a5dbf5c4559899a9cbf89a4e923",
                       "_66418cffc00ea",
                       "_6641906467731")  # branch reference

cavity_surveys <- get_epi_data(cID, secret, proj_slug, form_ref,
                              branch_ref_1)
## 4.1 Searched locations
epi_ss <- as.data.frame(cavity_surveys$ct1$data$entries) 

%>%
  select(c(ss = '6_What_Location_are_')) %>%
  mutate(ss = toupper(ss)) %>%
  distinct()

# 
# ## 4.1 Searched locations
# # Extract the entries list which contains the data we are interested in.
# entries <- cavity_surveys$ct2$data$entries
# 
df <- data.frame(
  ec5_branch_owner_uuid = entries$ec5_branch_owner_uuid,
  latitude = entries$`14_Location_within_t`$latitude,
  longitude = entries$`14_Location_within_t`$longitude
)
# 
# # 4. Field work locations ----
# 
# # 4.2 Get searched locations
# searched <- ss_plan_points %>%
#   semi_join(field_notes, by = c('Name' = 'ss'))
# 
# # 4.3 Convert points to 1 ha squares
# points <- st_transform(piwo_searched, crs = 3348)
# radius <- 50  # radius in meters
# squares <- points_to_squares(points, radius)
# 
# ## 4.4 Save the data ----
# st_write(piwo_searched, 
#          "3_output/shapefiles/piwo_searched.shp", append = FALSE)
# 
# # 5. Known cavity locations ----
# bu_cav <- cavities$ct1$data$entries %>%
#   dplyr::select(c(location = "6_Location_Name_plea",
#                   date = "3_Date",
#                   lat = "7_Latitude_6_decimal",
#                   lon = "8_Longitude_6_decima"))
# 
# # yeg_cav <- yeg_cavities$ct1$data$entries %>%
# #   dplyr::select(c(location = "6_Location_Name_plea",
# #                   date = "3_Date",
# #                   lat = "7_Latitude_6_decimal",
# #                   lon = "8_Longitude_6_decima"))
# 
# # 5.1 Combine the two data frames
# combined_df <- rbind(bu_cav, yeg_cav)
# 
# # 5.2 Make all longitudes negative
# combined_df$lon <- abs(combined_df$lon) * -1
# 
# # 5.3 Convert to an sf object
# combined_sf <- st_as_sf(combined_df, coords = c("lon", "lat"), 
#                         crs = 4326)
# 
# # 5.4 Save the data
# st_write(combined_sf, "3_output/shapefiles/piwo_cavities.shp")



form_1_visit_data <- read_csv("0_data/external/epicollect/bu-2024-aru-visit-form-csv/form-1__visit-data.csv")

wood_xy_4326 <- form_1_visit_data %>%
  dplyr::select(
    FCODE = `1_FCODE`, 
    lat_cell = `lat_14_Location_from_Cel`, 
    lon_cell = `long_14_Location_from_Cel`,
    lat_ir = `15_Latitude_inReach`, 
    lon_ir = `16_Longitude_inReach`,
    date = `12_Visit_Date`
  ) %>%
  mutate(
    lat = coalesce(lat_cell, lat_ir),
    lon = coalesce(lon_cell, lon_ir)
  ) %>%
  filter(FCODE == "WOOD") %>%
  dplyr::select(FCODE, lat, lon, date) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

st_write(wood_xy_4326, "0_data/manual/spatial/wood_2024_xy_4326.shp", 
         append = FALSE)
