# ---
# title: "Create a Shapefile of Nest Locations"
# author: "Brendan Casey"
# created: "2024-06-28"
# description: >
#   "This script creates a shapefile of nest locations using
#   various data sources."
# ---

# 1. Setup ----
## 1.1 Load packages ----
library(tidyverse)  # For data manipulation
library(sf)         # For spatial data
library(httr)       # For HTTP requests (Epicollect)
library(jsonlite)   # For JSON format

## 1.2 Epicollect login credentials ----
config <- "1_code/r_scripts/.epilogin.R"
source(config)

## 1.3 Source functions ----
source("1_code/r_scripts/functions/get_epicollect_data.R")

# 2. Get nest data ----
## 2.1 FMWIS ----
FWMIS_PIWO <- readxl::read_excel(
  "0_data/external/nest_locations/FWMIS_PIWO.xlsx")

FWMIS_PIWO_cavities_xy <- FWMIS_PIWO %>%
  select(`Project Location Id`, Latitude, Longitude) %>%
  rename(location = `Project Location Id`, lat = Latitude, 
         lon = Longitude) %>%
  mutate(source = "FWMIS") %>%
  distinct() %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# Save as spatial data frame
save(FWMIS_PIWO_cavities_xy, 
     file="0_data/manual/spatial/FWMIS_PIWO_cavities_xy_4326.rData")

## 2.2 From Simran ----
SB_PIWO <- readxl::read_excel(
  "0_data/external/Simran_Bains/Woodpecker_nest_data.xlsx")

SB_PIWO_cavities_xy <- SB_PIWO %>%
  filter(Nest == "y") %>%
  select(`Location Name`, Latitude, Longitude) %>%
  rename(location = `Location Name`, lat = Latitude, 
         lon = Longitude
         ) %>%
  mutate(diameter = "> 10.2") %>%
  mutate(source = "SB") %>%
  distinct() %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# Save as spatial data frame
save(SB_PIWO_cavities_xy, 
     file="0_data/manual/spatial/SB_PIWO_cavities_xy.rData")

## 2.3 BU nest cavities ----
### Enter Epicollect credentials 
cID <- Sys.getenv("cID_cavity")  # Client ID
secret <- Sys.getenv("secret_cavity")  # Client secret

# The following arguments can be found via the API tab in the 
# Epicollect form dashboard: 

# Project slug
proj_slug <- "bu-piwo-cavity-survey"  
# Form reference
form_ref <- "d9574500b12d4784925e688e9d4e8ad6_6501e829c9424"  
# Branch reference
branch_ref_1 <- "d9574500b12d4784925e688e9d4e8ad6_6501e829c9424_6530962eb8a46"  


#### Get data from Epicollect
bu_cavities <- get_epi_data(cID, secret, proj_slug, form_ref, branch_ref_1)

bu_PIWO_cavities_xy <- bu_cavities$ct1$data$entries %>%
  dplyr::select(location = "6_Location_Name_plea", 
                lat = "7_Latitude_6_decimal", 
                lon = "8_Longitude_6_decima",
                diameter = "23_Height_of_entranc") %>%
  mutate(diameter = "> 10.2") %>%
  mutate(source = "BU_epi", lon = abs(lon) * -1) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

## 2.4 BU nest cavities 2 ----
### Enter Epicollect credentials 
cID <- Sys.getenv("cID_cavity_assessment")  # Client ID
secret <- Sys.getenv("secret_cavity_assessment")  # Client secret

# The following arguments can be found via the API tab in the 
# Epicollect form dashboard: 

# Project slug
proj_slug <- "bu-2024-piwo-cavity-assessment"  
# Form reference
form_ref <- "317cb73d30fa4b5f976fcdc969265816_6501e829c9424"  
# Branch reference
branch_ref_1 <- "317cb73d30fa4b5f976fcdc969265816_6501e829c9424_6530962eb8a46"  


#### Get data from Epicollect
bu_cavities_2 <- get_epi_data(cID, secret, proj_slug, form_ref, branch_ref_1)

bu_2_PIWO_cavities_xy <- bu_cavities_2$ct1$data$entries %>%
  dplyr::select(
                location = "7_Location_Name_plea", 
                lat = "8_Latitude_6_decimal", 
                lon = "9_Longitude_6_decima") %>%
  mutate(diameter = "> 10.2") %>%
  mutate(source = "BU_epi", lon = abs(lon) * -1) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

## 2.5 YEG Pileated Woodpecker Cavity Survey ----
### Enter Epicollect credentials 
cID <- Sys.getenv("cID_yeg")  # Client ID
secret <- Sys.getenv("secret_yeg")  # Client secret

# The following arguments can be found via the API tab in the 
# Epicollect form dashboard: 

# Project slug
proj_slug <- "yeg-pileated-woodpecker-cavity-survey"  
# Form reference
form_ref <- "3a2c7dc2f9ec4290b719921f8d78a484_6501e829c9424"  
# Branch reference
branch_ref_1 <- "3a2c7dc2f9ec4290b719921f8d78a484_6501e829c9424_65f45b0ae8946"  


#### Get data from Epicollect
yeg_cavities <- get_epi_data(cID, secret, proj_slug, form_ref, branch_ref_1)

yeg_PIWO_cavities_xy <- yeg_cavities$ct1$data$entries %>%
  dplyr::select(location = "6_Location_Name_plea", 
                lat = "7_Latitude_6_decimal", 
                lon = "8_Longitude_6_decima",
                diameter = "23_Height_of_entranc") %>%
  mutate(diameter = "> 10.2") %>%
  mutate(source = "BU_epi", lon = abs(lon) * -1) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)


## 2.6 BU 2024 woodpecker cavity survey ----
### Enter Epicollect credentials 
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

#### Get data from Epicollect
cavity_surveys <- get_epi_data(cID, secret, proj_slug, form_ref,
                               branch_ref_1)

cavity_surveys_xy<-as.data.frame(cavity_surveys[["ct2"]][["data"]])

cavity_surveys_xy <- as.data.frame(cavity_surveys[["ct2"]][["data"]]) %>%
  unnest(`entries.14_Location_within_t`)%>%
  dplyr::select(location = "entries.ec5_branch_owner_uuid", 
                lat = "latitude", 
                lon = "longitude",
                diameter = "entries.17_Estimated_diamete") %>%
  mutate(diameter = str_extract(diameter, "\\d+\\.\\d+ to \\d+\\.\\d+ cm")) %>%
  mutate(diameter = str_replace(diameter, " to ", " - "),
         diameter = str_replace_all(diameter, " cm|cm", "")) %>%
  mutate(source = "BU_epi", lon = abs(lon) * -1) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  

# 3. Combine into single shapefile ----
PIWO_cavities_xy <- rbind(SB_PIWO_cavities_xy, 
                          bu_PIWO_cavities_xy, bu_2_PIWO_cavities_xy, 
                          yeg_PIWO_cavities_xy, cavity_surveys_xy)%>%
  mutate(d_class = case_when(
    diameter == "2.5 - 5.0" ~ "1",
    diameter == "5.0 - 7.6" ~ "2",
    diameter == "7.6 - 10.2" ~ "3",
    str_detect(diameter, "> 10.2") ~ "4", # Assuming diameter can have values like "> 10.2"
    TRUE ~ NA_character_ # For any cases that do not match the above conditions
  ))%>%
  ## add columns for size and color for visualizing in GEE
  mutate(color = case_when(
    diameter == "2.5 - 5.0" ~ 'd7b5d8',
    diameter == "5.0 - 7.6" ~ 'df65b0',
    diameter == "7.6 - 10.2" ~ 'dd1c77',
    str_detect(diameter, "> 10.2") ~ '980043', 
    TRUE ~ NA_character_ 
  ))%>%
  mutate(size = as.numeric(case_when(
    diameter == "2.5 - 5.0" ~ '4',
    diameter == "5.0 - 7.6" ~ '6',
    diameter == "7.6 - 10.2" ~ '8',
    str_detect(diameter, "> 10.2") ~ '10', 
    TRUE ~ NA_character_ 
  )))%>%
  mutate(d_class = as.numeric(d_class))%>%
  na.omit()

# Save as spatial data frame
save(PIWO_cavities_xy, 
     file="0_data/manual/spatial/cavities_xy.rData")

# Save as shapefile
st_write(PIWO_cavities_xy, "0_data/manual/spatial/cavities_xy.shp", 
         append = FALSE)

