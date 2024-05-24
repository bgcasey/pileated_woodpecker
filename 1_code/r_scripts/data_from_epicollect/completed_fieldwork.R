# ---
# title: "piwo_nest_cavities"
# author: "Brendan Casey"
# created: "2024-05-23"
# description: "code to extract piwo nest cavities data"
# ---

# Setup ----
## Load packages----
library(tidyverse) # for data manipulation
library(sf)  # for spatial data
library(httr) # for http requests (epicollect)
library(jsonlite)  # if needing json format

## Source functions----
source("1_code/r_scripts/functions/get_epicollect_data.R")
source("1_code/r_scripts/functions/misc_functions.R")

## Import data----
### Get planned locations----
ss_plan_points<-st_read(paste0("0_data/external/Simran_Bains/",
                               "fieldwork_2024/sample_locations_v4/",
                               "sample_locations_v4.shp"))
ss_plan_square<-st_read(paste0("0_data/external/Simran_Bains/",
                               "fieldwork_2024/sample_locations_v4/",
                               "sample_squares_v4.shp"))
### Survey notes----
load("0_data/manual/response/wood_field_notes_2024.RData")

## Get data from epicollect----
### BU nest cavities----
### Enter epicollect credentials
cID <- "5250"  # client ID
secret <- "WmTAMxGgH1fIkMks7Icx1Cx9KQYH6ooNYqn0WcDF"  # client secret

# The following arguments can be found via the API tab in the 
# eipcollect form dashboard. 
proj.slug <- "bu-piwo-cavity-survey"  # project slug
form.ref <- paste0("d9574500b12d4784925e688e9d4e8ad6",
                   "_6501e829c9424")  # form reference
branch.ref.1 <- paste0("d9574500b12d4784925e688e9d4e8ad6",
                       "_6501e829c9424",
                       "_6530962eb8a46") # branch reference
branch.ref.2 <- paste0("d9574500b12d4784925e688e9d4e8ad6",
                       "_6501e829c9424",
                       "_65d4fd53a4f68") # branch reference

cavities<-get_epi_data(cID, secret, proj.slug, form.ref, 
                       branch.ref.1)

### BU PIWO cavity survey photos----
### Enter epicollect credentials
cID <- "5249"  # client ID
secret <- "bEqr1LBQWmGFURt42V7Yogi7ZgQnuzjzVGqz4aDq"  # client secret

# The following arguments can be found via the API tab in the 
# eipcollect form dashboard. 
proj.slug <- "bu-piwo-cavity-survey-photos"  # project slug
form.ref <- paste0("916196c9db8345dfad511f18283b9f0b",
                   "_65f45a58f2e6f")  # form reference
branch.ref.1 <- paste0("916196c9db8345dfad511f18283b9f0b_",
                       "65f45a58f2e6f",
                       "_65f45b0ae8946")  # branch reference


survey_photos<-get_epi_data(cID, secret, proj.slug, form.ref, 
                            branch.ref.1)

### YEG Pileated Woodpecker Cavity Survey ----
### Enter epicollect credentials
## Enter epicollect credentials
cID <- "5255 "  # client ID
secret <- "tnjguGweQSZ9uErU1AATwmnZTG6J2D7kIok7oQ6r"  # client secret

# The following arguments can be found via the API tab in the 
# eipcollect form dashboard. 
proj.slug <- "yeg-pileated-woodpecker-cavity-survey"  # project slug
form.ref <- paste0("3a2c7dc2f9ec4290b719921f8d78a484",
                   "_6501e829c9424")  # form reference
branch.ref.1 <- paste0("3a2c7dc2f9ec4290b719921f8d78a484",
                       "_6501e829c9424",
                       "_65f45b0ae8946")  # branch reference

yeg_cavities<-get_epi_data(cID, secret, proj.slug, form.ref, 
                           branch.ref.1)


## ////////////////////////////////////////////////////////////////

# Field work locations ----
## Searched locations
epi_ss<-as.data.frame(survey_photos$ct1$data$entries)%>%
  select(c(ss='2_What_is_the_name_o'))%>%
  mutate(ss = toupper(ss))%>%
  distinct()

# Get searched locations
searched<-ss_plan_points%>%
  semi_join(field_notes, by=c('Name'='ss'))


# Convert points to 1 ha squares
points <- st_transform(piwo_searched, crs=3348)
radius <- 50  # radius in meters
squares <- points_to_squares(points, radius)

## Save the data----
st_write(piwo_searched, 
         "3_output/shapefiles/piwo_searched.shp", append = FALSE)

## ////////////////////////////////////////////////////////////////

# Known cavity locations----
bu_cav <- cavities$ct1$data$entries %>%
  dplyr::select(c(location = "6_Location_Name_plea",
                  date = "3_Date",
                  lat = "7_Latitude_6_decimal",
                  lon = "8_Longitude_6_decima",
                  ))

yeg_cav <- yeg_cavities$ct1$data$entries %>%
  dplyr::select(c(location = "6_Location_Name_plea",
                  date = "3_Date",
                  lat = "7_Latitude_6_decimal",
                  lon = "8_Longitude_6_decima",
  ))

# rbind the two data frames
combined_df <- rbind(bu_cav, yeg_cav)

# Make all latitudes negative
combined_df$lon <- abs(combined_df$lon) * -1

# Convert to an sf object
combined_sf <- st_as_sf(combined_df, coords = c("lon", "lat"), 
                        crs = 4326)

# Save the data
st_write(combined_sf, "3_output/shapefiles/piwo_cavities.shp")

