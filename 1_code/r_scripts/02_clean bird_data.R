# ---
# title: "Clean bird data"
# author: "Brendan Casey"
# created: "2024-01-13"
# description: "This script cleans and processes the raw ARU and 
# point count data fetched from WildTrax in 
# '01_gather_bird_data_from_WildTrax.R'. It filters and merges 
# the data, and calculates new spatial and temporal fields. The 
# cleaned data is then saved as wildtrax_cleaned_<date>.rData 
# where <date> is the current system date"
# ---

# 1. Setup ----

## 1.1 Load packages ----
library(tidyverse) # data manipulation and visualization
library(wildRtrax) # interacting with the WildTrax platform
library(lubridate) # handling and manipulating date times
library(suncalc) # calculating sunrise time
library(lutz) # For getting local timezone
library(sf) # for handling and analyzing spatial data

## 1.2 Import data ----
load("0_data/manual/response/wildtrax_raw_pc_2024-06-11.rData")
load("0_data/manual/response/wildtrax_raw_aru_2024-06-11.rData")

# Information on WildTrax projects. Will be used to filter data
instructions <- read.csv(file.path(
  "0_data/manual/response/projectInstructions.csv"))

# Bird species list
# Login to WildTrax
# The config file contains the login credentials. 
config <- "1_code/r_scripts/.wtlogin.R"
source(config)
wt_auth()

# Get species list
aves<-wt_get_species()%>%
  filter(species_class=="AVES")

# 2. Clean and merge bird data ----
## 2.1 Define columns of interest ----
colnms <- c("organization", "project", "project_id", 
            "survey_type", "location", "location_buffer_m", 
            "longitude", "latitude", "date", 
            "survey_duration_method", "max_duration", 
            "survey_distance_method", "detection_time", 
            "detection_distance", "task_method", "species_code", 
            "individual_order", "individual_count", 
            "vocalization")

## 2.2 Clean ARU survey methods ----
wildtrax_raw_aru_1 <- wildtrax_raw_aru %>%
  filter(task_duration %% 60 == 0 & task_duration != 0) %>%
  rename(date = recording_date_time, 
         max_duration = task_duration) %>%
  mutate(minutes = max_duration / 60,
         survey_duration_method = case_when(
           minutes == 1 ~ "0-1min",
           minutes == 2 ~ "0-1-2min",
           minutes == 3 ~ "0-1-2-3min",
           minutes == 4 ~ "0-1-2-3-4min",
           minutes == 5 ~ "0-1-2-3-4-5min",
           minutes == 6 ~ "0-1-2-3-4-5-6min",
           minutes == 7 ~ "0-1-2-3-4-5-6-7min",
           minutes == 8 ~ "0-1-2-3-4-5-6-7-8min",
           minutes == 9 ~ "0-1-2-3-4-5-6-7-8-9min",
           minutes == 10 ~ "0-1-2-3-4-5-6-7-8-9-10min"),
         survey_distance_method = "0m-INF-ARU",
         detection_distance = "0m-INF-ARU",
         detection_time = as.character(detection_time)) %>%
  dplyr::select(any_of(colnms))

## 2.3 Clean point count survey methods ----
wildtrax_raw_pc_1 <- wildtrax_raw_pc %>%
  dplyr::select(-c(observer)) %>%
  mutate(max_duration = as.numeric(
    str_extract(survey_duration_method, 
                "(?<=-)(\\d+)(?=min)")) * 60,
    date = survey_date) %>%
  dplyr::select(any_of(colnms))

## 2.4 Combine PC and ARU dataframes ----
wildtrax_all <- dplyr::bind_rows(wildtrax_raw_pc_1, 
                                 wildtrax_raw_aru_1)

# 3. Filter and clean data and calculate new fields ---- 
w <- wildtrax_all %>% 
  # Join with aves data to keep only bird records
  semi_join(aves) %>%
  # Group by project_id
  group_by(project_id) %>%
  # Remove single species projects that are not PIWO related
  filter((n_distinct(species_code) > 2) | 
           (n_distinct(species_code) < 2 & species_code=="PIWO")) %>%
  # Ungroup data
  ungroup() %>%
  # Remove duplicate rows
  distinct() %>%
  # Remove unsuitable projects according
  anti_join(instructions) %>%
  # Create additional temporal fields and modify some spatial fields
  mutate(location_buffer_m = ifelse(is.na(location_buffer_m), 
                                    0, location_buffer_m),
         ordinalDay = yday(date),
         year = year(date),
         month = month(date),
         start_time = hour(date) + minute(date) / 60,
         date = ymd(str_sub(date, 1, 10)),
         tz = tz_lookup_coords(latitude, longitude, 
                               method = "accurate"),
         lat = latitude, 
         lon = longitude) %>%
  # Filter based on various conditions
  filter(
    # Remove outliers for day of year (use 99% quantile)
    ordinalDay > quantile(ordinalDay, 0.005),
    ordinalDay < quantile(ordinalDay, 0.995),
    # Remove surveys with unknown survey time
    !str_sub(date, -8, -1) %in% c("00:00:01", "00:01:01"),
    !is.na(date),
    # Take out BBS. It's not useful for removal or distance sampling
    project != "BAM-BBS",
    # Remove training data
    organization != "BU-TRAINING",
    # Remove ABMI data since the locations are not accurate
    organization != "ABMI",
    # Remove surveys with no location
    !is.na(latitude),
    latitude > 0, 
    longitude < 0,
    year(date) > 1900,
    # Remove midnight PC surveys
    !(hour(date) == 0 & survey_type == "PC"),
    # Remove locations that are within a buffer
    location_buffer_m == 0
  ) %>%
  # Calculate survey effort (n_surveys * max_duration / 60)
  select(project_id, survey_type, location, 
         survey_duration_method, survey_distance_method, 
         year, date, max_duration) %>%
  distinct() %>%
  group_by(project_id, survey_type, location, 
           survey_duration_method, survey_distance_method, year) %>%
  mutate(n_surveys = n()) %>%
  ungroup() %>%
  mutate(survey_effort = n_surveys * max_duration / 60) %>%
  full_join(w) %>%
  # Add XY coordinates that are in meters 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = 3857) %>%
  mutate(x_3857 = sf::st_coordinates(.)[,1], 
         y_3857 = sf::st_coordinates(.)[,2]) %>%
  st_set_geometry(NULL)

## 3.1 Calculate time since sunrise ----
tzs <- unique(w$tz)
tzs <- Filter(function(x) !is.na(x), tzs)

sun.list <- list()
for(i in 1:length(tzs)){
  all.i <- w %>% 
    dplyr::filter(tz == tzs[i])
  
  all.i$sunrise <- getSunlightTimes(data = all.i, keep = "sunrise", 
                                    tz = tzs[i])$sunrise
  all.i$hssr <- all.i$start_time - (hour(all.i$sunrise) + 
                                      minute(all.i$sunrise) / 60)
  
  sun.list[[i]] <- all.i
}
sun <- do.call(rbind, sun.list)
w <- sun

# 4. Select and order columns ----
colnms <- c("organization", "project", "project_id", "survey_type", 
            "location", "location_buffer_m", "lon", "lat", "x_3857", 
            "y_3857", "date", "ordinalDay", "year", "month", 
            "start_time", "tz", "sunrise", "hssr", "task_method", 
            "survey_distance_method", "survey_duration_method", 
            "max_duration", "n_surveys", "survey_effort", 
            "detection_time", "detection_distance", "species_code", 
            "individual_order", "individual_count", "vocalization") 

wildtrax_cleaned <- w %>%
  dplyr::select(all_of(colnms))

# 5. Save ----
save(wildtrax_cleaned, 
     file = paste0("0_data/manual/wildtrax_cleaned_", 
                   Sys.Date(), ".rData"))

