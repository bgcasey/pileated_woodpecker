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
         detection_distance = "0m-INF",
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
## 3.1 Join with aves data to keep only bird records ----
w <- wildtrax_all %>% 
  semi_join(aves)

## 3.2 Remove single unsuitable projects ----
w <- w %>%
  group_by(project_id) %>%
  filter((n_distinct(species_code) > 2) | 
           (n_distinct(species_code) < 2 & species_code=="PIWO")) %>%
  ungroup() %>%
  distinct() %>%
  anti_join(instructions)

## 3.3 Modify fields add new temporal fields ----
w <- w %>%
  mutate(location_buffer_m = ifelse(!is.finite(location_buffer_m), 
                                    0, location_buffer_m),
         ordinalDay = yday(date),
         year = year(date),
         month = month(date),
         start_time = hour(date) + minute(date) / 60,
         start_time_hhmm = format(
           as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S"), 
           format = "%H:%M"),
         date = ymd(str_sub(date, 1, 10)),
         tz = tz_lookup_coords(latitude, longitude,
                               method = "accurate"),
         lat = latitude,
         lon = longitude,
         max_distance = str_remove(survey_distance_method, "-ARU"),
         max_distance = str_extract(max_distance, "(?<=-)[^-]+$"),
         max_distance = ifelse(str_detect(max_distance, "m"), 
                               str_remove(max_distance, "m"), 
                               max_distance),
         max_distance = ifelse(max_distance == "INF", Inf, 
                               as.numeric(max_distance))
  )

## 3.4 Filter data ----
w <- w %>%
  filter(
    ordinalDay > quantile(ordinalDay, 0.005),
    ordinalDay < quantile(ordinalDay, 0.995),
    !str_sub(date, -8, -1) %in% c("00:00:01", "00:01:01"),
    !is.na(date),
    project != "BAM-BBS",
    organization != "BU-TRAINING",
    organization != "ABMI",
    !is.na(latitude),
    latitude > 0,
    longitude < 0,
    year(date) > 1900,
    start_time > 2 & start_time < 12,
    location_buffer_m == 0
  )

## 3.5 Calculate survey effort (n_surveys * max_duration / 60) ----
w <- w %>%
  select(project_id, survey_type, location, 
         survey_duration_method, survey_distance_method, 
         year, date, max_duration) %>%
  distinct() %>%
  group_by(project_id, survey_type, location, 
           survey_duration_method, survey_distance_method, year) %>%
  mutate(n_surveys = n()) %>%
  ungroup() %>%
  mutate(survey_effort = n_surveys * max_duration / 60) %>%
  full_join(w)

## 3.6 Transform spatial coordinates ----
w <- w %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = 3978) %>%
  mutate(x_3978 = sf::st_coordinates(.)[,1],
         y_3978 = sf::st_coordinates(.)[,2]) %>%
  st_set_geometry(NULL)

  
## 3.7 Calculate time since sunrise ----
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

# save(w, file="2_pipeline/tmp/w.rData")

# 4. Select and order columns ----
colnms <- c("organization", "project", "project_id", "survey_type", 
            "location", "location_buffer_m", "lon", "lat", "x_3978", 
            "y_3978", "date", "ordinalDay", "year", "month", 
            "start_time", "start_time_hhmm", "tz", "sunrise", "hssr", 
            "task_method", "survey_distance_method", "max_distance", 
            "survey_duration_method", "max_duration", "n_surveys", 
            "survey_effort", "detection_time", "detection_distance", 
            "species_code", "individual_order", "individual_count", 
            "vocalization") 

wildtrax_cleaned <- w %>%
  dplyr::select(all_of(colnms))

rm(w)
gc()

# 5. Convert to wide format ----
wildtrax_cleaned_wide<-wildtrax_cleaned %>%
  mutate(individual_count = as.numeric(individual_count)) %>%
  pivot_wider(names_from = species_code, 
              values_from = individual_count, 
              values_fn = list(individual_count = sum)) %>%
  mutate(PIWO = replace_na(PIWO, 0)) 

## 5.1 Keep species of interest ----
wildtrax_cleaned_piwo <- wildtrax_cleaned_wide %>%
  dplyr::select(all_of(intersect(colnms, 
                                 names(wildtrax_cleaned_wide))), 
                "PIWO")

# 6. Save ----
save(wildtrax_cleaned, 
     file = paste0("0_data/manual/response/wildtrax_cleaned_", 
                   Sys.Date(), ".rData"))

save(wildtrax_cleaned_piwo, 
     file = paste0("0_data/manual/response/wildtrax_cleaned_piwo_", 
                   Sys.Date(), ".rData"))
