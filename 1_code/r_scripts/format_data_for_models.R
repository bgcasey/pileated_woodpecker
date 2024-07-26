# ---
# title: "Format data for models"
# author: "Brendan Casey"
# created: "2024-07-25"
# description: 
#   "This script performs [describe the main purpose of the script].
#   It includes steps to [briefly describe the main steps or processes].
#   The script uses [briefly describe data or object inputs]
#   The final output is [describe the final output]."
# ---

# 1. Setup ----

## 1.1 Load packages ----
# Include comments for what the packages are used for
# Example:
library(tidyverse) # data manipulation and visualization

## 1.2 Import data ----
load("0_data/manual/response/wildtrax_cleaned_piwo_with_offset_2024-06-13.rData")
load("0_data/manual/predictor/xy_scanfi.rData")
ss_canopy_mean_500 <- read_csv("0_data/manual/predictor/ss_canopy_mean_500.csv") %>%
  select(-c(`system:index`, .geo))
ss_ls_mean_500 <- read_csv("0_data/manual/predictor/ss_ls_mean_500.csv")%>%
  select(-c(`system:index`, .geo))
ss_terrain_first_00 <- read_csv("0_data/manual/predictor/ss_terrain_first_00.csv")%>%
  select(-c(`system:index`, .geo))
ss_s2_mean_500 <- read_csv("0_data/manual/predictor/ss_s2_mean_500.csv")%>%
  select(-c(`system:index`, .geo))

# 2. [heading] ----
# This section [describe the purpose of this section].
# The section uses [briefly describe data or object inputs].
# It includes steps to [briefly describe the main steps or processes].
# The section produces [describe the section's output].

# Perform left joins based on the 'location' field
joined_data <- ss_canopy_mean_500 %>%
  left_join(ss_ls_mean_500, by = "location") %>%
  left_join(ss_terrain_first_00, by = "location")



##////////////////////////////////////////////////////////////////
#Combine covariates into a single dataframe----
ss_cov<-ss_xy_lc_150%>%
  left_join(ss_xy_lc_565)%>%
  left_join(ss_xy_lc_1000)%>%
  left_join(ss_xy_ts_150)%>%
  left_join(ss_xy_ts_565)%>%
  left_join(ss_xy_ts_1000)%>%
  left_join(ss_xy_fixed)%>%
  # dplyr::select(-c(contains('TPI')))%>%
  left_join(ss_xy_fixed_pointLevel)%>%
  left_join(ss_xy_nfis)
  
# Replace '/' with '_'
colnames(ss_cov) <- gsub("/", "_", colnames(ss_cov))

# Replace '-' with '_'
colnames(ss_cov) <- gsub("-", "_", colnames(ss_cov))

# Replace ' ' with '_'
colnames(ss_cov) <- gsub(" ", "_", colnames(ss_cov))

##////////////////////////////////////////////////////////////////
# Join bird data, offsets, and covariates----
## Join by year and location
bird_cov<-bird_ab%>%
  mutate(date=as.Date(date))%>%
  group_by(location, date)%>%
  dplyr::summarise(across(14, sum))%>%
  ungroup()%>%
  left_join(visit_ab_with_offset)%>%
  # left_join(ss_GOA_lidar)%>%
  left_join(ss_cov, by = join_by(location, year))%>%
  dplyr::select(-c(id, project, observer, buffer, distanceMethod, durationMethod, TM, JDAY, hssr, TSSR, province, country, seedgrow, MAXDIS, MAXDUR))%>%
  dplyr::select(-c(sensor, date, DSLS, bcr, TREE, LCC2, LCC4))

save(bird_cov, file=paste0("0_data/manual/formatted_for_models/bird_cov_cleaned_", format(Sys.Date(), "%Y%m%d"), ".rData"))
write_csv(bird_cov, file=paste0("0_data/manual/formatted_for_models/bird_cov_cleaned_", format(Sys.Date(), "%Y%m%d"), ".csv"))

##////////////////////////////////////////////////////////////////
# Prepare data for models ----
# Should be one response column and the rest variable columns. Should be in a dataframe. 

#create an offset dataframe
o <- as.data.frame(df1$PIWO_offset)%>%rename(PIWO_offset=`df1$PIWO_offset`)
o <- df1$PIWO_offset

save(offset, bd_cov,
     "0_data/manual/formatted_for_models/data_for_models.rData")