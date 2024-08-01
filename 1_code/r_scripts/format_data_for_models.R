# ---
# title: "Format data for models"
# author: "Brendan Casey"
# created: "2024-07-25"
# description:
#   "This script prepares data for boosted regression trees. It
#   includes steps to load, clean, and merge response and predictor
#   variables.The script uses various CSV files and .RData objects as
#   inputs and outputs a formatted table ready for modelling."
# ---

# 1. Setup ----

## 1.1 Load packages ----
library(tidyverse) # data manipulation and visualization

## 1.2 Import data ----
# Function to read and process GEE csv's
read_and_process <- function(file_path, digits = 5) {
  columns_to_remove <- c(
    "month", "start_date", "end_date", "lat",
    "lon"
  )

  read_csv(file_path) %>%
    select(-c(`system:index`, .geo)) %>%
    mutate(across(where(is.character), trimws)) %>%
    mutate(across(where(is.numeric), ~ round(., digits))) %>%
    select(-any_of(columns_to_remove)) %>%
    filter(location != "WLNP-2-3" & location != "BBSAB:3:STOP44") %>%
    distinct() %>%
    select(location, everything())
}

# Import data
load("0_data/manual/response/wildtrax_cleaned_piwo_with_offset_2024-06-13.rData")
load("0_data/manual/predictor/xy_scanfi.rData")
ss_canopy_mean_500 <- read_and_process(
  "0_data/manual/predictor/ss_canopy_mean_500.csv"
)
ss_ls_mean_500 <- read_and_process(
  "0_data/manual/predictor/ss_ls_mean_500.csv"
)
ss_terrain_first_00 <- read_and_process(
  "0_data/manual/predictor/ss_terrain_first_00.csv"
)
ss_s2_mean_500 <- read_and_process(
  "0_data/manual/predictor/ss_s2_mean_500.csv"
)

# 2. Tidy response data frame ----
# Clean and summarize the response data
ss_dat <- wildtrax_cleaned_piwo_with_offset %>%
  group_by(organization, project_id, location, survey_type, date) %>%
  mutate(PIWO_abund = sum(PIWO, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(PIWO_occ = ifelse(PIWO_abund >= 1, 1, 0)) %>%
  dplyr::select(
    location, date, year, month,
    survey_effort, PIWO_occ, PIWO_offset
  ) %>%
  distinct()

# 3. Merge predictor datasets ----
# Combine multiple predictor tables into one. Clean and format
# columns.

cov <- ss_ls_mean_500 %>%
  left_join(ss_s2_mean_500) %>%
  left_join(xy_scanfi) %>%
  left_join(ss_canopy_mean_500) %>%
  left_join(ss_terrain_first_00) %>%
  dplyr::select(-hasAllBands, -lat, -lon) %>%
  select(location, year, everything()) %>%
  mutate(
    # Create biomass_mean_500 based on the closest year
    biomass_mean_500 = case_when(
      abs(year - 1985) <= 2 ~ biomass_1985_mean_500,
      abs(year - 1990) <= 2 ~ biomass_1990_mean_500,
      abs(year - 1995) <= 2 ~ biomass_1995_mean_500,
      abs(year - 2000) <= 2 ~ biomass_2000_mean_500,
      abs(year - 2005) <= 2 ~ biomass_2005_mean_500,
      abs(year - 2010) <= 2 ~ biomass_2010_mean_500,
      abs(year - 2015) <= 2 ~ biomass_2015_mean_500,
      abs(year - 2020) <= 2 ~ biomass_2020_mean_500,
      TRUE ~ NA_real_
    ),
    # Create closure_mean_500 based on the closest year
    closure_mean_500 = case_when(
      abs(year - 1985) <= 2 ~ closure_1985_mean_500,
      abs(year - 1990) <= 2 ~ closure_1990_mean_500,
      abs(year - 1995) <= 2 ~ closure_1995_mean_500,
      abs(year - 2000) <= 2 ~ closure_2000_mean_500,
      abs(year - 2005) <= 2 ~ closure_2005_mean_500,
      abs(year - 2010) <= 2 ~ closure_2010_mean_500,
      abs(year - 2015) <= 2 ~ closure_2015_mean_500,
      abs(year - 2020) <= 2 ~ closure_2020_mean_500,
      TRUE ~ NA_real_
    ),
    # Create height_mean_500 based on the closest year
    height_mean_500 = case_when(
      abs(year - 1985) <= 2 ~ height_1985_mean_500,
      abs(year - 1990) <= 2 ~ height_1990_mean_500,
      abs(year - 1995) <= 2 ~ height_1995_mean_500,
      abs(year - 2000) <= 2 ~ height_2000_mean_500,
      abs(year - 2005) <= 2 ~ height_2005_mean_500,
      abs(year - 2010) <= 2 ~ height_2010_mean_500,
      abs(year - 2015) <= 2 ~ height_2015_mean_500,
      abs(year - 2020) <= 2 ~ height_2020_mean_500,
      TRUE ~ NA_real_
    ),
    # Create prcB_mean_500 based on the closest year
    prcB_mean_500 = case_when(
      abs(year - 1985) <= 2 ~ prcB_1985_mean_500,
      abs(year - 1990) <= 2 ~ prcB_1990_mean_500,
      abs(year - 1995) <= 2 ~ prcB_1995_mean_500,
      abs(year - 2000) <= 2 ~ prcB_2000_mean_500,
      abs(year - 2005) <= 2 ~ prcB_2005_mean_500,
      abs(year - 2010) <= 2 ~ prcB_2010_mean_500,
      abs(year - 2015) <= 2 ~ prcB_2015_mean_500,
      abs(year - 2020) <= 2 ~ prcB_2020_mean_500,
      TRUE ~ NA_real_
    )
  ) %>%
  # Remove columns that include a year in the name
  select(-matches("\\d{4}"))

# 4. Prepare data for models ----
# Merge cleaned response data with predictor data, select relevant
# columns for modeling, and save.

data_brt <- ss_dat %>%
  inner_join(cov) %>%
  select(
    PIWO_occ, PIWO_offset, survey_effort, everything(),
    -location, -date, -month, -year
  ) %>%
  as.data.frame() # needs to be a dataframe for dismo::gbm.step()

save(data_brt,
  file = "0_data/manual/formatted_for_models/data_for_models.rData"
)
