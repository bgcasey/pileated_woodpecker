# ---
# title: "Get QPAD offsets"
# author: "Brendan Casey"
# created: "September 6, 2023"
# description: >
#   "This script calculates QPAD offsets for Pileated
#   Woodpecker detections. The offsets are joined to the
#   cleaned bird dataframe and saved as
#   'wildtrax_cleaned_piwo_with_offset_<date>.rData' where
#   <date> is the current system date. More information on
#   QPAD (including code, functions, and package
#   information) can be found at
#   https://github.com/borealbirds/qpad-offsets."
# ---

# 1. Setup ----
## 1.1 Load Packages ----
library(maptools) # reading, writing, and manipulating spatial data
library(intrval) # evaluating intervals.
library(raster) # raster manipulation
# remotes::install_github("psolymos/QPAD")
library(QPAD) # package for caclucalting statistical offsets

## 1.2 Load cleaned bird data ----
load("0_data/manual/response/wildtrax_cleaned_piwo_2024-06-13.rData")
dat <- wildtrax_cleaned_piwo
rm(wildtrax_cleaned_piwo)
gc()

## 1.3 Load QPAD v3 estimates ----
load_BAM_QPAD(version = 3)

## 1.4 Read raster data ----
rlcc <- raster("0_data/external/QPAD/lcc.tif")
rd1 <- raster("0_data/external/QPAD/seedgrow.tif")
rtree <- raster("0_data/external/QPAD/tree.tif")
rtz <- raster("0_data/external/QPAD/utcoffset.tif")
crs <- proj4string(rtree)

## 1.5 Source functions ----
source("1_code/r_scripts/functions/qpad_functions.R")

# 2. Define variables ----
## 2.1 Species of interest ----
spp <- "PIWO"

## 2.2 Date and time ----
dt <- dat$date # ISO 8601 in YYYY-MM-DD (0-padded)
tm <- dat$start_time_hhmm # ISO 8601 in hh:mm (24 hr clock, 0-padded)

## 2.3 Spatial coordinates ----
lon <- dat$lon # longitude WGS84 (EPSG: 4326)
lat <- dat$lat # latitude WGS84 (EPSG: 4326)

## 2.4 Point count duration and distance ----
dur <- dat$max_duration # minutes
dis <- dat$max_distance # meters

# 3. Organize predictors ----
x <- make_x(dt, tm, lon, lat, dur, dis, check_xy = FALSE)

# 4. Calculate offsets ----
o <- make_off(spp, x)
str(o)
o <- o %>%
  dplyr::select(offset) %>%
  rename(PIWO_offset=offset)

# 5. Add offsets to bird dataframe ----
wildtrax_cleaned_piwo_with_offset <- cbind(dat, o)

# 6. Save ----
save(wildtrax_cleaned_piwo_with_offset, 
     file=paste0("0_data/manual/response/wildtrax_cleaned_piwo",
                 "_with_offset_", 
                 Sys.Date(), ".rData"))
