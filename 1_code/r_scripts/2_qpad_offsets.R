# ---
# title: "Get QPAD offsets"
# author: "Brendan Casey"
# created: "September 6, 2023"
# ---

#Notes----
#Code to generate QPAD offsets. From https://github.com/borealbirds/qpad-offsets

#Setup----
##Load Packages----
library(maptools)
library(intrval)
library(raster)

remotes::install_github("psolymos/QPAD")
library(QPAD)

##load bird data
load("0_data/manual/response/bird_station_data.Rdata")

##load QPAD v3 estimates----
load_BAM_QPAD(version = 3)

##read raster data----
rlcc <- raster("0_data/external/QPAD/lcc.tif")
rd1 <- raster("0_data/external/QPAD/seedgrow.tif")
rtree <- raster("0_data/external/QPAD/tree.tif")
rtz <- raster("0_data/external/QPAD/utcoffset.tif")
crs <- proj4string(rtree)

##Source functions----
source("1_code/r_scripts/functions.R")

#Define variables----

## species of interest
spp <- "PIWO"

dat<-visit_ab

#Calculate offsets----
o <- make_off(spp, dat)
str(o)
o <- o%>%
  select(offset)%>%
  rename(PIWO_offset=offset)

visit_ab_with_offset<-cbind(visit_ab, o)

#add offsets to bird dataframe----
visit_ab_with_offset<-cbind(visit_ab, o)

#save----
save(visit_ab_with_offset, bird_ab, species,  file="0_data/manual/response/bird_station_data_with_offset.Rdata")









