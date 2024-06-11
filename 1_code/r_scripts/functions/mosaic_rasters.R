# ---
# title: "Mosaic rasters downloaded from earth engine
# author: "Brendan Casey"
# created: "2023-12-05"
# ---

#Setup----
##Load Packages----
library(raster)
library(terra)
library(tidyverse)
library(sf)
library(foreach)

## set temp directory ----
## Set temp directory to an external drive to free up harddrive space
rasterOptions(tmpdir=file.path("../r_tmp")) 
terraOptions(tempdir=file.path("../r_tmp")) 


# Mosaic rasters----
### Load study area----
# study_area<-st_read("0_data/external/Alberta/alberta.shp")

### List directories----
path_name<-"~/Google_Drive/My Drive/PIWO_raster_neighborhood (1)/"
bl<-list.dirs(path=path_name, full.names = F, recursive = F)


fl <- list.files(path=paste0(path_name, bl[1]), pattern = '*.tif$', recursive = T, full.names = T)
rsrc <- terra::sprc(fl)
m <- terra::mosaic(rsrc, fun="mean")
writeRaster(m, filename=paste0("0_data/manual/predictor/raster_mosaics/all_fixed_focalAll_resampled.tif"),  overwrite=T)

fl <- list.files(path=paste0(path_name, bl[2]), pattern = '*.tif$', recursive = T, full.names = T)
rsrc <- terra::sprc(fl)
m <- terra::mosaic(rsrc, fun="mean")
writeRaster(m, filename=paste0("0_data/manual/predictor/raster_mosaics/all_ts_1000.tif"),  overwrite=T)

fl <- list.files(path=paste0(path_name, bl[3]), pattern = '*.tif$', recursive = T, full.names = T)
rsrc <- terra::sprc(fl)
m <- terra::mosaic(rsrc, fun="mean")
writeRaster(m, filename=paste0("0_data/manual/predictor/raster_mosaics/all_ts_150.tif"),  overwrite=T)

fl <- list.files(path=paste0(path_name, bl[4]), pattern = '*.tif$', recursive = T, full.names = T)
rsrc <- terra::sprc(fl)
m <- terra::mosaic(rsrc, fun="mean")
writeRaster(m, filename=paste0("0_data/manual/predictor/raster_mosaics/all_ts_565.tif"),  overwrite=T)


##/////////////////////////////////////////////

# Stack rasters----


## Read each raster----
raster1 <- rast("0_data/manual/predictor/raster_mosaics/all_ts_150.tif")
raster2 <- rast("0_data/manual/predictor/raster_mosaics/all_ts_565.tif")
raster3 <- rast("0_data/manual/predictor/raster_mosaics/all_ts_1000.tif")
raster4 <-rast("0_data/manual/predictor/raster_mosaics/all_fixed_focalAll_resampled.tif")
raster5<-rast("~/Google_Drive/My Drive/PIWO_raster_neighborhood/lc_150_2019.tif")
raster6<-rast("~/Google_Drive/My Drive/PIWO_raster_neighborhood/lc_565_2019.tif")
raster7<-rast("~/Google_Drive/My Drive/PIWO_raster_neighborhood/lc_1000_2019.tif")
raster8<-rast("~/Google_Drive/My Drive/PIWO_raster_neighborhood/all_fixed_2.tif")

## Stack the rasters----
stacked_rasters<-c(raster1, raster2, raster3, raster4, raster5, raster6, raster7, raster8)

##Save the stacked raster to a new file----
writeRaster(stacked_rasters, filename = "0_data/manual/predictor/raster_mosaics/cov_all.tif", overwrite=T)






