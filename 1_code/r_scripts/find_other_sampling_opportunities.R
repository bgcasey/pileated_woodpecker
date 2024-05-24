# ---
# title: "Other Sampling opportunities"
# author: "Brendan Casey"
# created: "2024-02-27"
# description: "Determine other opportunuties for PIWO fieldwork"
# ---

#Setup ----

##Load packages----
library(terra) #rasters
library(sf) #polygons and points
library(tidyverse) #data manipulation
library(tmap)

##Import data----
r<-rast("~/Library/CloudStorage/GoogleDrive-bgcasey@ualberta.ca/My Drive/1_Projects/pileated_woodpecker_lidar/3_output/predict_rasters/brt_ls_hlc_terrain_canopy_29_2_p_piwo.tif")

### Apporv's planned points----
mgp <- read_csv("0_data/external/apoorv_Final_Mega_Grid_points-55_57.csv")%>%
  rename(seismic_line="Seismic Line")

##////////////////////////////////////////////////////////////////
#Classify raster ----
## Define breaks and labels for reclassification----
reclass_matrix <- matrix(c(0, .05, 0,
                           .05, .4, 1,  # .05-20 mapped to 1 (poor)
                           .4, .7, 2,  # 20-80 mapped to 2 (moderate)
                           .7, 1, 3  # 80-100 mapped to 3 (good)
), byrow=TRUE, ncol=3)

## Reclassify the raster----
reclassified_raster <- classify(r, rcl=reclass_matrix, right=TRUE)
writeRaster(reclassified_raster, file="3_output/rasters/p_piwo_reclass.tif")


##////////////////////////////////////////////////////////////////
#Convert point locations to spatial points----
  
mgp_4326<-mgp%>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=FALSE)

## Create a bounding box around points----
bb<-st_bbox(mgp_4326)
aoi<-st_as_sfc(st_bbox(bb))
aoi2<-st_buffer(aoi, 2000) #created a small bounding box to make the following test code run faster
aoi_bb<-st_bbox(aoi2)
aoi_bb2<-st_as_sfc(st_bbox(aoi_bb))
aoi_bb2_t<-st_transform(aoi_bb2, crs=st_crs(reclassified_raster))

##load alberta boundary
alberta<-st_read("0_data/external/Alberta/Alberta.shp")

mainmap<-
  # overlay difference polygon to mask content outside
  tm_shape(alberta)+
  tm_borders(lwd=.9, col="black")+
  tm_shape(aoi_bb2)+
  tm_borders(lwd=.9, col="red")+
  tm_shape(mgp_4326)+
  tm_symbols(shape = 1, alpha = .3, size = .2, col = "red")+
  tm_scale_bar(position=c("left", "BOTTOM"), text.color = "black", color.light="lightgrey")+
  tm_graticules(lines=FALSE)

##////////////////////////////////////////////////////////////////

#Extract raster values to points----
reclassified_raster<-project(reclassified_raster, crs=st_crs(mgp_4326))

mgp_4326_2<-terra::extract(reclassified_raster, mgp_4326, fun=max, ID=TRUE, bind=TRUE)%>%
  as.data.frame()%>%
  rename(class=PIWO_occ)
  

# #to a buffer
# mgp_4326_buff<-st_buffer(mgp_4326, 10000)
# mgp_4326_buff_2<-terra::extract(reclassified_raster, mgp_4326_buff, max, na.rm=TRUE)%>%
#   cbind(mgp_4326_buff)

## Identify nearest pixels
#filter pixels
cropped_raster<-crop(reclassified_raster, aoi_bb2_t)


## Nearest class 2 points---
#convert pixels to points
cropped_raster2<-cropped_raster
cropped_raster2[cropped_raster2 <= 1]<-NA
cropped_raster2[cropped_raster2 > 2]<-NA
cropped_raster2[is.nan(cropped_raster2)]<-NA
num_pixels <- sum(values(cropped_raster2) == 2, na.rm = TRUE)
samples_2 <- spatSample(cropped_raster2, size=num_pixels, method="random", as.points=TRUE, values=TRUE, na.rm=TRUE)
names(samples_2)<-"class"

samples_2<-st_as_sf(samples_2)%>%
  mutate(index=row_number())%>%
  select(index, class, geometry)%>%
  st_transform(crs=st_crs(mgp_4326))
  
# Find the index of the nearest point in 'pts' for each point in 'xy'
nearest_indices <- st_nearest_feature(mgp_4326, samples_2)

# Use the indices to extract the nearest points from 'pts'
nearest_class_2_points <- samples_2[nearest_indices, ]%>%
  dplyr::mutate(nearest_2_long = sf::st_coordinates(.)[,1],
                nearest_2_lat = sf::st_coordinates(.)[,2])%>%
  rename(nearest_2_class=class)%>%
  as.data.frame()%>%
  select(nearest_2_class, nearest_2_lat, nearest_2_long)

new_class_2_points<-mgp_4326_2%>%
  cbind(nearest_class_2_points)

## Nearest class 3 points---
#convert pixels to points
cropped_raster3<-cropped_raster
cropped_raster3[cropped_raster3 <= 2]<-NA
cropped_raster3[is.nan(cropped_raster3)]<-NA

num_pixels <- sum(values(cropped_raster3) == 3, na.rm = TRUE)
samples_3 <- spatSample(cropped_raster3, size=num_pixels, method="random", as.points=TRUE, values=TRUE, na.rm=TRUE)
names(samples_3)<-"class"

samples_3<-st_as_sf(samples_3)%>%
  mutate(index=row_number())%>%
  select(index, class, geometry)%>%
  st_transform(crs=st_crs(mgp_4326))

# Find the index of the nearest point in 'pts' for each point in 'xy'
nearest_indices <- st_nearest_feature(mgp_4326, samples_3)

# Use the indices to extract the nearest points from 'pts'
nearest_class_3_points <- samples_3[nearest_indices, ]%>%
  dplyr::mutate(nearest_3_long = sf::st_coordinates(.)[,1],
                nearest_3_lat = sf::st_coordinates(.)[,2])%>%
  rename(nearest_3_class=class)%>%
  as.data.frame()%>%
  select(nearest_3_class, nearest_3_lat, nearest_3_long)

new_points<-new_class_2_points%>%
  cbind(nearest_class_3_points)

write_csv(new_points, file="3_output/tables/new_points.csv")


