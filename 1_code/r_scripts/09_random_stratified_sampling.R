# ---
# title: "Random Stratified Sampling"
# author: "Brendan Casey"
# created: "2024-02-27"
# description: "Select site locations using random stratified sampling"
# ---

#Setup ----

##Load packages----
library(terra) #rasters
library(sf) #polygons and points
library(tidyverse) #data manipulation

##Import data----
r<-rast("~/Library/CloudStorage/GoogleDrive-bgcasey@ualberta.ca/My Drive/1_Projects/pileated_woodpecker/3_output/predict_rasters/brt_ls_hlc_terrain_canopy_29_2_p_piwo.tif")

#pipeline path
path<-st_read("~/Library/CloudStorage/GoogleDrive-bgcasey@ualberta.ca/My Drive/1_Projects/pileated_woodpecker/0_data/external/Spatial/CanadianNatural/17324_Pathways_GlobalRaymac/Shapefiles/17324_ProjectBoundary_nad83_csrs_z12.shp")
# horizon<-st_read("0_data/external/JCR_PIWOAssessment/JCR_PIWOAssessmentArea.shp")
##////////////////////////////////////////////////////////////////
# #Resample raster to 100x100 ----
# 
# ext <- res(r)  # Get the cell size of the raster
# r2<-aggregate(r, fact=2, method="mean")
# r3<-disagg(r2, fact=3, method="bilinear")
# 
# #check cell size
# ext<-r3

##////////////////////////////////////////////////////////////////
#Classify raster ----
## Define breaks and labels for reclassification----
reclass_matrix <- matrix(c(0, .05, 0,
                          .05, .4, 1,  # .05-20 mapped to 1 (poor)
                           .4, .7, 2,  # 20-70 mapped to 2 (moderate)
                           .7, 1, 3  # 70-100 mapped to 3 (good)
), byrow=TRUE, ncol=3)

## Reclassify the raster
reclassified_raster <- classify(r, rcl=reclass_matrix, right=TRUE)

# writeRaster(reclassified_raster, file="3_output/predict_rasters/reclassified_raster.tif", overwrite=TRUE)

##////////////////////////////////////////////////////////////////

# Crop raster to pipeline path----
## transform pipleline path CRS----
# transform pipleline path to match the crs of the predictive raster
path_1<-st_zm(path)
path_2<-st_transform(path, crs=st_crs(r))

# path_1<-st_zm(horizon)
# path_2<-st_transform(horizon, crs=st_crs(r))

## Crop raster----
cropped_raster<-crop(reclassified_raster, path_2)

## Mask raster----
r_path<-mask(cropped_raster, path_2)

## Inspect values----
### Extract values from the raster----
vals <- values(r_path)

### Remove NA values----
non_na_values <- na.omit(vals)

### Plot the histogram of non-NA values----
hist(non_na_values, main="Histogram of Non-NA Raster Values", xlab="Value", ylab="Frequency")

##////////////////////////////////////////////////////////////////
# Random sampling cells----

## Create separate masks for each class----
mask1 <- ifel(r_path == 1, 1, NA)
mask2 <- ifel(r_path == 2, 2, NA)
mask3 <- ifel(r_path == 3, 3, NA)

## Sample separately for each class----
samples1 <- spatSample(mask1, size=6, method="random", as.points=TRUE, values=TRUE, na.rm=TRUE)
samples2 <- spatSample(mask2, size=10, method="random", as.points=TRUE, values=TRUE, na.rm=TRUE)
samples3 <- spatSample(mask3, size=4, method="random", as.points=TRUE, values=TRUE, na.rm=TRUE)

## Combine samples into single dataframe----
samples<-rbind(samples1, samples2, samples3)
names(samples)<-"class"

## Convert sf object---
samples_1<-st_as_sf(samples)%>%
  mutate(index=row_number())%>%
  select(index, class, geometry)

## Check sampling distribution----
hist(samples_1$class)

##////////////////////////////////////////////////////////////////
# Convert point locations to 1 ha squares----

## Function to create a square from a center point and radius----
create_square <- function(center, radius) {
  # Calculate the half side length of the square (which is the radius of the circle)
  half_side <- radius
  
  # Coordinates of the center
  x <- center[1]
  y <- center[2]
  
  # Calculate the vertices of the square
  vertices <- matrix(nrow = 5, ncol = 2,
                     data = c(x - half_side, y - half_side,
                              x + half_side, y - half_side,
                              x + half_side, y + half_side,
                              x - half_side, y + half_side,
                              x - half_side, y - half_side), byrow = TRUE)
  
  # Create an sf polygon
  square <- st_polygon(list(vertices))
  return(square)
}

## Create square polygons for each point----
squares <- lapply(1:nrow(samples_1), function(i) {
  center <- st_coordinates(samples_1[i, ])
  radius <- 50
  square <- create_square(center, radius)
  return(square)
})

## Combine squares into a MULTIPOINT sf object----
squares_sfc <- st_sfc(squares, crs = st_crs(samples_1))
samples_square<-samples_1
samples_square$geometry <- squares_sfc 

## Save as shapefile----
st_write(samples_square, "3_output/data/samples_square.shp")
st_write(samples_1, "3_output/data/samples_1.shp")


# st_write(samples_square, "3_output/data/jcr_square.shp")
# st_write(samples_1, "3_output/data/jcr_points.shp")


test<-st_read("3_output/data/jcr_mine_points_v2/jcr_squares.shp")
