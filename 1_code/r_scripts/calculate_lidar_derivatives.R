# ---
# title: "Calculate LiDAR Derivitives"
# author: "Brendan Casey"
# created: "2023-10-30"
# description: "Code to process LAS/LAZ files, filter ground points, and calculate custom LiDAR dervitives using the lidR package."
# ---

#Setup ----

##Load packages----
library(lidR)
library(sf)
# library(moments) # for calcluating skewness, kurtosis, and entropy

##Import data----
las<-readLAS("0_data/external/Spatial/ABMI/LiDAR/LAZ/373_6165.laz")

##////////////////////////////////////////////////////////////////

#Clip points to an area of interest----

##Import or create AOI----

## Here I used the las to create an aoi as an example
bb<-st_bbox(las)
aoi<-st_as_sfc(st_bbox(bb))
aoi2<-st_buffer(aoi, -450) #created a small bounding box to make the following test code run faster
aoi_bb<-st_bbox(aoi2)

## Crop las by aoi----
las2<-clip_roi(las, aoi2)
plot(las2, size = 15, bg = "white")


##////////////////////////////////////////////////////////////////

# Normalize las----
## Need to normalize the las so that z coordinates represent height above ground and not elevation

## Create a dtm----
## this is a raster surface representing bare earth/ground
dtm <- rasterize_terrain(las2, 1, knnidw())
plot(dtm, col = gray(1:50/50))

## Normalize point cloud by subtracting DEM----
nlas <- las2 - dtm
plot(nlas, size = 8, bg = "white")

## filter out ground points and noise----

## View point classifications
## Can get class definitions from the LiDAR documentation
unique(las2$Classification)

## In this case we are only keeping vegetation (class=1)
nlas_v<-filter_poi(nlas, Classification == 1)
## Check result
unique(nlas_v$Classification)


##////////////////////////////////////////////////////////////////

# generate metrics ----

## standard metrics ----
## Creates a SpatRaster with standard metrics
met<-pixel_metrics(nlas_v, func = .stdmetrics_z, res=5)

## custom metrics ----

### Define functions for metrics----
f<-function(z) {
  list(
    zmean = mean(z), # mean height
    zsd = sd(z), # standard deviation of height
    # zcv = sd(z)/mean(z)*100, 
    zmax = max(z), # max height
    # zskew = skewness(z),
    # zkurt = kurtosis(z),
    # zentropy = entropy(z),
    zq50 = quantile(z, 0.5), #50th percentile of height
    zq95 = quantile(z, 0.95), #95th percentile of height
    pzabovezmean = sum(z>mean(z))/length(z)*100, # percent of height returns above mean height
    pzabovez3 = sum(z>3)/length(z)*100, # percent of returns greater than 3 m
    pz_0_to_1 = sum(0<z & z<=1)/length(z)*100, #proportion between 0 and 1 m
    # pz_0_to_3 = sum(0<=z & z<=3)/length(z)*100, #proportion between 0 and 3 m
    pz_1_to_3 = sum(1<z & z<=3)/length(z)*100 # proportion between 1 and 3 m
    # pz_3_to_5 = sum(3<z & z<=5)/length(z)*100 # proportion between 3 and 5 m
  )
}

### Calculate custum metrics----
cust_met <- pixel_metrics(nlas_v, func = ~f(Z), res=1)

