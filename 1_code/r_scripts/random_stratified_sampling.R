# ---
# title: "Random Stratified Sampling"
# author: "Brendan Casey"
# created: "2024-02-27"
# description: > 
#   Using random stratified sampling on the PIWO predictive map 
#   to deternine where to do field work to valifat the model.
# ---

# 1. Setup ----
## 1.1 Load packages ----
library(terra)      # rasters
library(sf)         # polygons and points
library(tidyverse)  # data manipulation

## 1.2 Import data ----
r <- rast(paste0("3_output/predict_rasters/",
                 "brt_ls_hlc_terrain_canopy_29_2_p_piwo.tif"))
path <- st_read(paste0("0_data/external/Spatial/CanadianNatural/",
                       "17324_Pathways_GlobalRaymac/Shapefiles/",
                       "17324_ProjectBoundary_nad83_csrs_z12.shp"))

# 2. Classify raster ----
## 2.1 Define breaks and labels for reclassification ----
reclass_matrix <- matrix(c(0, .05, 0,
                           .05, .4, 1, # .05-20 mapped to 1 (poor)
                           .4, .7, 2,  # 20-70 mapped to 2 (moderate)
                           .7, 1, 3),  # 70-100 mapped to 3 (good)
                         byrow=TRUE, ncol=3)

## 2.2 Reclassify the raster ----
reclassified_raster <- classify(r, rcl=reclass_matrix, right=TRUE)

# 3. Crop raster to pipeline path ----
## 3.1 Transform pipeline path CRS ----
path_1 <- st_zm(path)
path_2 <- st_transform(path, crs=st_crs(r))

## 3.2 Crop raster ----
cropped_raster <- crop(reclassified_raster, path_2)

## 3.3 Mask raster ----
r_path <- mask(cropped_raster, path_2)

## 3.4 Inspect values ----
### 3.4.1 Extract and plot values ----
vals <- values(r_path)
non_na_values <- na.omit(vals)
hist(non_na_values, main="Histogram of Non-NA Raster Values", 
     xlab="Value", ylab="Frequency")

# 4. Random sampling cells ----
## 4.1 Create separate masks for each class ----
mask1 <- ifel(r_path == 1, 1, NA)
mask2 <- ifel(r_path == 2, 2, NA)
mask3 <- ifel(r_path == 3, 3, NA)

## 4.2 Sample separately for each class ----
samples1 <- spatSample(mask1, size=6, method="random", 
                       as.points=TRUE, values=TRUE, na.rm=TRUE)
samples2 <- spatSample(mask2, size=10, method="random", 
                       as.points=TRUE, values=TRUE, na.rm=TRUE)
samples3 <- spatSample(mask3, size=4, method="random", 
                       as.points=TRUE, values=TRUE, na.rm=TRUE)

## 4.3 Combine samples into single dataframe ----
samples <- rbind(samples1, samples2, samples3)
names(samples) <- "class"

## 4.4 Convert to sf object and check distribution ----
samples_1 <- st_as_sf(samples) %>%
  mutate(index=row_number()) %>%
  select(index, class, geometry)
hist(samples_1$class)

# 5. Convert point locations to 1 ha squares ----
## 5.1 Function to create a square ----
create_square <- function(center, radius) {
  half_side <- radius
  x <- center[1]
  y <- center[2]
  vertices <- matrix(nrow = 5, ncol = 2,
                     data = c(x - half_side, y - half_side,
                              x + half_side, y - half_side,
                              x + half_side, y + half_side,
                              x - half_side, y + half_side,
                              x - half_side, y - half_side), 
                     byrow = TRUE)
  square <- st_polygon(list(vertices))
  return(square)
}

## 5.2 Create squares for each point ----
squares <- lapply(1:nrow(samples_1), function(i) {
  center <- st_coordinates(samples_1[i, ])
  radius <- 50
  square <- create_square(center, radius)
  return(square)
})

## 5.3 Combine squares into a MULTIPOINT sf object and save ----
squares_sfc <- st_sfc(squares, crs = st_crs(samples_1))
samples_square <- samples_1
samples_square$geometry <- squares_sfc

st_write(samples_square, "3_output/data/samples_square.shp")
st_write(samples_1, "3_output/data/samples_1.shp")