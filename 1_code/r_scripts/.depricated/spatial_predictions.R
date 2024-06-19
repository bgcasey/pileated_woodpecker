# ---
# title: "5_spatial_predictions"
# author: "Brendan Casey"
# created: "2023-09-12"
# ---

#Notes----
# Code for generating predictive maps from the BRT models.

##////////////////////////////////////////////////////////////////

#Setup ----

##Load packages----
library(dismo)
library(raster)
library(gbm)
library(terra)
library(sf)

##load rasters----
# NFIS<-raster::stack("/Volumes/Projects/pileated_woodpecker_lidar/0_data/external/lionel/2_outputs/stackHardBuffer150-565-1000-Austin.tif")
cov_all<-rast("0_data/manual/predictor/raster_mosaics/cov_all.tif")
cov_all<-stacked_rasters


## Crreat latitdude and longitude raster----
### Create lat and lon values for each cell ----
lon <- init(cov_all, 'x')
names(lon)<-"lon"
lat <- init(cov_all, 'y')
names(lat)<-"lat"

cov_all<-c(cov_all, lon, lat)
# # Names of layers to be removed
# layers_to_remove <- c("FirstReturns.H150", "FirstReturns.H565", "FirstReturns.H1000")
# 
# # Remove layers by name
# NFIS <- raster::dropLayer(NFIS, layers_to_remove)
# 
# new_layer_names<- c("N_biomass_150", "N_biomass_565", "N_biomass_1000", 
#                     "N_decid_150", "N_decid_565", "N_decid_1000", 
#                     "N_age_150", "N_age_565", "N_age_1000", 
#                     "N_volume_150", "N_volume_565", "N_volume_1000", 
#                     "N_height_150", "N_height_565", "N_height_1000")

# names(NFIS) <- new_layer_names

## rename to match renamed covariates
# Replace '/' with '_'
names(cov_all) <- gsub("/", "_", names(cov_all))
# Replace '-' with '_'
names(cov_all) <- gsub("-", "_", names(cov_all))
# Replace ' ' with '_'
# names(cov_all) <- gsub(" ", "_", names(cov_all))

##load BRT model----
load("3_output/models/WT/brt_ls_hlc_terrain_canopy_29_2.rData")
# load("3_output/models/WT/brt_ls_29.rData")


##////////////////////////////////////////////////////////////////

# make prediction ----

# There is no raster for year so we'll create a data frame with a constant value to plug into the predict function.
load("0_data/manual/formatted_for_models/bird_cov_cleaned_20231121.rData")
df2<-bird_cov
df2$year<-as.factor(df2$year)
year <- factor('2017', levels = levels(df2$year))
add <- data.frame(year)


p_piwo <- predict(cov_all, brt_ls_hlc_terrain_canopy_29_2, const=add,
                    n.trees=brt_ls_hlc_terrain_canopy_29_2$gbm.call$best.trees, type="response", na.rm=TRUE)


names(p_piwo)<-"PIWO_occ"

writeRaster(p_piwo, file="3_output/predict_rasters/brt_ls_hlc_terrain_canopy_29_2_p_piwo.tif", overwrite=TRUE)

##////////////////////////////////////////////////////////////////

# Plot predictive rasters----
p_piwo<-rast("3_output/predict_rasters/brt_ls_hlc_terrain_canopy_29_2_p_piwo.tif")

# Change crs
a<-project(p_piwo, "epsg:4326")

## crop and mask to study area ----
aoi<-st_read("0_data/external/Alberta/alberta.shp")
aoi_s<-st_simplify(aoi)
aoi_t<-st_transform(aoi_s, crs="epsg:4326" )

buffer_distance <- 1000  # Adjust the buffer distance as needed
aoi_buff <- st_buffer(aoi_t, dist = buffer_distance)

a2<-terra::mask(a, aoi_t)
a3<-terra::crop(a2, aoi_buff)

## set plot parameters ----
plg = list(
  title = "Probility of occupancy",
  title.cex = 0.6,
  cex = 0.7,
  shrink=1
)
pax <- list(retro=TRUE)

## Plot ----
# Set up save 
png(file="3_output/predict_rasters/brt_ls_hlc_terrain_canopy_29_2_p_piwo.png", width=4, height=6, units="in", res=300)

# Plot rasters side by side
# par(mfrow=c(1,2))  # Set up a 1x2 plotting layout
# Plot raster_A
plot(a3, xlim=c(-141,-53),ylim=c(40,85),main="", cex.main=0.6, plg=plg, pax=pax, legend=TRUE)
plot(aoi_t,col = adjustcolor("blue", alpha.f = 0.0), add=TRUE)
# plot(buffered_sf$geometry, add=TRUE)

# Plot raster_B
# plot(b1, main="BRT with LiDAR", cex.main=0.6,plg=plg, pax=pax, legend=TRUE)
dev.off()

##////////////////////////////////////////////////////////////////

# 1. Load necessary libraries
library(terra)  # For spatial data manipulation and prediction
library(parallel)  # For parallel computing
library(progress)
# 2. Load the bootstrapped models and prediction grid
load("3_output/model_results/bootstrap_models.rData")  # Bootstrapped models
prediction_grid <- rast("0_data/spatial/prediction_grid.tif")  # Prediction grid with terra

prediction_grid<-dummy_rasters

# 3. Function to generate spatial predictions from a list of models
generate_spatial_predictions <- function(models_list, prediction_grid) {
  # Initialize an empty list to store prediction rasters
  prediction_rasters <- list()
  
  # Loop through each model in the list
  for (i in seq_along(models_list)) {
    model <- models_list[[i]]
    
    # Generate predictions
    prediction <- predict(prediction_grid, model, type="response")
    
    # Store the prediction raster in the list
    prediction_rasters[[paste("prediction", i, sep = "_")]] <- prediction
    
    prediction_stack <- rast(spatial_predictions)
    mean_raster <- app(prediction_stack, mean)
    sd_raster <- app(prediction_stack, sd)
  }
  
  # Return the list of prediction rasters
  return(list(mean_raster = mean_raster, sd_raster = sd_raster))
}

spatial_predictions <- generate_spatial_predictions(bootstrap_models$models, prediction_grid)



plot# Note: Ensure the directory "3_output/spatial_predictions/" exists or adjust the path as necessary.


test<-predict(prediction_grid, bootstrap_models$models$model_2, type = "response")
test1<-predict(prediction_grid, bootstrap_models$models$model_1, type = "response")

