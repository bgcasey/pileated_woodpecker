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

##load rasters----
NFIS<-raster::stack("/Volumes/Projects/pileated_woodpecker_lidar/0_data/external/lionel/2_outputs/stackHardBuffer150-565-1000-Austin.tif")

# Names of layers to be removed
layers_to_remove <- c("FirstReturns.H150", "FirstReturns.H565", "FirstReturns.H1000")

# Remove layers by name
NFIS <- raster::dropLayer(NFIS, layers_to_remove)

new_layer_names<- c("N_biomass_150", "N_biomass_565", "N_biomass_1000", 
                    "N_decid_150", "N_decid_565", "N_decid_1000", 
                    "N_age_150", "N_age_565", "N_age_1000", 
                    "N_volume_150", "N_volume_565", "N_volume_1000", 
                    "N_height_150", "N_height_565", "N_height_1000")

names(NFIS) <- new_layer_names



##load BRT model----
load("2_pipeline/store/models/brt_PIWO_tuned_2.rData")



##////////////////////////////////////////////////////////////////

# make prediction ----
p_piwo <- predict(NFIS, brt_PIWO_tuned_2,
                    n.trees=brt_PIWO_tuned_2$gbm.call$best.trees, type="response")

names(p_piwo)<-"PIWO_occ"

writeRaster(p_summer, file="3_output/offset_rasters/meanTemp_summer_offset.tif", format='GTiff', overwrite=TRUE)

##////////////////////////////////////////////////////////////////

