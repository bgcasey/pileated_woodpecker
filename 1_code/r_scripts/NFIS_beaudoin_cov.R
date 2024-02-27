# ---
# title: "NFIS and beaudoin covariates"
# author: "Brendan Casey"
# created: "2023-11-17"
# description: "extracting NFIS and beaudoin predictor variables to station locations."
# ---

#Setup ----

##Load packages----
library(terra)

##Import data----

#### Wildtrax all
load("0_data/manual/response/ss_xy_ab.rData")


#### NFIS data
lc<-rast("0_data/external/lionel/2_outputs/stackHardBuffer150-565-1000-Austin.tif")

##////////////////////////////////////////////////////////////////
##Extract raster values to points----

ss_xy_lc<-terra::extract(lc, ss_xy_ab, method="simple")

ss_xy_nfis<-cbind(as.data.frame(ss_xy_ab), ss_xy_lc)%>%
  dplyr::select(-c(2:4))

save(ss_xy_nfis, file="0_data/manual/predictor/wt/ss_xy_nfis.rData")

##////

#Austin's data
load("0_data/manual/response/ss_xy_az_1.rData")

ss_xy_az_lc<-terra::extract(lc, ss_xy_az_1, method="simple")

ss_xy_az_lc_1<-cbind(as.data.frame(ss_xy_az_1), ss_xy_az_lc)%>%
  dplyr::select(-c(2:3))

Master_data <- read_csv("0_data/external/Austin_Zeller/code/PIWO_Bayesian_Model_Comparison-main/Master_data.csv")

master_1<-Master_data%>% dplyr::select(c(location, PIWOocc, longitude, latitude))%>%left_join(ss_xy_az_lc_1)

save(master_1, file="0_data/manual/formatted_for_models/master1.rData")



