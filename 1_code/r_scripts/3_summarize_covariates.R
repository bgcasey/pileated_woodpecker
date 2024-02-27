# ---
# title: "Gather covariates"
# author: "Brendan Casey"
# created: "2023-11-17"
# description: "extracting landcover predictor variables to station locations."
# ---

#Setup ----

##Load packages----
library(terra)

##Import data----

#### Wildtrax all
# load("0_data/manual/response/ss_xy_ab.rData")

#### Austins data
Master_data <- read_csv("0_data/external/Austin_Zeller/code/PIWO_Bayesian_Model_Comparison-main/Master_data.csv")
# location_years <- read_csv("0_data/external/Austin_Zeller/code/PIWO_Bayesian_Model_Comparison-main/locationyears.csv")

#### From earth engine
ss_xy_fixed <- read_csv("0_data/manual/predictor/ss_xy_fixed.csv")%>%
  dplyr::select(-c(`system:index`, .geo))
ss_xy_LC <- read_csv("0_data/manual/predictor/ss_xy_LC.csv")%>%
  dplyr::select(-c(`system:index`, .geo))
ss_xy_ts_150 <- read_csv("0_data/manual/predictor/ss_xy_ts_150.csv")%>%
  dplyr::select(-c(`system:index`, .geo, date, month))
ss_xy_ts_565 <- read_csv("0_data/manual/predictor/ss_xy_ts_565.csv")%>%
  dplyr::select(-c(`system:index`, .geo, date, month))
ss_xy_ts_1000 <- read_csv("0_data/manual/predictor/ss_xy_ts_1000.csv")%>%
  dplyr::select(-c(`system:index`, .geo, date, month))

ss_xy_fixed <- read_csv("0_data/manual/predictor/wt/ss_xy_fixed.csv")%>%
  dplyr::select(-c(`system:index`, .geo))
ss_xy_LC <- read_csv("0_data/manual/predictor/wt/ss_xy_LC.csv")%>%
  dplyr::select(-c(`system:index`, .geo))


ss_xy_ts_1000 <- read_csv("0_data/manual/predictor/wt/ss_xy_ts_1000.csv")%>%
  dplyr::select(-c(`system:index`, .geo, date, month))

ss_xy_ts_565 <- read_csv("0_data/manual/predictor/wt/ss_xy_ts_565_20.csv")%>%
  dplyr::select(-c(`system:index`, .geo, date, month))
ss_xy_ts_150 <- read_csv("0_data/manual/predictor/wt/ss_xy_ts_150.csv")


%>%
  dplyr::select(-c(`system:index`, .geo, date, month))

##////////////////////////////////////////////////////////////////
#Combine covariates into a single dataframe----
ss_cov<-ss_xy_ts_150%>%
  left_join(ss_xy_ts_565)%>%
  left_join(ss_xy_ts_1000)%>%
  left_join(ss_xy_LC)%>%
  left_join(ss_xy_fixed)

# Replace '/' with '_'
colnames(ss_cov) <- gsub("/", "_", colnames(ss_cov))

# Replace '-' with '_'
colnames(ss_cov) <- gsub("-", "_", colnames(ss_cov))

# Replace ' ' with '_'
colnames(ss_cov) <- gsub(" ", "_", colnames(ss_cov))


##////////////////////////////////////////////////////////////////
# Join bird data, offsets, and covariates----
## Join by year and location
bird_cov<-Master_data%>%
  # left_join(ss_GOA_lidar)%>%
  left_join(ss_cov, by = join_by(location))%>%
  filter(year==2019)%>%
  dplyr::select(-year)%>%
  dplyr::select(1:2, 35, 3:188)

save(bird_cov, file=paste0("0_data/manual/formatted_for_models/bird_cov_cleaned_", format(Sys.Date(), "%Y%m%d"), ".rData"))
write_csv(bird_cov, file=paste0("0_data/manual/formatted_for_models/bird_cov_cleaned_", format(Sys.Date(), "%Y%m%d"), ".csv"))

##////////////////////////////////////////////////////////////////


