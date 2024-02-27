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
load("0_data/manual/response/bird_station_data_with_offset.Rdata")

#### From earth engine
load("0_data/manual/predictor/wt/ss_xy_nfis.rData")

ss_xy_fixed_pointLevel <- read_csv("0_data/manual/predictor/wt/test/ss_xy_fixed_pointLevel.csv")%>%
  dplyr::select(-c(TWI, HLI))

ss_xy_fixed <- read_csv("0_data/manual/predictor/wt/test/ss_xy_fixed.csv")%>%
  dplyr::select(c(location, contains('TWI'), contains('HLI'), contains('canopy')))


ss_xy_lc_150 <- read_csv("0_data/manual/predictor/wt/test/ss_xy_lc_150.csv")
ss_xy_lc_565 <- read_csv("0_data/manual/predictor/wt/test/ss_xy_lc_565.csv")
ss_xy_lc_1000 <- read_csv("0_data/manual/predictor/wt/test/ss_xy_lc_1000.csv")

ss_xy_ts_1000 <- read_csv("0_data/manual/predictor/wt/test/ss_xy_ts_1000.csv")
ss_xy_ts_565 <- read_csv("0_data/manual/predictor/wt/test/ss_xy_ts_565.csv")
ss_xy_ts_150 <- read_csv("0_data/manual/predictor/wt/test/ss_xy_ts_150.csv")


##////////////////////////////////////////////////////////////////
#Combine covariates into a single dataframe----
ss_cov<-ss_xy_lc_150%>%
  left_join(ss_xy_lc_565)%>%
  left_join(ss_xy_lc_1000)%>%
  left_join(ss_xy_ts_150)%>%
  left_join(ss_xy_ts_565)%>%
  left_join(ss_xy_ts_1000)%>%
  left_join(ss_xy_fixed)%>%
  # dplyr::select(-c(contains('TPI')))%>%
  left_join(ss_xy_fixed_pointLevel)%>%
  left_join(ss_xy_nfis)
  
# Replace '/' with '_'
colnames(ss_cov) <- gsub("/", "_", colnames(ss_cov))

# Replace '-' with '_'
colnames(ss_cov) <- gsub("-", "_", colnames(ss_cov))

# Replace ' ' with '_'
colnames(ss_cov) <- gsub(" ", "_", colnames(ss_cov))

##////////////////////////////////////////////////////////////////
# Join bird data, offsets, and covariates----
## Join by year and location
bird_cov<-bird_ab%>%
  mutate(date=as.Date(date))%>%
  group_by(location, date)%>%
  dplyr::summarise(across(14, sum))%>%
  ungroup()%>%
  left_join(visit_ab_with_offset)%>%
  # left_join(ss_GOA_lidar)%>%
  left_join(ss_cov, by = join_by(location, year))%>%
  dplyr::select(-c(id, project, observer, buffer, distanceMethod, durationMethod, TM, JDAY, hssr, TSSR, province, country, seedgrow, MAXDIS, MAXDUR))%>%
  dplyr::select(-c(sensor, date, DSLS, bcr, TREE, LCC2, LCC4))

save(bird_cov, file=paste0("0_data/manual/formatted_for_models/bird_cov_cleaned_", format(Sys.Date(), "%Y%m%d"), ".rData"))
write_csv(bird_cov, file=paste0("0_data/manual/formatted_for_models/bird_cov_cleaned_", format(Sys.Date(), "%Y%m%d"), ".csv"))

##////////////////////////////////////////////////////////////////


