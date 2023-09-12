# ---
# title: "Boosted regression trees"
# author: "Brendan Casey"
# created: "September 8, 2023"
# ---

#Notes----
#Combine data into a single dataframe

##////////////////////////////////////////////////////////////////

#Setup----
##Load Packages----
library(tidyverse)
library(dismo)
library(gbm)

##Import data
load("0_data/manual/response/bird_station_data_with_offset.rData")
load("0_data/manual/predictor/ss_GOA_lidar.rData")
earth_engine_indices <- read_csv("0_data/manual/predictor/earth_engine_indices.csv")


###combine into a single dataframe----
bird_cl_1<-bird_cl%>%
  mutate(date=as.Date(date))%>%
  group_by(location, date)%>%
  dplyr::summarise(across(14, sum))%>%
  ungroup()%>%
  left_join(visit_cl_with_offset)%>%
  left_join(ss_GOA_lidar)%>%
  left_join(earth_engine_indices)%>%
  dplyr::select(-c(id, project, observer, buffer, distanceMethod, durationMethod, TM, JDAY, hssr, TSSR, province, country, seedgrow, MAXDIS, MAXDUR))%>%
  na.omit()


save(bird_cl_1, file="2_pipeline/store/bird_cl_1.rData")
df1<-bird_cl_1%>%
  dplyr::select(-c(location, sensor, lat, lon, year, date, DSLS, bcr, TREE, LCC2, LCC4, month,
                   NDMI, `water-permanent-coverfraction`, `water-seasonal-coverfraction`, sd_elev_stddev, sd_elev_0pnt15_to_2pnt00_return_proportion,
                   sd_elev_2pnt00_to_4pnt00_return_proportion, sd_total_all_returns, 
                   sd_percentage_first_returns_above_mean, sd_percentage_first_returns_above_2pnt00))%>%
  mutate(PIWO = ifelse(PIWO > 0, 1, 0))%>%
  as.data.frame()

df2<-df1%>%dplyr::select(-c(PIWO_offset))%>%
  mutate(PIWO = ifelse(PIWO > 0, 1, 0))


#create an offset dataframe
o <- as.data.frame(df1$PIWO_offset)%>%rename(PIWO_offset=`df1$PIWO_offset`)
o <- df1$PIWO_offset

##////////////////////////////////////////////////////////////////

#Boosted regression trees----
##Tune parameters----
### Create a hyper parameter grid----
set.seed(123)
random_index <- sample(1:nrow(df1), nrow(df1))
random_df1 <- df1[random_index, ]

# create hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(.01, .05, .1),
  interaction.depth = c(2, 3),
  # n.trees = seq(100, 1000, by = 100),
  n.minobsinnode = c(10, 15, 20, 30),
  bag.fraction = c(.5, .75, .85), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                 # a place to dump results
)

# total number of combinations
nrow(hyper_grid)


### Build gbm models for each combination of parameters.----
# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune <- gbm(
    formula = PIWO ~ offset(PIWO_offset) + .,
    distribution = "bernoulli",
    data = random_df1,
    n.trees = 6000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

# save
tune_param_mean_1<-hyper_grid%>%
  dplyr::arrange(min_RMSE) 
save(tune_param_mean_1, file="2_pipeline/store/tune_param_mean_1.rData")

##////////////////////////////////////////////////////////////////

## Apply dismo's `gbm.step` to tuned parameters---- 

set.seed(123)
###Use gbm.step using tuned parameters----
brt_PIWO_tuned_1 <- gbm.step(data=df2, gbm.x = c(2:ncol(df2)), gbm.y = 1, offset=o,
                             family = "bernoulli", tree.complexity = 2,  n.minobsinnode = 15,
                             learning.rate = 0.01, bag.fraction = 0.75, silent = F)

# brt_PIWO_tuned_1 <- gbm.step(data=df2, gbm.x = c(2:ncol(df2)), gbm.y = 1,
#                              family = "bernoulli", tree.complexity = 2,  n.minobsinnode = 10,
#                              learning.rate = 0.1, bag.fraction = 0.75)


save(brt_PIWO_tuned_1, file="2_pipeline/store/models/brt_PIWO_tuned_1.rData")

###View results----
#### view relative importance of predictors----
summary(brt_PIWO_tuned_1)

#### view plots of all variables----
gbm.plot(brt_PIWO_tuned_1, n.plots=21, write.title = FALSE)

#### view optimal number of trees----
gbm.perf(brt_PIWO_tuned_1)
#[1] 557

#### get model stats----
###### put relevant stats into a dataframe (e.g. explained deviance)
varimp.brt_PIWO_tuned_1 <- as.data.frame(brt_PIWO_tuned_1$contributions)
names(varimp.brt_PIWO_tuned_1)[2] <- "brt_PIWO_tuned_1"
cvstats.brt_PIWO_tuned_1 <- as.data.frame(brt_PIWO_tuned_1$cv.statistics[c(1,3)])
cvstats.brt_PIWO_tuned_1$deviance.null <- brt_PIWO_tuned_1$self.statistics$mean.null
cvstats.brt_PIWO_tuned_1$deviance.explained <- (cvstats.brt_PIWO_tuned_1$deviance.null-cvstats.brt_PIWO_tuned_1$deviance.mean)/cvstats.brt_PIWO_tuned_1$deviance.null
cvstats.brt_PIWO_tuned_1$model_name<-"PIWO_tuned_1"

##////////////////////////////////////////////////////////////////

## Drop variables that don't improve model performance.----
simp_PIWO_tuned <- gbm.simplify(brt_PIWO_tuned_1)
save(simp_PIWO_tuned, file="2_pipeline/store/models/simp_PIWO_tuned.rData")

###  remove non-numeric characters from the row names----
rownames(simp_PIWO_tuned$deviance.summary) <- gsub("[^0-9]", "", rownames(simp_PIWO_tuned$deviance.summary))

### get the optimal number of drops----
optimal_no_drops<-as.numeric(rownames(simp_PIWO_tuned$deviance.summary%>%slice_min(mean))) 


##////////////////////////////////////////////////////////////////
## Repeat the above steps with reduced predictors----

### remove droped variables from the dataframe----
df2<-df1%>%
  dplyr::select(PIWO,simp_PIWO_tuned$pred.list[[optimal_no_drops]])

set.seed(123)
random_index <- sample(1:nrow(df2), nrow(df2))
random_df2 <- df2[random_index, ]

### grid search---- 
for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune <- gbm(
    formula = PIWO ~ .,
    distribution = "poisson",
    data = random_df2,
    n.trees = 5000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

# save
tune_param_mean_2<-hyper_grid%>%
  dplyr::arrange(min_RMSE) 
save(tune_param_mean_2, file="2_pipeline/store/tune_param_mean_2.rData")

### GBM step----
brt_PIWO_tuned_2 <- gbm.step(data=df2, gbm.x = c(2:ncol(df2)), gbm.y = 1, offset=PIWO_offset,
                             family = "poisson", tree.complexity = 3,  n.minobsinnode = 10,
                             learning.rate = 0.1, bag.fraction = 0.5, max.trees = 50000)


save(brt_PIWO_tuned_2, file="2_pipeline/store/models/brt_PIWO_tuned_2.rData")

### Model results----
summary(brt_PIWO_tuned_2)

varimp.brt_PIWO_tuned_2 <- as.data.frame(brt_PIWO_tuned_2$contributions)
names(varimp.brt_PIWO_tuned_2)[2] <- "brt_PIWO_tuned_2"
cvstats.brt_PIWO_tuned_2<- as.data.frame(brt_PIWO_tuned_2$cv.statistics[c(1,3)])
cvstats.brt_PIWO_tuned_2$deviance.null <- brt_PIWO_tuned_2$self.statistics$mean.null
cvstats.brt_PIWO_tuned_2$deviance.explained <- (cvstats.brt_PIWO_tuned_2$deviance.null-cvstats.brt_PIWO_tuned_2$deviance.mean)/cvstats.brt_PIWO_tuned_2$deviance.null
cvstats.brt_PIWO_tuned_2$model_name<-"PIWO_tuned_2"
