# ---
# title: "Boosted regression trees"
# author: "Brendan Casey"
# created: "September 8, 2023"
# ---

##////////////////////////////////////////////////////////////////

#Setup----
##Load Packages----
library(tidyverse)
library(dismo)
library(gbm)

##Load data----
load("0_data/manual/formatted_for_models/bird_cov_cleaned_20231121.rData")

##////////////////////////////////////////////////////////////////
# BRT----
##brt_NFIS_12----
### Setup data----
df1<-bird_cov%>%
  na.omit()%>%
  dplyr::select(c(2:6, 85:102))%>%
  filter(year>2010)%>%
  as.data.frame()

df2<-df1%>%dplyr::select(-c(PIWO_offset))%>%
  mutate(PIWO = ifelse(PIWO > 0, 1, 0))


#create an offset dataframe
o <- as.data.frame(df1$PIWO_offset)%>%rename(PIWO_offset=`df1$PIWO_offset`)
o <- df1$PIWO_offset

set.seed(123)
####Use gbm.step using tuned parameters----
brt_NFIS<- gbm.step(data=df2, gbm.x = c(2:ncol(df2)), gbm.y = 1, offset=o,
                                    family = "bernoulli", tree.complexity = 2,  n.minobsinnode = 15,
                                    learning.rate = 0.01, bag.fraction = 0.85, silent = F)
save(brt_NFIS, file="3_output/models/WT/brt_NFIS.rData")

##### get model stats----
###### put relevant stats into a dataframe (e.g. explained deviance)
varimp.brt_NFIS<- as.data.frame(brt_NFIS$contributions)%>%
  mutate(model="brt_NFIS")
save(varimp.brt_NFIS, file="3_output/model_results/WT/varimp.brt_NFIS.rData")

cvstats.brt_NFIS<- as.data.frame(brt_NFIS$cv.statistics[c(1,3,5)])
cvstats.brt_NFIS$deviance.null <- brt_NFIS$self.statistics$mean.null
cvstats.brt_NFIS$deviance.explained <- (cvstats.brt_NFIS$deviance.null-cvstats.brt_NFIS$deviance.mean)/cvstats.brt_NFIS$deviance.null
cvstats.brt_NFIS$model_name<-"brt_NFIS"
colnames(cvstats.brt_NFIS)[colnames(cvstats.brt_NFIS) == "discrimination.mean"] ="AUC"
save(cvstats.brt_NFIS, file="3_output/model_results/WT/cvstats.brt_NFIS.rData")


##///////////////////////////

##brt_NFIS_ls_12----
### Setup data----
df1<-bird_cov%>%
  na.omit()%>%
  dplyr::select(c(2:6, 34:63, 85:102))%>%
  dplyr::select(-c(contains("stressed")))%>%
  filter(year>2010)%>%
  as.data.frame()

df2<-df1%>%dplyr::select(-c(PIWO_offset))%>%
  mutate(PIWO = ifelse(PIWO > 0, 1, 0))


#create an offset dataframe
o <- as.data.frame(df1$PIWO_offset)%>%rename(PIWO_offset=`df1$PIWO_offset`)
o <- df1$PIWO_offset

set.seed(123)
####Use gbm.step using tuned parameters----
brt_NFIS_ls_12<- gbm.step(data=df2, gbm.x = c(2:ncol(df2)), gbm.y = 1, offset=o,
                    family = "bernoulli", tree.complexity = 2,  n.minobsinnode = 15,
                    learning.rate = 0.01, bag.fraction = 0.85, silent = F)
save(brt_NFIS_ls_12, file="3_output/models/WT/brt_NFIS_ls_12.rData")

##### get model stats----
###### put relevant stats into a dataframe (e.g. explained deviance)
varimp.brt_NFIS_ls_12<- as.data.frame(brt_NFIS_ls_12$contributions)%>%
  mutate(model="brt_NFIS_ls_12")
save(varimp.brt_NFIS_ls_12, file="3_output/model_results/WT/varimp.brt_NFIS_ls_12.rData")

cvstats.brt_NFIS_ls_12<- as.data.frame(brt_NFIS_ls_12$cv.statistics[c(1,3,5)])
cvstats.brt_NFIS_ls_12$deviance.null <- brt_NFIS_ls_12$self.statistics$mean.null
cvstats.brt_NFIS_ls_12$deviance.explained <- (cvstats.brt_NFIS_ls_12$deviance.null-cvstats.brt_NFIS_ls_12$deviance.mean)/cvstats.brt_NFIS_ls_12$deviance.null
cvstats.brt_NFIS_ls_12$model_name<-"brt_NFIS_ls_12"
colnames(cvstats.brt_NFIS_ls_12)[colnames(cvstats.brt_NFIS_ls_12) == "discrimination.mean"] ="AUC"
save(cvstats.brt_NFIS_ls_12, file="3_output/model_results/WT/cvstats.brt_NFIS_ls_12.rData")

##///////////////////////////

##brt_ls_29----
### Setup data----
df1<-bird_cov%>%
  na.omit()%>%
  dplyr::select(c(2:6, 34:63))%>%
  dplyr::select(-c(contains("stressed")))%>%
  as.data.frame()

df2<-df1%>%dplyr::select(-c(PIWO_offset))%>%
  mutate(PIWO = ifelse(PIWO > 0, 1, 0))


#create an offset dataframe
o <- as.data.frame(df1$PIWO_offset)%>%rename(PIWO_offset=`df1$PIWO_offset`)
o <- df1$PIWO_offset

set.seed(123)
####Use gbm.step using tuned parameters----
brt_ls_29<- gbm.step(data=df2, gbm.x = c(2:ncol(df2)), gbm.y = 1, offset=o,
                          family = "bernoulli", tree.complexity = 2,  n.minobsinnode = 15,
                          learning.rate = 0.01, bag.fraction = 0.85, silent = F)
save(brt_ls_29, file="3_output/models/WT/brt_ls_29.rData")

##### get model stats----
###### put relevant stats into a dataframe (e.g. explained deviance)
varimp.brt_ls_29<- as.data.frame(brt_ls_29$contributions)%>%
  mutate(model="brt_ls_29")
save(varimp.brt_ls_29, file="3_output/model_results/WT/varimp.brt_ls_29.rData")

cvstats.brt_ls_29<- as.data.frame(brt_ls_29$cv.statistics[c(1,3,5)])
cvstats.brt_ls_29$deviance.null <- brt_ls_29$self.statistics$mean.null
cvstats.brt_ls_29$deviance.explained <- (cvstats.brt_ls_29$deviance.null-cvstats.brt_ls_29$deviance.mean)/cvstats.brt_ls_29$deviance.null
cvstats.brt_ls_29$model_name<-"brt_ls_29"
colnames(cvstats.brt_ls_29)[colnames(cvstats.brt_ls_29) == "discrimination.mean"] ="AUC"
save(cvstats.brt_ls_29, file="3_output/model_results/WT/cvstats.brt_ls_29.rData")


##///////////////////////////

##brt_hlc_29----
### Setup data----
df1<-bird_cov%>%
  na.omit()%>%
  dplyr::select(c(2:33))%>%
  as.data.frame()

df2<-df1%>%dplyr::select(-c(PIWO_offset))%>%
  mutate(PIWO = ifelse(PIWO > 0, 1, 0))


#create an offset dataframe
o <- as.data.frame(df1$PIWO_offset)%>%rename(PIWO_offset=`df1$PIWO_offset`)
o <- df1$PIWO_offset

set.seed(123)
####Use gbm.step using tuned parameters----
brt_hlc_29<- gbm.step(data=df2, gbm.x = c(2:ncol(df2)), gbm.y = 1, offset=o,
                     family = "bernoulli", tree.complexity = 2,  n.minobsinnode = 15,
                     learning.rate = 0.01, bag.fraction = 0.85, silent = F)
save(brt_hlc_29, file="3_output/models/WT/brt_hlc_29.rData")

##### get model stats----
###### put relevant stats into a dataframe (e.g. explained deviance)
varimp.brt_hlc_29<- as.data.frame(brt_hlc_29$contributions)%>%
  mutate(model="brt_hlc_29")
save(varimp.brt_hlc_29, file="3_output/model_results/WT/varimp.brt_hlc_29.rData")

cvstats.brt_hlc_29<- as.data.frame(brt_hlc_29$cv.statistics[c(1,3,5)])
cvstats.brt_hlc_29$deviance.null <- brt_hlc_29$self.statistics$mean.null
cvstats.brt_hlc_29$deviance.explained <- (cvstats.brt_hlc_29$deviance.null-cvstats.brt_hlc_29$deviance.mean)/cvstats.brt_hlc_29$deviance.null
cvstats.brt_hlc_29$model_name<-"brt_hlc_29"
colnames(cvstats.brt_hlc_29)[colnames(cvstats.brt_hlc_29) == "discrimination.mean"] ="AUC"
save(cvstats.brt_hlc_29, file="3_output/model_results/WT/cvstats.brt_hlc_29.rData")

##///////////////////////////

##brt_ls_hlc_29----
### Setup data----
df1<-bird_cov%>%
  na.omit()%>%
  dplyr::select(c(2:63))%>%
  dplyr::select(-c(contains("stressed")))%>%
  as.data.frame()

df2<-df1%>%dplyr::select(-c(PIWO_offset))%>%
  mutate(PIWO = ifelse(PIWO > 0, 1, 0))


#create an offset dataframe
o <- as.data.frame(df1$PIWO_offset)%>%rename(PIWO_offset=`df1$PIWO_offset`)
o <- df1$PIWO_offset

set.seed(123)
####Use gbm.step using tuned parameters----
brt_ls_hlc_29<- gbm.step(data=df2, gbm.x = c(2:ncol(df2)), gbm.y = 1, offset=o,
                      family = "bernoulli", tree.complexity = 2,  n.minobsinnode = 15,
                      learning.rate = 0.01, bag.fraction = 0.85, silent = F)
save(brt_ls_hlc_29, file="3_output/models/WT/brt_ls_hlc_29.rData")

##### get model stats----
###### put relevant stats into a dataframe (e.g. explained deviance)
varimp.brt_ls_hlc_29<- as.data.frame(brt_ls_hlc_29$contributions)%>%
  mutate(model="brt_ls_hlc_29")
save(varimp.brt_ls_hlc_29, file="3_output/model_results/WT/varimp.brt_ls_hlc_29.rData")

cvstats.brt_ls_hlc_29<- as.data.frame(brt_ls_hlc_29$cv.statistics[c(1,3,5)])
cvstats.brt_ls_hlc_29$deviance.null <- brt_ls_hlc_29$self.statistics$mean.null
cvstats.brt_ls_hlc_29$deviance.explained <- (cvstats.brt_ls_hlc_29$deviance.null-cvstats.brt_ls_hlc_29$deviance.mean)/cvstats.brt_ls_hlc_29$deviance.null
cvstats.brt_ls_hlc_29$model_name<-"brt_ls_hlc_29"
colnames(cvstats.brt_ls_hlc_29)[colnames(cvstats.brt_ls_hlc_29) == "discrimination.mean"] ="AUC"
save(cvstats.brt_ls_hlc_29, file="3_output/model_results/WT/cvstats.brt_ls_hlc_29.rData")



##///////////////////////////

##brt_ls_hlc_terrain_29----
### Setup data----
df1<-bird_cov%>%
  na.omit()%>%
  dplyr::select(c(2:69, 76:84))%>%
  dplyr::select(-c(contains("stressed")))%>%
  as.data.frame()

df2<-df1%>%dplyr::select(-c(PIWO_offset))%>%
  mutate(PIWO = ifelse(PIWO > 0, 1, 0))


#create an offset dataframe
o <- as.data.frame(df1$PIWO_offset)%>%rename(PIWO_offset=`df1$PIWO_offset`)
o <- df1$PIWO_offset

set.seed(123)
####Use gbm.step using tuned parameters----
brt_ls_hlc_terrain_29<- gbm.step(data=df2, gbm.x = c(2:ncol(df2)), gbm.y = 1, offset=o,
                         family = "bernoulli", tree.complexity = 2,  n.minobsinnode = 15,
                         learning.rate = 0.01, bag.fraction = 0.85, silent = F)
save(brt_ls_hlc_terrain_29, file="3_output/models/WT/brt_ls_hlc_terrain_29.rData")

##### get model stats----
###### put relevant stats into a dataframe (e.g. explained deviance)
varimp.brt_ls_hlc_terrain_29<- as.data.frame(brt_ls_hlc_terrain_29$contributions)%>%
  mutate(model="brt_ls_hlc_terrain_29")
save(varimp.brt_ls_hlc_terrain_29, file="3_output/model_results/WT/varimp.brt_ls_hlc_terrain_29.rData")

cvstats.brt_ls_hlc_terrain_29<- as.data.frame(brt_ls_hlc_terrain_29$cv.statistics[c(1,3,5)])
cvstats.brt_ls_hlc_terrain_29$deviance.null <- brt_ls_hlc_terrain_29$self.statistics$mean.null
cvstats.brt_ls_hlc_terrain_29$deviance.explained <- (cvstats.brt_ls_hlc_terrain_29$deviance.null-cvstats.brt_ls_hlc_terrain_29$deviance.mean)/cvstats.brt_ls_hlc_terrain_29$deviance.null
cvstats.brt_ls_hlc_terrain_29$model_name<-"brt_ls_hlc_terrain_29"
colnames(cvstats.brt_ls_hlc_terrain_29)[colnames(cvstats.brt_ls_hlc_terrain_29) == "discrimination.mean"] ="AUC"
save(cvstats.brt_ls_hlc_terrain_29, file="3_output/model_results/WT/cvstats.brt_ls_hlc_terrain_29.rData")


##///////////////////////////

##brt_ls_hlc_terrain_canopy_29----
### Setup data----
df1<-bird_cov%>%
  na.omit()%>%
  dplyr::select(c(2:84))%>%
  dplyr::select(-c(contains("stressed")))%>%
  as.data.frame()

df2<-df1%>%dplyr::select(-c(PIWO_offset))%>%
  mutate(PIWO = ifelse(PIWO > 0, 1, 0))


#create an offset dataframe
o <- as.data.frame(df1$PIWO_offset)%>%rename(PIWO_offset=`df1$PIWO_offset`)
o <- df1$PIWO_offset

set.seed(123)
####Use gbm.step using tuned parameters----
brt_ls_hlc_terrain_canopy_29<- gbm.step(data=df2, gbm.x = c(2:ncol(df2)), gbm.y = 1, offset=o,
                                 family = "bernoulli", tree.complexity = 2,  n.minobsinnode = 15,
                                 learning.rate = 0.01, bag.fraction = 0.85, silent = F)
save(brt_ls_hlc_terrain_canopy_29, file="3_output/models/WT/brt_ls_hlc_terrain_canopy_29.rData")

##### get model stats----
###### put relevant stats into a dataframe (e.g. explained deviance)
varimp.brt_ls_hlc_terrain_canopy_29<- as.data.frame(brt_ls_hlc_terrain_canopy_29$contributions)%>%
  mutate(model="brt_ls_hlc_terrain_canopy_29")
save(varimp.brt_ls_hlc_terrain_canopy_29, file="3_output/model_results/WT/varimp.brt_ls_hlc_terrain_canopy_29.rData")

cvstats.brt_ls_hlc_terrain_canopy_29<- as.data.frame(brt_ls_hlc_terrain_canopy_29$cv.statistics[c(1,3,5)])
cvstats.brt_ls_hlc_terrain_canopy_29$deviance.null <- brt_ls_hlc_terrain_canopy_29$self.statistics$mean.null
cvstats.brt_ls_hlc_terrain_canopy_29$deviance.explained <- (cvstats.brt_ls_hlc_terrain_canopy_29$deviance.null-cvstats.brt_ls_hlc_terrain_canopy_29$deviance.mean)/cvstats.brt_ls_hlc_terrain_canopy_29$deviance.null
cvstats.brt_ls_hlc_terrain_canopy_29$model_name<-"brt_ls_hlc_terrain_canopy_29"
colnames(cvstats.brt_ls_hlc_terrain_canopy_29)[colnames(cvstats.brt_ls_hlc_terrain_canopy_29) == "discrimination.mean"] ="AUC"
save(cvstats.brt_ls_hlc_terrain_canopy_29, file="3_output/model_results/WT/cvstats.brt_ls_hlc_terrain_canopy_29.rData")


### Drop variables that don't improve model performance.----
brt_ls_hlc_terrain_canopy_29_droppedVar <- gbm.simplify(brt_ls_hlc_terrain_canopy_29, n.drops = 50)
save(brt_ls_hlc_terrain_canopy_29_droppedVar, file="3_output/models/brt_ls_hlc_terrain_canopy_29_droppedVar.rData")

####  remove non-numeric characters from the row names----
rownames(brt_ls_hlc_terrain_canopy_29_droppedVar$deviance.summary) <- gsub("[^0-9]", "", rownames(brt_ls_hlc_terrain_canopy_29_droppedVar$deviance.summary))

#### get the optimal number of drops----
optimal_no_drops<-as.numeric(rownames(brt_ls_hlc_terrain_canopy_29_droppedVar$deviance.summary%>%slice_min(mean))) 

#### remove droped variables from the dataframe----
df2<-df1%>%
  dplyr::select(CAWA,brt_ls_hlc_terrain_canopy_29_droppedVar$pred.list[[optimal_no_drops]])

#### GBM step----
brt_ls_hlc_terrain_canopy_29_simp <- gbm.step(data=df2, gbm.x = c(2:ncol(df2)), gbm.y = 1, offset=o,
                                     family = "bernoulli", tree.complexity = 2,  n.minobsinnode = 30,
                                     learning.rate = 0.001, bag.fraction = 0.85, silent = F)
save(brt_ls_hlc_terrain_canopy_29_simp, file="3_output/models/brt_ls_hlc_terrain_canopy_29_simp.rData")

#### Model results----
varimp.brt_ls_hlc_terrain_canopy_29_simp <- as.data.frame(brt_ls_hlc_terrain_canopy_29_simp$contributions)%>%
  mutate(model="brt_ls_hlc_terrain_canopy_29_simp")
save(varimp.brt_ls_hlc_terrain_canopy_29_simp, file="3_output/model_results/varimp.brt_ls_hlc_terrain_canopy_29_simp.rData")


cvstats.brt_ls_hlc_terrain_canopy_29_simp<- as.data.frame(brt_ls_hlc_terrain_canopy_29_simp$cv.statistics[c(1,3,5)])
cvstats.brt_ls_hlc_terrain_canopy_29_simp$deviance.null <- brt_ls_hlc_terrain_canopy_29_simp$self.statistics$mean.null
cvstats.brt_ls_hlc_terrain_canopy_29_simp$deviance.explained <- (cvstats.brt_ls_hlc_terrain_canopy_29_simp$deviance.null-cvstats.brt_ls_hlc_terrain_canopy_29_simp$deviance.mean)/cvstats.brt_ls_hlc_terrain_canopy_29_simp$deviance.null
cvstats.brt_ls_hlc_terrain_canopy_29_simp$model_name<-"brt_ls_hlc_terrain_canopy_29_simp"
colnames(cvstats.brt_ls_hlc_terrain_canopy_29_simp)[colnames(cvstats.brt_ls_hlc_terrain_canopy_29_simp) == "discrimination.mean"] ="AUC"
save(cvstats.brt_ls_hlc_terrain_canopy_29_simp, file="3_output/model_results/cvstats.brt_ls_hlc_terrain_canopy_29_simp.rData")

##///////////////////////////

##brt_ls_hlc_terrain_canopy_29_2----
### Setup data----
df1<-bird_cov%>%
  na.omit()%>%
  dplyr::select(c(2:37, 39:47, 49:57, 59:77, 79:84))%>% #excluded DRS and aspect
  dplyr::select(-c(contains("stressed")))%>%
  as.data.frame()

df2<-df1%>%dplyr::select(-c(PIWO_offset))%>%
  mutate(PIWO = ifelse(PIWO > 0, 1, 0))


#create an offset dataframe
o <- as.data.frame(df1$PIWO_offset)%>%rename(PIWO_offset=`df1$PIWO_offset`)
o <- df1$PIWO_offset

set.seed(123)
####Use gbm.step using tuned parameters----
brt_ls_hlc_terrain_canopy_29_2<- gbm.step(data=df2, gbm.x = c(2:ncol(df2)), gbm.y = 1, offset=o,
                                        family = "bernoulli", tree.complexity = 2,  n.minobsinnode = 15,
                                        learning.rate = 0.01, bag.fraction = 0.85, silent = F)
save(brt_ls_hlc_terrain_canopy_29_2, file="3_output/models/WT/brt_ls_hlc_terrain_canopy_29_2.rData")

##### get model stats----
###### put relevant stats into a dataframe (e.g. explained deviance)
varimp.brt_ls_hlc_terrain_canopy_29_2<- as.data.frame(brt_ls_hlc_terrain_canopy_29_2$contributions)%>%
  mutate(model="brt_ls_hlc_terrain_canopy_29_2")
save(varimp.brt_ls_hlc_terrain_canopy_29_2, file="3_output/model_results/WT/varimp.brt_ls_hlc_terrain_canopy_29_2.rData")

cvstats.brt_ls_hlc_terrain_canopy_29_2<- as.data.frame(brt_ls_hlc_terrain_canopy_29_2$cv.statistics[c(1,3,5)])
cvstats.brt_ls_hlc_terrain_canopy_29_2$deviance.null <- brt_ls_hlc_terrain_canopy_29_2$self.statistics$mean.null
cvstats.brt_ls_hlc_terrain_canopy_29_2$deviance.explained <- (cvstats.brt_ls_hlc_terrain_canopy_29_2$deviance.null-cvstats.brt_ls_hlc_terrain_canopy_29_2$deviance.mean)/cvstats.brt_ls_hlc_terrain_canopy_29_2$deviance.null
cvstats.brt_ls_hlc_terrain_canopy_29_2$model_name<-"brt_ls_hlc_terrain_canopy_29_2"
colnames(cvstats.brt_ls_hlc_terrain_canopy_29_2)[colnames(cvstats.brt_ls_hlc_terrain_canopy_29_2) == "discrimination.mean"] ="AUC"
save(cvstats.brt_ls_hlc_terrain_canopy_29_2, file="3_output/model_results/WT/cvstats.brt_ls_hlc_terrain_canopy_29_2.rData")










##///////////////////////////

##brt_nfis_ls_hlc_terrain_canopy_29----
### Setup data----
df1<-bird_cov%>%
  na.omit()%>%
  # dplyr::select(c(2:6, 7:36))%>%
  dplyr::select(-c(1))%>%
  as.data.frame()

df2<-df1%>%dplyr::select(-c(PIWO_offset))%>%
  mutate(PIWO = ifelse(PIWO > 0, 1, 0))


#create an offset dataframe
o <- as.data.frame(df1$PIWO_offset)%>%rename(PIWO_offset=`df1$PIWO_offset`)
o <- df1$PIWO_offset

set.seed(123)
####Use gbm.step using tuned parameters----
brt_nfis_ls_hlc_terrain_canopy_29<- gbm.step(data=df2, gbm.x = c(2:ncol(df2)), gbm.y = 1, offset=o,
                                        family = "bernoulli", tree.complexity = 2,  n.minobsinnode = 15,
                                        learning.rate = 0.01, bag.fraction = 0.85, silent = F)
save(brt_nfis_ls_hlc_terrain_canopy_29, file="3_output/models/WT/brt_nfis_ls_hlc_terrain_canopy_29.rData")

##### get model stats----
###### put relevant stats into a dataframe (e.g. explained deviance)
varimp.brt_nfis_ls_hlc_terrain_canopy_29<- as.data.frame(brt_nfis_ls_hlc_terrain_canopy_29$contributions)%>%
  mutate(model="brt_nfis_ls_hlc_terrain_canopy_29")
save(varimp.brt_nfis_ls_hlc_terrain_canopy_29, file="3_output/model_results/WT/varimp.brt_nfis_ls_hlc_terrain_canopy_29.rData")

cvstats.brt_nfis_ls_hlc_terrain_canopy_29<- as.data.frame(brt_nfis_ls_hlc_terrain_canopy_29$cv.statistics[c(1,3,5)])
cvstats.brt_nfis_ls_hlc_terrain_canopy_29$deviance.null <- brt_nfis_ls_hlc_terrain_canopy_29$self.statistics$mean.null
cvstats.brt_nfis_ls_hlc_terrain_canopy_29$deviance.explained <- (cvstats.brt_nfis_ls_hlc_terrain_canopy_29$deviance.null-cvstats.brt_nfis_ls_hlc_terrain_canopy_29$deviance.mean)/cvstats.brt_nfis_ls_hlc_terrain_canopy_29$deviance.null
cvstats.brt_nfis_ls_hlc_terrain_canopy_29$model_name<-"brt_nfis_ls_hlc_terrain_canopy_29"
colnames(cvstats.brt_nfis_ls_hlc_terrain_canopy_29)[colnames(cvstats.brt_nfis_ls_hlc_terrain_canopy_29) == "discrimination.mean"] ="AUC"
save(cvstats.brt_nfis_ls_hlc_terrain_canopy_29, file="3_output/model_results/WT/cvstats.brt_nfis_ls_hlc_terrain_canopy_29.rData")


##///////////////////////////
##brt_parsimonious





##///////////////////////////
##brt_NFIS_noOff_12_noOff----
### Setup data----
df1<-bird_cov%>%
  na.omit()%>%
  dplyr::select(c(2:6, 85:102))%>%
  filter(year>2010)%>%
  as.data.frame()

df2<-df1%>%dplyr::select(-c(PIWO_offset))%>%
  mutate(PIWO = ifelse(PIWO > 0, 1, 0))


set.seed(123)
####Use gbm.step using tuned parameters----
brt_NFIS_noOff<- gbm.step(data=df2, gbm.x = c(2:ncol(df2)), gbm.y = 1,
                    family = "bernoulli", tree.complexity = 2,  n.minobsinnode = 15,
                    learning.rate = 0.01, bag.fraction = 0.85, silent = F)
save(brt_NFIS_noOff, file="3_output/models/WT/brt_NFIS_noOff.rData")

##### get model stats----
###### put relevant stats into a dataframe (e.g. explained deviance)
varimp.brt_NFIS_noOff<- as.data.frame(brt_NFIS_noOff$contributions)%>%
  mutate(model="brt_NFIS_noOff")
save(varimp.brt_NFIS_noOff, file="3_output/model_results/WT/varimp.brt_NFIS_noOff.rData")

cvstats.brt_NFIS_noOff<- as.data.frame(brt_NFIS_noOff$cv.statistics[c(1,3,5)])
cvstats.brt_NFIS_noOff$deviance.null <- brt_NFIS_noOff$self.statistics$mean.null
cvstats.brt_NFIS_noOff$deviance.explained <- (cvstats.brt_NFIS_noOff$deviance.null-cvstats.brt_NFIS_noOff$deviance.mean)/cvstats.brt_NFIS_noOff$deviance.null
cvstats.brt_NFIS_noOff$model_name<-"brt_NFIS_noOff"
colnames(cvstats.brt_NFIS_noOff)[colnames(cvstats.brt_NFIS_noOff) == "discrimination.mean"] ="AUC"
save(cvstats.brt_NFIS_noOff, file="3_output/model_results/WT/cvstats.brt_NFIS_noOff.rData")



##brt_nfis_ls_hlc_terrain_canopy_29_2----
### Setup data----
df1<-bird_cov%>%
  na.omit()%>%
  # dplyr::select(c(2:6, 7:36))%>%
  dplyr::select(-c(1))%>%
  as.data.frame()

df2<-df1%>%dplyr::select(-c(PIWO_offset))%>%
  mutate(PIWO = ifelse(PIWO > 0, 1, 0))


#create an offset dataframe
o <- as.data.frame(df1$PIWO_offset)%>%rename(PIWO_offset=`df1$PIWO_offset`)
o <- df1$PIWO_offset

set.seed(123)
####Use gbm.step using tuned parameters----
brt_nfis_ls_hlc_terrain_canopy_29_2<- gbm.step(data=df2, gbm.x = c(2:ncol(df2)), gbm.y = 1, offset=o,
                                             family = "bernoulli", tree.complexity = 2,  n.minobsinnode = 15,
                                             learning.rate = 0.001, bag.fraction = 0.5, silent = F)
save(brt_nfis_ls_hlc_terrain_canopy_29_2, file="3_output/models/WT/brt_nfis_ls_hlc_terrain_canopy_29_2.rData")

##### get model stats----
###### put relevant stats into a dataframe (e.g. explained deviance)
varimp.brt_nfis_ls_hlc_terrain_canopy_29_2<- as.data.frame(brt_nfis_ls_hlc_terrain_canopy_29_2$contributions)%>%
  mutate(model="brt_nfis_ls_hlc_terrain_canopy_29_2")
save(varimp.brt_nfis_ls_hlc_terrain_canopy_29_2, file="3_output/model_results/WT/varimp.brt_nfis_ls_hlc_terrain_canopy_29_2.rData")

cvstats.brt_nfis_ls_hlc_terrain_canopy_29_2<- as.data.frame(brt_nfis_ls_hlc_terrain_canopy_29_2$cv.statistics[c(1,3,5)])
cvstats.brt_nfis_ls_hlc_terrain_canopy_29_2$deviance.null <- brt_nfis_ls_hlc_terrain_canopy_29_2$self.statistics$mean.null
cvstats.brt_nfis_ls_hlc_terrain_canopy_29_2$deviance.explained <- (cvstats.brt_nfis_ls_hlc_terrain_canopy_29_2$deviance.null-cvstats.brt_nfis_ls_hlc_terrain_canopy_29_2$deviance.mean)/cvstats.brt_nfis_ls_hlc_terrain_canopy_29_2$deviance.null
cvstats.brt_nfis_ls_hlc_terrain_canopy_29_2$model_name<-"brt_nfis_ls_hlc_terrain_canopy_29_2"
colnames(cvstats.brt_nfis_ls_hlc_terrain_canopy_29_2)[colnames(cvstats.brt_nfis_ls_hlc_terrain_canopy_29_2) == "discrimination.mean"] ="AUC"
save(cvstats.brt_nfis_ls_hlc_terrain_canopy_29_2, file="3_output/model_results/WT/cvstats.brt_nfis_ls_hlc_terrain_canopy_29_2.rData")




