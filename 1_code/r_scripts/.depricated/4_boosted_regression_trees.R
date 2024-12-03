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
load("0_data/manual/formatted_for_models/bird_cov_cleaned_20231120.rData")

##////////////////////////////////////////////////////////////////
# BRT Austin's data----
##brt_beaudoin ----
### Setup data----
df1<-bird_cov%>%
  na.omit()%>%
  dplyr::select(c(PIWOocc,    
                   contains('B_')))%>% 
  as.data.frame()

set.seed(123)
####Use gbm.step using tuned parameters----
brt_beaudoin<- gbm.step(data=df1, gbm.x = c(2:ncol(df1)), gbm.y = 1,
                                    family = "bernoulli", tree.complexity = 2,  n.minobsinnode = 15,
                                    learning.rate = 0.001, bag.fraction = 0.85, silent = F)
save(brt_beaudoin, file="3_output/models/AZ/brt_beaudoin.rData")

##### get model stats----
###### put relevant stats into a dataframe (e.g. explained deviance)
varimp.brt_beaudoin<- as.data.frame(brt_beaudoin$contributions)%>%
  mutate(model="brt_beaudoin")
save(varimp.brt_beaudoin, file="3_output/model_results/AZ/varimp.brt_beaudoin.rData")

cvstats.brt_beaudoin<- as.data.frame(brt_beaudoin$cv.statistics[c(1,3,5)])
cvstats.brt_beaudoin$deviance.null <- brt_beaudoin$self.statistics$mean.null
cvstats.brt_beaudoin$deviance.explained <- (cvstats.brt_beaudoin$deviance.null-cvstats.brt_beaudoin$deviance.mean)/cvstats.brt_beaudoin$deviance.null
cvstats.brt_beaudoin$model_name<-"brt_beaudoin"
colnames(cvstats.brt_beaudoin)[colnames(cvstats.brt_beaudoin) == "discrimination.mean"] ="AUC"
save(cvstats.brt_beaudoin, file="3_output/model_results/AZ/cvstats.brt_beaudoin.rData")


##////////////////////////////////////////////////////////////////
##brt_NFIS----
### Setup data----
df1<-bird_cov%>%
  na.omit()%>%
  dplyr::select(c(PIWOocc,    
                  contains('N_')))%>% 
  as.data.frame()

set.seed(123)
####Use gbm.step using tuned parameters----
brt_NFIS<- gbm.step(data=df1, gbm.x = c(2:ncol(df1)), gbm.y = 1,
                        family = "bernoulli", tree.complexity = 2,  n.minobsinnode = 15,
                        learning.rate = 0.001, bag.fraction = 0.5, silent = F)
save(brt_NFIS, file="3_output/models/AZ/brt_NFIS.rData")

##### get model stats----
###### put relevant stats into a dataframe (e.g. explained deviance)
varimp.brt_NFIS<- as.data.frame(brt_NFIS$contributions)%>%
  mutate(model="brt_NFIS")
save(varimp.brt_NFIS, file="3_output/model_results/AZ/varimp.brt_NFIS.rData")

cvstats.brt_NFIS<- as.data.frame(brt_NFIS$cv.statistics[c(1,3,5)])
cvstats.brt_NFIS$deviance.null <- brt_NFIS$self.statistics$mean.null
cvstats.brt_NFIS$deviance.explained <- (cvstats.brt_NFIS$deviance.null-cvstats.brt_NFIS$deviance.mean)/cvstats.brt_NFIS$deviance.null
cvstats.brt_NFIS$model_name<-"brt_NFIS"
colnames(cvstats.brt_NFIS)[colnames(cvstats.brt_NFIS) == "discrimination.mean"] ="AUC"
save(cvstats.brt_NFIS, file="3_output/model_results/AZ/cvstats.brt_NFIS.rData")


##////////////////////////////////////////////////////////////////
##brt_NFIS_1----
### Setup data----
df1<-master_1%>%
  na.omit()%>%
  dplyr::select(-location)%>% 
  as.data.frame()

set.seed(123)
####Use gbm.step using tuned parameters----
brt_NFIS_1<- gbm.step(data=df1, gbm.x = c(2:ncol(df1)), gbm.y = 1,
                    family = "bernoulli", tree.complexity = 2,  n.minobsinnode = 15,
                    learning.rate = 0.001, bag.fraction = 0.5, silent = F)
save(brt_NFIS_1, file="3_output/models/AZ/brt_NFIS_1.rData")

##### get model stats----
###### put relevant stats into a dataframe (e.g. explained deviance)
varimp.brt_NFIS_1<- as.data.frame(brt_NFIS_1$contributions)%>%
  mutate(model="brt_NFIS_1")
save(varimp.brt_NFIS_1, file="3_output/model_results/AZ/varimp.brt_NFIS_1.rData")

cvstats.brt_NFIS_1<- as.data.frame(brt_NFIS_1$cv.statistics[c(1,3,5)])
cvstats.brt_NFIS_1$deviance.null <- brt_NFIS_1$self.statistics$mean.null
cvstats.brt_NFIS_1$deviance.explained <- (cvstats.brt_NFIS_1$deviance.null-cvstats.brt_NFIS_1$deviance.mean)/cvstats.brt_NFIS_1$deviance.null
cvstats.brt_NFIS_1$model_name<-"brt_NFIS_1"
colnames(cvstats.brt_NFIS_1)[colnames(cvstats.brt_NFIS_1) == "discrimination.mean"] ="AUC"
save(cvstats.brt_NFIS_1, file="3_output/model_results/AZ/cvstats.brt_NFIS_1.rData")



##////////////////////////////////////////////////////////////////
##brt_GEE----
### Setup data----
df1<-bird_cov%>%
  na.omit()%>%
  dplyr::select(-c(location, PIWOcount, longitude, latitude,   
                   contains('B_'),contains('N_') ))%>% 
  as.data.frame()

set.seed(123)
####Use gbm.step using tuned parameters----
brt_GEE<- gbm.step(data=df1, gbm.x = c(2:ncol(df1)), gbm.y = 1,
                    family = "bernoulli", tree.complexity = 2,  n.minobsinnode = 15,
                    learning.rate = 0.001, bag.fraction = 0.5, silent = F)
save(brt_GEE, file="3_output/models/AZ/brt_GEE.rData")

##### get model stats----
###### put relevant stats into a dataframe (e.g. explained deviance)
varimp.brt_GEE<- as.data.frame(brt_GEE$contributions)%>%
  mutate(model="brt_GEE")
save(varimp.brt_GEE, file="3_output/model_results/AZ/varimp.brt_GEE.rData")

cvstats.brt_GEE<- as.data.frame(brt_GEE$cv.statistics[c(1,3,5)])
cvstats.brt_GEE$deviance.null <- brt_GEE$self.statistics$mean.null
cvstats.brt_GEE$deviance.explained <- (cvstats.brt_GEE$deviance.null-cvstats.brt_GEE$deviance.mean)/cvstats.brt_GEE$deviance.null
cvstats.brt_GEE$model_name<-"brt_GEE"
colnames(cvstats.brt_GEE)[colnames(cvstats.brt_GEE) == "discrimination.mean"] ="AUC"
save(cvstats.brt_GEE, file="3_output/model_results/AZ/cvstats.brt_GEE.rData")


##////////////////////////////////////////////////////////////////
##brt_NFIS_and_GEE----
### Setup data----
df1<-bird_cov%>%
  na.omit()%>%
  dplyr::select(-c(location, PIWOcount, longitude, latitude,   
                   contains('B_')))%>% 
  as.data.frame()

set.seed(123)
####Use gbm.step using tuned parameters----
brt_NFIS_and_GEE<- gbm.step(data=df1, gbm.x = c(2:ncol(df1)), gbm.y = 1,
                   family = "bernoulli", tree.complexity = 2,  n.minobsinnode = 15,
                   learning.rate = 0.001, bag.fraction = 0.5, silent = F)
save(brt_NFIS_and_GEE, file="3_output/models/AZ/brt_NFIS_and_GEE.rData")

##### get model stats----
###### put relevant stats into a dataframe (e.g. explained deviance)
varimp.brt_NFIS_and_GEE<- as.data.frame(brt_NFIS_and_GEE$contributions)%>%
  mutate(model="brt_NFIS_and_GEE")
save(varimp.brt_NFIS_and_GEE, file="3_output/model_results/AZ/varimp.brt_NFIS_and_GEE.rData")

cvstats.brt_NFIS_and_GEE<- as.data.frame(brt_NFIS_and_GEE$cv.statistics[c(1,3,5)])
cvstats.brt_NFIS_and_GEE$deviance.null <- brt_NFIS_and_GEE$self.statistics$mean.null
cvstats.brt_NFIS_and_GEE$deviance.explained <- (cvstats.brt_NFIS_and_GEE$deviance.null-cvstats.brt_NFIS_and_GEE$deviance.mean)/cvstats.brt_NFIS_and_GEE$deviance.null
cvstats.brt_NFIS_and_GEE$model_name<-"brt_NFIS_and_GEE"
colnames(cvstats.brt_NFIS_and_GEE)[colnames(cvstats.brt_NFIS_and_GEE) == "discrimination.mean"] ="AUC"
save(cvstats.brt_NFIS_and_GEE, file="3_output/model_results/AZ/cvstats.brt_NFIS_and_GEE.rData")






##////////////////////////////////////////////////////////////////
##brt_beaudoin_and_GEE----
### Setup data----
df1<-bird_cov%>%
  na.omit()%>%
  dplyr::select(-c(location, PIWOcount, longitude, latitude,   
                  contains('N_')))%>% 
  as.data.frame()

set.seed(123)
####Use gbm.step using tuned parameters----
brt_beaudoin_and_GEE<- gbm.step(data=df1, gbm.x = c(2:ncol(df1)), gbm.y = 1,
                   family = "bernoulli", tree.complexity = 2,  n.minobsinnode = 15,
                   learning.rate = 0.001, bag.fraction = 0.5, silent = F)
save(brt_beaudoin_and_GEE, file="3_output/models/AZ/brt_beaudoin_and_GEE.rData")

##### get model stats----
###### put relevant stats into a dataframe (e.g. explained deviance)
varimp.brt_beaudoin_and_GEE<- as.data.frame(brt_beaudoin_and_GEE$contributions)%>%
  mutate(model="brt_beaudoin_and_GEE")
save(varimp.brt_beaudoin_and_GEE, file="3_output/model_results/AZ/varimp.brt_beaudoin_and_GEE.rData")

cvstats.brt_beaudoin_and_GEE<- as.data.frame(brt_beaudoin_and_GEE$cv.statistics[c(1,3,5)])
cvstats.brt_beaudoin_and_GEE$deviance.null <- brt_beaudoin_and_GEE$self.statistics$mean.null
cvstats.brt_beaudoin_and_GEE$deviance.explained <- (cvstats.brt_beaudoin_and_GEE$deviance.null-cvstats.brt_beaudoin_and_GEE$deviance.mean)/cvstats.brt_beaudoin_and_GEE$deviance.null
cvstats.brt_beaudoin_and_GEE$model_name<-"brt_beaudoin_and_GEE"
colnames(cvstats.brt_beaudoin_and_GEE)[colnames(cvstats.brt_beaudoin_and_GEE) == "discrimination.mean"] ="AUC"
save(cvstats.brt_beaudoin_and_GEE, file="3_output/model_results/AZ/cvstats.brt_beaudoin_and_GEE.rData")





