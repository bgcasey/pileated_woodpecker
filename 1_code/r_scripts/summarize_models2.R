#Specify the full path to the directory containing the RData files
directory_path <- "3_output/model_results/WT"

# List all RData files in the directory that include the string "varimp"
file_list <- list.files(path = directory_path, pattern = "varimp.*\\.RData", full.names = TRUE, ignore.case = TRUE)

# Load each RData file into the R environment
for (file in file_list) {
  load(file)
}


# List all RData files in the directory that include the string "cvstat"
file_list <- list.files(path = directory_path, pattern = "cvstat.*\\.RData", full.names = TRUE, ignore.case = TRUE)

# Load each RData file into the R environment
for (file in file_list) {
  load(file)
}


test<-rbind(cvstats.brt_hlc_29, cvstats.brt_ls_29, cvstats.brt_ls_hlc_29, cvstats.brt_ls_hlc_terrain_29, cvstats.brt_ls_hlc_terrain_canopy_29,
            cvstats.brt_NFIS, cvstats.brt_NFIS_ls_12, cvstats.brt_nfis_ls_hlc_terrain_canopy_29, cvstats.brt_nfis_ls_hlc_terrain_canopy_29_2, cvstats.brt_NFIS_noOff)


## R2 for individual predictors

#select predictor
y_new <- df2[,5]

num <- var(predict(brt_NFIS)-y_new)
den <- var(y_new)

R2 <- 1-(num/den)
print(R2)



Anguilla_train


