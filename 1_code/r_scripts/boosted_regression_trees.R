# ---
# title: "Boosted regression trees"
# author: "Brendan Casey"
# created: "2024-07-20"
# description: >
#   This script performs a series of operations to fit
#   boosted regression tree models to Pileated Woodpecker
#   data. It includes model fitting with tuned parameters,
#   simplification of the initial model by dropping
#   non-informative variables, and a bootstrap procedure
#   to assess model stability. Custom functions are
#   utilized for calculating model statistics. The script
#   saves output files containing the bootstrapped models,
#   and a dataframe with model statistics.
# ---

# 1. Setup ----
## 1.1 Load Packages ----
library(tidyverse) # For data manipulation
library(dismo) # For species distribution modelling and BRTs
library(gbm) # For boosted regression trees
library(pROC) # For AUC calculation
library(doParallel)
library(foreach)
library(parallel) # For parallel processing

## 1.2 Set file directory path ----
# path <- "3_output/models/s2"
path <- "3_output/models/ls_noOff_noYear"
# path <- "3_output/models/ls"
# path <- "3_output/models/ls_noOff"

## 1.3 Load custom functions ----
# including function for getting BRT model stats.
source("1_code/r_scripts/functions/calculate_brt_stats.R")
source("1_code/r_scripts/functions/tune_gbm_model.R")
source("1_code/r_scripts/functions/bootstrap_brt.R")

## 1.4 Load data ----
load("0_data/manual/formatted_for_models/data_for_models.rData")

## 1.5 Set up data ----
data_brt <- data_brt %>%
  dplyr::select(
    -starts_with("s2_"),
    -x_3978,
    -y_3978,
    -year,
    -ends_with("_0")
  ) %>%
  na.omit() %>%
  filter(nrname != "grassland")

## 1.6 Set seed ----
set.seed(123)

## 1.7 Randomize data ----
random_index <- sample(1:nrow(data_brt), nrow(data_brt))
random_data <- data_brt[random_index, ]

## 1.8 Define offset ----
o <- log(random_data$PIWO_offset)
# random_data <- dplyr::select(random_data, -c(PIWO_offset, survey_effort))
random_data <- dplyr::select(random_data, -c(PIWO_offset))

# 2. BRT 1 ----
## 2.1 Tune parameters ----
tune_data <- random_data
# tune_data$offset <- o
# tuned_param_1 <- tune_gbm_model(data = tune_data,
#                                 formula = PIWO_occ ~ . +
#                                 offset(offset))
tuned_param_1 <- tune_gbm_model(
  data = tune_data,
  formula = PIWO_occ ~ .
)
save(tuned_param_1,
  file = paste0(path, "/tuned_param_1.rData")
)

## 2.2 BRT First iteration ----
# Apply `dismo::gbm.step` to tuned parameters
brt_1 <- gbm.step(
  data = random_data,
  gbm.x = 2:ncol(random_data),
  gbm.y = 1,
  # offset = o,
  family = "bernoulli",
  tree.complexity = tuned_param_1$interaction.depth[1],
  n.minobsinnode = tuned_param_1$n.minobsinnode[1],
  learning.rate = tuned_param_1$shrinkage[1],
  bag.fraction = tuned_param_1$bag.fraction[1],
  cv.folds = 10,
  silent = FALSE,
  plot.main = FALSE
)

save(brt_1,
  file = paste0(path, "/brt_1.rData")
)

## 2.2 Get model stats ----
brt_1_stats <- calculate_brt_stats(model = brt_1) %>%
  rename(auc = discrimination.mean)

save(brt_1_stats,
  file = paste0(path, "/brt_1_stats.rData")
)

# 3. BRT 2 ----
## Simplify Boosted Regression Tree by relative influence.

## 3.1 Drop variables that don't improve model performance ----
# Extract the summary of the model
model_summary <- summary(brt_1, plotit = FALSE)

# Filter out predictors with relative influence < 1
important_predictors <- model_summary$var[model_summary$rel.inf >= 1]

# Subset the data to include only the important predictors
random_data_simp <- random_data[, c("PIWO_occ", important_predictors)]

## 3.2. Tune parameters ----
# Tune model with reduced number of predictors
tune_data <- random_data_simp
# tune_data$offset <- o
# tuned_param_2 <- tune_gbm_model(data = tune_data,
#                               formula = PIWO_occ ~ . +
#                                 offset(offset))
tuned_param_2 <- tune_gbm_model(
  data = tune_data,
  formula = PIWO_occ ~ .
)
save(tuned_param_2,
  file = paste0(path, "/tuned_param_2.rData")
)

## 3.3 BRT Second Iteration ----
brt_2 <- gbm.step(
  data = random_data_simp,
  gbm.x = 2:ncol(random_data_simp),
  gbm.y = 1,
  # offset = o,
  family = "bernoulli",
  tree.complexity = tuned_param_2$interaction.depth[1],
  n.minobsinnode = tuned_param_2$n.minobsinnode[1],
  learning.rate = tuned_param_2$shrinkage[1],
  bag.fraction = tuned_param_2$bag.fraction[1],
  cv.folds = 10,
  silent = FALSE,
  plot.main = FALSE
)

save(brt_2,
  file = paste0(path, "/brt_2.rData")
)

## 3.4 Get model stats ----
brt_2_stats <- calculate_brt_stats(model = brt_2) %>%
  rename(auc = discrimination.mean)

save(brt_2_stats,
  file = paste0(path, "/brt_2_stats.rData")
)


# 4. BRT 3 ----
# Simplify Boosted Regression Tree by mean change in deviance.

## 4.1 gbm.simplify ----
brt_1_simp <- gbm.simplify(brt_1, plot = FALSE)

save(brt_1_simp,
  file = paste0(path, "/brt_1_simp.rData")
)

## 4.2 Remove non-numeric characters from the row names ----
rownames(brt_1_simp$deviance.summary) <-
  gsub("[^0-9]", "", rownames(brt_1_simp$deviance.summary))

## 4.3 Get the optimal number of drops ----
optimal_no_drops <- as.numeric(rownames(
  brt_1_simp$deviance.summary %>% slice_min(mean)
))

## 4.4 Remove dropped variables from the dataframe ----
random_data_simp <- random_data %>%
  dplyr::select(PIWO_occ, brt_1_simp$pred.list[[optimal_no_drops[1]]])

save(random_data_simp,
  file = paste0(path, "/random_data_simp.rData")
)

## 4.5 Tune parameters ----
tune_data <- random_data_simp
# tune_data$offset <- o
# tuned_param_3 <- tune_gbm_model(data = tune_data,
#                                 formula = PIWO_occ ~ . +
#                                   offset(offset))
tuned_param_3 <- tune_gbm_model(
  data = tune_data,
  formula = PIWO_occ ~ .
)
save(tuned_param_3,
  file = paste0(path, "/tuned_param_3.rData")
)

## 4.6 BRT Third Iteration ----
brt_3 <- gbm.step(
  data = random_data_simp,
  gbm.x = 2:ncol(random_data_simp),
  gbm.y = 1,
  # offset = o,
  family = "bernoulli",
  tree.complexity = tuned_param_3$interaction.depth[1],
  n.minobsinnode = tuned_param_3$n.minobsinnode[1],
  learning.rate = tuned_param_3$shrinkage[1],
  bag.fraction = tuned_param_3$bag.fraction[1],
  cv.folds = 10,
  silent = FALSE,
  plot.main = FALSE
)

save(brt_3,
  file = paste0(path, "/brt_3.rData")
)

## 4.7 Get model stats ----
brt_3_stats <- calculate_brt_stats(model = brt_3) %>%
  rename(auc = discrimination.mean)

save(brt_3_stats,
  file = paste0(path, "/brt_3_stats.rData")
)

# 5. View model stats ----
brt_all_stats <- bind_rows(brt_1_stats, brt_2_stats, brt_3_stats)

save(brt_all_stats,
  file = paste0(path, "/brt_all_stats.rData")
)

# 4. Bootstrap model ----
# Perform bootstrap iterations through the simplified
# model. It is computationally intensive and may take a long time to
# run. Adjust the number of iterations based on computational
# resources and needs.

## 4.1 Set up ----
# add the offset back to the main dataframe
# random_data_simp$PIWO_offset <- o

# Set the number of iterations
n_iterations <- 100

## 4.2 run the bootstrap function ----
bootstrap_models <- bootstrap_brt(
  random_data_simp,
  n_iterations,
  tuned_param_3
)

## 4.3 Save models ----
save(bootstrap_models,
  file = paste0(path, "/bootstrap_models.rData")
)

## 4.4 Summarize model stats ----
# Assuming bootstrap_models$stats_df is your data frame
stats_df <- bootstrap_models$stats_df[2:ncol(bootstrap_models$stats_df)]

# Calculate the mean of each column
mean_values <- colMeans(stats_df, na.rm = TRUE)
# Calculate the standard deviation of each column
sd_values <- apply(stats_df, 2, sd, na.rm = TRUE)

# Combine the mean and standard deviation into a single data frame
bootstrap_models_stats <- rbind(
  mean = mean_values,
  sd = sd_values
  ) %>%
  as.data.frame() %>%
  dplyr::select(
    deviance.mean,
    correlation.mean,
    discrimination.mean,
    predict_AUC,
    everything()
  )

# Save
save(bootstrap_models_stats,
  file = paste0(path, "/bootstrap_models_stats.rData")
)
