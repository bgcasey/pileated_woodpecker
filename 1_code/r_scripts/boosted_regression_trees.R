# ---
# title: "Boosted regression trees"
# author: "Brendan Casey"
# created: "September 8, 2023"
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
## 1.1. Load Packages ----
library(tidyverse)  # For data manipulation
library(dismo)      # For species distribution modeling and BRTs
library(gbm)        # For boosted regression trees
library(pROC)       # For AUC calculation

## 1.2. Load custom functions ----
# including function for getting BRT model stats.
source("1_code/r_scripts/functions/utils.R") 

## 1.3. Load data ----
load("0_data/manual/formatted_for_models/data_for_models.rData")
data <- data_for_models

## 1.4. Load tuned parameters ----
load("2_pipeline/store/tuned_param.rData")

## 1.5. Randomize data ----
set.seed(123)

random_index <- sample(1:nrow(data), nrow(data))
random_data <- data[random_index, ]
o <- random_data$PIWO_offset
random_data <- dplyr::select(random_data, -c(PIWO_offset))

# 2. Boosted Regression Tree ----
## 2.1. Apply `dismo::gbm.step` to tuned parameters ----
brt_1 <- gbm.step(data = random_data, 
                  gbm.x = 2:ncol(random_data), 
                  gbm.y = 1, 
                  offset = o,
                  family = "bernoulli", 
                  tree.complexity = 2,  
                  n.minobsinnode = 10,
                  learning.rate = 0.01, 
                  bag.fraction = 0.85, 
                  silent = FALSE)
save(brt_1, file = "3_output/models/WT/brt_1.rData")

## 2.2. Get model stats ----
brt_1_stats <- calculate_brt_stats(model = brt_1)
save(brt_1_stats, file = "3_output/model_results/brt_1_stats.rData")

# 3. Simplified Boosted Regression Tree ----
## 3.1. Drop variables that don't improve model performance ----
brt_1_simp <- gbm.simplify(brt_1)
save(brt_1_simp, file = "2_pipeline/store/models/brt_1_simp.rData")

## 3.2. Remove non-numeric characters from the row names ----
rownames(brt_1_simp$deviance.summary) <- 
  gsub("[^0-9]", "", rownames(brt_1_simp$deviance.summary))

## 3.3. Get the optimal number of drops ----
optimal_no_drops <- as.numeric(rownames(
  brt_1_simp$deviance.summary %>% slice_min(mean)))

## 3.4. Remove dropped variables from the dataframe ----
random_data <- random_data %>%
  dplyr::select(PIWO, brt_1_simp$pred.list[[optimal_no_drops]])

## 3.5 BRT Second Iteration ----
brt_2 <- gbm.step(data = random_data, 
                  gbm.x = 2:ncol(random_data), 
                  gbm.y = 1, 
                  offset = o,
                  family = "bernoulli", 
                  tree.complexity = 2,  
                  n.minobsinnode = 10,
                  learning.rate = 0.01, 
                  bag.fraction = 0.85, 
                  silent = FALSE)

save(brt_2, file = "2_pipeline/store/models/brt_2.rData")

## 3.6 Get model stats ----
summary(brt_2)
brt_2_stats <- calculate_brt_stats(model = brt_2)
save(brt_2_stats, file = "3_output/model_results/brt_2_stats.rData")

# 4. Bootstrap model ----
# Perform bootstrap iterations through the simplified 
# model. It is computationally intensive and may take a long time to 
# run. Adjust the number of iterations based on computational 
# resources and needs.

## 4.1. Define the bootstrap function ----
# Function to perform bootstrap iterations for a boosted regression 
# tree. It returns an object containing a dataframe of model 
# statistics and a list of the models.
#
# Parameters:
#   data: The data to be used for bootstrapping.
#   n_iterations: The number of bootstrap iterations to perform.
#
# Returns:
#   A list containing:
#     - models: A list of the bootstrapped models.
#     - stats_df: A dataframe with statistics for each model.

bootstrap_brt <- function(data, n_iterations) {
  # Initialize an empty dataframe for stats
  all_stats_df <- data.frame(model = character(), 
                             deviance.mean = numeric(),
                             correlation.mean = numeric(), 
                             AUC = numeric(),
                             deviance.null = numeric(),
                             deviance.explained = numeric(), 
                             cov_1 = numeric(),
                             cov_2 = numeric(), 
                             cov_3 = numeric(),
                             cov_4 = numeric(), 
                             cov_5 = numeric(),
                             cov_6 = numeric(),
                             cov_7 = numeric(),
                             predict_AUC = numeric(), 
                             stringsAsFactors = FALSE)
  
  # Initialize a list to store models
  models_list <- list()
  
  for (i in 1:n_iterations) {
    # Sampling with replacement for bootstrap
    samp <- sample(nrow(data), round(0.75 * nrow(data)), 
                   replace = TRUE)
    train_data <- data[samp, ]
    test_data <- data[-samp, ]
    
    # Extract and remove offset
    o <- train_data$PIWO_offset
    train_data <- dplyr::select(train_data, -PIWO_offset)
    
    # Fit the model
    model <- gbm.step(data = train_data, 
                      gbm.x = 2:ncol(train_data), 
                      gbm.y = 1, 
                      offset = o,
                      family = "bernoulli", 
                      tree.complexity = 2,  
                      n.minobsinnode = 10,
                      learning.rate = 0.01, 
                      bag.fraction = 0.85, 
                      silent = FALSE,
                      plot.main = FALSE)
    
    # Predict and calculate AUC
    predictions <- predict(model, newdata = test_data[, -1], 
                           n.trees = model$gbm.call$best.trees, 
                           type = "response")
    roc_result <- pROC::roc(response = test_data[, 1], 
                            predictor = predictions)
    auc_value <- pROC::auc(roc_result)
    
    # Calculate stats for the trained model
    model_stats <- calculate_brt_stats(model = model)
    
    # Add model name and AUC to model_stats
    model_name <- paste("model", i, sep = "_")
    model_stats$model <- model_name
    model_stats$predict_AUC <- auc_value
    
    # Append to the accumulating dataframe
    all_stats_df <- rbind(all_stats_df, model_stats)
    
    # Store the model in the list
    models_list[[model_name]] <- model
  }
  
  return(list(models = models_list, stats_df = all_stats_df))
}

# Example usage:
# bootstrap_models <- bootstrap_brt(data, n_iterations)

## 4.2. Run the bootstraps ----
# Set the number of iterations
n_iterations <- 50    

# run the bootstrap function
bootstrap_models <- bootstrap_brt(data, n_iterations)

## 4.3. Save models ----
save(bootstrap_models, 
     file = "3_output/model_results/bootstrap_models.rData")

