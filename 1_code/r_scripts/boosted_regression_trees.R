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
# dependencies:
#   - tidyverse
#   - dismo
#   - gbm
#   - pROC
#   - 1_code/r_scripts/functions/utils.R
# ---

# 1. Setup ----
## 1.1. Load Packages ----
library(tidyverse) # For data manipulation
library(dismo) # For species distribution modelling and BRTs
library(gbm) # For boosted regression trees
library(pROC) # For AUC calculation
library(parallel) # For parallel processing

## 1.2. Set file directory path ----
path <- "3_output/models/s2_vars_noOff"

## 1.3. Load custom functions ----
# including function for getting BRT model stats.
source("1_code/r_scripts/functions/utils.R")

## 1.4. Load data ----
load("0_data/manual/formatted_for_models/data_for_models.rData")

# data_brt <- data_brt %>% dplyr::select(-starts_with("ls_"))
data_brt <- data_brt %>% dplyr::select(-starts_with("ls_"))
data_brt <- na.omit(data_brt)

## 1.5. Load tuned parameters ----
load(paste0(path, "/tuned_param.rData"))

## 1.6. Set seed ----
set.seed(123)

## 1.7. Randomize data ----
random_index <- sample(1:nrow(data_brt), nrow(data_brt))
random_data <- data_brt[random_index, ]
# o <- random_data$PIWO_offset
o <- log(random_data$survey_effort)
# random_data <- dplyr::select(random_data, -c(PIWO_offset))
random_data <- dplyr::select(random_data, -c(PIWO_offset))

# 2. Boosted Regression Tree ----
## 2.1. Apply `dismo::gbm.step` to tuned parameters ----
brt_1 <- gbm.step(
  data = random_data,
  gbm.x = 2:ncol(random_data),
  gbm.y = 1,
  # offset = o,
  family = "bernoulli",
  tree.complexity = tuned_param$interaction.depth[1],
  n.minobsinnode = tuned_param$n.minobsinnode[1],
  learning.rate = tuned_param$shrinkage[1],
  bag.fraction = tuned_param$bag.fraction[1],
  cv.folds = 10,
  silent = FALSE,
  plot.main = FALSE
)

save(brt_1,
  file = paste0(path, "/brt_1.rData")
)

## 2.2. Get model stats ----
brt_1_stats <- calculate_brt_stats(model = brt_1) %>%
  rename(auc = discrimination.mean)

save(brt_1_stats,
  file = paste0(path, "/brt_1_stats.rData")
)

# 3. Simplified Boosted Regression Tree ----
## 3.1. Drop variables that don't improve model performance ----
brt_1_simp <- gbm.simplify(brt_1, plot = FALSE)

save(brt_1_simp,
  file = paste0(path, "/brt_1_simp.rData")
)

## 3.2. Remove non-numeric characters from the row names ----
rownames(brt_1_simp$deviance.summary) <-
  gsub("[^0-9]", "", rownames(brt_1_simp$deviance.summary))

## 3.3. Get the optimal number of drops ----
optimal_no_drops <- as.numeric(rownames(
  brt_1_simp$deviance.summary %>% slice_min(mean)
))

## 3.4. Remove dropped variables from the dataframe ----
random_data_simp <- random_data %>%
  dplyr::select(PIWO_occ, brt_1_simp$pred.list[[optimal_no_drops[1]]])

## 3.5. Re tune model ----
# Re tune model with reduced number of predictors using tune_models.R.
load(paste0(path, "/tuned_param_2.rData"))

## 3.6 BRT Second Iteration ----
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
  silent = FALSE
)

save(brt_2,
  file = paste0(path, "/brt_2.rData")
)

## 3.7. Get model stats ----
brt_2_stats <- calculate_brt_stats(model = brt_2) %>%
  rename(auc = discrimination.mean)

save(brt_2_stats,
  file = paste0(path, "/brt_2_stats.rData")
)

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

  # Number of predictors in random_data
  num_predictors <- ncol(random_data) - 1

  # Create the column names for covariates dynamically
  cov_columns <- paste0("cov_", 1:num_predictors)

  # Create the data frame with dynamic covariate columns
  all_stats_df <- data.frame(
    model = character(),
    deviance.mean = numeric(),
    correlation.mean = numeric(),
    discrimination.mean = numeric(),
    deviance.null = numeric(),
    deviance.explained = numeric(),
    predict_AUC = numeric(),
    stringsAsFactors = FALSE
  )

  # Add covariate columns
  for (cov in cov_columns) {
    all_stats_df[[cov]] <- numeric()
  }

  # Initialize a list to store models
  models_list <- list()

  # Define the function to run a single bootstrap iteration
  run_iteration <- function(i) {
    # Sampling with replacement for bootstrap
    samp <- sample(nrow(data),
      round(0.75 * nrow(data)),
      replace = TRUE
    )
    train_data <- data[samp, ]
    test_data <- data[-samp, ]

    # Extract and remove offset
    o_train <- train_data$PIWO_offset
    train_data <- dplyr::select(train_data, -PIWO_offset)

    o_test <- test_data$PIWO_offset
    test_data <- dplyr::select(test_data, -PIWO_offset)

    # Fit the model
    model <- gbm.step(
      data = train_data,
      gbm.x = 2:ncol(train_data),
      gbm.y = 1,
      # offset = o_train,
      family = "bernoulli",
      tree.complexity = tuned_param_2$interaction.depth[1],
      n.minobsinnode = tuned_param_2$n.minobsinnode[1],
      learning.rate = tuned_param_2$shrinkage[1],
      bag.fraction = tuned_param_2$bag.fraction[1],
      silent = FALSE,
      plot.main = FALSE
    )

    # Predict and calculate AUC
    predictions <- predict(model,
      newdata = test_data[, -1],
      n.trees = model$gbm.call$best.trees,
      type = "response"
    )
    roc_result <- pROC::roc(
      response = test_data[, 1],
      predictor = predictions
    )
    auc_value <- pROC::auc(roc_result)

    # Calculate stats for the trained model
    model_stats <- calculate_brt_stats(model = model)

    # Add model name and AUC to model_stats
    model_name <- paste("model", i, sep = "_")
    model_stats$model <- model_name
    model_stats$predict_AUC <- auc_value

    return(list(model = model, stats = model_stats))
  }

  # Run the bootstrap iterations in parallel
  results <- mclapply(1:n_iterations,
    run_iteration,
    mc.cores = detectCores() - 1
  )

  # Combine the results
  for (result in results) {
    models_list[[result$stats$model]] <- result$model
    all_stats_df <- rbind(all_stats_df, result$stats)
  }

  return(list(models = models_list, stats_df = all_stats_df))
}

# Example usage:
# bootstrap_models <- bootstrap_brt(data, n_iterations)

## 4.2. Run the bootstraps ----
# Set the number of iterations
n_iterations <- 100

# run the bootstrap function
random_data_simp$PIWO_offset <- o
bootstrap_models <- bootstrap_brt(random_data_simp, n_iterations)

## 4.3. Save models ----
save(bootstrap_models,
  file = paste0(path, "/bootstrap_models.rData")
)
