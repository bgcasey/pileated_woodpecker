# title: "Tune models"
# author: "Brendan Casey"
# created: "2024-06-10"
# description: >
#   This script tunes boosted regression tree models by
#   using a hyperparameter grid to systematically evaluate
#   combinations of model parameters. Code is based on
#   https://uc-r.github.io/gbm_regression and Kuhn, M., &
#   Johnson, K. (2013). Applied predictive modeling (Vol.
#   26, p. 13). New York: Springer. It produces a dataframe
#   of models using different parameter combinations sorted
#   by RMSE.

# 1. Setup ----
## 1.1 Load packages ----
library(dplyr) # Data wrangling
library(gbm) # For fitting generalized boosted regression models
library(doParallel) # For parallel processing
library(foreach) # For parallel processing
library(progress) # For progress bar

## 1.2 Load data ----
load("0_data/manual/formatted_for_models/data_for_models.rData")

## 1.3 Set seed ----
set.seed(123) # Ensure reproducibility

# 2. Tune model parameters ----
## 2.1 Create hyperparameter grid ----
hyper_grid <- expand.grid(
  shrinkage = c(.001, .01),
  interaction.depth = c(2, 3),
  n.minobsinnode = c(10, 15, 20, 30, 50, 100),
  bag.fraction = c(.5, .75, .85),
  optimal_trees = 0, # Placeholder for results
  min_RMSE = 0 # Placeholder for results
)

# Check total number of combinations
nrow(hyper_grid)

## 2.2 Randomize data ----
# Randomize to remove patterns in data ordering (e.g., by site or
# neighborhood)
random_index <- sample(1:nrow(data_brt), nrow(data_brt))
random_data <- data_brt[random_index, ]

o <- random_data$PIWO_offset
random_data <- dplyr::select(random_data, -c(PIWO_offset))

## 2.3 Model tuning based on the hyperparameter grid ----
# Set up parallel processing
cl <- makeCluster(detectCores() - 1) # Use all but one core
registerDoParallel(cl)

# Initialize progress bar
pb <- progress_bar$new(
  format = "  Tuning [:bar] :percent in :elapsed, ETA: :eta",
  total = nrow(hyper_grid),
  clear = FALSE,
  width = 60
)

# Parallel processing with foreach
results <- foreach(
  i = 1:nrow(hyper_grid),
  .combine = rbind,
  .packages = c("gbm", "dplyr")
) %dopar% {
  # Ensure reproducibility
  set.seed(123)

  # Train model
  gbm.tune <- gbm(
    formula = PIWO_occ ~ . + offset(o),
    distribution = "gaussian",
    data = random_data,
    n.trees = 5000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL, # Use all cores by default
    verbose = FALSE
  )

  # Update progress bar
  pb$tick()

  # Return results
  data.frame(
    optimal_trees = which.min(gbm.tune$valid.error),
    min_RMSE = sqrt(min(gbm.tune$valid.error))
  )
}

# Combine results with hyper_grid
hyper_grid$optimal_trees <- results$optimal_trees
hyper_grid$min_RMSE <- results$min_RMSE

# Stop parallel processing
stopCluster(cl)

## 3. Save tuned parameters ----
# Arrange by minimum RMSE and save
tuned_param <- hyper_grid %>%
  dplyr::arrange(min_RMSE)
save(tuned_param, file = "2_pipeline/store/tuned_param.rData")
