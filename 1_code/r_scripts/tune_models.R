# ---
# title: "Tune models"
# author: "Brendan Casey"
# created: "2024-06-10"
# description: >
#   "This script tunes boosted regression tree models by
#   using a hyperparameter grid to systematically evaluate
#   combinations of model parameters. Code is based on
#   https://uc-r.github.io/gbm_regression and Kuhn, M., &
#   Johnson, K. (2013). Applied predictive modeling (Vol.
#   26, p. 13). New York: Springer. It produces a dataframe
#   of models using different parameter combinations sorted
#   by RMSE."
# ---

# 1. Setup ----
## 1.1 Load packages ----
library(gbm)   # For fitting generalized boosted regression models

## 1.2 Load data ----
load("0_data/manual/formatted_for_models/data_for_models.rData")

# 2. Tune model parameters ----
set.seed(123) # Ensure reproducibility

## 2.1 Create hyperparameter grid ----
hyper_grid <- expand.grid(
  shrinkage = c(.001, .01, .1),
  interaction.depth = c(2, 3),
  n.minobsinnode = c(10, 15, 20, 30),
  bag.fraction = c(.5, .75, .85), 
  optimal_trees = 0,  # Placeholder for results
  min_RMSE = 0        # Placeholder for results
)

# Check total number of combinations
nrow(hyper_grid)

## 2.2 Randomize data ----
# Randomize to remove patterns in data ordering (e.g., by site or 
# neighborhood)
random_index <- sample(1:nrow(df1), nrow(df1))
random_df1 <- df1[random_index, ]

## 2.3 Model tuning based on the hyperparameter grid ----
for(i in 1:nrow(hyper_grid)) {
  
  # Ensure reproducibility
  set.seed(123)
  
  # Train model
  gbm.tune <- gbm(
    formula = PIWO ~ .,
    distribution = "gaussian",
    data = random_df1,
    n.trees = 5000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL, # Use all cores by default
    verbose = FALSE
  )
  
  # Update hyper_grid with model performance metrics
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

## 3. Save tuned parameters ----
# Arrange by minimum RMSE and save
tuned_param <- hyper_grid %>%
  dplyr::arrange(min_RMSE) 
save(tuned_param, file="2_pipeline/store/tuned_param.rData")
