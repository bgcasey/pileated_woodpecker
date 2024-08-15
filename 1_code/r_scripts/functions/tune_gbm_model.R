#' Tune GBM Model
#'
#' This function tunes the hyperparameters of a Gradient Boosting
#' Machine (GBM) model using a grid search approach and parallel
#' processing.
#'
#' @param data data.frame. The dataset to be used for training the
#' model.
#' @param formula formula. The formula specifying the model to be
#' trained.
#' @return data.frame. A data frame containing the hyperparameter
#' grid with the optimal number of trees and minimum RMSE for each
#' combination.
#'
#' @example
#' # Example usage of the function
#' library(gbm)
#' library(dplyr)
#' library(doParallel)
#' library(foreach)
#'
#' # Example data
#' random_data_simp <- data.frame(
#'   PIWO_occ = sample(0:1, 100, replace = TRUE),
#'   offset = runif(100),
#'   var1 = rnorm(100),
#'   var2 = rnorm(100)
#' )
#'
#' # Example function call
#' tuned_results <- tune_gbm_model(data = random_data_simp,
#'                                 formula = PIWO_occ ~ . +
#'                                 offset(offset))
#'
#' # Example result printing
#' print(tuned_results)
tune_gbm_model <- function(data, formula) {
  # Step 1: Create hyperparameter grid
  hyper_grid <- expand.grid(
    shrinkage = c(.001, .01),
    interaction.depth = c(2, 3),
    n.minobsinnode = c(10, 15, 20, 30, 50, 100),
    bag.fraction = c(.5, .75, .85),
    optimal_trees = 0, # Placeholder for results
    min_RMSE = 0 # Placeholder for results
  )

  # Step 2: Set up parallel processing
  cl <- makeCluster(detectCores() - 1) # Use all but one core
  registerDoParallel(cl)

  # Step 3: Parallel processing with foreach
  results <- foreach(
    i = 1:nrow(hyper_grid),
    .combine = rbind,
    .packages = c("gbm", "dplyr")
  ) %dopar% {
    # Ensure reproducibility
    set.seed(123)

    # Train model
    gbm.tune <- gbm(
      formula = formula,
      distribution = "bernoulli",
      data = data,
      n.trees = 5000,
      interaction.depth = hyper_grid$interaction.depth[i],
      shrinkage = hyper_grid$shrinkage[i],
      n.minobsinnode = hyper_grid$n.minobsinnode[i],
      bag.fraction = hyper_grid$bag.fraction[i],
      train.fraction = .75,
      verbose = FALSE
    )

    # Return results
    data.frame(
      optimal_trees = which.min(gbm.tune$valid.error),
      min_RMSE = sqrt(min(gbm.tune$valid.error))
    )
  }

  # Step 4: Combine results with hyper_grid
  hyper_grid$optimal_trees <- results$optimal_trees
  hyper_grid$min_RMSE <- results$min_RMSE
  hyper_grid <- hyper_grid %>%
    dplyr::arrange(min_RMSE)

  # Step 5: Stop parallel processing
  stopCluster(cl)

  return(hyper_grid)
}
