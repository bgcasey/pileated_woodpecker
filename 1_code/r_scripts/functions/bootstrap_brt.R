#' Perform Bootstrap Iterations for Boosted Regression Tree
#'
#' This function performs bootstrap iterations for a boosted 
#' regression tree (BRT). It returns an object containing a 
#' dataframe of model statistics and a list of the models.
#'
#' @param data data.frame. The data to be used for bootstrapping.
#' @param n_iterations integer. The number of bootstrap iterations 
#' to perform.
#' @param tuned_param data.frame. A data frame containing the tuned 
#' hyperparameters.
#' @return list. A list containing:
#'   - models: A list of the bootstrapped models.
#'   - stats_df: A dataframe with statistics for each model.
#' 
#' @example 
#' # Example usage of the function
#' library(gbm)
#' library(dplyr)
#' library(pROC)
#' library(parallel)
#' 
#' # Example data
#' random_data <- data.frame(
#'   PIWO_occ = sample(0:1, 100, replace = TRUE),
#'   PIWO_offset = runif(100),
#'   var1 = rnorm(100),
#'   var2 = rnorm(100)
#' )
#' 
#' # Example tuned parameters
#' tuned_param <- data.frame(
#'   interaction.depth = 3,
#'   n.minobsinnode = 20,
#'   shrinkage = 0.01,
#'   bag.fraction = 0.75
#' )
#' 
#' # Example function call
#' bootstrap_models <- bootstrap_brt(data = random_data, 
#'                                   n_iterations = 10,
#'                                   tuned_param = tuned_param)
#' 
#' # Example result printing
#' print(bootstrap_models$stats_df)
bootstrap_brt <- function(data, n_iterations, tuned_param) {
  # Step 1: Initialize an empty dataframe for stats
  num_predictors <- ncol(data) - 1
  cov_columns <- paste0("cov_", 1:num_predictors)
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
  for (cov in cov_columns) {
    all_stats_df[[cov]] <- numeric()
  }
  
  # Step 2: Initialize a list to store models
  models_list <- list()
  
  # Step 3: Define the function to run a single bootstrap iteration
  run_iteration <- function(i) {
    samp <- sample(nrow(data), round(0.75 * nrow(data)), 
                   replace = TRUE)
    train_data <- data[samp, ]
    test_data <- data[-samp, ]
    # o_train <- train_data$PIWO_offset
    # train_data <- dplyr::select(train_data, -PIWO_offset)
    # o_test <- test_data$PIWO_offset
    # test_data <- dplyr::select(test_data, -PIWO_offset)
    model <- gbm.step(
      data = train_data,
      gbm.x = 2:ncol(train_data),
      gbm.y = 1,
      # offset = o_train,
      family = "bernoulli",
      tree.complexity = tuned_param$interaction.depth[1],
      n.minobsinnode = tuned_param$n.minobsinnode[1],
      learning.rate = tuned_param$shrinkage[1],
      bag.fraction = tuned_param$bag.fraction[1],
      silent = FALSE,
      plot.main = FALSE
    )
    predictions <- predict(model, newdata = test_data[, -1], 
                           n.trees = model$gbm.call$best.trees, 
                           type = "response")
    roc_result <- pROC::roc(response = test_data[, 1], 
                            predictor = predictions)
    auc_value <- pROC::auc(roc_result)
    model_stats <- calculate_brt_stats(model = model)
    model_name <- paste("model", i, sep = "_")
    model_stats$model <- model_name
    model_stats$predict_AUC <- auc_value
    return(list(model = model, stats = model_stats))
  }
  
  # Step 4: Run the bootstrap iterations in parallel
  results <- mclapply(1:n_iterations, run_iteration, 
                      mc.cores = detectCores() - 1)
  
  # Step 5: Combine the results
  for (result in results) {
    models_list[[result$stats$model]] <- result$model
    all_stats_df <- rbind(all_stats_df, result$stats)
  }
  
  return(list(models = models_list, stats_df = all_stats_df))
}