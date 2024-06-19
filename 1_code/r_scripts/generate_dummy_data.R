# ---
# title: "Create dummy data"
# author: "Brendan Casey"
# created: "2024-06-18"
# description: >
#   Create dummy data for testing purposes. This script generates a
#   dummy data frame and raster stack.
# ---

# 1. Setup ----
## 1.1 Load Packages ----
library(tidyverse) # for data manipulation
library(terra)     # for working with rasters

set.seed(123)      # For reproducibility

# 2. Create dummy data frame ----
## 2.1 Generate covariates ----
cov_1 = runif(500, 0, 100)    # Continuous covariate
cov_2 = rnorm(500, 50, 10)    # Continuous covariate
cov_3 = sample(1:10, 500, replace = TRUE) # Discrete covariate
cov_4 = rnorm(500, 0, 1)      # Continuous covariate
cov_5 = runif(500, -50, 50)   # Continuous covariate

## 2.2 Calculate a linear combination of covariates ----
linear_combination = -2 + 0.05 * cov_1 - 0.04 * cov_2 + 
  0.5 * cov_3 + 0.1 * cov_4 - 0.02 * cov_5

## 2.3 Apply logistic function to get probabilities ----
probabilities = 1 / (1 + exp(-linear_combination))

## 2.4 Generate PIWO based on probabilities ----
PIWO = rbinom(500, 1, probabilities)

## 2.5 Create the dataframe ----
dummy_df <- data.frame(
  PIWO = PIWO,
  PIWO_offset = rnorm(500, 0, 1),
  cov_1 = cov_1,
  cov_2 = cov_2,
  cov_3 = cov_3,
  cov_4 = cov_4,
  cov_5 = cov_5
)

# 3. Create dummy rasters ----
## 3.1 Define raster parameters ----
nrows <- 100  # Number of rows
ncols <- 50   # Number of columns

## 3.2 Create an empty raster template ----
raster_template <- rast(nrows=nrows, ncols=ncols, vals=NA)

## 3.3 Generate values ----
cov_1 = runif(5000, 0, 100)    # Continuous covariate
cov_2 = rnorm(5000, 50, 10)    # Continuous covariate
cov_3 = sample(1:10, 5000, replace = TRUE) # Discrete covariate
cov_4 = rnorm(5000, 0, 1)      # Continuous covariate
cov_5 = runif(5000, -50, 50)   # Continuous covariate

## 3.4 Function to create rasters ----
create_raster_with_pattern <- function(template, values, pattern) {
  r <- template
  nrows <- nrow(r)
  ncols <- ncol(r)
  if (pattern == "NS_Gradient") {
    sorted_values <- sort(values)
    values(r) <- sorted_values
  } else if (pattern == "EW_Gradient") {
    sorted_values <- sort(values)
    repeat_factor <- ceiling(ncols / length(sorted_values))
    extended_values <- rep(sorted_values, times = repeat_factor)
    extended_values <- extended_values[1:ncols]
    for (col in 1:ncols) {
      r[, col] <- extended_values[col]
    }
  } else if (pattern == "Uniform") {
    values(r) <- values
  } else if (pattern == "Clustered") {
    num_clusters <- sqrt(length(values)) / 2
    cluster_centers <- matrix(runif(2 * num_clusters, 1, 
                                    min(nrows, ncols)), ncol = 2)
    raster_matrix <- matrix(NA, nrow = nrows, ncol = ncols)
    for (row in 1:nrows) {
      for (col in 1:ncols) {
        distances <- sqrt((cluster_centers[,1] - row)^2 + 
                            (cluster_centers[,2] - col)^2)
        nearest_cluster <- which.min(distances)
        raster_matrix[row, col] <- values[(nearest_cluster %% 
                                             length(values)) + 1]
      }
    }
    values(r) <- as.vector(raster_matrix)
  } else if (pattern == "Random_Spatial") {
    shuffled_values <- sample(values)
    values(r) <- shuffled_values
  } else {
    # Default to random if no pattern matches
    values(r) <- sample(values)
  }
  return(r)
}

## 3.5 Create rasters with different patterns ----
cov_1_raster <- create_raster_with_pattern(raster_template, cov_1, 
                                           "EW_Gradient")
cov_2_raster <- create_raster_with_pattern(raster_template, cov_2, 
                                           "NS_Gradient")
cov_3_raster <- create_raster_with_pattern(raster_template, cov_3, 
                                           "Random_Spatial")
cov_4_raster <- create_raster_with_pattern(raster_template, cov_4, 
                                           "Uniform")
cov_5_raster <- create_raster_with_pattern(raster_template, cov_5, 
                                           "Clustered")

## 3.6 Stack the rasters ----
dummy_rasters <- c(cov_1_raster, cov_2_raster, cov_3_raster, 
                   cov_4_raster, cov_5_raster)

## 3.7 Set names for each layer in the stack ----
names(dummy_rasters) <- c("cov_1", "cov_2", "cov_3", "cov_4", "cov_5")

