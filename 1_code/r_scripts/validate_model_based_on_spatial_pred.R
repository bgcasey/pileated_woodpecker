# ---
# title: "Validating Occupancy Probability Predictions Using 
#         Human Surveys with Random Stratified Sampling"
# author: "Brendan Casey"
# created: "2024-10-01"
# inputs: 
#   - "path_to_prediction_raster: Raster file containing 
#      occupancy probability predictions."
#   - "path_to_survey_data: CSV or shapefile containing human 
#      survey data with observed occupancy status."
# outputs: 
#   - "validation_metrics.csv: CSV file with Log Loss, AUC, and 
#      Brier Score."
#   - "predicted_vs_observed_plot.png: Calibration plot comparing 
#      predicted probabilities and observed occupancy."
# notes: 
#   "This script validates occupancy probability predictions against 
#   observed occupancy data from human surveys conducted using 
#   random stratified sampling. The script computes metrics like 
#   Log Loss, AUC-ROC, and Brier Score to assess the quality of 
#   the predictions."
# ---

# 1. Setup ----

## 1.1 Load packages ----
# Load required libraries for raster, spatial, and data analysis
library(tidyr)
library(terra)   # For working with raster data (version: 3.4-13)
library(sf)      # For handling spatial vector data (version: 1.0-8)
library(dplyr)   # Data manipulation (version: 1.0.10)
library(pROC)    # For calculating AUC (version: 1.18.0)
library(ggplot2) # For plotting (version: 3.3.6)
library(cowplot) # For combining plots (version: 1.1.1)

## 1.2 Set model directory ----
# Define the model directory for loading and saving data
model_directory <- "3_output/models/s2_noOff_noYear"

## 1.3 Import data ----

### Load spatial prediction raster ----
prediction_raster <- rast(paste0(model_directory, 
                                 "/spatial_pred/mean_raster.tif"))
# prediction_raster <- rast(paste0(model_directory, 
#                                  "/spatial_pred/mean_raster_rescale.tif"))
# 
# prediction_raster <- rast("0_data/external/dryocopus_pileatus.tif")


### Load cavity data and search locations ----
cavities <- st_read("3_output/shapefiles/cavities_all_20241128.shp") %>%
  st_transform(crs = 3978)

cavities_all <- cavities
cavities_piwo <- cavities %>%
  filter(d_class == 4)
cavities_piwo_3_4  <- cavities %>%
  filter(d_class == 4 |d_class == 3 )

search_areas <- st_read("3_output/shapefiles/piwo_searched_20241119.shp") %>%
  st_transform(crs = 3978)

# 2. Validate with Search Area ----
# Validate model predictions using systematic search data.

## 2.1 Function to calculate validation metrics ----
validate <- function(cavities_data, dataset_name) {
  # Join search areas with cavities and buffer by 500m
  surveys <- search_areas %>%
    st_buffer(dist = 500) %>%
    st_join(cavities_data, join = st_intersects) %>%
    mutate(size = if_else(is.na(size), 0, size)) %>% # Replace NA with 0
    dplyr::select(Location, geometry, d_class) %>%
    mutate(piwo_cav = if_else(is.na(d_class) | d_class < 4, 0, 1)) %>%
    select(-d_class) %>%
    distinct()
  
  # Extract predicted probabilities
  extracted_values <- terra::extract(prediction_raster, 
                                     surveys, 
                                     fun = "mean", 
                                     na.rm = TRUE)
  
  # Add predicted probabilities to survey data
  surveys$predicted_prob <- extracted_values[, 2]
  
  # Compute validation metrics
  log_loss <- -mean(
    surveys$piwo_cav * log(surveys$predicted_prob + 1e-15) +
      (1 - surveys$piwo_cav) * log(1 - surveys$predicted_prob + 1e-15)
  )
  
  roc_curve <- roc(surveys$piwo_cav, surveys$predicted_prob)
  auc_value <- auc(roc_curve)
  
  brier_score <- mean((surveys$predicted_prob - surveys$piwo_cav)^2)
  
  # Return metrics as a data frame
  data.frame(
    Dataset = dataset_name,
    Log_Loss = log_loss, 
    AUC = auc_value, 
    Brier_Score = brier_score
  )
}

## 2.2 Calculate validation metrics ----
# Compute metrics for all cavities and PIWO cavities
validation_metrics_all <- validate(cavities_all, "cavities_all")
validation_metrics_piwo <- validate(cavities_piwo, "cavities_piwo")
validation_metrics_piwo_3_4 <- validate(cavities_piwo_3_4, "cavities_piwo_3_4")

# Combine metrics
validation_metrics_combined <- rbind(validation_metrics_all, 
                                     validation_metrics_piwo,
                                     validation_metrics_piwo_3_4)

## 2.3 Save metrics ----
# Save validation metrics to CSV
write.csv(validation_metrics_combined, 
          paste0(model_directory, 
                 "/pred_validation_metrics_search_areas.csv"), 
          row.names = FALSE)


# 3. Validation with Cavities Data Only ----
# Validate the model using cavity presence data compared to randomly 
# generated points.

## 3.1 Define the AOI ----
# Load the raster defining the area of interest (AOI)
nat <- rast("0_data/manual/predictor/nat_raster.tif")

# Mask out cells classified as "grassland"
grassland_mask <- nat[[1]] == "grassland"
non_grassland_mask <- !grassland_mask
nat_masked <- mask(nat, non_grassland_mask, maskvalues = FALSE)

## 3.2 Preprocess Cavity Data ----
# Prepare datasets for validation
datasets <- list(
  cavities_all = cavities_all,
  cavities_piwo = cavities_piwo,
  cavities_piwo_3_4 = cavities_piwo_3_4
)

# Add predicted probabilities to each dataset
for (dataset_name in names(datasets)) {
  current_cavities <- datasets[[dataset_name]]
  
  # Create a 1-km buffer around each point
  # cavities_buffered <- st_buffer(current_cavities, dist = 0)
  
  # Extract raster values within each buffer and calculate the mean
  mean_values <- terra::extract(prediction_raster, current_cavities, 
                                fun = mean, na.rm = TRUE)
  
  # Assign mean predicted probabilities to the dataset
  current_cavities$predicted_prob <- mean_values[, 2]
  
  # Update the dataset in the list
  datasets[[dataset_name]] <- current_cavities
}

## 3.3 Setup the Validation Loop ----
# Initialize a container for bootstrap results
bootstrap_results_combined <- list()

# Set bootstrap and random point parameters
n_bootstraps <- 2
n_points <- 2000
set.seed(133)  # Ensure reproducibility

for (dataset_name in names(datasets)) {
  current_cavities <- datasets[[dataset_name]]
  
  # Dataframe for storing bootstrap results
  bootstrap_results <- data.frame(
    Mean_Presence = numeric(),
    Mean_Random = numeric(),
    AUC_ROC = numeric(),
    KS_Statistic = numeric(),
    KS_p_value = numeric(),
    Wilcoxon_stat = numeric(),
    Wilcoxon_p_value = numeric(),
    Brier_Score = numeric(),
    Log_Loss = numeric()
  )
  
  for (i in 1:n_bootstraps) {
    # Generate random points within the AOI
    random_points <- spatSample(nat_masked, size = n_points, 
                                method = "random", na.rm = TRUE, 
                                xy = TRUE)
    random_points_sf <- st_as_sf(data.frame(random_points), 
                                 coords = c("x", "y"), 
                                 crs = crs(nat)) %>%
      st_transform(crs = st_crs(current_cavities))
    
    # Create a 1-km buffer around each random point
    # random_points_buffered <- st_buffer(random_points_sf, dist = 0)
    
    # Extract raster values within each buffer and calculate the mean
    random_values <- terra::extract(prediction_raster, 
                                    random_points_sf, 
                                    fun = mean, na.rm = TRUE)
    
    # Assign mean predicted probabilities to the buffered random points
    random_points_sf$predicted_prob <- random_values[, 2]
    
    # Combine presence and random points
    presence_random_labels <- c(rep(1, nrow(current_cavities)), 
                                rep(0, nrow(random_points_sf)))
    presence_random_probs <- c(current_cavities$predicted_prob, 
                               random_points_sf$predicted_prob)
    
    # Filter valid data and constrain probabilities
    valid_indices <- which(!is.na(presence_random_probs) & 
                             !is.na(presence_random_labels))
    presence_random_probs <- presence_random_probs[valid_indices]
    presence_random_labels <- presence_random_labels[valid_indices]
    presence_random_probs <- pmin(pmax(presence_random_probs, 0), 1)
    
    # Calculate validation metrics
    roc_curve <- roc(presence_random_labels, presence_random_probs)
    auc_value <- auc(roc_curve)
    brier_score <- mean((presence_random_probs - 
                           presence_random_labels)^2)
    log_loss <- -mean(presence_random_labels * 
                        log(presence_random_probs + 1e-15) + 
                        (1 - presence_random_labels) * 
                        log(1 - presence_random_probs + 1e-15))
    ks_test <- ks.test(current_cavities$predicted_prob, 
                       random_points_sf$predicted_prob)
    wilcox_test <- wilcox.test(current_cavities$predicted_prob, 
                               random_points_sf$predicted_prob, 
                               alternative = "greater")
    mean_presence <- mean(current_cavities$predicted_prob, 
                          na.rm = TRUE)
    mean_random <- mean(random_points_sf$predicted_prob,
                        na.rm = TRUE)
    
    # Store metrics in the results dataframe
    bootstrap_results <- rbind(bootstrap_results, data.frame(
      Mean_Presence = mean_presence,
      Mean_Random = mean_random,
      AUC_ROC = auc_value,
      KS_Statistic = ks_test$statistic,
      KS_p_value = ks_test$p.value,
      Wilcoxon_stat = wilcox_test$statistic,
      Wilcoxon_p_value = wilcox_test$p.value,
      Brier_Score = brier_score,
      Log_Loss = log_loss
    ))
  }
  
  # Append bootstrap results for this dataset
  bootstrap_results_combined[[dataset_name]] <- bootstrap_results
}


## 3.5 Summarize Statistics Across Bootstraps ----
format_value <- function(x) {
  ifelse(x < 0.001, format(x, scientific = TRUE, digits = 3), 
         round(x, 3))
}

final_results <- lapply(names(bootstrap_results_combined), 
                        function(dataset_name) {
                          dataset_results <- bootstrap_results_combined[[dataset_name]]
                          
                          dataset_summary <- dataset_results %>%
                            summarise(
                              Mean_Presence_Mean = mean(Mean_Presence),
                              Mean_Presence_SD = sd(Mean_Presence),
                              Mean_Random_Mean = mean(Mean_Random),
                              Mean_Random_SD = sd(Mean_Random),
                              AUC_ROC_Mean = mean(AUC_ROC),
                              AUC_ROC_SD = sd(AUC_ROC),
                              KS_Statistic_Mean = mean(KS_Statistic),
                              KS_Statistic_SD = sd(KS_Statistic),
                              KS_p_value_Mean = mean(KS_p_value),
                              KS_p_value_SD = sd(KS_p_value),
                              Wilcoxon_stat_Mean = mean(Wilcoxon_stat),
                              Wilcoxon_stat_SD = sd(Wilcoxon_stat),
                              Wilcoxon_p_value_Mean = mean(Wilcoxon_p_value),
                              Wilcoxon_p_value_SD = sd(Wilcoxon_p_value),
                              Brier_Score_Mean = mean(Brier_Score),
                              Brier_Score_SD = sd(Brier_Score),
                              Log_Loss_Mean = mean(Log_Loss),
                              Log_Loss_SD = sd(Log_Loss)
                            ) %>%
                            mutate(Dataset = dataset_name)
                        })

final_dataframe <- do.call(rbind, final_results) %>%
  pivot_longer(-Dataset, names_to = "Metric", values_to = "Value") %>%
  mutate(
    Stat = case_when(
      grepl("_Mean$", Metric) ~ "mean",
      grepl("_SD$", Metric) ~ "sd"
    ),
    Metric = sub("(_Mean|_SD)$", "", Metric)
  ) %>%
  pivot_wider(names_from = Stat, values_from = Value) %>%
  mutate(across(c(mean, sd), format_value))

## 3.6 Save Metrics ----
write.csv(final_dataframe, paste0(model_directory, 
                                  "/pred_validation_metrics_cavities.csv"), 
          row.names = FALSE)

## 3.7 Visualization ----
random_points <- spatSample(nat_masked, size = n_points, 
                            method = "random", 
                            na.rm = TRUE, 
                            xy = TRUE)
random_points_sf <- st_as_sf(data.frame(random_points), 
                             coords = c("x", "y"), crs = crs(nat)) %>%
  st_transform(crs = st_crs(current_cavities))

random_values <- terra::extract(prediction_raster, random_points_sf, 
                                fun = "mean", na.rm = TRUE)
random_points_sf$predicted_prob <- random_values[, 2]

combined_data <- rbind(
  data.frame(Source = "Cavities_All", 
             Predicted_Prob = datasets$cavities_all$predicted_prob),
  data.frame(Source = "Cavities_PIWO", 
             Predicted_Prob = datasets$cavities_piwo$predicted_prob),
  data.frame(Source = "Random", 
             Predicted_Prob = random_points_sf$predicted_prob)
)

density_plot <- ggplot(combined_data, 
                       aes(x = Predicted_Prob, fill = Source)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Cavities_All" = "blue", 
                               "Cavities_PIWO" = "green", 
                               "Random" = "red")) +
  ggtitle("Density Plot of Predicted Probabilities") +
  xlab("Predicted Probability") +
  ylab("Density") +
  theme_minimal() +
  labs(fill = "Source")

ggsave(paste0(model_directory, 
              "/density_plot_presence_vs_random.png"), 
       density_plot, width = 8, height = 6)

# 4. Validate with acoustic data ----


# End of Script ----








