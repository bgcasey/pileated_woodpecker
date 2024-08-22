# ---
# title: "Summarize Results"
# author: "Brendan Casey"
# created: "2023-12-01"
# description:
#   "This script summarizes gbm model results. It includes steps to
#   plot variable importance and generate partial dependence plots.
#   The script uses bootstrapped model statistics and a GBM model.
#   The final output is a set of plots saved as PNG files."
# ---

# 1. Setup ----
## 1.1 Load packages ----
library(tidyverse) # Data manipulation
library(ggplot2) # visualization
# Boosted Regression trees
library(gbm)
library(dismo)
# Arranging multiple plots
library(gridExtra)
library(grid)
library(terra) # process raster data
library(sf)

## 1.2 Set file path ----
path <- "3_output/models/s2_noOff_noYear"

## 1.3 Load data ----
load(paste0(path, "/bootstrap_models_stats.rData"))
load(paste0(path, "/brt_3.rData"))
load(paste0(path, "/brt_1.rData"))
pred_raster_mean <- rast(paste0(
  path,
  "/spatial_pred/mean_raster_rescale.tif"
))
aoi <- st_read("0_data/external/Alberta/alberta.shp")

# 2. Plot variable importance ----
# Plot the variable importance from the bootstrapped
# models.

## 2.1 Convert data from wide to long format ----
bootstrap_models_stats <- bootstrap_models_stats %>%
  rownames_to_column(var = "Metric")

## 2.2 Separate means and standard deviations ----
means <- bootstrap_models_stats %>%
  filter(Metric == "mean")

sds <- bootstrap_models_stats %>%
  filter(Metric == "sd")

## 2.3 Merge means and standard deviations ----
importance_df <- means %>%
  gather(key = "Variable", value = "Importance", -Metric) %>%
  dplyr::select(-Metric) %>%
  left_join(
    sds %>%
      gather(key = "Variable", value = "SD", -Metric) %>%
      dplyr::select(-Metric),
    by = "Variable"
  ) %>%
  slice(-1:-6)

## 2.4 Generate bar plot with error bars ----
variable_importance_plot <- importance_df %>%
  mutate(Variable = str_replace_all(
    Variable, c("ls_" = "", "s2_" = "", "_mean_500" = "")
  )) %>%
  ggplot(aes(
    x = reorder(Variable, Importance),
    y = Importance
  )) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(
    ymin = Importance - SD,
    ymax = Importance + SD
  ), width = 0.2) +
  coord_flip() + # Flip coordinates for readability
  theme_minimal(base_size = 14) + # Set a base font size
  labs(
    x = "Predictor Variables",
    y = "Variable Importance"
  ) +
  theme(
    panel.grid.major = element_blank(), # Remove major gridlines
    panel.grid.minor = element_blank(), # Remove minor gridlines
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10), # Adjust plot margins
    panel.border = element_blank(), # Remove panel border
    axis.line = element_line(color = "black") # Add axis lines
  )

ggsave(paste0(path, "/plots/variable_importance_plot.png"),
  plot = variable_importance_plot, width = 10, height = 8,
  dpi = 300
)

print(variable_importance_plot)

# 3. Partial dependence plots ----
# Generate partial dependence plots for the top 6
# variables sorted by importance. It includes steps to extract variable
# importance, generate partial dependence plots, and arrange them in
# a grid with a common y-axis label.

## 3.1 Extract variable importance ----
model <- brt_3

importance_df_sorted <- importance_df %>%
  arrange(desc(Importance)) %>%
  head()

top_variables <- head(importance_df_sorted$Variable, 6)

## 3.2 Generate partial dependence plots ----
plot_list <- list()

for (var in top_variables) {
  temp_plot <- plot.gbm(model,
    i.var = var, return.grid = TRUE,
    smooth = TRUE, type = "response"
  )

  if (is.factor(temp_plot[[1]]) || is.character(temp_plot[[1]])) {
    temp_plot[[1]] <- as.factor(temp_plot[[1]])
    p <- ggplot(temp_plot, aes_string(
      x = names(temp_plot)[1],
      y = "y"
    )) +
      geom_bar(stat = "identity") +
      labs(
        x = str_replace_all(var, c(
          "ls_" = "", "s2_" = "",
          "_mean_500" = ""
        )),
        y = NULL
      ) + # Remove individual y-axis labels
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } else {
    p <- ggplot(temp_plot, aes_string(
      x = names(temp_plot)[1],
      y = "y"
    )) +
      geom_line() +
      labs(
        x = str_replace_all(var, c(
          "ls_" = "", "s2_" = "",
          "_mean_500" = ""
        )),
        y = NULL
      ) + # Remove individual y-axis labels
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  }

  plot_list[[var]] <- p
}

## 3.3 Arrange plots in a grid ----
grid_plots <- do.call(grid.arrange, c(plot_list, ncol = 2))

## 3.4 Create and save combined grob ----
combined_grob <- arrangeGrob(grid_plots,
  left = textGrob("Occupancy Probability",
    rot = 90,
    gp = gpar(fontsize = 15)
  )
)
ggsave(paste0(path, "/plots/partial_dependence_plots.png"),
  plot = combined_grob, width = 12, height = 9
)

# Print the combined grob
grid.newpage()
grid.draw(combined_grob)

# 4. Show dropped variables ----
# Extract variable names models
vars_brt_1 <- brt_1$var.names
vars_brt_3 <- brt_3$var.names

# Find the variables that were dropped
dropped_vars <- setdiff(vars_brt_1, vars_brt_3)

save(dropped_vars, vars_brt_1, vars_brt_3,
  file = paste0(path, "/dropped_vars.rData")
)

# 5. Interactions ----
# Detect the most important interactions
interactions <- gbm.interactions(brt_3)

# View the ranked list of interactions
interaction_df <- interactions$rank.list %>%
  mutate(var1.names = str_replace_all(var1.names, c(
    "ls_" = "",
    "s2_" = "",
    "_mean_500" = "",
    "_mode_500" = ""
  ))) %>%
  mutate(var2.names = str_replace_all(var2.names, c(
    "ls_" = "",
    "s2_" = "",
    "_mean_500" = "",
    "_mode_500" = ""
  )))


# Create a bar plot of the interaction strengths
interaction_plot <- ggplot(
  interaction_df,
  aes(x = reorder(
    paste(
      var1.names,
      "and",
      var2.names
    ),
    int.size
  ), y = int.size)
) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + # Flip the coordinates for readability
  theme_minimal(base_size = 14) +
  labs(
    x = "Variable Pairs",
    y = "Interaction Strength"
  ) +
  theme(
    panel.grid.major = element_blank(), # Remove major gridlines
    panel.grid.minor = element_blank(), # Remove minor gridlines
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10), # Adjust plot margins
    panel.border = element_blank(), # Remove panel border
    axis.line = element_line(color = "black") # Add axis lines
  )

ggsave(paste0(path, "/plots/interaction_plot.png"),
  plot = interaction_plot, width = 10, height = 8,
  dpi = 300
)

# 6. Plot map ----

## 6.1 Format raster for plot ----
r_transformed <- project(pred_raster_mean, "EPSG:4326")
aoi_s <- st_simplify(aoi)
aoi_t <- st_transform(aoi_s, crs = crs(r_transformed))
buffer_distance <- 2000 # Adjust the buffer distance as needed
aoi_buff <- st_buffer(aoi_t, dist = buffer_distance)
r_crop <- crop(r_transformed, aoi_buff)

## 6.2 Set plot parameters ----
plg <- list(
  title = "Relative suitability",
  title.cex = 0.6,
  cex = 0.7,
  shrink = 1
)
pax <- list(retro = TRUE)

## 6.3 Plot ----
# Set up save
png(
  file = paste0(path, "/plots/prediction_map.png"),
  width = 4, height = 6, units = "in", res = 300
)


# Plot raster
plot(r_crop,
  xlim = c(-141, -53),
  ylim = c(40, 85),
  main = "",
  cex.main = 0.6,
  plg = plg,
  pax = pax,
  legend = TRUE
)

# Create a clean border
plot(aoi_t, col = adjustcolor("blue", alpha.f = 0.0), add = TRUE)

dev.off()
