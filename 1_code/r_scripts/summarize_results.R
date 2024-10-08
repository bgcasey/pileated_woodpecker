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
load(paste0(path, "/bootstrap_models.rData"))
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
  slice(-1:-6) %>%
  arrange(desc(Importance))

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
top_variables <- head(importance_df$Variable, 6)

## 3.2 Generate partial dependence data for all bootstrapped models ----
partial_dependence_data <- list()

for (var in top_variables) {
  var_data <- list()
  
  for (i in 1:length(bootstrap_models$models)) {
    temp_plot <- plot.gbm(bootstrap_models$models[[i]],
                          i.var = var, return.grid = TRUE,
                          smooth = TRUE, type = "response"
    )
    
    temp_plot$model <- i
    var_data[[i]] <- temp_plot
  }
  
  combined_var_data <- bind_rows(var_data)
  partial_dependence_data[[var]] <- combined_var_data
}

## 3.3 Calculate mean and standard error for each variable ----
plot_list <- list()

for (var in top_variables) {
  var_data <- partial_dependence_data[[var]]
  
  summary_data <- var_data %>%
    group_by_at(names(var_data)[1]) %>%
    summarise(
      mean_y = mean(y),
      sd_y = sd(y)
    )
  
  x_var <- sym(names(summary_data)[1])
  
  if (is.factor(summary_data[[1]]) || is.character(summary_data[[1]])) {
    summary_data[[1]] <- as.factor(summary_data[[1]])
    p <- ggplot(summary_data, aes(!!x_var, mean_y)) +
      geom_bar(stat = "identity") +
      geom_errorbar(aes(
        ymin = mean_y - sd_y,
        ymax = mean_y + sd_y
      ), width = 0.2) +
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
    p <- ggplot(summary_data, aes(!!x_var, mean_y)) +
      geom_line() +
      geom_ribbon(aes(
        ymin = mean_y - sd_y,
        ymax = mean_y + sd_y
      ), alpha = 0.2) +
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

## 3.4 Arrange plots in a grid ----
grid_plots <- do.call(grid.arrange, c(plot_list, ncol = 2))

## 3.5 Create and save combined grob ----
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
# Initialize an empty list to store interaction data frames
interaction_list <- list()

# Loop through each bootstrapped model and extract interaction 
# strengths
for (i in 1:length(bootstrap_models)) {
  interactions <- gbm.interactions(bootstrap_models[[i]])
  
  interaction_df <- interactions$rank.list %>%
    mutate(var1.names = str_replace_all(
      var1.names, c(
        "ls_" = "",
        "s2_" = "",
        "_mean_500" = "",
        "_mode_500" = ""
      )
    )) %>%
    mutate(var2.names = str_replace_all(
      var2.names, c(
        "ls_" = "",
        "s2_" = "",
        "_mean_500" = "",
        "_mode_500" = ""
      )
    )) %>%
    mutate(model = i) # Add a column to identify the model
  
  interaction_list[[i]] <- interaction_df
}

# Combine all interaction data frames into one
all_interactions <- bind_rows(interaction_list)

# Calculate mean and standard error for each variable pair
interaction_summary <- all_interactions %>%
  group_by(var1.names, var2.names) %>%
  summarise(
    mean_int.size = mean(int.size),
    se_int.size = sd(int.size) / sqrt(n())
  ) %>%
  ungroup()

# Create a bar plot with error bars
interaction_plot <- ggplot(
  interaction_summary,
  aes(x = reorder(
    paste(
      var1.names,
      "and",
      var2.names
    ),
    mean_int.size
  ), y = mean_int.size)
) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(
    ymin = mean_int.size - se_int.size,
    ymax = mean_int.size + se_int.size
  ), width = 0.2) +
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

# Save the plot
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


# 7. Study area map ----

## Load natural regions
nat <- st_read(
  paste0(
    "~/Library/CloudStorage/GoogleDrive-bgcasey@ualberta.ca/",
    "My Drive/3_Resources/data/spatial/Alberta_Landcover_Data/",
    "GOA Products/Natural_Regions_Subregions_of_Alberta/",
    "Natural_Regions_Subregions_of_Alberta.shp"
  )
)

# Dissolve boundaries
nat_dissolved <- nat %>%
  group_by(NRNAME) %>%
  summarize(geometry = st_union(geometry))

aoi_t <- st_transform(aoi, crs = crs(nat))

## Load point locations
load("0_data/manual/spatial/ss_xy_3978.rData")
load("0_data/manual/formatted_for_models/data_for_models.rData")

## Filter points to those used in analyses
data_brt <- data_brt %>%
  filter(nrname != "grassland")

xy <- semi_join(ss_xy_3978, data_brt)

# Transform xy to match the CRS of nat
xy_transformed <- st_transform(xy, st_crs(nat))

# Create the plot
study_aoi <- ggplot() +
  geom_sf(data = nat_dissolved, aes(fill = NRNAME), color = NA) +
  geom_sf(data = aoi_t, fill = NA, color = "black") +
  geom_sf(
    data = xy_transformed, aes(color = "Point Counts"),
    size = 0.5
  ) +
  scale_fill_brewer(palette = "Set3", name = "Natural Region") +
  scale_color_manual(values = "black", name = "Legend") +
  theme_minimal() +
  guides(
    fill = guide_legend(ncol = 1),
    color = guide_legend(override.aes = list(size = 4))
  ) +
  theme(
    legend.spacing = unit(0.5, "cm"),
    legend.spacing.y = unit(0.5, "cm"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("3_output/maps/aoi.png",
  plot = study_aoi, width = 6,
  height = 7, dpi = 300
)
