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
library(ggplot2) #visualization
# Boosted Regression trees
library(gbm)
# Arranging multiple plots
library(gridExtra)
library(grid)

## 1.2 Set file path ----
path <- "3_output/models/s2_noOff_noYear"

## 1.3 Load data ----
# Bootstrapped models
load(paste0(path, "/bootstrap_models_stats.rData"))
load(paste0(path, "/brt_3.rData"))

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
  left_join(sds %>% 
              gather(key = "Variable", value = "SD", -Metric) %>% 
              dplyr::select(-Metric),
            by = "Variable") %>%
  slice(-1:-6) %>%
  mutate(Variable = str_replace_all(Variable, c("ls_" = "",
                                                "s2_" = "",
                                                "_mean_500" = "")))

## 2.4 Generate bar plot with error bars ----
variable_importance_plot <- ggplot(importance_df, 
                                   aes(x = reorder(Variable, 
                                                   Importance), 
                                       y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = Importance - SD, ymax = Importance + SD), 
                width = 0.2) +
  coord_flip() +  # Flip coordinates for readability
  theme_minimal(base_size = 14) +  # Set a base font size
  labs(
    x = "Predictor Variables",
    y = "Variable Importance") +
  theme(
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.text = element_text(size = 12, color = "black"),  
    axis.title = element_text(size = 14, face = "bold"),  
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  
    plot.margin = margin(10, 10, 10, 10),  # Adjust plot margins
    panel.border = element_blank(),  # Remove panel border
    axis.line = element_line(color = "black")  # Add axis lines
  )

ggsave(paste0(path, "/variable_importance_plot.png"), 
       plot = variable_importance_plot, width = 10, height = 8, 
       dpi = 300)

# 3. Partial dependence plots ----
# Generate partial dependence plots for the top 6
# variables sorted by importance. It includes steps to extract variable
# importance, generate partial dependence plots, and arrange them in
# a grid with a common y-axis label.

## 3.1 Extract variable importance ----
model <- brt_3
variable_importance <- summary(model, plotit = FALSE)
top_variables <- head(variable_importance$var, 6)

## 3.2 Generate partial dependence plots ----
plot_list <- list()

for (var in top_variables) {
  temp_plot <- plot.gbm(model, i.var = var, return.grid = TRUE, 
                        smooth = TRUE, type= "response")
  
  if (is.factor(temp_plot[[1]]) || is.character(temp_plot[[1]])) {
    temp_plot[[1]] <- as.factor(temp_plot[[1]])
    p <- ggplot(temp_plot, aes_string(x = names(temp_plot)[1], y = "y")) +
      geom_bar(stat = "identity") +
      labs(x = str_replace_all(var, c("ls_" = "", "s2_" = "", "_mean_500" = "")), 
           y = NULL) +  # Remove individual y-axis labels
      theme_minimal() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } else {
    p <- ggplot(temp_plot, aes_string(x = names(temp_plot)[1], y = "y")) +
      geom_line() +
      labs(x = str_replace_all(var, c("ls_" = "", "s2_" = "", "_mean_500" = "")), 
           y = NULL) +  # Remove individual y-axis labels
      theme_minimal() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
  }
  
  plot_list[[var]] <- p
}

## 3.3 Arrange plots in a grid ----
grid_plots <- do.call(grid.arrange, c(plot_list, ncol = 2))

## 3.4 Create and save combined grob ----
combined_grob <- arrangeGrob(grid_plots, 
                             left = textGrob("Occupancy Probability", 
                                             rot = 90, 
                                             gp = gpar(fontsize = 15)))
ggsave(paste0(path, "/partial_dependence_plots.png"), 
       plot = combined_grob, width = 12, height = 9)

# Print the combined grob
grid.newpage()
grid.draw(combined_grob)


