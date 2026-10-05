# 01_EDA_correlation.R
#
# This script generates the correlation heatmap for the Turin HVAC dataset.
# It uses the pre-cleaned data and isolates only the raw thermodynamic 
# variables to demonstrate their inherent linear dependencies prior to 
# feature engineering.

library(dplyr)
library(ggcorrplot)
library(ggplot2)

# Set the output filepath for the LaTeX manuscript figure
output_filepath <- "manuscript/figures/fig_01_correlation_heatmap.png"

# Load the cleaned and engineered dataset
# (Assumes execution from the R project root)
load("data/hvac_engineered.rda")

# Define the raw thermodynamic variables (matching the naive YAML config)
raw_vars <- c(
  "t_supply", "t_return", "sp_return", "t_saturation", "t_outdoor", 
  "rh_supply", "rh_return", "rh_outdoor", "energy", "power"
)

# Calculate correlation matrix on the raw variables only
cor_matrix <- cor(hvac_engineered %>% select(all_of(raw_vars)), use = "pairwise.complete.obs")

# Set the diagonal to NA to de-emphasize trivial self-correlations
diag(cor_matrix) <- NA

# Generate the square ggplot-based heatmap
heatmap_plot <- ggcorrplot(
  cor_matrix,
  method = "square",
  type = "full",
  lab = TRUE,              
  lab_size = 3.5,
  lab_col = "black",        
  colors = c("#f8766d", "white", "#00bfc4"), 
  outline.color = "gray90",
  legend.title = "Correlation"
) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold")
  ) +
  scale_y_discrete(limits = rev)

# Save the plot for the manuscript
ggsave(output_filepath, plot = heatmap_plot, width = 8, height = 8, dpi = 300)