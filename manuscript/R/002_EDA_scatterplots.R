# 02_EDA_scatterplots.R
#
# This script generates the pairwise scatterplot matrix for the Turin HVAC dataset,
# mapping Pearson correlation to panel background colors.
# It uses the pre-cleaned data and isolates only the raw thermodynamic 
# variables to reveal multi-modal dependencies prior to feature engineering.

library(dplyr)
library(GGally)
library(ggplot2)

# Set the output filepath for the LaTeX manuscript figure
output_filepath <- "manuscript/figures/fig_02_scatterplot_matrix.jpg"

# Load the cleaned and engineered dataset
# (Assumes execution from the R project root)
load("data/hvac_engineered.rda")

# Define the raw thermodynamic variables (matching the naive YAML config)
raw_vars <- c(
  "t_supply", "t_return", "sp_return", "t_saturation", "t_outdoor", 
  "rh_supply", "rh_return", "rh_outdoor", "energy", "power"
)

# 1. Custom function to calculate correlation and map it to a background color
custom_scatter_bg <- function(data, mapping, ...) {
  x_data <- eval_data_col(data, mapping$x)
  y_data <- eval_data_col(data, mapping$y)
  
  corr <- cor(x_data, y_data, use = "pairwise.complete.obs")
  
  # Map correlation (-1 to 1) to a color scale
  color_fun <- colorRamp(c("#f8766d", "white", "#00bfc4")) 
  scaled_corr <- (corr + 1) / 2 
  
  rgb_vec <- color_fun(scaled_corr)
  hex_color <- rgb(rgb_vec[1], rgb_vec[2], rgb_vec[3], maxColorValue = 255)
  
  # Plot points over colored background
  ggplot(data, mapping) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, 
             fill = hex_color, alpha = 0.4) +
    geom_point(...)
}

# 2. Generate the plot (sampling 5000 rows for rendering performance)
set.seed(42) # Set seed for reproducible sampling
p_pairs <- hvac_engineered %>%
  select(all_of(raw_vars)) %>% 
  sample_n(1000) %>% 
  ggpairs(
    lower = list(continuous = wrap(custom_scatter_bg, alpha = 0.1, size = 0.5)),
    upper = list(continuous = wrap(custom_scatter_bg, alpha = 0.1, size = 0.5)),
    title = "Pairwise Scatterplot Matrix (HVAC Telemetry)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray80", fill = NA)
  )

# Save the plot for the manuscript
ggsave(output_filepath, plot = p_pairs, width = 8, height = 8, dpi = 300, device = "jpeg")