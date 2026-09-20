# manuscript/R/03_evaluate_forest_stability.R
library(dynaRchetypes)
library(ggplot2)
library(dplyr)

dir.create("manuscript/figures", recursive = TRUE, showWarnings = FALSE)

# 1. Initialize Engine
data("hvac_features")
engine <- new("DiagnosticEngine", config_path = "inst/extdata/turin_hvac_config.yaml")
engine <- ingest_telemetry(engine, hvac_features)

# 2. Run the Evaluation Helper (High resolution at the low end)
df_stability <- evaluate_forest_stability(
  engine,
  ntrees_seq = c(10, 25, 50, 75, 100, 150, 200, 300, 400, 500),
  sample_size_seq = c(64, 128, 256, 512)
)

df_stability <- df_stability %>%
  mutate(sample_size_fct = factor(sample_size, levels = c(64, 128, 256, 512)))

# 3. Visualization
message("Generating refined hyperparameter stability plot...")

fig3 <- ggplot(df_stability, aes(x = ntrees, y = spearman_rho, color = sample_size_fct)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0.99, linetype = "dashed", color = "black", alpha = 0.6) +
  scale_color_viridis_d(name = "Sample Size", option = "mako", end = 0.8) +
  # Anchor the Y-axis to give the early instability room to breathe visually
  scale_y_continuous(breaks = seq(0.85, 1.0, by = 0.025), limits = c(min(df_stability$spearman_rho), 1.0)) +
  scale_x_continuous(breaks = c(10, 50, 100, 200, 300, 400, 500)) +
  labs(
    title = "Isolation Forest Rank Stability",
    subtitle = "Spearman correlation of anomaly scores against a 2000-tree baseline.",
    x = "Number of Trees",
    y = "Spearman Rank Correlation (ρ)"
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

output_path <- "manuscript/figures/fig3_forest_stability.pdf"
ggsave(filename = output_path, plot = fig3, width = 8, height = 5, device = "pdf")
output_path <- "manuscript/figures/fig3_forest_stability.png"
ggsave(filename = output_path, plot = fig3, width = 8, height = 5, device = "png")

message(sprintf("Figure saved to %s", output_path))