# manuscript/R/08_plot_drift_medoids.R
library(dynaRchetypes)
library(dplyr)
library(ggplot2)

dir.create("manuscript/figures", recursive = TRUE, showWarnings = FALSE)

# 1. Initialize and Score
message("Initializing Engine for Drift Medoids...")
data("hvac_features")
engine <- new("DiagnosticEngine", config_path = "inst/extdata/turin_hvac_drift_config.yaml")
engine <- ingest_telemetry(engine, hvac_features)
engine <- extract_anomalies(engine)

# 2. Cluster at the stable k=2
message("Computing spatial matrix and extracting k=2...")
engine <- suppressMessages(compute_distances(engine))
engine <- suppressMessages(calibrate(engine, min_k = 2, max_k = 2))

# 3. Extract Medoid Windows
plot_data <- extract_medoid_windows(engine, k = 2)

# Clean up the variable names for the new drift features
variable_order <- c(
  "t_supply", "sp_return", "temp_error", 
  "t_supply_roll_sd_1h", "energy", "energy_lag_24h"
)

plot_data <- plot_data %>%
  mutate(
    Variable_Clean = factor(
      Variable, levels = variable_order,
      labels = c("Supply Temp", "Setpoint", "Temp Error", 
                 "Supply Volatility (1h SD)", "Energy", "Energy (24h Lag)")
    )
  ) %>%
  filter(!is.na(Variable_Clean))

# 4. Visualization
message("Generating Drift Medoid Matrix...")
fig8 <- ggplot(plot_data, aes(x = Relative_Time, y = Value, color = Variable_Clean)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  facet_grid(Variable_Clean ~ Archetype, scales = "free_y") +
  scale_color_viridis_d(option = "mako") + 
  labs(
    title = "Slow-Moving Control Drift Medoids (k=2)",
    subtitle = "4-hour trajectory windows capturing thermal volatility and 24-hour baseline deviations.",
    x = "Relative Time from Structural Break (Hours)",
    y = "Telemetry Value"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text.x = element_text(face = "bold", size = 13),
    strip.text.y = element_text(face = "bold", size = 11, angle = 270),
    strip.background = element_rect(fill = "#f0f0f0", color = "grey80", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0, "pt"),
    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5)
  )

output_path <- "manuscript/figures/fig8_drift_medoids.pdf"
ggsave(filename = output_path, plot = fig8, width = 9, height = 10, device = "pdf")
output_png <- "manuscript/figures/fig8_drift_medoids.png"
ggsave(filename = output_png, plot = fig8, width = 9, height = 10, device = "png")

message(sprintf("Drift medoids extracted. Check %s", output_path))