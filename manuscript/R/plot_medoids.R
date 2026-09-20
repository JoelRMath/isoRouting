# manuscript/R/02_plot_medoids.R
library(dynaRchetypes)
library(ggplot2)
library(dplyr)

dir.create("manuscript/figures", recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. PIPELINE EXECUTION
# -----------------------------------------------------------------------------
message("Executing pipeline...")
data("hvac_features")
engine <- new("DiagnosticEngine", config_path = "inst/extdata/turin_hvac_config.yaml")

engine <- ingest_telemetry(engine, hvac_features)
engine <- extract_anomalies(engine, threshold = 0.68)
engine <- compute_distances(engine)
engine <- calibrate(engine, min_k = 4, max_k = 4)

# -----------------------------------------------------------------------------
# 2. DATA EXTRACTION (Via Helper)
# -----------------------------------------------------------------------------
message("Extracting medoid windows...")
plot_data <- extract_medoid_windows(engine, k = 4)

# Define the optimized visual hierarchy:
# 1. Temp Error removed.
# 2. Delta T Deriv moved directly below Delta T.
variable_order <- c(
  "t_supply", "t_return", "sp_return",  
  "delta_t", "delta_t_deriv_15m",       
  "energy", "energy_deriv_15m"          
)

plot_data <- plot_data %>%
  mutate(
    Variable_Clean = factor(
      Variable,
      levels = variable_order,
      labels = c(
        "Supply Temp", "Return Temp", "Setpoint", 
        "Delta T", "Delta T Deriv", 
        "Energy", "Energy Deriv"
      )
    ),
    # Swap these to match the specific physical shapes of your k=4 output
    Archetype_Label = case_when(
      Archetype == "Archetype 1" ~ "1: Mechanical Fault",
      Archetype == "Archetype 2" ~ "2: Setpoint Step-Up",
      Archetype == "Archetype 3" ~ "3: Setpoint Setback",
      Archetype == "Archetype 4" ~ "4: Envelope Breach",
      TRUE ~ Archetype
    )
  ) %>%
  filter(!is.na(Variable_Clean))

# -----------------------------------------------------------------------------
# 3. VISUALIZATION: The Optimized Matrix
# -----------------------------------------------------------------------------
message("Generating the final archetype matrix...")

fig2 <- ggplot(plot_data, aes(x = Relative_Time, y = Value, color = Variable_Clean)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  facet_grid(Variable_Clean ~ Archetype_Label, scales = "free_y") +
  scale_color_viridis_d(option = "turbo") + 
  labs(
    title = "Multivariate Trajectories of HVAC Fault Archetypes",
    subtitle = "Top rows display interpretable context; bottom rows display computed algorithmic features.",
    x = "Relative Time from Structural Break (Hours)",
    y = "Telemetry Value"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    # Increase font size for variables (y-axis strips) and titles (x-axis strips)
    strip.text.x = element_text(face = "bold", size = 12),
    strip.text.y = element_text(face = "bold", size = 13, angle = 270),
    strip.background = element_rect(fill = "#f0f0f0", color = "grey80", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    
    # Eliminate the double-line gaps by setting spacing to 0 and unifying borders
    panel.spacing = unit(0, "pt"),
    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5)
  )

# -----------------------------------------------------------------------------
# 4. EXPORT
# -----------------------------------------------------------------------------
# Shrunk the height to 11 to match the removal of the 8th row
output_path <- "manuscript/figures/fig2_vertical_matrix.pdf"
ggsave(filename = output_path, plot = fig2, width = 11, height = 11, device = "pdf")
output_path <- "manuscript/figures/fig2_vertical_matrix.png"
ggsave(filename = output_path, plot = fig2, width = 11, height = 11, device = "png")
message(sprintf("Figure 2 successfully saved to %s", output_path))