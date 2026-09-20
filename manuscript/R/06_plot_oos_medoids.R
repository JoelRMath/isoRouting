# manuscript/R/06_plot_oos_medoids.R
library(dynaRchetypes)
library(dplyr)
library(ggplot2)

dir.create("manuscript/figures", recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. INITIALIZATION & SCORING
# -----------------------------------------------------------------------------
message("Initializing Engine and evaluating OOS anomalies...")
data("hvac_features")
engine <- new("DiagnosticEngine", config_path = "inst/extdata/turin_hvac_config.yaml")
engine <- ingest_telemetry(engine, hvac_features)

df_drift <- evaluate_prognostic_drift(
  engine, 
  train_fraction = 0.6, 
  ntrees = 500, 
  sample_size = 256
)

# -----------------------------------------------------------------------------
# 2. EXTRACT K=4 STABILITY REGION (Threshold 0.66)
# -----------------------------------------------------------------------------
message("Extracting anomalies from the stable k=4 region...")
n_target <- sum(df_drift$baseline_score >= 0.66)

oos_anomalies <- df_drift %>% 
  arrange(desc(oos_score)) %>% 
  slice(1:n_target) %>%
  select(timestamp, anomaly_score = oos_score)

# Inject and compute spatial matrix
oos_engine <- engine
oos_engine@anomalies <- oos_anomalies
oos_engine@domain_state <- "anomalies_extracted"

oos_engine <- suppressMessages(compute_distances(oos_engine))
# Force k=4 to extract the specific architectural states
oos_engine <- suppressMessages(calibrate(oos_engine, min_k = 4, max_k = 4))

# -----------------------------------------------------------------------------
# 3. EXTRACT MEDOID WINDOWS
# -----------------------------------------------------------------------------
message("Extracting OOS Medoid Windows...")
plot_data <- extract_medoid_windows(oos_engine, k = 4)

variable_order <- c(
  "t_supply", "t_return", "sp_return",  
  "delta_t", "delta_t_deriv_15m",       
  "energy", "energy_deriv_15m"          
)

plot_data <- plot_data %>%
  mutate(
    Variable_Clean = factor(
      Variable, levels = variable_order,
      labels = c("Supply Temp", "Return Temp", "Setpoint", 
                 "Delta T", "Delta T Deriv", "Energy", "Energy Deriv")
    )
  ) %>%
  filter(!is.na(Variable_Clean))
# Map arbitrary OOS cluster IDs to the established physical state machine
plot_data <- plot_data %>%
  mutate(
    Archetype_Aligned = case_when(
      Archetype == "Archetype 2" ~ "1: Anomalous Cooling Shutdown",
      Archetype == "Archetype 1" ~ "2: Extreme Cooling Startup",
      Archetype == "Archetype 4" ~ "3: Extreme Heating Startup",
      Archetype == "Archetype 3" ~ "4: Anomalous Heating Shutdown"
    )
  ) %>%
  # Enforce factor levels so ggplot draws the columns in 1, 2, 3, 4 order
  mutate(Archetype_Aligned = factor(Archetype_Aligned, 
                                    levels = c("1: Anomalous Cooling Shutdown", 
                                               "2: Extreme Cooling Startup", 
                                               "3: Extreme Heating Startup", 
                                               "4: Anomalous Heating Shutdown")))

# Update the facet_grid in the ggplot call:
# facet_grid(Variable_Clean ~ Archetype_Aligned, scales = "free_y") +
# -----------------------------------------------------------------------------
# 4. VISUALIZATION
# -----------------------------------------------------------------------------
message("Generating OOS Medoid Matrix...")
fig6 <- ggplot(plot_data, aes(x = Relative_Time, y = Value, color = Variable_Clean)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  facet_grid(Variable_Clean ~ Archetype_Aligned, scales = "free_y") + # Updated here
  scale_color_viridis_d(option = "turbo") + 
  labs(
    title = "Out-of-Sample Medoid Trajectories (k=4 Plateau)",
    subtitle = "Physical shapes of the clusters formed by the historical model's triage queue.",
    x = "Relative Time from Structural Break (Hours)",
    y = "Telemetry Value"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text.x = element_text(face = "bold", size = 12),
    strip.text.y = element_text(face = "bold", size = 13, angle = 270),
    strip.background = element_rect(fill = "#f0f0f0", color = "grey80", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0, "pt"),
    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5)
  )

output_path <- "manuscript/figures/fig6_oos_medoids.pdf"
ggsave(filename = output_path, plot = fig6, width = 11, height = 11, device = "pdf")
output_path <- "manuscript/figures/fig6_oos_medoids.png"
ggsave(filename = output_path, plot = fig6, width = 11, height = 11, device = "png")
message(sprintf("Validation complete. Check %s", output_path))