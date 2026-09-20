# manuscript/R/01_plot_threshold_stability.R
library(dynaRchetypes)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork) # For multi-panel plots

dir.create("manuscript/figures", recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. DATA GENERATION: Utilizing Package Helpers
# -----------------------------------------------------------------------------
data("hvac_features")
engine <- new("DiagnosticEngine", config_path = "inst/extdata/turin_hvac_config.yaml")
engine <- ingest_telemetry(engine, hvac_features)

# Generate Panel A data (Broad exploration)
df_counts <- explore_thresholds(engine, thresholds = seq(0.60, 0.75, by = 0.005))

# Generate Panel B data (High-resolution stability sweep)
df_stability <- evaluate_k_stability(
  engine, 
  thresholds = seq(0.65, 0.71, by = 0.0025), 
  min_k = 2, max_k = 10, max_n = 350
)

# -----------------------------------------------------------------------------
# 2. VISUALIZATION: Panel A (Anomaly Counts)
# -----------------------------------------------------------------------------
# Filter out 0-counts which break log scales and cause visual line drops
df_counts_clean <- df_counts %>% filter(Event_Count > 0)
max_y <- max(df_counts_clean$Event_Count)

p1 <- ggplot(df_counts_clean, aes(x = Threshold, y = Event_Count)) +
  # Use explicit y-limits for the rectangle to prevent log10 rendering failures
  annotate("rect", xmin = 0.65, xmax = 0.71, ymin = 0.5, ymax = max_y * 1.2, alpha = 0.1, fill = "blue") +
  geom_line(color = "black", linewidth = 1) +
  geom_point(size = 2) +
  scale_y_log10(
    labels = scales::comma,
    limits = c(1, max_y * 1.2), # Anchor the y-axis cleanly
    breaks = c(1, 10, 100, 1000)
  ) +
  labs(
    title = "A) Event Extraction Sensitivity",
    x = "Isolation Forest Threshold",
    y = "Number of Anomalies (Log Scale)"
  ) +
  theme_bw(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(), 
    plot.title = element_text(face = "bold")
  )

# -----------------------------------------------------------------------------
# 3. VISUALIZATION: Panel B (Cluster Stability)
# -----------------------------------------------------------------------------
plot_data_b <- df_stability %>%
  filter(!is.na(Opt_k_Silhouette)) %>%
  pivot_longer(cols = starts_with("Opt_k"), names_to = "Metric", values_to = "Optimal_k") %>%
  mutate(Metric = ifelse(Metric == "Opt_k_Silhouette", "Global Silhouette", "Harmonic Pairwise"))

p2 <- ggplot(plot_data_b, aes(x = Threshold, y = Optimal_k, color = Metric, group = Metric)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(breaks = seq(2, 10, by = 1), limits = c(2, 10)) +
  scale_color_manual(values = c("Global Silhouette" = "#0072B2", "Harmonic Pairwise" = "#D55E00")) +
  labs(
    title = "B) Cluster Architecture Stability",
    x = "Anomaly Threshold",
    y = "Optimal Number of Clusters (k)"
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

# -----------------------------------------------------------------------------
# 4. STITCH AND EXPORT
# -----------------------------------------------------------------------------
# Stitch with patchwork (Panel B gets 2x the width for clarity)
combined_plot <- p1 + p2 + plot_layout(widths = c(1, 2))

ggsave(
  filename = "manuscript/figures/fig1_threshold_stability.pdf",
  plot = combined_plot,
  width = 12, height = 5, device = "pdf"
)

ggsave(
  filename = "manuscript/figures/fig1_threshold_stability.png",
  plot = combined_plot,
  width = 12, height = 5, device = "png"
)

message("Figure 1 (Dual-Panel) successfully saved.")