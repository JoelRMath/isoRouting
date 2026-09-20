# manuscript/R/01_plot_threshold_stability.R
library(dynaRchetypes)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

dir.create("manuscript/figures", recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. DATA GENERATION
# -----------------------------------------------------------------------------
message("Initializing engine and loading data...")
data("hvac_features")
engine <- new("DiagnosticEngine", config_path = "inst/extdata/turin_hvac_config.yaml")
engine <- ingest_telemetry(engine, hvac_features)

# Data for Panel A (Broad exploration)
df_counts <- explore_thresholds(engine, thresholds = seq(0.60, 0.75, by = 0.005))

# Data for Panel B (Specific calibration curve at our chosen threshold)
message("Computing calibration curve for Panel B (Threshold = 0.68)...")
tmp_engine <- extract_anomalies(engine, threshold = 0.68)
tmp_engine <- compute_distances(tmp_engine)
tmp_engine <- calibrate(tmp_engine, min_k = 2, max_k = 10)
df_k_scores <- tmp_engine@k_metrics

# Data for Panel C (High-resolution stability sweep)
message("Evaluating stability sweep for Panel C...")
df_stability <- evaluate_k_stability(
  engine, 
  thresholds = seq(0.65, 0.71, by = 0.0025), 
  min_k = 2, max_k = 10, max_n = 350
)

# -----------------------------------------------------------------------------
# 2. VISUALIZATION: Panel A (Anomaly Counts)
# -----------------------------------------------------------------------------
df_counts_clean <- df_counts %>% filter(Event_Count > 0)
max_y <- max(df_counts_clean$Event_Count)

p1 <- ggplot(df_counts_clean, aes(x = Threshold, y = Event_Count)) +
  annotate("rect", xmin = 0.65, xmax = 0.71, ymin = 0.5, ymax = max_y * 1.2, alpha = 0.1, fill = "blue") +
  geom_line(color = "black", linewidth = 1) +
  geom_point(size = 2) +
  scale_y_log10(labels = scales::comma, limits = c(1, max_y * 1.2), breaks = c(1, 10, 100, 1000)) +
  labs(
    title = "A) Event Extraction Sensitivity",
    x = "Isolation Forest Threshold",
    y = "Anomalies Extracted (Log Scale)"
  ) +
  theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))

# -----------------------------------------------------------------------------
# 3. VISUALIZATION: Panel B (Calibration Curve at Threshold 0.68)
# -----------------------------------------------------------------------------
plot_data_b <- df_k_scores %>%
  pivot_longer(
    cols = c(Global_Silhouette, Product_Weighted_Harmonic), 
    names_to = "Metric", values_to = "Score"
  ) %>%
  mutate(Metric = ifelse(Metric == "Global_Silhouette", "Global Silhouette", "Harmonic Pairwise"))

p2 <- ggplot(plot_data_b, aes(x = k, y = Score, color = Metric)) +
  geom_vline(xintercept = 4, linetype = "dashed", color = "black", alpha = 0.5) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = seq(2, 10, by = 1)) +
  scale_color_manual(values = c("Global Silhouette" = "#0072B2", "Harmonic Pairwise" = "#D55E00")) +
  labs(
    title = "B) Cluster Calibration (Threshold 0.68)",
    x = "Number of Clusters (k)",
    y = "Clustering Score"
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "none", # Legend will be shared in Panel C
    panel.grid.minor = element_blank(), 
    plot.title = element_text(face = "bold")
  )

# -----------------------------------------------------------------------------
# 4. VISUALIZATION: Panel C (Cluster Stability)
# -----------------------------------------------------------------------------
plot_data_c <- df_stability %>%
  filter(!is.na(Opt_k_Silhouette)) %>%
  pivot_longer(cols = starts_with("Opt_k"), names_to = "Metric", values_to = "Optimal_k") %>%
  mutate(Metric = ifelse(Metric == "Opt_k_Silhouette", "Global Silhouette", "Harmonic Pairwise"))

p3 <- ggplot(plot_data_c, aes(x = Threshold, y = Optimal_k, color = Metric, group = Metric)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(breaks = seq(2, 10, by = 1), limits = c(2, 10)) +
  scale_color_manual(values = c("Global Silhouette" = "#0072B2", "Harmonic Pairwise" = "#D55E00")) +
  labs(
    title = "C) Cluster Architecture Stability Across Anomaly Pools",
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
# 5. STITCH AND EXPORT
# -----------------------------------------------------------------------------
# Stitch with patchwork: Top row (A | B), Bottom row (C)
# We allocate slightly more height to the top row to keep the proportions clean
combined_plot <- (p1 | p2) / p3 + plot_layout(heights = c(1, 1.2))

# Increased height to 9 inches to comfortably fit the 2-row layout
ggsave(
  filename = "manuscript/figures/fig1_threshold_stability.pdf",
  plot = combined_plot,
  width = 12, height = 10, device = "pdf"
)

ggsave(
  filename = "manuscript/figures/fig1_threshold_stability.png",
  plot = combined_plot,
  width = 12, height = 10, device = "png"
)

message("Figure 1 (Dual-Panel) successfully saved.")