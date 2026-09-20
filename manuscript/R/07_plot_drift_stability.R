# manuscript/R/07_plot_drift_stability.R
library(dynaRchetypes)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)


# -----------------------------------------------------------------------------
# 1. INITIALIZATION
# -----------------------------------------------------------------------------
message("Initializing Engine for Slow-Drift Versatility Analysis...")
data("hvac_features")
# Pointing to the new configuration targeting volatility and 24h lag
engine <- new("DiagnosticEngine", config_path = "inst/extdata/turin_hvac_drift_config.yaml")
engine <- ingest_telemetry(engine, hvac_features)

# Score the feature matrix once to use for both Panel A and Panel C
set.seed(42)
iso_model <- suppressWarnings(isotree::isolation.forest(
  engine@feature_matrix, 
  ntrees = 500, 
  sample_size = 256, 
  missing_action = "fail"
))
anomaly_scores <- stats::predict(iso_model, engine@feature_matrix)

# -----------------------------------------------------------------------------
# 2. PANEL A: EVENT EXTRACTION SENSITIVITY
# -----------------------------------------------------------------------------
message("Generating Panel A data...")
thresholds_a <- seq(0.60, 0.75, by = 0.005)
counts_a <- sapply(thresholds_a, function(th) sum(anomaly_scores >= th))
df_panel_a <- data.frame(Threshold = thresholds_a, Anomalies = counts_a)

p_a <- ggplot(df_panel_a, aes(x = Threshold, y = Anomalies)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "A) Drift Event Extraction Sensitivity",
    x = "Isolation Forest Threshold",
    y = "Anomalies Extracted (Log Scale)"
  ) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))

# -----------------------------------------------------------------------------
# 3. PANEL B: CLUSTER CALIBRATION (AT YAML THRESHOLD)
# -----------------------------------------------------------------------------
message("Generating Panel B data...")
# Run the standard pipeline at the threshold defined in the drift YAML
engine_b <- extract_anomalies(engine)
engine_b <- suppressMessages(compute_distances(engine_b))
engine_b <- suppressMessages(calibrate(engine_b, min_k = 2, max_k = 10))

df_panel_b <- engine_b@k_metrics %>%
  pivot_longer(cols = c(Global_Silhouette, Product_Weighted_Harmonic), 
               names_to = "Metric", values_to = "Score") %>%
  mutate(Metric = ifelse(Metric == "Global_Silhouette", "Global Silhouette", "Harmonic Pairwise"))

# Dynamically extract optimal k for the reference line
opt_k <- engine_b@k_metrics$k[which.max(engine_b@k_metrics$Global_Silhouette)]

p_b <- ggplot(df_panel_b, aes(x = k, y = Score, color = Metric)) +
  geom_vline(xintercept = opt_k, linetype = "dashed", color = "black", alpha = 0.5) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = seq(2, 10, by = 1)) +
  scale_color_manual(values = c("Global Silhouette" = "#0072B2", "Harmonic Pairwise" = "#D55E00")) +
  labs(
    title = "B) Drift Cluster Calibration (Threshold 0.68)",
    x = "Number of Clusters (k)",
    y = "Clustering Score"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))

# -----------------------------------------------------------------------------
# 4. PANEL C: CLUSTER ARCHITECTURE STABILITY
# -----------------------------------------------------------------------------
message("Executing Threshold Sweep for Panel C. This will take a moment...")
threshold_seq <- seq(0.65, 0.71, by = 0.0025)
results_c <- list()

for (th in threshold_seq) {
  # Extract dynamic anomaly pool
  idx <- which(anomaly_scores >= th)
  if (length(idx) < 5) next 
  
  tmp_anomalies <- data.frame(
    timestamp = engine@raw_telemetry$timestamp[idx],
    anomaly_score = anomaly_scores[idx]
  )
  
  tmp_engine <- engine
  tmp_engine@anomalies <- tmp_anomalies
  tmp_engine@domain_state <- "anomalies_extracted"
  
  tmp_engine <- suppressMessages(compute_distances(tmp_engine))
  tmp_engine <- suppressMessages(calibrate(tmp_engine, min_k = 2, max_k = 10))
  
  df_k <- tmp_engine@k_metrics
  opt_sil <- df_k$k[which.max(df_k$Global_Silhouette)]
  opt_har <- df_k$k[which.max(df_k$Product_Weighted_Harmonic)]
  
  results_c[[length(results_c) + 1]] <- data.frame(
    Threshold = th,
    Opt_k_Silhouette = opt_sil,
    Opt_k_Harmonic = opt_har
  )
}

df_panel_c <- bind_rows(results_c) %>%
  pivot_longer(cols = starts_with("Opt_k"), names_to = "Metric", values_to = "Optimal_k") %>%
  mutate(Metric = ifelse(Metric == "Opt_k_Silhouette", "Global Silhouette", "Harmonic Pairwise"))

p_c <- ggplot(df_panel_c, aes(x = Threshold, y = Optimal_k, color = Metric, group = Metric)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(breaks = seq(2, 10, by = 1), limits = c(2, 10)) +
  scale_color_manual(values = c("Global Silhouette" = "#0072B2", "Harmonic Pairwise" = "#D55E00")) +
  labs(
    title = "C) Drift Architecture Stability Across Anomaly Pools",
    x = "Anomaly Threshold",
    y = "Optimal Number of Clusters (k)"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))

# -----------------------------------------------------------------------------
# 5. STITCH AND EXPORT
# -----------------------------------------------------------------------------
message("Stitching panels and saving Figure 7...")
fig7 <- (p_a | p_b) / p_c + plot_layout(heights = c(1, 1.5))

output_path <- "manuscript/figures/fig7_drift_stability.pdf"
ggsave(filename = output_path, plot = fig7, width = 11, height = 9, device = "pdf")
output_png <- "manuscript/figures/fig7_drift_stability.png"
ggsave(filename = output_png, plot = fig7, width = 11, height = 9, device = "png")

message("Versatility analysis complete. Check fig7_drift_stability.")