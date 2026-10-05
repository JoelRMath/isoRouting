# 004_kinematic.R
#
# This script evaluates cluster stability for the kinematic architecture 
# (engineered physical and temporal features). It sweeps across Isolation Forest 
# anomaly thresholds to locate the "stable region" where optimal k converges.

library(dplyr)
library(ggplot2)
library(patchwork)
library(isotree)
library(dynaRchetypes)

# 1. LOAD DATA AND INITIALIZE
yaml_filepath <- "inst/extdata/naive_config.yaml"

message("Loading engineered telemetry data...")
load("data/hvac_features.rda")

# Initialize the engine
engine <- new("DiagnosticEngine", config_path = yaml_filepath)
engine <- ingest_telemetry(engine, data = hvac_features)

# 2. TRAIN ONCE
message("Training Kinematic Isolation Forest...")
set.seed(42)
iso_model <- suppressWarnings(isotree::isolation.forest(
  engine@feature_matrix, 
  ntrees = 500, 
  sample_size = 256, 
  missing_action = "fail"
))
anomaly_scores <- stats::predict(iso_model, engine@feature_matrix)

# 3. SWEEP THRESHOLD
thr_min <- 0.55
threshold_seq <- seq(thr_min, 0.65, by = 0.005) 
km <- 25
results_sweep <- list()

message("Executing threshold sweep...")
for (th in threshold_seq) {
  idx <- which(anomaly_scores >= th)
  print(paste("Testing threshold:", th, "; N = ", length(idx)))
  
  # Skip if N drops too low to cluster meaningfully
  if (length(idx) < 15) {
    print("Not enough anomalies, skipping...")
    next 
  }
  
  tmp_anomalies <- data.frame(
    timestamp = engine@raw_telemetry$timestamp[idx],
    anomaly_score = anomaly_scores[idx]
  )
  
  tmp_engine <- engine
  tmp_engine@anomalies <- tmp_anomalies
  tmp_engine@domain_state <- "anomalies_extracted"
  
  tmp_engine <- suppressMessages(compute_distances(tmp_engine))
  tmp_engine <- suppressMessages(calibrate(tmp_engine, min_k = 2, max_k = km))
  
  df_k <- tmp_engine@k_metrics
  opt_sil <- df_k$k[which.max(df_k$Global_Silhouette)]
  
  results_sweep[[length(results_sweep) + 1]] <- data.frame(
    Threshold = th, 
    N_Anomalies = length(idx),
    Opt_k_Silhouette = opt_sil
  )
}

df_sweep <- bind_rows(results_sweep)

# 4. PLOT STABILITY (Two-Panel Patchwork)
p_n <- ggplot(df_sweep, aes(x = Threshold, y = N_Anomalies)) +
  geom_line(linewidth = 1, color = "gray40") +
  geom_point(size = 2, color = "gray40") +
  scale_y_log10() + 
  scale_x_continuous(limits = c(thr_min, max(df_sweep$Threshold))) + 
  labs(
    title = "Cluster Stability (Engineered Kinematic Variables)",
    y = "Isolated Events (N)",
    x = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(face = "bold", size = 13)
  )

p_k <- ggplot(df_sweep, aes(x = Threshold, y = Opt_k_Silhouette)) +
  geom_line(linewidth = 1.2, color = "#009E73") + # Changed to a green theme for the "successful" model
  geom_point(size = 3, color = "#009E73") +
  scale_y_continuous(breaks = seq(2, km, by = 2), limits = c(2, km)) +
  scale_x_continuous(limits = c(thr_min, max(df_sweep$Threshold))) + 
  labs(
    x = "Anomaly Threshold",
    y = "Optimal Clusters (k)"
  ) +
  theme_bw(base_size = 12)

p_combined <- p_n / p_k
print(p_combined)

ggsave("manuscript/figures/fig_04_kinematic_stability.png", p_combined, width = 8, height = 7, dpi = 300)