# manuscript/R/09_plot_naive_baseline.R
library(dynaRchetypes)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(isotree)

dir.create("manuscript/figures", recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. INITIALIZATION & KITCHEN SINK SCORING
# -----------------------------------------------------------------------------
message("Initializing Engine with Naive Raw Features...")
data("hvac_features")
engine <- new("DiagnosticEngine", config_path = "inst/extdata/turin_hvac_naive_config.yaml")
engine <- ingest_telemetry(engine, hvac_features)

# Score the feature matrix
set.seed(42)
iso_model <- suppressWarnings(isolation.forest(
  engine@feature_matrix, 
  ntrees = 500, 
  sample_size = 256, 
  missing_action = "fail"
))
anomaly_scores <- stats::predict(iso_model, engine@feature_matrix)

# -----------------------------------------------------------------------------
# 2. STABILITY SWEEP (UPDATED FOR DEPRESSED SCORES)
# -----------------------------------------------------------------------------
message("Executing Naive Threshold Sweep. This will take a moment...")
# Lowered the sweep window to capture the depressed anomaly scores
#threshold_seq <- seq(0.57, 0.65, by = 0.005) 
threshold_seq <- seq(0.6, 0.65, by = 0.005) 
results_sweep <- list()

for (th in threshold_seq) {
  print(th)
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
  
  results_sweep[[length(results_sweep) + 1]] <- data.frame(
    Threshold = th, Opt_k_Silhouette = opt_sil
  )
}

df_sweep <- bind_rows(results_sweep)

p_sweep <- ggplot(df_sweep, aes(x = Threshold, y = Opt_k_Silhouette)) +
  geom_line(linewidth = 1.2, color = "#0072B2") +
  geom_point(size = 3, color = "#0072B2") +
  scale_y_continuous(breaks = seq(2, 10, by = 1), limits = c(2, 10)) +
  labs(
    title = "Naive Architecture Stability",
    subtitle = "Optimal k across threshold sweep using raw, unengineered features.",
    x = "Anomaly Threshold",
    y = "Optimal Number of Clusters (k)"
  ) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))

ggsave("manuscript/figures/fig9a_naive_stability_new.pdf", plot = p_sweep, width = 8, height = 4, device = "pdf")
ggsave("manuscript/figures/fig9a_naive_stability_new.png", plot = p_sweep, width = 8, height = 4, device = "png")

# -----------------------------------------------------------------------------
# 3. DYNAMIC MEDOID EXTRACTION
# -----------------------------------------------------------------------------
message("Extracting Top 50 Naive Anomalies for Medoid analysis...")

# Dynamically grab the top 50 anomalies instead of hardcoding a threshold
top_indices <- order(anomaly_scores, decreasing = TRUE)[1:50]

engine@anomalies <- data.frame(
  timestamp = engine@raw_telemetry$timestamp[top_indices],
  anomaly_score = anomaly_scores[top_indices]
)
engine@domain_state <- "anomalies_extracted"

engine <- suppressMessages(compute_distances(engine))
engine <- suppressMessages(calibrate(engine, min_k = 2, max_k = 10))

# Dynamically pull the optimal k found by the algorithm
opt_k <- engine@k_metrics$k[which.max(engine@k_metrics$Global_Silhouette)]
message(sprintf("Algorithm locked into k=%d for the naive dataset.", opt_k))

plot_data <- extract_medoid_windows(engine, k = opt_k)

# Clean labels for the massive facet grid
plot_data <- plot_data %>%
  mutate(
    Variable_Clean = factor(Variable, levels = c(
      "t_supply", "t_return", "sp_return", "t_saturation", "t_outdoor", 
      "rh_supply", "rh_return", "rh_outdoor", "energy", "power"
    ), labels = c(
      "Supply Temp", "Return Temp", "Setpoint", "Saturation Temp", "Outdoor Temp",
      "Supply RH", "Return RH", "Outdoor RH", "Energy", "Power"
    ))
  ) %>%
  filter(!is.na(Variable_Clean))

message("Generating Naive Medoid Matrix...")
p_medoids <- ggplot(plot_data, aes(x = Relative_Time, y = Value, color = Variable_Clean)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  facet_grid(Variable_Clean ~ Archetype, scales = "free_y") +
  scale_color_viridis_d(option = "turbo") + 
  labs(
    title = sprintf("Naive Baseline Medoid Trajectories (k=%d)", opt_k),
    subtitle = "Physical shapes formed when bypassing feature engineering.",
    x = "Relative Time (Hours)",
    y = "Telemetry Value"
  ) +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "none",
    strip.text.x = element_text(face = "bold"),
    strip.text.y = element_text(face = "bold", angle = 270, size = 8),
    strip.background = element_rect(fill = "#f0f0f0", color = "grey80"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0, "pt"),
    panel.border = element_rect(color = "grey80", fill = NA)
  )

ggsave("manuscript/figures/fig9b_naive_medoids.pdf", plot = p_medoids, width = 12, height = 14, device = "pdf")
ggsave("manuscript/figures/fig9b_naive_medoids.png", plot = p_medoids, width = 12, height = 14, device = "png")

message("Naive baseline execution complete. Check figures 9a and 9b.")