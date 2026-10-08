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
yaml_filepath <- "inst/extdata/kinematic_config.yaml"

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
thr_min <- 0.6
threshold_seq <- seq(thr_min, 0.70, by = 0.005) 
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

  engine@model <- iso_model
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

km2 <- max(df_sweep$Opt_k_Silhouette)+2
p_k <- ggplot(df_sweep, aes(x = Threshold, y = Opt_k_Silhouette)) +
  geom_line(linewidth = 1.2, color = "#009E73") + # Changed to a green theme for the "successful" model
  geom_point(size = 3, color = "#009E73") +
  scale_y_continuous(breaks = seq(2, km2, by = 2), limits = c(2, km2)) +
  scale_x_continuous(limits = c(thr_min, max(df_sweep$Threshold))) + 
  labs(
    x = "Anomaly Threshold",
    y = "Optimal Clusters (k)"
  ) +
  theme_bw(base_size = 12)

p_combined <- p_n / p_k
print(p_combined)

ggsave("manuscript/figures/fig_04_kinematic_stability.png", p_combined, width = 8, height = 7, dpi = 300)

# -------------------------------------------------------------------------
# 5. DUAL-DASHBOARD MEDOID VISUALIZATION (Targets vs. Contexts)
# -------------------------------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)

message("Executing final medoid extraction for stable plateau (th = 0.66, k = 4)...")

# Isolate anomalies for the target threshold
target_th <- 0.66
target_k <- 4

idx_target <- which(anomaly_scores >= target_th)
engine@model <- iso_model
engine@anomalies <- data.frame(
  timestamp = engine@raw_telemetry$timestamp[idx_target],
  anomaly_score = anomaly_scores[idx_target]
)
engine@domain_state <- "anomalies_extracted"

# Compute topological distance and extract the physical medoids
engine <- suppressMessages(compute_distances(engine))
engine <- suppressMessages(calibrate(engine, min_k = target_k, max_k = target_k))
engine <- suppressMessages(extract_medoids(engine, k = target_k))
engine <- suppressMessages(commit_queues(engine, k = target_k))

# Combine the 4 medoid dataframes
medoid_df <- bind_rows(engine@medoids)

# Fetch target and context variables directly from the engine's YAML schema
targets <- engine@feature_schema$target_variables
contexts <- engine@feature_schema$context_variables
plot_vars <- c(targets, contexts)

global_data <- engine@raw_telemetry

message("Computing global physical bounds (\u00B1 2 SD) for all variables...")
# Create a reference dataframe containing the normal operating bands
bands_list <- list()
for (tv in plot_vars) {
  g_mean <- mean(global_data[[tv]], na.rm = TRUE)
  g_sd <- sd(global_data[[tv]], na.rm = TRUE)
  
  bands_list[[tv]] <- data.frame(
    variable = tv,
    global_mean = g_mean,
    band_min = g_mean - (2 * g_sd),
    band_max = g_mean + (2 * g_sd)
  )
}
bands_df <- bind_rows(bands_list)

# Reshape raw data to long format and join bounds
medoid_long <- medoid_df %>%
  select(timestamp, cluster, medoid_timestamp, all_of(plot_vars)) %>%
  pivot_longer(
    cols = all_of(plot_vars), 
    names_to = "variable", 
    values_to = "raw_value"
  ) %>%
  mutate(time_offset = as.numeric(difftime(timestamp, medoid_timestamp, units = "hours"))) %>%
  left_join(bands_df, by = "variable")

# Lock factor levels to maintain the YAML order
medoid_long$variable <- factor(medoid_long$variable, levels = plot_vars)

# -------------------------------------------------------------------------
# 5A. ANALYTICS DASHBOARD (Target Variables Only)
# -------------------------------------------------------------------------
message("Plotting Analytics Dashboard (Target Variables)...")
medoid_targets <- medoid_long %>% filter(variable %in% targets)

p_targets <- ggplot(medoid_targets, aes(x = time_offset)) +
  geom_ribbon(aes(ymin = band_min, ymax = band_max), fill = "gray80", alpha = 0.5) +
  geom_hline(aes(yintercept = global_mean), linetype = "dotted", color = "gray50") +
  geom_line(aes(y = raw_value, color = variable), linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.6) +
  facet_grid(variable ~ cluster, scales = "free_y") +
  scale_color_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Kinematic Topology: Anomaly Target Variables",
    subtitle = "Mathematical fault signature (Gray band = Global \u00B1 2 SD)",
    x = "Time Offset from Anomaly Event (Hours)",
    y = "Raw Telemetry Value"
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold")
  )

print(p_targets)
ggsave("manuscript/figures/fig_07a_analytics_targets.png", p_targets, width = 11, height = 7, dpi = 300)

# -------------------------------------------------------------------------
# 5B. TECHNICIAN DASHBOARD (Context Variables Only)
# -------------------------------------------------------------------------
message("Plotting Technician Dashboard (Context Variables)...")
medoid_contexts <- medoid_long %>% filter(variable %in% contexts)

p_contexts <- ggplot(medoid_contexts, aes(x = time_offset)) +
  geom_ribbon(aes(ymin = band_min, ymax = band_max), fill = "gray80", alpha = 0.5) +
  geom_hline(aes(yintercept = global_mean), linetype = "dotted", color = "gray50") +
  # Using a distinct, single professional blue for physical context vectors
  geom_line(aes(y = raw_value, color = variable), linewidth = 1) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.6) +
  facet_grid(variable ~ cluster, scales = "free_y") +
  scale_color_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Physical State Routing: Equipment Context Variables",
    subtitle = "Diagnostic telemetry for mechanic triage (Gray band = Global \u00B1 2 SD)",
    x = "Time Offset from Anomaly Event (Hours)",
    y = "Raw Telemetry Value"
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold")
  )

print(p_contexts)
ggsave("manuscript/figures/fig_07b_technician_context.png", p_contexts, width = 11, height = 8, dpi = 300)