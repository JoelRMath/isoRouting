# manuscript/R/05_validate_oos_clusters.R
library(dynaRchetypes)
library(dplyr)
library(tidyr)
library(ggplot2)

dir.create("manuscript/figures", recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. INITIALIZATION & OOS SCORING
# -----------------------------------------------------------------------------
message("Initializing Engine and calculating OOS anomalies...")
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
# 2. OOS CLUSTER STABILITY SWEEP
# -----------------------------------------------------------------------------
# We mirror the exact threshold sequence used in Figure 1C
threshold_seq <- seq(0.65, 0.71, by = 0.0025)
results <- list()

message(sprintf("Executing OOS stability sweep across %d threshold equivalents. This will take a moment...", length(threshold_seq)))

for (th in threshold_seq) {
  # 1. Determine the baseline target size
  n_target <- sum(df_drift$baseline_score >= th)
  
  # Skip if the pool starves (e.g., < 5 anomalies)
  if (n_target < 5) next 
  
  # 2. Extract the equivalent top N from the historical model's triage queue
  oos_anomalies <- df_drift %>% 
    arrange(desc(oos_score)) %>% 
    slice(1:n_target) %>%
    select(timestamp, anomaly_score = oos_score)
  
  # 3. Inject and Cluster
  tmp_engine <- engine
  tmp_engine@anomalies <- oos_anomalies
  tmp_engine@domain_state <- "anomalies_extracted"
  
  # Suppress the iterative messages to keep the console clean during the loop
  tmp_engine <- suppressMessages(compute_distances(tmp_engine))
  tmp_engine <- suppressMessages(calibrate(tmp_engine, min_k = 2, max_k = 10))
  
  # 4. Extract Optima
  df_k <- tmp_engine@k_metrics
  opt_sil <- df_k$k[which.max(df_k$Global_Silhouette)]
  opt_har <- df_k$k[which.max(df_k$Product_Weighted_Harmonic)]
  
  results[[length(results) + 1]] <- data.frame(
    Equivalent_Threshold = th,
    N_Anomalies = n_target,
    Opt_k_Silhouette = opt_sil,
    Opt_k_Harmonic = opt_har
  )
}

df_oos_stability <- bind_rows(results)

# -----------------------------------------------------------------------------
# 3. VISUALIZATION
# -----------------------------------------------------------------------------
message("Generating OOS Cluster Stability plot...")

plot_data <- df_oos_stability %>%
  pivot_longer(cols = starts_with("Opt_k"), names_to = "Metric", values_to = "Optimal_k") %>%
  mutate(Metric = ifelse(Metric == "Opt_k_Silhouette", "Global Silhouette", "Harmonic Pairwise"))

fig5 <- ggplot(plot_data, aes(x = Equivalent_Threshold, y = Optimal_k, color = Metric, group = Metric)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(breaks = seq(2, 10, by = 1), limits = c(2, 10)) +
  scale_color_manual(values = c("Global Silhouette" = "#0072B2", "Harmonic Pairwise" = "#D55E00")) +
  labs(
    title = "Out-of-Sample Cluster Architecture Stability",
    subtitle = "Optimal k sweeping across triage queues generated strictly by the historical model.",
    x = "Equivalent Anomaly Threshold",
    y = "Optimal Number of Clusters (k)"
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave("manuscript/figures/fig5_oos_stability.pdf", plot = fig5, width = 10, height = 5, device = "pdf")
ggsave("manuscript/figures/fig5_oos_stability.png", plot = fig5, width = 10, height = 5, device = "png")

message("Validation complete. Check fig5_oos_stability.pdf")