library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(lubridate)
library(ggplot2)
library(isotree)
library(dynaRchetypes)

# 1. LOAD CSV AND APPLY CLEANING
data_filepath <- "../HVAC/data/Turin/HVAC_NE_EC_19-21.csv"
yaml_filepath <- "inst/extdata/naive_config.yaml"

message("Loading, cleaning, and engineering features...")
hvac_data <- read_csv(data_filepath, show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(timestamp = ymd_hms(timestamp)) %>%
  filter(!is.na(timestamp)) %>%
  
  # 1. Force perfect 15-minute grid to separate continuous operating blocks
  complete(
    timestamp = seq(
      min(timestamp, na.rm = TRUE), 
      max(timestamp, na.rm = TRUE), 
      by = "15 mins"
    )
  ) %>%
  arrange(timestamp) %>%
  
  # 2. Calculate Context and Target features
  mutate(
    # Context
    is_weekend = if_else(wday(timestamp, week_start = 1) %in% c(6, 7), 1, 0),
    temp_error = t_return - sp_return,
    
    # Common-Mode Rejection (Physics)
    delta_t = t_return - t_supply,
    delta_rh = rh_return - rh_supply,
    
    # 15-Minute Instantaneous Derivatives
    energy_deriv_15m = energy - lag(energy, n = 1),
    delta_t_deriv_15m = delta_t - lag(delta_t, n = 1),
    delta_rh_deriv_15m = delta_rh - lag(delta_rh, n = 1),
    t_return_deriv_15m = t_return - lag(t_return, n = 1),
    t_saturation_deriv_15m = t_saturation - lag(t_saturation, n = 1),
    
    # 1-Hour Volatility 
    # (Requires 3 prior rows; will result in NA for the first 45 mins of a restart)
    t_supply_roll_sd_1h = slider::slide_dbl(t_supply, sd, .before = 3, .complete = TRUE)
  ) %>%
  
  # 3. Safely purge missing gaps and the brief restart windows lacking history
  drop_na()

message(sprintf("Data cleaned. Remaining rows: %d", nrow(hvac_data)))

# Initialize the engine
engine <- new("DiagnosticEngine", config_path = yaml_filepath)
engine <- ingest_telemetry(engine, data = hvac_data)

# 2. TRAIN ONCE (Isolating the threshold variance)
message("Training static Isolation Forest...")
set.seed(42)
iso_model <- suppressWarnings(isotree::isolation.forest(
  engine@feature_matrix, 
  ntrees = 500, 
  sample_size = 256, 
  missing_action = "fail"
))
anomaly_scores <- stats::predict(iso_model, engine@feature_matrix)

# 3. SWEEP THRESHOLD ACROSS STATIC SCORES
threshold_seq <- seq(0.58, 0.65, by = 0.005) 
results_sweep <- list()

message("Executing threshold sweep...")
for (th in threshold_seq) {
  idx <- which(anomaly_scores >= th)
  print(paste("Testing threshold:", th, "; N = ", length(idx)))
  
  if (length(idx) < 5) {
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
  
  km <- 14
  
  tmp_engine <- suppressMessages(compute_distances(tmp_engine))
  tmp_engine <- suppressMessages(calibrate(tmp_engine, min_k = 2, max_k = km))
  
  df_k <- tmp_engine@k_metrics
  opt_sil <- df_k$k[which.max(df_k$Global_Silhouette)]
  
  results_sweep[[length(results_sweep) + 1]] <- data.frame(
    Threshold = th, Opt_k_Silhouette = opt_sil
  )
}

df_sweep <- bind_rows(results_sweep)

# 4. PLOT REPRODUCTION
p_sweep <- ggplot(df_sweep, aes(x = Threshold, y = Opt_k_Silhouette)) +
  geom_line(linewidth = 1.2, color = "#0072B2") +
  geom_point(size = 3, color = "#0072B2") +
  scale_y_continuous(breaks = seq(2, km, by = 2), limits = c(2, km)) +
  labs(
    title = "Test: CSV Reproduction of Historical Instability",
    x = "Anomaly Threshold",
    y = "Optimal Number of Clusters (k)"
  ) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))

print(p_sweep)