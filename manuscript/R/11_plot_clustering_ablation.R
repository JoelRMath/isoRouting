# manuscript/R/11_plot_clustering_ablation.R
library(dynaRchetypes)
library(dplyr)
library(isotree)
library(mclust) # For Adjusted Rand Index

# Helper function to run the ablation test
run_ablation_test <- function(dataset, config_file, k_val) {
  # 1. Initialize & Score
  engine <- new("DiagnosticEngine", config_path = config_file)
  engine <- ingest_telemetry(engine, dataset)
  
  set.seed(42)
  iso_model <- suppressWarnings(isolation.forest(
    engine@feature_matrix, ntrees = 500, sample_size = 256, missing_action = "fail"
  ))
  
  anomaly_scores <- stats::predict(iso_model, engine@feature_matrix)
  
  # Ensure we only pick anomalies with a safe temporal buffer
  safe_buffer <- 12 # ~3 hours of 15-min intervals
  n_rows <- nrow(engine@feature_matrix)
  ordered_idx <- order(anomaly_scores, decreasing = TRUE)
  
  safe_idx <- ordered_idx[ordered_idx > safe_buffer & ordered_idx < (n_rows - safe_buffer)]
  top_idx <- safe_idx[1:50] # Grab exactly 50 safe events
  
  engine@anomalies <- data.frame(
    timestamp = engine@raw_telemetry$timestamp[top_idx],
    anomaly_score = anomaly_scores[top_idx]
  )
  engine@domain_state <- "anomalies_extracted"
  
  # 2. Trajectory Clustering (MDTW)
  engine_mdtw <- suppressMessages(compute_distances(engine))
  engine_mdtw <- suppressMessages(calibrate(engine_mdtw, min_k = k_val, max_k = k_val))
  
  # --- THE FIX: Extract the labels directly from the cached tree ---
  labels_mdtw <- stats::cutree(engine_mdtw@hierarchical_tree, k = k_val)
  # -----------------------------------------------------------------
  
  # 3. Static Vector Clustering (Euclidean)
  static_matrix <- engine@feature_matrix[top_idx, ]
  static_scaled <- scale(static_matrix)
  dist_euclidean <- dist(static_scaled, method = "euclidean")
  hc_static <- hclust(dist_euclidean, method = "ward.D2")
  labels_static <- cutree(hc_static, k = k_val)
  
  # 4. Return ARI
  return(adjustedRandIndex(labels_mdtw, labels_static))
}

# -----------------------------------------------------------------------------
# EXECUTE DUAL ABLATION
# -----------------------------------------------------------------------------
# Load the newly created dual datasets
data("hvac_raw")
data("hvac_engineered")

message("Testing Hypothesis 1: Raw Variables (Expected Low Similarity)...")
ari_raw <- run_ablation_test(
  dataset = hvac_raw, 
  config_file = "inst/extdata/turin_hvac_naive_config.yaml", 
  k_val = 10 # We know the naive dataset defaults to k=10 fragmentation
)

message("Testing Hypothesis 2: Engineered Variables (Expected High Similarity)...")
ari_eng <- run_ablation_test(
  dataset = hvac_engineered, 
  config_file = "inst/extdata/turin_hvac_config.yaml", 
  k_val = 4 # We know the engineered dataset locks into k=4
)

# -----------------------------------------------------------------------------
# RESULTS
# -----------------------------------------------------------------------------
message("\n--- ABLATION RESULTS ---")
message(sprintf("Raw Feature ARI:        %.3f", ari_raw))
message(sprintf("Engineered Feature ARI: %.3f", ari_eng))
message("------------------------")