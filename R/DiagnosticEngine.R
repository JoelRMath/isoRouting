#' @title DiagnosticEngine Class
#'
#' @description 
#' A stateful S4 class that acts as a domain-agnostic anomaly detection 
#' and routing engine. It ingests telemetry via a YAML configuration, 
#' computes geometric distances between anomalies, and optimizes 
#' operational routing queues using edge-weighted harmonic pairwise penalties.
#'
#' @slot config_path character. Path to the YAML configuration file.
#' @slot feature_schema list. Parsed rules for dynamic feature engineering.
#' @slot domain_state character. Current lifecycle state of the engine.
#' @slot raw_telemetry data.frame. The base ingested dataset.
#' @slot feature_matrix data.frame. Data after applying YAML mutations.
#' @slot anomalies data.frame. Isolated extreme events triggering the routing logic.
#' @slot distance_matrix matrix. Multivariate dynamic time warping (MDTW) distance matrix.
#' @slot hierarchical_tree ANY. The cached DIANA clustering object.
#' @slot k_metrics data.frame. Tracking of structural integrity across k-values.
#' @slot medoids list. Extracted physical archetypes for human-in-the-loop validation.
#' @slot active_k numeric. The operational ground-truth number of queues.
#' @slot routing_queues data.frame. Final anomalies assigned to physical states.
#'
#' @import methods
#' @export
setClass(
  "DiagnosticEngine",
  slots = list(
    config_path = "character",
    feature_schema = "list",
    domain_state = "character",
    raw_telemetry = "data.frame",
    feature_matrix = "data.frame",
    anomalies = "data.frame",
    distance_matrix = "matrix",
    hierarchical_tree = "ANY",
    k_metrics = "data.frame",
    medoids = "list",
    active_k = "numeric",
    routing_queues = "data.frame"
  ),
  
  # Set safe default values for initialization
  prototype = list(
    domain_state = "uninitialized",
    active_k = NA_real_
  ),
  
  validity = function(object) {
    if (length(object@config_path) == 0) {
      return("Engine requires a valid YAML configuration path upon instantiation.")
    }
    return(TRUE)
  }
)

# --- ENGINE API (GENERICS) ---

#' @title Ingest and Engineer Telemetry
#' @description Ingests raw telemetry and dynamically applies feature engineering 
#' as defined by the engine's YAML schema.
#' @param object A DiagnosticEngine object.
#' @param data A data.frame containing the raw telemetry.
#' @return An updated DiagnosticEngine object with the feature_matrix populated.
#' @export
setGeneric("ingest_telemetry", function(object, data) standardGeneric("ingest_telemetry"))

#' @title Extract Anomalies via Isolation Forest
#' @description Scores the feature matrix using an Isolation Forest and extracts 
#' extreme events exceeding the specified threshold.
#' @param object A DiagnosticEngine object.
#' @param threshold Numeric limit for empirical p-values (e.g., 0.60).
#' @return An updated DiagnosticEngine object with the anomalies slot populated.
#' @export
setGeneric("extract_anomalies", function(object, threshold) standardGeneric("extract_anomalies"))

#' @title Compute MDTW Distance Matrix
#' @description Calculates the N x N Multivariate Dynamic Time Warping distance 
#' matrix for all isolated anomalies across a dynamic temporal window.
#' @param object A DiagnosticEngine object.
#' @return An updated DiagnosticEngine object with distance_matrix and hierarchical_tree populated.
#' @export
setGeneric("compute_distances", function(object) standardGeneric("compute_distances"))

#' @title Calibrate Cluster Integrity
#' @description Iterates through potential k values and calculates geometric integrity 
#' metrics (Global Silhouette, Product-Weighted Harmonic Pairwise) to locate the fracture point.
#' @param object A DiagnosticEngine object.
#' @param min_k Integer. Minimum number of operational queues to test. Default is 2.
#' @param max_k Integer. Maximum number of operational queues to test. Default is 8.
#' @return An updated DiagnosticEngine object with k_metrics and medoids populated.
#' @export
setGeneric("calibrate", function(object, min_k = 2, max_k = 8) standardGeneric("calibrate"))

#' @title Commit Routing Queues
#' @description Locks in the mathematically or operationally validated k-value 
#' and assigns all anomalies to their final physical dispatch queues.
#' @param object A DiagnosticEngine object.
#' @param k Integer. The finalized number of distinct physical states.
#' @return An updated DiagnosticEngine object ready for dispatch export.
#' @export
setGeneric("commit_queues", function(object, k) standardGeneric("commit_queues"))

# --- INITIALIZATION METHOD ---

#' @title Initialize DiagnosticEngine
#' @description Custom initialization method to automatically parse the 
#' YAML configuration file upon object instantiation.
#' @param .Object The DiagnosticEngine object being instantiated.
#' @param config_path Character string pointing to the YAML schema.
#' @param ... Additional arguments passed to standard S4 initialization.
#' @importFrom yaml read_yaml
#' @export
setMethod("initialize", "DiagnosticEngine", function(.Object, config_path, ...) {
  
  if (!missing(config_path)) {
    # 1. Validate file existence
    if (!file.exists(config_path)) {
      stop(sprintf("Fatal Error: Configuration file not found at '%s'", config_path))
    }
    
    # 2. Assign the path and parse the schema
    .Object@config_path <- config_path
    .Object@feature_schema <- yaml::read_yaml(config_path)
    
    # 3. Advance the domain state
    .Object@domain_state <- "initialized"
    
    message(sprintf("DiagnosticEngine initialized. Schema loaded from: %s", basename(config_path)))
  }
  
  # 4. Standard S4 inheritance passthrough
  .Object <- callNextMethod(.Object, ...)
  return(.Object)
})

#' @export
setMethod("ingest_telemetry", "DiagnosticEngine", function(object, data) {
  # 1. State Validation
  if (object@domain_state == "uninitialized") {
    stop("Fatal Error: Engine must be initialized with a YAML schema prior to ingestion.")
  }
  
  # 2. Store Raw Data
  object@raw_telemetry <- data
  
  # 3. Dynamic Feature Engineering (via YAML)
  targets <- object@feature_schema$target_variables
  
  if (is.null(targets)) {
    stop("YAML schema missing 'target_variables' array.")
  }
  
  if (!all(targets %in% colnames(data))) {
    missing_cols <- setdiff(targets, colnames(data))
    stop(sprintf("Data missing schema targets: %s", paste(missing_cols, collapse = ", ")))
  }
  
  # 4. Lock in the computational feature space
  object@feature_matrix <- data[, targets, drop = FALSE]
  
  # 5. Advance State
  object@domain_state <- "telemetry_ingested"
  message(sprintf("Telemetry ingested successfully. Feature matrix locked at %d rows by %d features.", 
                  nrow(object@feature_matrix), ncol(object@feature_matrix)))
  
  return(object)
})

#' @export
setMethod("extract_anomalies", "DiagnosticEngine", function(object, threshold) {
  # 1. State Validation
  if (!object@domain_state %in% c("telemetry_ingested", "calibrated")) {
    stop("Fatal Error: Telemetry must be ingested before extracting anomalies.")
  }
  
  # 2. Fallback to YAML config if threshold is not provided manually
  if (missing(threshold)) {
    threshold <- object@feature_schema$anomaly_threshold
    if (is.null(threshold)) {
      warning("Threshold not provided in arguments or YAML. Defaulting to 0.60.")
      threshold <- 0.60
    }
  }
  
  # 3. Train Isolation Forest and Score Space
  # Using isotree for highly optimized C++ scoring
  message("Training Isolation Forest and scoring feature space...")
  set.seed(7)
  iso_model <- isotree::isolation.forest(object@feature_matrix, ntrees = 500, nthreads = 1)
  scores <- predict(iso_model, object@feature_matrix)
  
  # 4. Bind and Filter
  # We bind to raw_telemetry to ensure timestamps and external IDs are preserved
  scored_data <- object@raw_telemetry
  scored_data$anomaly_score <- scores
  
  object@anomalies <- scored_data[scored_data$anomaly_score > threshold, ]
  
  # 5. Advance State
  object@domain_state <- "anomalies_extracted"
  message(sprintf("Anomaly extraction complete. %d extreme events isolated (Score > %s).", 
                  nrow(object@anomalies), threshold))
  
  return(object)
})

#' @export
setMethod("compute_distances", "DiagnosticEngine", function(object) {
  # 1. State Validation
  if (!object@domain_state %in% c("anomalies_extracted", "calibrated")) {
    stop("Fatal Error: Anomalies must be extracted before computing distances.")
  }
  
  targets <- object@feature_schema$target_variables
  n_events <- nrow(object@anomalies)
  
  # Fetch dynamic window from YAML
  window_hours <- object@feature_schema$temporal_window_hours
  if (is.null(window_hours)) {
    warning("YAML missing 'temporal_window_hours'. Defaulting to 2.")
    window_hours <- 2
  }
  
  message(sprintf("Extracting ±%d-hour multivariate time-series windows for %d events...", window_hours, n_events))
  
  # 2. Dynamic Window Extraction Function
  extract_ts_window_multi <- function(target_time) {
    window_data <- object@raw_telemetry[
      object@raw_telemetry$timestamp >= (target_time - lubridate::hours(window_hours)) & 
        object@raw_telemetry$timestamp <= (target_time + lubridate::hours(window_hours)), 
    ]
    
    # Isolate targets, scale, and cast to matrix
    window_matrix <- as.matrix(scale(window_data[, targets, drop = FALSE]))
    return(window_matrix)
  }
  
  # Apply multivariate extraction to all anomalies
  event_ts_list <- purrr::map(object@anomalies$timestamp, extract_ts_window_multi)
  
  message("Computing optimized N x N MDTW distance matrix...")
  
  # 3. Compute the O(N^2) MDTW distance matrix (Symmetric Optimization)
  mdtw_dist_matrix <- matrix(0, nrow = n_events, ncol = n_events)
  
  for (i in 1:(n_events - 1)) {
    for (j in (i + 1):n_events) {
      dist_val <- dtw::dtw(
        event_ts_list[[i]], 
        event_ts_list[[j]], 
        distance.only = TRUE
      )$normalizedDistance
      
      # Populate symmetrically
      mdtw_dist_matrix[i, j] <- dist_val
      mdtw_dist_matrix[j, i] <- dist_val
    }
  }
  
  # 4. Cache the matrix and hierarchical tree
  object@distance_matrix <- mdtw_dist_matrix
  
  message("Constructing DIANA hierarchical tree cache...")
  # Convert raw matrix to a 'dist' object for the clustering algorithm
  object@hierarchical_tree <- as.hclust(cluster::diana(stats::as.dist(mdtw_dist_matrix)))
  
  # 5. Advance State
  object@domain_state <- "distances_computed"
  message("Distance computation complete.")
  
  return(object)
})

#' @export
setMethod("calibrate", "DiagnosticEngine", function(object, min_k = 2, max_k = 8) {
  # 1. State Validation
  if (!object@domain_state %in% c("distances_computed", "calibrated")) {
    stop("Fatal Error: Distances must be computed before calibrating.")
  }
  
  message(sprintf("Calibrating cluster integrity from k=%d to k=%d...", min_k, max_k))
  
  dist_mat <- object@distance_matrix
  hc_tree <- object@hierarchical_tree
  
  # 2. Define the Edge-Weighted Harmonic Function Internally
  calc_product_weighted_harmonic <- function(d_mat, lbls) {
    k_unique <- sort(unique(lbls))
    k_count <- length(k_unique)
    
    pairwise_scores <- c()
    pairwise_weights <- c()
    
    for (i in 1:(k_count - 1)) {
      for (j in (i + 1):k_count) {
        cl_A <- k_unique[i]
        cl_B <- k_unique[j]
        
        idx <- which(lbls %in% c(cl_A, cl_B))
        sub_mat <- d_mat[idx, idx]
        sub_labels <- as.character(lbls[idx])
        
        # Calculate standard pairwise silhouette
        sil_obj <- cluster::silhouette(as.numeric(as.factor(sub_labels)), dmatrix = sub_mat)
        mean_sil <- mean(sil_obj[, "sil_width"])
        
        # Strict Penalty: Clamp negative/zero scores
        if (is.na(mean_sil) || mean_sil <= 0) mean_sil <- 0.001 
        
        pairwise_scores <- c(pairwise_scores, mean_sil)
        
        # Edge-Weighting: Product of cluster sizes
        weight <- sum(lbls == cl_A) * sum(lbls == cl_B)
        pairwise_weights <- c(pairwise_weights, weight)
      }
    }
    
    # Harmonic Mean Formula
    return(sum(pairwise_weights) / sum(pairwise_weights / pairwise_scores))
  }
  
  # 3. Iteration & Medoid Extraction Loop
  results <- data.frame(k = min_k:max_k, Global_Silhouette = NA, Product_Weighted_Harmonic = NA)
  medoids_list <- list()
  
  for (i in seq_along(results$k)) {
    curr_k <- results$k[i]
    lbls <- stats::cutree(hc_tree, k = curr_k)
    
    # Calculate Metrics
    global_sil <- cluster::silhouette(lbls, dmatrix = dist_mat)
    results$Global_Silhouette[i] <- mean(global_sil[, "sil_width"])
    results$Product_Weighted_Harmonic[i] <- calc_product_weighted_harmonic(dist_mat, lbls)
    
    # Extract Medoids for this specific k
    k_medoids <- c()
    for (c in 1:curr_k) {
      cluster_idx <- which(lbls == c)
      
      # Handle potential micro-clusters of size 1 safely
      if (length(cluster_idx) == 1) {
        medoid_global <- cluster_idx
      } else {
        sub_d <- dist_mat[cluster_idx, cluster_idx]
        medoid_local <- which.min(rowSums(sub_d))
        medoid_global <- cluster_idx[medoid_local]
      }
      
      # Extract the exact timestamp of the medoid
      k_medoids <- c(k_medoids, as.character(object@anomalies$timestamp[medoid_global]))
    }
    
    # Store in the list using k as the name (e.g., medoids_list[["4"]])
    medoids_list[[as.character(curr_k)]] <- k_medoids
  }
  
  # 4. Write to Object Slots
  object@k_metrics <- results
  object@medoids <- medoids_list
  object@domain_state <- "calibrated"
  
  # 5. Output the Mathematical Recommendation
  best_k <- results$k[which.max(results$Product_Weighted_Harmonic)]
  message(sprintf("Calibration complete. Mathematical recommendation based on Edge-Weighted Harmonic penalty: k=%d", best_k))
  
  return(object)
})

#' @export
setMethod("commit_queues", "DiagnosticEngine", function(object, k) {
  # 1. State Validation
  if (!object@domain_state %in% c("calibrated", "ready_for_dispatch")) {
    stop("Fatal Error: Engine must be calibrated before committing queues.")
  }
  
  # 2. Input Validation
  if (missing(k) || !is.numeric(k) || k %% 1 != 0) {
    stop("Please provide a valid integer for k (e.g., k = 4).")
  }
  
  if (!as.character(k) %in% names(object@medoids)) {
    stop(sprintf("k=%d was not evaluated during calibration. Evaluated k values: %s", 
                 k, paste(names(object@medoids), collapse = ", ")))
  }
  
  message(sprintf("Committing dispatch queues for k=%d physical states...", k))
  
  # 3. Execute the final tree cut
  final_labels <- stats::cutree(object@hierarchical_tree, k = k)
  
  # 4. Bind the assignments directly to the anomalies dataframe
  dispatch_df <- object@anomalies
  dispatch_df$operational_queue <- as.factor(paste0("Queue_", final_labels))
  
  # Reorder columns to put the operational_queue first for the dispatch dashboard
  cols <- c("operational_queue", setdiff(names(dispatch_df), "operational_queue"))
  dispatch_df <- dispatch_df[, cols]
  
  # 5. Lock in the operational slots
  object@active_k <- k
  object@routing_queues <- dispatch_df
  object@domain_state <- "ready_for_dispatch"
  
  message("Queues successfully committed. Call `@routing_queues` to export the dispatch ledger.")
  
  return(object)
})