#' @title DiagnosticEngine Class
#'
#' @description 
#' A stateful S4 class that acts as a deterministic forensic and predictive engine for 
#' multivariate time series. It utilizes Isolation Forests to extract extreme 
#' events, avoiding Euclidean distances by constructing a proximity 
#' matrix based on terminal node signatures (Hamming distance). 
#' It applies divisive hierarchical clustering and global silhouette to extract mathematically stable anomaly archetypes.
#'
#' @slot config_path character. Path to the YAML configuration file.
#' @slot feature_schema list. Parsed rules for dynamic feature engineering.
#' @slot hyperparameters list. Machine learning tuning parameters (thresholds, ntrees).
#' @slot domain_state character. Current lifecycle state of the engine.
#' @slot raw_time_course data.frame. The base ingested dataset.
#' @slot feature_matrix data.frame. Data after applying YAML mutations.
#' @slot anomalies data.frame. Isolated extreme events triggering the analysis.
#' @slot model ANY. The trained isotree model object.
#' @slot distance_matrix matrix. Topological (Hamming) distance matrix of terminal nodes.
#' @slot hierarchical_tree ANY. The cached DIANA clustering object.
#' @slot k_metrics data.frame. Tracking of structural integrity across k-values.
#' @slot medoids list. Extracted physical archetypes representing anomaly signatures.
#' @slot baseline_signatures matrix. Cached terminal node paths of the medoids for predictive routing.
#' @slot active_k numeric. The mathematically validated number of fault clusters.
#' @slot forensic_ledger data.frame. Ranked historical ledger mapping anomalies to their medoid.
#'
#' @import methods
#' @export
setClass(
  "DiagnosticEngine",
  slots = list(
    config_path = "character",
    feature_schema = "list",
    hyperparameters = "list",
    domain_state = "character",
    raw_time_course = "data.frame",
    feature_matrix = "data.frame",
    anomalies = "data.frame",
    model = "ANY",
    distance_matrix = "matrix",
    hierarchical_tree = "ANY",
    k_metrics = "data.frame",
    medoids = "list",
    baseline_signatures = "matrix",
    active_k = "numeric",
    forensic_ledger = "data.frame"
  ),
  
  # Set safe default values for initialization
  prototype = list(
    domain_state = "uninitialized",
    active_k = NA_real_,
    distance_matrix = matrix(nrow = 0, ncol = 0),
    baseline_signatures = matrix(nrow = 0, ncol = 0)
  ),
  
  validity = function(object) {
    if (length(object@config_path) == 0) {
      return("Engine requires a valid YAML configuration path upon instantiation.")
    }
    return(TRUE)
  }
)

# --- ENGINE API (GENERICS) ---

# --- ENGINE API (GENERICS) ---

#' @title Get Engine Hyperparameters
#' @description Returns the machine learning parameters parsed from the configuration.
#' @param object A DiagnosticEngine object.
#' @export
setGeneric("get_hyperparameters", function(object) standardGeneric("get_hyperparameters"))

#' @title Get Feature Schema
#' @description Returns the target and context variables defined for the engine.
#' @param object A DiagnosticEngine object.
#' @export
setGeneric("get_features", function(object) standardGeneric("get_features"))

#' @title Get Forensic Ledger
#' @description Extracts the ranked anomaly ledger as a flat data.frame.
#' @param object A DiagnosticEngine object.
#' @export
setGeneric("get_ledger", function(object) standardGeneric("get_ledger"))

#' @title Get Topological Medoids
#' @description Extracts the unscaled physical anomaly archetypes as a combined data.frame.
#' @param object A DiagnosticEngine object.
#' @export
setGeneric("get_medoids", function(object) standardGeneric("get_medoids"))

#' @title Ingest Time Course Data
#' @description Ingests raw multivariate time series and applies dynamic feature engineering 
#' as defined by the engine's YAML schema.
#' @param object A DiagnosticEngine object.
#' @param data A data.frame containing the raw time course.
#' @return An updated DiagnosticEngine object with the feature_matrix populated.
#' @export
setGeneric("ingest_time_course", function(object, data) standardGeneric("ingest_time_course"))

#' @title Extract Anomalies via Isolation Forest
#' @description Scores the feature matrix and extracts extreme events exceeding the specified threshold.
#' @param object A DiagnosticEngine object.
#' @param threshold Numeric limit for empirical p-values (e.g., 0.60).
#' @return An updated DiagnosticEngine object with the anomalies slot populated.
#' @export
setGeneric("extract_anomalies", function(object, threshold) standardGeneric("extract_anomalies"))

#' @title Compute Distance Matrix
#' @description Calculates the N x N topological Hamming distance matrix of terminal nodes.
#' @param object A DiagnosticEngine object.
#' @return An updated DiagnosticEngine object with distance_matrix and hierarchical_tree populated.
#' @export
setGeneric("compute_distances", function(object) standardGeneric("compute_distances"))

#' @title Calibrate Cluster Integrity
#' @description Iterates through k-values and calculates global silhouette to locate the structural fracture point.
#' @param object A DiagnosticEngine object.
#' @param min_k Integer. Minimum number of clusters to test. Default is 2.
#' @param max_k Integer. Maximum number of clusters to test. Default is 8.
#' @return An updated DiagnosticEngine object with k_metrics populated.
#' @export
setGeneric("calibrate", function(object, min_k = 2, max_k = 8) standardGeneric("calibrate"))

#' @title Extract Topological Medoids
#' @description Computes the topological center of each cluster and extracts physical time courses.
#' @param object A DiagnosticEngine object.
#' @param k Integer. The target number of anomaly clusters.
#' @return An updated DiagnosticEngine object with the medoids and baseline_signatures slots populated.
#' @export
setGeneric("extract_medoids", function(object, k) standardGeneric("extract_medoids"))

#' @title Commit Forensic Ledger
#' @description Locks in the k-value and ranks all anomalies by their topological distance to the medoid.
#' @param object A DiagnosticEngine object.
#' @param k Integer. The finalized number of distinct states.
#' @return An updated DiagnosticEngine object ready for historical export.
#' @export
setGeneric("commit_ledger", function(object, k) standardGeneric("commit_ledger"))

#' @title Predict Streaming Data
#' @description Routes new multivariate time course data into established historical medoids 
#' or flags them as novel anomalies based on terminal node proximity.
#' @param object A DiagnosticEngine object (must be fully calibrated with medoids extracted).
#' @param new_data A data.frame of new observations.
#' @return A data.frame containing the anomaly scores, assigned clusters, and proximity metrics.
#' @export
setGeneric("predict_stream", function(object, new_data) standardGeneric("predict_stream"))

# --- INITIALIZATION METHOD ---

#' @export
setMethod("initialize", "DiagnosticEngine", function(.Object, config_path, ...) {
  
  if (!missing(config_path)) {
    if (!file.exists(config_path)) {
      stop(sprintf("Fatal Error: Configuration file not found at '%s'", config_path))
    }
    
    raw_yaml <- yaml::read_yaml(config_path)
    .Object@config_path <- config_path
    
    # Defensive Check: Ensure targets exist
    if (is.null(raw_yaml$target_variables) || length(raw_yaml$target_variables) == 0) {
      stop("Fatal Error: YAML configuration must contain at least one variable in 'target_variables'.")
    }
    
    .Object@feature_schema <- list(
      target_variables = raw_yaml$target_variables,
      context_variables = raw_yaml$context_variables # Can be NULL, that's safe
    )
    
    # Parse ML Hyperparameters with safe defaults if missing from YAML
    .Object@hyperparameters <- list(
      anomaly_threshold = if (!is.null(raw_yaml$anomaly_threshold)) raw_yaml$anomaly_threshold else 0.60,
      temporal_window_hours = if (!is.null(raw_yaml$temporal_window_hours)) raw_yaml$temporal_window_hours else 2.0,
      sample_size = if (!is.null(raw_yaml$sample_size)) raw_yaml$sample_size else 256,
      ntrees = if (!is.null(raw_yaml$ntrees)) raw_yaml$ntrees else 500
    )
    
    .Object@domain_state <- "initialized"
    message(sprintf("DiagnosticEngine initialized. Schema loaded from: %s", basename(config_path)))
  }
  
  .Object <- callNextMethod(.Object, ...)
  return(.Object)
})

# --- ACCESSORS ---

# --- ACCESSORS ---

#' @export
setMethod("get_hyperparameters", "DiagnosticEngine", function(object) {
  return(object@hyperparameters)
})

#' @export
setMethod("get_features", "DiagnosticEngine", function(object) {
  return(object@feature_schema)
})

#' @export
setMethod("get_ledger", "DiagnosticEngine", function(object) {
  if (nrow(object@forensic_ledger) == 0) {
    warning("Forensic ledger is empty. Ensure commit_ledger() has been executed.")
  }
  return(object@forensic_ledger)
})

#' @export
setMethod("get_medoids", "DiagnosticEngine", function(object) {
  if (length(object@medoids) == 0) {
    warning("Medoids list is empty. Ensure extract_medoids() has been executed.")
  }
  # Automatically bind the list into a flat dataframe for immediate tidyverse compatibility
  return(dplyr::bind_rows(object@medoids))
})

# --- CORE METHODS ---

#' @export
setMethod("ingest_time_course", "DiagnosticEngine", function(object, data) {
  if (object@domain_state == "uninitialized") {
    stop("Fatal Error: Engine must be initialized with a YAML schema prior to ingestion.")
  }
  
  # Updated slot name
  object@raw_time_course <- data
  targets <- object@feature_schema$target_variables
  
  if (is.null(targets)) {
    stop("YAML schema missing 'target_variables' array.")
  }
  
  if (!all(targets %in% colnames(data))) {
    missing_cols <- setdiff(targets, colnames(data))
    stop(sprintf("Data missing schema targets: %s", paste(missing_cols, collapse = ", ")))
  }
  
  object@feature_matrix <- data[, targets, drop = FALSE]
  object@domain_state <- "time_course_ingested"
  
  message(sprintf("Time course ingested successfully. Feature matrix locked at %d rows by %d features.", 
                  nrow(object@feature_matrix), ncol(object@feature_matrix)))
  
  return(object)
})

#' @export
setMethod("extract_anomalies", "DiagnosticEngine", function(object, threshold) {
  # 1. Update state checks to match the new ingestion terminology
  if (!object@domain_state %in% c("time_course_ingested", "calibrated")) {
    stop("Fatal Error: Time course data must be ingested before extracting anomalies.")
  }
  
  # 2. Safely rely on the hardened initialize method for defaults
  if (missing(threshold)) {
    threshold <- object@hyperparameters$anomaly_threshold
  }
  
  sample_size <- object@hyperparameters$sample_size
  ntrees <- object@hyperparameters$ntrees
  
  message(sprintf("Training Isolation Forest (ntrees=%d, sample_size=%d) and scoring feature space...", 
                  ntrees, sample_size))
  
  # Enforce determinism for forensic consistency
  set.seed(42)
  
  actual_sample_size <- min(sample_size, nrow(object@feature_matrix))
  
  iso_model <- suppressWarnings(isotree::isolation.forest(
    object@feature_matrix, 
    ntrees = ntrees, 
    sample_size = actual_sample_size, 
    missing_action = "fail"
  ))
  
  scores <- stats::predict(iso_model, object@feature_matrix)
  
  # 3. Update slot reference to raw_time_course
  scored_data <- object@raw_time_course
  scored_data$anomaly_score <- scores
  
  # 4. Change strictly greater (>) to greater-or-equal (>=) to match earlier threshold sweeps
  object@anomalies <- scored_data[scored_data$anomaly_score >= threshold, ]
  
  # Cache the model for downstream topology extraction and predictive routing
  object@model <- iso_model
  
  object@domain_state <- "anomalies_extracted"
  message(sprintf("Anomaly extraction complete. %d extreme events isolated (Score >= %s).", 
                  nrow(object@anomalies), threshold))
  
  return(object)
})

#' @export
setMethod("compute_distances", "DiagnosticEngine", function(object) {
  if (!object@domain_state %in% c("anomalies_extracted", "calibrated")) {
    stop("Fatal Error: Anomalies must be extracted before computing distances.")
  }
  
  if (is.null(object@model)) {
    stop("Fatal Error: Isolation Forest model not found in the DiagnosticEngine object.")
  }
  
  targets <- object@feature_schema$target_variables
  n_events <- nrow(object@anomalies)
  
  message(sprintf("Extracting terminal node topology for %d isolated events...", n_events))
  
  # Updated to reflect the new domain-agnostic time course slot
  anomaly_data <- object@raw_time_course[
    object@raw_time_course$timestamp %in% object@anomalies$timestamp, 
    targets, 
    drop = FALSE
  ]
  
  node_matrix <- stats::predict(object@model, anomaly_data, type = "tree_num")
  
  message("Computing topological Hamming distance proximity matrix...")
  
  n_trees <- ncol(node_matrix)
  raw_hamming <- e1071::hamming.distance(node_matrix)
  hamming_dist_matrix <- raw_hamming / n_trees
  
  object@distance_matrix <- as.matrix(hamming_dist_matrix)
  
  message("Constructing DIANA hierarchical tree from forest topology...")
  object@hierarchical_tree <- as.hclust(cluster::diana(stats::as.dist(hamming_dist_matrix)))
  
  object@domain_state <- "distances_computed"
  message("Distance computation complete.")
  
  return(object)
})

#' @export
setMethod("calibrate", "DiagnosticEngine", function(object, min_k = 2, max_k = 8) {
  if (!object@domain_state %in% c("distances_computed", "calibrated")) {
    stop("Fatal Error: Distances must be computed before calibrating.")
  }
  
  message(sprintf("Calibrating cluster integrity from k=%d to k=%d using Global Silhouette...", min_k, max_k))
  
  dist_mat <- object@distance_matrix
  hc_tree <- object@hierarchical_tree
  
  # Initialize results dataframe with only standard, peer-reviewable metrics
  results <- data.frame(k = min_k:max_k, global_silhouette = NA_real_)
  
  for (i in seq_along(results$k)) {
    curr_k <- results$k[i]
    
    # Cannot cluster into more groups than we have observations
    if (curr_k >= nrow(dist_mat)) next
    
    lbls <- stats::cutree(hc_tree, k = curr_k)
    
    # Calculate standard Global Silhouette using the Breiman topological matrix
    global_sil <- suppressWarnings(cluster::silhouette(lbls, dmatrix = dist_mat))
    
    if (is.matrix(global_sil) && "sil_width" %in% colnames(global_sil)) {
      results$global_silhouette[i] <- mean(global_sil[, "sil_width"], na.rm = TRUE)
    } else {
      results$global_silhouette[i] <- NA_real_
    }
  }
  
  object@k_metrics <- results
  object@domain_state <- "calibrated"
  
  # Determine optimal k based strictly on maximizing the Global Silhouette
  valid_idx <- which(!is.na(results$global_silhouette))
  if (length(valid_idx) > 0) {
    best_k <- results$k[valid_idx[which.max(results$global_silhouette[valid_idx])]]
    message(sprintf("Calibration complete. Mathematical recommendation based on maximum Global Silhouette: k=%d", best_k))
  } else {
    message("Calibration complete. No valid silhouette metrics generated (clusters too small).")
  }
  
  return(object)
})

#' @export
setMethod("extract_medoids", "DiagnosticEngine", function(object, k) {
  # Updated state checks to reflect the DS terminology
  if (!object@domain_state %in% c("distances_computed", "calibrated", "ledger_committed")) {
    stop("Fatal Error: Must compute distances before extracting medoids.")
  }
  
  message(sprintf("Extracting topological medoids for k = %d...", k))
  
  cluster_assignments <- stats::cutree(object@hierarchical_tree, k = k)
  
  medoid_list <- list()
  medoid_indices <- integer(k)
  
  # Safe to assume window_hours exists due to hardened initialize()
  window_hours <- object@hyperparameters$temporal_window_hours
  
  for (c_id in 1:k) {
    idx_in_cluster <- which(cluster_assignments == c_id)
    
    if (length(idx_in_cluster) == 1) {
      medoid_idx <- idx_in_cluster
    } else {
      sub_matrix <- object@distance_matrix[idx_in_cluster, idx_in_cluster]
      medoid_relative_idx <- which.min(rowSums(sub_matrix))
      medoid_idx <- idx_in_cluster[medoid_relative_idx]
    }
    
    medoid_indices[c_id] <- medoid_idx
    medoid_timestamp <- object@anomalies$timestamp[medoid_idx]
    
    # Extract unscaled physical time course window 
    start_time <- medoid_timestamp - lubridate::hours(window_hours)
    end_time <- medoid_timestamp + lubridate::hours(window_hours)
    
    medoid_data <- object@raw_time_course[
      object@raw_time_course$timestamp >= start_time & 
        object@raw_time_course$timestamp <= end_time, 
    ]
    
    medoid_data$cluster <- paste("Cluster", c_id)
    medoid_data$medoid_timestamp <- medoid_timestamp
    
    medoid_list[[c_id]] <- medoid_data
  }
  
  object@medoids <- medoid_list
  
  # --- NEW: Cache baseline terminal node signatures for predict_stream ---
  message("Caching terminal node signatures for predictive routing...")
  targets <- object@feature_schema$target_variables
  
  # Iteratively bind the target features for medoids to ensure exact 1:k row order
  medoid_feature_data <- data.frame()
  for (idx in medoid_indices) {
    target_ts <- object@anomalies$timestamp[idx]
    row_data <- object@raw_time_course[object@raw_time_course$timestamp == target_ts, targets, drop = FALSE]
    medoid_feature_data <- rbind(medoid_feature_data, row_data)
  }
  
  baseline_sigs <- stats::predict(object@model, medoid_feature_data, type = "tree_num")
  rownames(baseline_sigs) <- paste("Cluster", 1:k)
  object@baseline_signatures <- as.matrix(baseline_sigs)
  
  message("Medoid extraction complete. Archetypes saved to @medoids and @baseline_signatures.")
  
  return(object)
})

#' @export
setMethod("commit_ledger", "DiagnosticEngine", function(object, k) {
  if (!object@domain_state %in% c("calibrated", "medoids_extracted", "ledger_committed")) {
    stop("Fatal Error: Engine must be calibrated before committing the forensic ledger.")
  }
  
  if (missing(k) || !is.numeric(k) || k %% 1 != 0) {
    stop("Please provide a valid integer for k (e.g., k = 4).")
  }
  
  if (!k %in% object@k_metrics$k) {
    stop(sprintf("k=%d was not evaluated during calibration. Evaluated k values: %s", 
                 k, paste(object@k_metrics$k, collapse = ", ")))
  }
  
  message(sprintf("Committing ranked forensic ledger for k=%d anomaly archetypes...", k))
  
  final_labels <- stats::cutree(object@hierarchical_tree, k = k)
  
  ledger_df <- object@anomalies
  ledger_df$anomaly_cluster <- paste0("Cluster_", final_labels)
  ledger_df$topological_distance <- NA_real_
  ledger_df$cluster_rank <- NA_integer_
  
  # Calculate topological distance to medoid and rank for each cluster
  for (c_id in 1:k) {
    idx_in_cluster <- which(final_labels == c_id)
    
    if (length(idx_in_cluster) == 1) {
      ledger_df$topological_distance[idx_in_cluster] <- 0
      ledger_df$cluster_rank[idx_in_cluster] <- 1
    } else {
      # Pull the sub-matrix for this specific cluster
      sub_matrix <- object@distance_matrix[idx_in_cluster, idx_in_cluster]
      
      # Identify the medoid (minimum sum of distances)
      medoid_relative_idx <- which.min(rowSums(sub_matrix))
      
      # Extract the distance from every point in the cluster to the medoid
      dist_to_medoid <- sub_matrix[, medoid_relative_idx]
      ledger_df$topological_distance[idx_in_cluster] <- dist_to_medoid
      
      # Rank them (1 = the exact medoid, ascending outwards)
      ledger_df$cluster_rank[idx_in_cluster] <- rank(dist_to_medoid, ties.method = "min")
    }
  }
  
  # Reorder dataframe: primary sort by Cluster, secondary sort by Rank
  ledger_df <- ledger_df[order(ledger_df$anomaly_cluster, ledger_df$cluster_rank), ]
  
  # Move routing columns to the front for readability
  routing_cols <- c("anomaly_cluster", "cluster_rank", "topological_distance")
  cols <- c(routing_cols, setdiff(names(ledger_df), routing_cols))
  ledger_df <- ledger_df[, cols]
  
  object@active_k <- k
  object@forensic_ledger <- ledger_df
  object@domain_state <- "ledger_committed"
  
  message("Ledger successfully committed and ranked. Call `get_ledger(engine)` to export.")
  
  return(object)
})

#' @export
setMethod("predict_stream", "DiagnosticEngine", function(object, new_data) {
  # Ensure the engine is fully calibrated and medoids are locked
  if (nrow(object@baseline_signatures) == 0) {
    stop("Fatal Error: Engine must have extracted medoids (@baseline_signatures) to route new data.")
  }
  
  message(sprintf("Ingesting %d new streaming observations...", nrow(new_data)))
  
  targets <- object@feature_schema$target_variables
  if (!all(targets %in% colnames(new_data))) {
    missing_cols <- setdiff(targets, colnames(new_data))
    stop(sprintf("Fatal Error: Streaming data is missing target variables: %s", 
                 paste(missing_cols, collapse = ", ")))
  }
  
  # Step 1: Score the new data
  feature_data <- new_data[, targets, drop = FALSE]
  scores <- stats::predict(object@model, feature_data)
  
  # Initialize routing columns in the output dataframe
  results_df <- new_data
  results_df$anomaly_score <- scores
  results_df$predicted_cluster <- NA_character_
  results_df$distance_to_medoid <- NA_real_
  
  # Step 2: Isolate new anomalies based on the frozen configuration threshold
  threshold <- object@hyperparameters$anomaly_threshold
  idx_anomalies <- which(scores >= threshold)
  
  if (length(idx_anomalies) == 0) {
    message("No anomalies detected in the streaming batch. All systems normal.")
    return(results_df)
  }
  
  message(sprintf("%d new anomalies detected. Routing via topological signatures...", length(idx_anomalies)))
  
  # Step 3: Extract terminal node topologies for the new anomalies
  anomaly_features <- feature_data[idx_anomalies, , drop = FALSE]
  new_signatures <- stats::predict(object@model, anomaly_features, type = "tree_num")
  
  baseline_sigs <- object@baseline_signatures
  n_trees <- ncol(new_signatures)
  
  predicted_clusters <- character(length(idx_anomalies))
  distances <- numeric(length(idx_anomalies))
  
  # Step 4: Calculate Hamming distance against frozen medoids and route
  for (i in seq_along(idx_anomalies)) {
    # Calculate the proportional leaf-node disagreement between this new event and all medoids
    dist_vec <- apply(baseline_sigs, 1, function(baseline_row) {
      sum(new_signatures[i, ] != baseline_row) / n_trees
    })
    
    # Identify the closest topological medoid
    best_match_idx <- which.min(dist_vec)
    distances[i] <- dist_vec[best_match_idx]
    predicted_clusters[i] <- rownames(baseline_sigs)[best_match_idx]
  }
  
  results_df$predicted_cluster[idx_anomalies] <- predicted_clusters
  results_df$distance_to_medoid[idx_anomalies] <- distances
  
  message("Routing complete. Return dataframe contains anomaly scores and predicted physical states.")
  
  return(results_df)
})