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
#' @slot optimal_k numeric. The optimal number k of cluster (global silouhette).
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
    optimal_k = "integer",
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
#' as defined by the engine's YAML schema. Includes optional temporal continuity enforcement.
#' @param object A DiagnosticEngine object.
#' @param data A data.frame containing the raw time course.
#' @param tolerance Numeric. Allowable variance in seconds between timestamps. If NULL, enforcement is skipped.
#' @return An updated DiagnosticEngine object with the feature_matrix populated.
#' @export
setGeneric("ingest_time_course", function(object, data, tolerance = NULL) standardGeneric("ingest_time_course"))

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

#' Extract Topological Medoids
#' 
#' @param object A \code{DiagnosticEngine} object.
#' @param k Integer. The number of archetypal clusters to extract.
#' @param threshold Numeric. Optional. The anomaly score threshold to define the statistical substrate.
#' @param N Integer. Optional. The target number of anomalies to isolate.
#' @export
setGeneric("extract_medoids", function(object, threshold = NULL, N = NULL, k = NULL) {
  standardGeneric("extract_medoids")
})

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

#' @title Get Ingested Time Course
#' @description Returns the schema-validated raw time course data.
#' @param object A DiagnosticEngine object.
#' @export
setGeneric("get_time_course", function(object) standardGeneric("get_time_course"))

#' @title Explore Anomaly Thresholds
#' @description Rapidly scores the feature space and returns a mapping of 
#' anomaly thresholds to the number of extracted events.
#' @param object A DiagnosticEngine object.
#' @param thresholds Numeric vector of thresholds to test.
#' @param ntrees Integer. Overrides YAML if provided.
#' @param sample_size Integer. Overrides YAML if provided.
#' @export
setGeneric("explore_thresholds", function(object, thresholds = seq(0.50, 0.80, by = 0.01), ntrees = NULL, sample_size = NULL) standardGeneric("explore_thresholds"))

#' Evaluate Topological Stability of Clusters Across Anomaly Thresholds
#'
#' Sweeps a range of anomaly thresholds defined by sample size bounds (`max_N` and `min_N`) 
#' to evaluate the structural stability of the clustering manifold. To optimize performance, 
#' a master distance matrix is computed once at `max_N` and instantaneously subsetted 
#' for higher thresholds.
#'
#' @param object A \code{DiagnosticEngine} object.
#' @param max_N Integer. The maximum number of anomalies to evaluate (defines the lower threshold bound).
#' @param min_N Integer. The minimum number of anomalies to evaluate (defines the upper threshold bound).
#' @param nb_steps Integer. The number of discrete threshold steps to evaluate between the bounds. Default is 50.
#' @param min_k Integer. The minimum number of clusters to evaluate during DIANA partitioning. Default is 2.
#' @param max_k Integer. The maximum number of clusters to evaluate during DIANA partitioning. Default is 10.
#'
#' @return A \code{data.frame} containing the stability profile with columns: \code{Threshold}, 
#' \code{N_Anomalies}, \code{Opt_k}, \code{Max_Silhouette}, and \code{Hartigans_D}.
#' @export
setGeneric("evaluate_k_stability", function(object, max_N, min_N, nb_steps = 50, min_k = 2, max_k = 10) {
  standardGeneric("evaluate_k_stability")
})

#' Retrieve Anomaly Score Threshold for a Target Sample Size
#'
#' Identifies the exact anomaly score threshold required to isolate the top \code{n} 
#' most anomalous observations. If anomaly scores have not yet been computed in the 
#' \code{DiagnosticEngine}, a temporary model is trained using the initialized hyperparameters.
#'
#' @param object A \code{DiagnosticEngine} object.
#' @param n Integer. The target number of top anomalies to isolate.
#'
#' @return Numeric. The anomaly score threshold corresponding to the target sample size.
#' @export
setGeneric("get_threshold_at_N", function(object, n) {
  standardGeneric("get_threshold_at_N")
})


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
      target_variables = as.character(unlist(raw_yaml$target_variables)),
      context_variables = as.character(unlist(raw_yaml$context_variables))
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
  .Object@optimal_k <- NA_integer_
  return(.Object)
})


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

#' @export
setMethod("get_time_course", "DiagnosticEngine", function(object) {
  if (nrow(object@raw_time_course) == 0) {
    warning("Time course is empty. Ensure ingest_time_course() has been executed.")
  }
  return(object@raw_time_course)
})

# --- CORE METHODS ---

#' @export
setMethod("ingest_time_course", "DiagnosticEngine", function(object, data, tolerance = NULL) {
  if (object@domain_state == "uninitialized") {
    stop("Fatal Error: Engine must be initialized with a YAML schema prior to ingestion.")
  }
  
  if (nrow(data) < 3) {
    stop("Fatal Error: Ingested data must contain at least 3 observations to compute temporal continuity and baseline statistics.")
  }
  
  # 1. Enforce timestamp existence
  if (!"timestamp" %in% colnames(data)) {
    stop("Fatal Error: Ingested data must contain a column named 'timestamp'.")
  }
  
  # 2. Enforce datetime class for downstream lubridate compatibility
  if (!inherits(data$timestamp, "POSIXt")) {
    stop("Fatal Error: The 'timestamp' column must be a POSIXct/POSIXlt datetime object.")
  }
  
  if (!is.null(tolerance)) {
    continuity_check <- check_time_continuity(data$timestamp, tolerance = tolerance)
    if (!continuity_check$is_continuous) {
      stop(sprintf("Fatal Error: Ingested data contains time discontinuities exceeding the %d-second tolerance.", tolerance))
    }
  }
  
  # 3. Schema mapping and column culling
  targets <- object@feature_schema$target_variables
  contexts <- object@feature_schema$context_variables
  
  if (is.null(targets)) stop("YAML schema missing 'target_variables'.")
  
  if (!all(targets %in% colnames(data))) {
    missing_cols <- setdiff(targets, colnames(data))
    stop(sprintf("Data missing schema targets: %s", paste(missing_cols, collapse = ", ")))
  }
  
  schema_cols <- unique(c("timestamp", targets, contexts))
  keep_cols <- intersect(schema_cols, colnames(data))
  
  object@raw_time_course <- data[, keep_cols, drop = FALSE]
  object@feature_matrix <- data[, targets, drop = FALSE]
  object@domain_state <- "time_course_ingested"
  
  message(sprintf("Time course ingested successfully. Data locked at %d rows by %d schema variables.", 
                  nrow(object@raw_time_course), ncol(object@raw_time_course)))
  
  return(object)
})

#' @export
setMethod("extract_anomalies", "DiagnosticEngine", function(object, threshold) {
  # Allow re-extraction at any point after initial ingestion
  valid_states <- c("time_course_ingested", "anomalies_extracted", "distances_computed", "calibrated", "ledger_committed")
  
  if (!object@domain_state %in% valid_states) {
    stop("Fatal Error: Time course data must be ingested before extracting anomalies.")
  }
  
  # --- THE OVERRIDE LOGIC ---
  if (missing(threshold)) {
    threshold <- object@hyperparameters$anomaly_threshold
  } else {
    # If the user provides a threshold at runtime, update the slot so the forensic record is accurate
    object@hyperparameters$anomaly_threshold <- threshold
    message(sprintf("YAML threshold overridden. Using runtime threshold: %s", threshold))
  }
  
  sample_size <- object@hyperparameters$sample_size
  ntrees <- object@hyperparameters$ntrees
  
  message(sprintf("Training Isolation Forest (ntrees=%d, sample_size=%d) and scoring feature space...", 
                  ntrees, sample_size))
  
  set.seed(42)
  actual_sample_size <- min(sample_size, nrow(object@feature_matrix))
  
  iso_model <- suppressWarnings(isotree::isolation.forest(
    object@feature_matrix, 
    ntrees = ntrees, 
    sample_size = actual_sample_size, 
    missing_action = "fail"
  ))
  
  scores <- stats::predict(iso_model, object@feature_matrix)
  
  scored_data <- object@raw_time_course
  scored_data$anomaly_score <- scores
  
  object@anomalies <- scored_data[scored_data$anomaly_score >= threshold, ]
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
    object@optimal_k <- as.integer(best_k)
    message(sprintf("Calibration complete. Mathematical recommendation based on maximum Global Silhouette: k=%d", best_k))
  } else {
    message("Calibration complete. No valid silhouette metrics generated (clusters too small).")
  }
  
  return(object)
})


#' Extract Topological Medoids for DiagnosticEngine
#'
#' Extracts archetypal time-course medoids from the anomaly topology. If a 
#' \code{threshold} or \code{N} is provided, the method automatically extracts 
#' the corresponding statistical substrate, computes distance matrices, and 
#' calibrates the optimal number of clusters.
#'
#' @param object A \code{DiagnosticEngine} object.
#' @param threshold Numeric. Optional. The anomaly score threshold used to define the subset of extreme events.
#' @param N Integer. Optional. The target number of top anomalies to isolate (mapped to a threshold internally).
#' @param k Integer. Optional. Overrides the calibrated optimal $k$ to force a specific number of clusters.
#'
#' @return A \code{DiagnosticEngine} object with populated \code{@medoids} and \code{@baseline_signatures} slots.
#' @rdname extract_medoids
#' @export
setGeneric("extract_medoids", function(object, threshold = NULL, N = NULL, k = NULL) {
  standardGeneric("extract_medoids")
})

#' @rdname extract_medoids
#' @export
setMethod("extract_medoids", "DiagnosticEngine", function(object, threshold = NULL, N = NULL, k = NULL) {
  
  # 1. Handle dynamic subsetting
  if (!is.null(N)) {
    threshold <- get_threshold_at_N(object, N)
    message(sprintf("Mapped N = %d to anomaly threshold = %.4f", N, threshold))
  }
  
  if (!is.null(threshold)) {
    message(sprintf("Slicing feature space at threshold = %.4f...", threshold))
    object <- extract_anomalies(object, threshold = threshold)
    object <- compute_distances(object)
    # Always calibrate to ensure the hierarchical_tree is built for cutree
    object <- calibrate(object)
  } else {
    if (!object@domain_state %in% c("distances_computed", "calibrated", "ledger_committed")) {
      stop("Fatal Error: Must provide 'threshold' or 'N', OR manually compute distances before extracting medoids.")
    }
  }
  
  # 2. Resolve target k
  if (is.null(k)) {
    if (!"calibrated" %in% object@domain_state) {
      object <- calibrate(object)
    }
    # Safe check for the optimal_k slot
    if (.hasSlot(object, "optimal_k")) {
      k <- object@optimal_k 
    } else {
      stop("k is NULL, but no '@optimal_k' slot exists in the object. Please explicitly provide k.")
    }
  }
  
  message(sprintf("Extracting topological medoids for k = %d...", k))
  
  cluster_assignments <- stats::cutree(object@hierarchical_tree, k = k)
  
  medoid_list <- list()
  medoid_indices <- integer(k)
  
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
  
  # Cache baseline terminal node signatures for predict_stream 
  targets <- object@feature_schema$target_variables
  
  medoid_feature_data <- data.frame()
  for (idx in medoid_indices) {
    target_ts <- object@anomalies$timestamp[idx]
    row_data <- object@raw_time_course[object@raw_time_course$timestamp == target_ts, targets, drop = FALSE]
    medoid_feature_data <- rbind(medoid_feature_data, row_data)
  }
  
  baseline_sigs <- stats::predict(object@model, medoid_feature_data, type = "tree_num")
  rownames(baseline_sigs) <- paste("Cluster", 1:k)
  object@baseline_signatures <- as.matrix(baseline_sigs)
  
  message("Medoid extraction complete. Archetypes saved to @medoids.")
  
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
setMethod("explore_thresholds", "DiagnosticEngine", function(object, thresholds, ntrees, sample_size) {
  if (!object@domain_state %in% c("time_course_ingested", "anomalies_extracted", "distances_computed", "calibrated", "ledger_committed")) {
    stop("Fatal Error: Time course must be ingested before exploring thresholds.")
  }
  
  hyps <- object@hyperparameters
  if (is.null(ntrees)) ntrees <- hyps$ntrees
  if (is.null(sample_size)) sample_size <- hyps$sample_size
  
  message("Scoring feature space to build threshold curve...")
  set.seed(42)
  actual_size <- min(sample_size, nrow(object@feature_matrix))
  
  iso_model <- suppressWarnings(isotree::isolation.forest(
    object@feature_matrix, ntrees = ntrees, sample_size = actual_size, missing_action = "fail"
  ))
  scores <- stats::predict(iso_model, object@feature_matrix)
  
  counts <- purrr::map_int(thresholds, ~sum(scores >= .x))
  return(data.frame(Threshold = thresholds, Event_Count = counts))
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

#' @title Evaluate k-Cluster Stability Across Anomaly Thresholds
#' @description Sweeps across a dynamically determined range of anomaly thresholds (based on 
#' bounding sample sizes `max_N` and `min_N`) to compute topological and clustering stability metrics. 
#' Extracts the optimal k, maximum global silhouette, Hartigan's Dip statistic, and 
#' Local Intrinsic Dimensionality (LID) profiles for each threshold step.
#' @param object A populated `DiagnosticEngine` object (must have ingested a time course).
#' @param max_N Integer. The maximum number of anomalies to evaluate (sets the lower threshold bound).
#' @param min_N Integer. The minimum number of anomalies to evaluate (sets the upper threshold bound).
#' @param nb_steps Integer. The number of evenly spaced threshold steps to evaluate. Default is 50.
#' @param min_k Integer. The minimum number of clusters to test during calibration. Default is 2.
#' @param max_k Integer. The maximum number of clusters to test during calibration. Default is 10.
#' @return A data frame containing the sweep results: Threshold, N_Anomalies, Opt_k, Max_Silhouette, 
#' Hartigans_D, Global_LID, Min_Cluster_LID, and Max_Cluster_LID.
#' @export
setGeneric("evaluate_k_stability", function(object, max_N, min_N, nb_steps = 50, min_k = 2, max_k = 10) {
  standardGeneric("evaluate_k_stability")
})

#' @rdname evaluate_k_stability
#' @export
setMethod("evaluate_k_stability", "DiagnosticEngine", function(object, max_N, min_N, nb_steps = 50, min_k = 2, max_k = 10) {
  if (!object@domain_state %in% c("time_course_ingested", "anomalies_extracted", "distances_computed", "calibrated", "ledger_committed")) {
    stop("Fatal Error: Time course must be ingested before evaluating k-stability.")
  }
  
  hyps <- object@hyperparameters
  ntrees <- hyps$ntrees
  sample_size <- hyps$sample_size
  
  message("Training static Isolation Forest for stability sweep...")
  set.seed(42)
  actual_size <- min(sample_size, nrow(object@feature_matrix))
  
  iso_model <- suppressWarnings(isotree::isolation.forest(
    object@feature_matrix, ntrees = ntrees, sample_size = actual_size, missing_action = "fail"
  ))
  scores <- stats::predict(iso_model, object@feature_matrix)
  
  base_data <- object@raw_time_course
  base_data$anomaly_score <- scores
  
  # --- 1. DETERMINE THRESHOLD BOUNDS FROM N ---
  sorted_scores <- sort(scores, decreasing = TRUE)
  total_obs <- length(scores)
  
  actual_max_N <- min(max_N, total_obs)
  actual_min_N <- min(min_N, actual_max_N)
  
  if (actual_min_N <= max_k) {
    warning(sprintf("min_N (%d) is too small for max_k (%d). Adjusting min_N to %d.", actual_min_N, max_k, max_k + 1))
    actual_min_N <- max_k + 1
  }
  
  t_min <- sorted_scores[actual_max_N]
  t_max <- sorted_scores[actual_min_N]
  thresholds <- seq(t_min, t_max, length.out = nb_steps)
  
  # --- 2. EXECUTE STABLE SWEEP ---
  results_list <- list()
  
  message("Sweeping threshold interval...")
  for (th in thresholds) {
    idx <- which(scores >= th)
    
    # Must have more anomalies than max_k to cluster successfully
    if (length(idx) <= max_k) {
      results_list[[as.character(th)]] <- data.frame(
        Threshold = th, N_Anomalies = length(idx), Opt_k = NA_real_, 
        Max_Silhouette = NA_real_, Hartigans_D = NA_real_,
        Global_LID = NA_real_, Min_Cluster_LID = NA_real_, Max_Cluster_LID = NA_real_
      )
      next
    }
    
    # Create temporary engine state to utilize existing S4 methods
    tmp_engine <- object
    tmp_engine@anomalies <- base_data[idx, ]
    tmp_engine@model <- iso_model
    tmp_engine@domain_state <- "anomalies_extracted"
    
    # Suppress console flood during iterative calibration
    tmp_engine <- suppressMessages(compute_distances(tmp_engine))
    
    d_mat <- tmp_engine@distance_matrix
    
    # Retain square matrix coercion for the LID helper
    if (inherits(d_mat, "dist")) {
      d_mat_square <- as.matrix(d_mat)
    } else {
      d_mat_square <- d_mat 
    }
    
    # --- CALCULATE HARTIGAN'S DIP STATISTIC (D) ---
    dip_stat <- compute_dip_statistic(d_mat)
    
    tmp_engine <- suppressMessages(calibrate(tmp_engine, min_k = min_k, max_k = max_k))
    
    df_k <- tmp_engine@k_metrics
    valid_idx <- which(!is.na(df_k$global_silhouette))
    
    if (length(valid_idx) > 0) {
      best_idx <- valid_idx[which.max(df_k$global_silhouette[valid_idx])]
      opt_k <- df_k$k[best_idx]
      max_sil <- df_k$global_silhouette[best_idx]
    } else {
      opt_k <- NA_real_
      max_sil <- NA_real_
    }
    
    # --- CALCULATE LOCAL INTRINSIC DIMENSIONALITY (LID) ---
    if (!is.na(opt_k)) {
      lbls <- stats::cutree(tmp_engine@hierarchical_tree, k = opt_k)
      
      lid_res <- compute_lid_profile(d_mat_square, cluster_labels = lbls, k = 5)
      
      global_lid <- lid_res$Global_LID
      min_lid <- lid_res$Min_Cluster_LID
      max_lid <- lid_res$Max_Cluster_LID
    } else {
      global_lid <- NA_real_
      min_lid <- NA_real_
      max_lid <- NA_real_
    }
    
    results_list[[as.character(th)]] <- data.frame(
      Threshold = th, N_Anomalies = length(idx), Opt_k = opt_k, 
      Max_Silhouette = max_sil, Hartigans_D = dip_stat,
      Global_LID = global_lid, Min_Cluster_LID = min_lid, Max_Cluster_LID = max_lid
    )
  }
  
  res_df <- dplyr::bind_rows(results_list)
  
  return(res_df)
})

#' Retrieve Anomaly Score Threshold for a Target Sample Size
#'
#' Identifies the exact anomaly score threshold required to isolate the top \code{n} 
#' most anomalous observations. If anomaly scores have not yet been computed in the 
#' \code{DiagnosticEngine}, a temporary model is trained using the initialized hyperparameters.
#'
#' @param object A \code{DiagnosticEngine} object.
#' @param n Integer. The target number of top anomalies to isolate.
#'
#' @return Numeric. The anomaly score threshold corresponding to the target sample size. 
#' Note: Due to potential ties in Isolation Forest scores, filtering a dataset with 
#' \code{score >= threshold} will return \strong{at least} \code{n} elements.
#' @export
setGeneric("get_threshold_at_N", function(object, n) {
  standardGeneric("get_threshold_at_N")
})

#' @rdname get_threshold_at_N
#' @export
setMethod("get_threshold_at_N", signature("DiagnosticEngine"), function(object, n) {
  features <- object@feature_matrix
  n_rows <- nrow(features)
  
  if (n_rows == 0) {
    stop("Feature matrix is empty. Please ingest data first.")
  }
  
  if (n > n_rows) {
    warning(sprintf("Requested n (%d) exceeds dataset size (%d). Returning minimum anomaly score.", n, n_rows))
    n <- n_rows
  }
  
  hyps <- object@hyperparameters
  actual_size <- min(hyps$sample_size, n_rows)
  
  # Set seed to ensure consistency with stability sweep logic
  set.seed(42)
  temp_model <- suppressWarnings(
    isotree::isolation.forest(
      features, 
      ntrees = hyps$ntrees, 
      sample_size = actual_size, 
      missing_action = "fail"
    )
  )
  
  scores <- stats::predict(temp_model, features)
  sorted_scores <- sort(scores, decreasing = TRUE)
  
  return(sorted_scores[n])
})