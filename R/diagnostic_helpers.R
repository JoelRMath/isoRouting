#' @title Assess Isolation Forest Rank Stability
#' @description 
#' Conducts a grid search across tree counts and sample sizes to evaluate 
#' the Spearman rank correlation of anomaly scores against a massive baseline model.
#' Useful for pre-flight hyperparameter tuning.
#' 
#' @param data A data.frame containing the numeric features to score.
#' @param target_features Character vector of column names to include in the model.
#' @param tree_counts Numeric vector of tree counts to evaluate (e.g., c(10, 50, 100, 500)).
#' @param sample_sizes Numeric vector of sample sizes to evaluate (e.g., c(64, 128, 256)).
#' @param baseline_trees Integer. The number of trees for the ground-truth baseline model (default 2000).
#' @param seed Integer for reproducibility.
#' 
#' @return A tidy data.frame containing the Spearman correlation for each parameter combination.
#' @export
assess_forest_stability <- function(data, 
                                    target_features, 
                                    tree_counts = c(10, 50, 100, 200, 300, 400, 500), 
                                    sample_sizes = c(64, 128, 256, 512), 
                                    baseline_trees = 2000, 
                                    seed = 42) {
  
  # Ensure clean feature matrix
  if (!all(target_features %in% colnames(data))) {
    stop("Missing target features in the provided dataset.")
  }
  
  feature_matrix <- data[, target_features, drop = FALSE]
  feature_matrix <- stats::na.omit(feature_matrix)
  
  # 1. Establish the Baseline Ground-Truth Model
  message(sprintf("Fitting %d-tree baseline model for ground-truth rankings...", baseline_trees))
  set.seed(seed)
  baseline_model <- isotree::isolation.forest(
    feature_matrix, 
    ntrees = baseline_trees, 
    sample_size = min(max(sample_sizes), nrow(feature_matrix)), 
    missing_action = "fail"
  )
  baseline_scores <- stats::predict(baseline_model, feature_matrix)
  
  # 2. Grid Search Initialization
  results_list <- list()
  iteration <- 1
  total_fits <- length(tree_counts) * length(sample_sizes)
  
  # 3. Iterate through hyperparameter combinations
  for (s_size in sample_sizes) {
    for (n_trees in tree_counts) {
      
      message(sprintf("Testing %d/%d: ntrees = %d, sample_size = %d", 
                      iteration, total_fits, n_trees, s_size))
      
      set.seed(seed)
      test_model <- suppressWarnings(isotree::isolation.forest(
        feature_matrix, 
        ntrees = n_trees, 
        sample_size = min(s_size, nrow(feature_matrix)), 
        missing_action = "fail"
      ))
      
      test_scores <- stats::predict(test_model, feature_matrix)
      
      # Calculate Spearman rank correlation against the baseline
      spearman_rho <- stats::cor(test_scores, baseline_scores, method = "spearman")
      
      results_list[[iteration]] <- data.frame(
        ntrees = n_trees,
        sample_size = s_size,
        spearman_rho = spearman_rho
      )
      
      iteration <- iteration + 1
    }
  }
  
  message("Stability assessment complete.")
  return(do.call(rbind, results_list))
}


#' Detect gaps or irregular intervals in a time series
#' 
#' @param time_vector A POSIXct vector of timestamps.
#' @param tolerance Numeric. Allowable variance in seconds (default 0).
#' @return A list containing a boolean flag and a summary of intervals.
#' @export
check_time_continuity <- function(time_vector, tolerance = 0) {
  if (!inherits(time_vector, "POSIXt")) {
    stop("time_vector must be a datetime object (POSIXct/POSIXlt).")
  }
  
  # Calculate time differences in seconds
  time_diffs <- as.numeric(difftime(time_vector[-1], time_vector[-length(time_vector)], units = "secs"))
  
  # Determine the median interval (assumed to be the intended frequency)
  expected_diff <- median(time_diffs, na.rm = TRUE)
  
  # Identify discontinuities outside the tolerance
  gaps <- which(abs(time_diffs - expected_diff) > tolerance)
  
  is_continuous <- length(gaps) == 0
  
  list(
    is_continuous = is_continuous,
    expected_interval_secs = expected_diff,
    gap_indices = gaps,
    summary = table(time_diffs)
  )
}

#' Compute Local Intrinsic Dimensionality (LID) Profile
#'
#' @param dist_mat An N x N distance matrix (from isotree topology).
#' @param cluster_labels A numeric or character vector of cluster assignments.
#' @param k Integer. The number of nearest neighbors to evaluate (default: 5).
#' @param eps Numeric. Small constant to prevent log(0) for topological duplicates.
#' @return A list containing Global_LID, Min_Cluster_LID, and Max_Cluster_LID
#' @export
compute_lid_profile <- function(dist_mat, cluster_labels, k = 5, eps = 1e-8) {
  
  # Coerce 'dist' objects to full matrix if necessary
  if (inherits(dist_mat, "dist")) dist_mat <- as.matrix(dist_mat)
  
  N <- nrow(dist_mat)
  lids <- numeric(N)
  
  for (i in 1:N) {
    # Extract distances to all OTHER points
    d_all <- dist_mat[i, -i]
    
    # Sort to find the k-nearest neighbors
    d_sorted <- sort(d_all)
    r_i <- d_sorted[1:k]
    r_k <- r_i[k]
    
    if (r_k < eps) {
      # If the k-th neighbor is perfectly identical, the local dimension is 0
      lids[i] <- 0
    } else {
      # Clamp r_i to eps to avoid log(0) for closer identical neighbors
      r_i_clamped <- pmax(r_i, eps)
      ratio <- r_i_clamped / max(r_k, eps)
      
      # Classic LID Maximum Likelihood Estimator
      lid_val <- - ( (1/k) * sum(log(ratio)) )^(-1)
      lids[i] <- max(0, lid_val)
    }
  }
  
  # Aggregate by cluster
  cluster_means <- tapply(lids, cluster_labels, mean, na.rm = TRUE)
  
  return(list(
    Global_LID = mean(lids, na.rm = TRUE),
    Min_Cluster_LID = min(cluster_means, na.rm = TRUE),
    Max_Cluster_LID = max(cluster_means, na.rm = TRUE)
  ))
}

#' @title Simulate Control Quantiles for Local Intrinsic Dimensionality (LID)
#' @description Generates an expected baseline distribution for LID 
#' by uniformly sampling N observations from the complete time course. 
#' This preserves natural multivariate dependencies and physical constraints, 
#' providing a conservative control baseline against which anomaly breakdown 
#' (variable coupling) can be evaluated.
#' @param engine A populated `DiagnosticEngine` object.
#' @param N Integer. The subset size to sample.
#' @param iterations Integer. Number of Monte Carlo iterations. Default is 200.
#' @param k Integer. The number of nearest neighbors for LID estimation. Default is 5.
#' @return A data.frame containing N, Median, and quantiles (5%, 25%, 50%, 75%, 95%).
#' @export
simulate_lid_control <- function(engine, N, iterations = 200, k = 5) {
  features <- engine@feature_matrix
  total_rows <- nrow(features)
  
  if (N > total_rows) {
    stop("N cannot exceed total ingested observations.")
  }
  
  hyps <- engine@hyperparameters
  ntrees <- hyps$ntrees
  
  lids <- numeric(iterations)
  
  for (i in seq_len(iterations)) {
    # Conservative baseline: sample directly from the entire dataset
    sampled_idx <- sample(seq_len(total_rows), size = N, replace = FALSE)
    block_features <- features[sampled_idx, , drop = FALSE]
    
    # Train temporary model
    iso_model <- suppressWarnings(
      isotree::isolation.forest(block_features, ntrees = ntrees, sample_size = N, missing_action = "fail")
    )
    
    # --- EXACT PARITY WITH compute_distances() ---
    # 1. Extract terminal nodes
    node_matrix <- stats::predict(iso_model, block_features, type = "tree_num")
    
    # 2. Compute proportional Hamming distance
    n_trees_actual <- ncol(node_matrix)
    raw_hamming <- e1071::hamming.distance(node_matrix)
    d_mat <- as.matrix(raw_hamming / n_trees_actual)
    # ---------------------------------------------
    
    # Compute LID profile using existing helper
    lid_res <- compute_lid_profile(d_mat, cluster_labels = rep(1, N), k = k)
    lids[i] <- lid_res$Global_LID
  }
  
  quant <- stats::quantile(lids, probs = c(0.05, 0.25, 0.50, 0.75, 0.95), na.rm = TRUE)
  
  return(data.frame(
    N = N,
    Median = median(lids, na.rm = TRUE),
    Q05 = quant[1],
    Q25 = quant[2],
    Q50 = quant[3],
    Q75 = quant[4],
    Q95 = quant[5]
  ))
}

#' Simulate Control Quantiles for Hartigan's Dip Statistic
#'
#' Estimates the baseline distance heterogeneity (Hartigan's D) by uniformly 
#' sampling N observations from the complete time course. This preserves natural 
#' multivariate dependencies and physical constraints, providing a conservative 
#' control baseline against which anomaly clustering can be evaluated.
#'
#' @param object A \code{DiagnosticEngine} object.
#' @param N Integer. The subset size to sample.
#' @param iterations Integer. Number of Monte Carlo iterations (default is 200).
#'
#' @return A \code{data.frame} containing the sample size (\code{N}), 
#' \code{Median}, \code{Q05}, and \code{Q95} bounds of the simulated Dip statistics.
#' @export
simulate_dip_control <- function(object, N, iterations = 200) {
  features <- object@feature_matrix
  total_rows <- nrow(features)
  
  if (N > total_rows) stop("N cannot exceed total ingested observations.")
  
  dip_results <- numeric(iterations)
  
  for (i in seq_len(iterations)) {
    # Conservative baseline: sample directly from the real, bounded topology
    sample_idx <- sample(seq_len(total_rows), size = N, replace = FALSE)
    sampled_features <- features[sample_idx, , drop = FALSE]
    
    # Train temporary model
    temp_model <- suppressWarnings(
      isotree::isolation.forest(sampled_features, ntrees = object@hyperparameters$ntrees)
    )
    
    # --- EXACT PARITY WITH compute_distances() ---
    # 1. Extract terminal nodes
    node_matrix <- stats::predict(temp_model, sampled_features, type = "tree_num")
    
    # 2. Compute proportional Hamming distance
    n_trees <- ncol(node_matrix)
    raw_hamming <- e1071::hamming.distance(node_matrix)
    dist_matrix <- as.matrix(raw_hamming / n_trees)
    # ---------------------------------------------
    
    # Compute Hartigan's D 
    dip_results[i] <- compute_dip_statistic(dist_matrix)
  }
  
  data.frame(
    N = N,
    Median = median(dip_results, na.rm = TRUE),
    Q05 = quantile(dip_results, 0.05, na.rm = TRUE),
    Q95 = quantile(dip_results, 0.95, na.rm = TRUE)
  )
}

#' @title Compute Hartigan's Dip Statistic
#' @description Evaluates the pairwise distance distribution of a manifold to test for structural 
#' multimodality (clustering tendency). 
#' @param dist_mat An N x N distance matrix or a native R `dist` object.
#' @return A numeric value representing the dip statistic, or NA_real_ if the sample size is too small.
#' @export
compute_dip_statistic <- function(dist_mat) {
  # Optimize extraction: 'dist' objects are inherently flattened triangle vectors
  if (inherits(dist_mat, "dist")) {
    d_vec <- as.numeric(dist_mat)
  } else {
    d_vec <- dist_mat[upper.tri(dist_mat)]
  }
  
  if (length(d_vec) >= 4) {
    return(diptest::dip(d_vec))
  } else {
    return(NA_real_)
  }
}