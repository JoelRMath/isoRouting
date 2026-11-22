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
#' @examples
#' set.seed(42)
#' df <- data.frame(x1 = rnorm(100), x2 = rnorm(100))
#' res <- assess_forest_stability(
#'   data = df,
#'   target_features = c("x1", "x2"),
#'   tree_counts = c(10, 25),
#'   sample_sizes = c(32, 64),
#'   baseline_trees = 100,
#'   seed = 42
#' )
#' print(res)
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
  baseline_model <- suppressWarnings(isotree::isolation.forest(
    feature_matrix, 
    ntrees = baseline_trees, 
    sample_size = min(max(sample_sizes), nrow(feature_matrix)), 
    missing_action = "fail"
  ))
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
#' @examples
#' ts <- seq.POSIXt(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
#'                  by = "hour", length.out = 10)
#' check_time_continuity(ts)
#'
#' # Introduce a 5-hour gap
#' ts_gap <- ts[-c(5:8)]
#' check_time_continuity(ts_gap)
#' @export
check_time_continuity <- function(time_vector, tolerance = 0) {
  if (!inherits(time_vector, "POSIXt")) {
    stop("time_vector must be a datetime object (POSIXct/POSIXlt).")
  }
  
  # Calculate time differences in seconds
  time_diffs <- as.numeric(difftime(time_vector[-1], time_vector[-length(time_vector)], units = "secs"))
  
  # Determine the median interval (assumed to be the intended frequency)
  expected_diff <- stats::median(time_diffs, na.rm = TRUE)
  
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
#' @examples
#' set.seed(42)
#' pts <- rbind(matrix(rnorm(30, mean = 0), ncol = 2),
#'              matrix(rnorm(30, mean = 5), ncol = 2))
#' d_mat <- as.matrix(stats::dist(pts))
#' labels <- rep(c(1L, 2L), each = 15)
#' compute_lid_profile(d_mat, cluster_labels = labels, k = 5)
#' @export
compute_lid_profile <- function(dist_mat, cluster_labels, k = 5, eps = 1e-8) {
  if (inherits(dist_mat, "dist")) dist_mat <- as.matrix(dist_mat)
  
  N <- nrow(dist_mat)
  # Ensure diagonal is -Inf so sorting places self-distance at index 1
  diag(dist_mat) <- -Inf
  
  # Sort each column once; rows 2:(k+1) are the k nearest neighbors for each observation
  sorted_mat <- apply(dist_mat, 2, sort)
  knn_mat <- sorted_mat[2:(k + 1), , drop = FALSE]
  r_k <- knn_mat[k, ]
  
  lids <- numeric(N)
  valid <- r_k >= eps
  
  if (any(valid)) {
    knn_valid <- pmax(knn_mat[, valid, drop = FALSE], eps)
    rk_valid <- pmax(r_k[valid], eps)
    # Vectorized log-ratio sum across the k neighbors
    log_ratios <- log( sweep(knn_valid, 2, rk_valid, "/") )
    lid_vals <- -1 / colMeans(log_ratios)
    lids[valid] <- pmax(0, lid_vals)
  }
  
  cluster_means <- tapply(lids, cluster_labels, mean, na.rm = TRUE)
  
  return(list(
    Global_LID = mean(lids, na.rm = TRUE),
    Min_Cluster_LID = min(cluster_means, na.rm = TRUE),
    Max_Cluster_LID = max(cluster_means, na.rm = TRUE)
  ))
}

#' @title Simulate Control Quantiles for Local Intrinsic Dimensionality (LID)
#' @description Generates Monte Carlo null distribution quantiles for Local Intrinsic Dimensionality across sample sizes.
#' @param engine A \code{DiagnosticEngine} object with an ingested time course.
#' @param N Integer vector of sample sizes to evaluate.
#' @param iterations Integer. Number of Monte Carlo iterations (default 200).
#' @param k Integer. Number of nearest neighbors for LID estimation (default 5).
#' @return A \code{data.frame} containing simulated LID quantiles and score thresholds across \code{N}.
#' @examples
#' set.seed(42)
#' df <- data.frame(
#'   timestamp = seq.POSIXt(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
#'                          by = "hour", length.out = 100),
#'   x1 = rnorm(100),
#'   x2 = rnorm(100)
#' )
#' cfg <- tempfile(fileext = ".yaml")
#' yaml::write_yaml(list(
#'   ntrees = 20,
#'   sample_size = 32,
#'   temporal_window_hours = 2,
#'   anomaly_threshold = 0.60,
#'   context_variables = list(),
#'   target_variables = c("x1", "x2")
#' ), cfg)
#'
#' eng <- methods::new("DiagnosticEngine", config_path = cfg)
#' eng <- ingest_time_course(eng, df)
#' lid_ctrl <- simulate_lid_control(eng, N = c(10, 20), iterations = 10, k = 5)
#' print(lid_ctrl)
#' unlink(cfg)
#' @export
simulate_lid_control <- function(engine, N, iterations = 200, k = 5) {
  features <- engine@feature_matrix
  total_rows <- nrow(features)
  
  if (any(N > total_rows)) {
    stop("N cannot exceed total ingested observations.")
  }
  
  # 1. Reuse trained model if available; otherwise fit once
  iso_model <- engine@model
  if (is.null(iso_model)) {
    hyps <- engine@hyperparameters
    set.seed(42)
    iso_model <- suppressWarnings(
      isotree::isolation.forest(
        features,
        ntrees = hyps$ntrees,
        sample_size = min(hyps$sample_size, total_rows),
        missing_action = "fail"
      )
    )
  }
  
  # 2. Reuse cached scores for Threshold mapping if available; otherwise predict once
  if (nrow(engine@scored_time_course) == total_rows && "anomaly_score" %in% colnames(engine@scored_time_course)) {
    scores <- engine@scored_time_course$anomaly_score
  } else {
    scores <- stats::predict(iso_model, features)
  }
  sorted_scores <- sort(scores, decreasing = TRUE)
  
  # 3. Extract terminal leaf signatures ONCE across all N values
  t_all_nodes <- t(stats::predict(iso_model, features, type = "tree_num"))
  n_trees_actual <- nrow(t_all_nodes)
  
  N_vec <- as.integer(N)
  max_N <- max(N_vec)
  
  # Matrix to store MC results: rows = iterations, cols = length(N_vec)
  lid_mat <- matrix(NA_real_, nrow = iterations, ncol = length(N_vec))
  
  for (i in seq_len(iterations)) {
    # Sample max_N points once and compute the max_N x max_N Hamming matrix
    sampled_idx <- sample.int(total_rows, size = max_N, replace = FALSE)
    t_nodes <- t_all_nodes[, sampled_idx, drop = FALSE]
    
    master_d <- vapply(
      seq_len(max_N),
      function(j) colSums(t_nodes != t_nodes[, j]) / n_trees_actual,
      numeric(max_N)
    )
    
    # Subset the precomputed matrix for each requested n in N_vec
    for (idx_n in seq_along(N_vec)) {
      curr_n <- N_vec[idx_n]
      sub_d <- master_d[seq_len(curr_n), seq_len(curr_n), drop = FALSE]
      lid_Mat_res <- compute_lid_profile(sub_d, cluster_labels = rep(1L, curr_n), k = k)
      lid_mat[i, idx_n] <- lid_Mat_res$Global_LID
    }
  }
  
  # Summarize quantiles for each N
  res_list <- lapply(seq_along(N_vec), function(idx_n) {
    lids <- lid_mat[, idx_n]
    quant <- stats::quantile(lids, probs = c(0.05, 0.25, 0.50, 0.75, 0.95), na.rm = TRUE)
    data.frame(
      N = N_vec[idx_n],
      Median = stats::median(lids, na.rm = TRUE),
      Q05 = unname(quant[1]),
      Q25 = unname(quant[2]),
      Q50 = unname(quant[3]),
      Q75 = unname(quant[4]),
      Q95 = unname(quant[5]),
      Threshold = sorted_scores[N_vec[idx_n]]
    )
  })
  
  return(do.call(rbind, res_list))
}

#' @title Simulate Control Quantiles for Hartigan's Dip Statistic
#' @description Generates Monte Carlo null distribution quantiles for Hartigan's dip statistic across sample sizes.
#' @param object A \code{DiagnosticEngine} object with an ingested time course.
#' @param N Integer vector of sample sizes to evaluate.
#' @param iterations Integer. Number of Monte Carlo iterations (default 200).
#' @return A \code{data.frame} containing simulated Hartigan's dip quantiles and score thresholds across \code{N}.
#' @examples
#' set.seed(42)
#' df <- data.frame(
#'   timestamp = seq.POSIXt(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
#'                          by = "hour", length.out = 100),
#'   x1 = rnorm(100),
#'   x2 = rnorm(100)
#' )
#' cfg <- tempfile(fileext = ".yaml")
#' yaml::write_yaml(list(
#'   ntrees = 20,
#'   sample_size = 32,
#'   temporal_window_hours = 2,
#'   anomaly_threshold = 0.60,
#'   context_variables = list(),
#'   target_variables = c("x1", "x2")
#' ), cfg)
#'
#' eng <- methods::new("DiagnosticEngine", config_path = cfg)
#' eng <- ingest_time_course(eng, df)
#' dip_ctrl <- simulate_dip_control(eng, N = c(10, 20), iterations = 10)
#' print(dip_ctrl)
#' unlink(cfg)
#' @export
simulate_dip_control <- function(object, N, iterations = 200) {
  features <- object@feature_matrix
  total_rows <- nrow(features)
  
  if (any(N > total_rows)) stop("N cannot exceed total ingested observations.")
  
  # 1. Reuse trained model if available; otherwise fit once
  iso_model <- object@model
  if (is.null(iso_model)) {
    hyps <- object@hyperparameters
    set.seed(42)
    iso_model <- suppressWarnings(
      isotree::isolation.forest(
        features,
        ntrees = hyps$ntrees,
        sample_size = min(hyps$sample_size, total_rows),
        missing_action = "fail"
      )
    )
  }
  
  # 2. Reuse cached scores for Threshold mapping if available; otherwise predict once
  if (nrow(object@scored_time_course) == total_rows && "anomaly_score" %in% colnames(object@scored_time_course)) {
    scores <- object@scored_time_course$anomaly_score
  } else {
    scores <- stats::predict(iso_model, features)
  }
  sorted_scores <- sort(scores, decreasing = TRUE)
  
  # 3. Extract terminal leaf signatures ONCE across all N values
  t_all_nodes <- t(stats::predict(iso_model, features, type = "tree_num"))
  n_trees <- nrow(t_all_nodes)
  
  N_vec <- as.integer(N)
  max_N <- max(N_vec)
  
  dip_mat <- matrix(NA_real_, nrow = iterations, ncol = length(N_vec))
  
  for (i in seq_len(iterations)) {
    # Sample max_N points once and compute the max_N x max_N Hamming matrix
    sample_idx <- sample.int(total_rows, size = max_N, replace = FALSE)
    t_nodes <- t_all_nodes[, sample_idx, drop = FALSE]
    
    master_dist <- vapply(
      seq_len(max_N),
      function(j) colSums(t_nodes != t_nodes[, j]) / n_trees,
      numeric(max_N)
    )
    
    # Subset the precomputed matrix for each requested n in N_vec
    for (idx_n in seq_along(N_vec)) {
      curr_n <- N_vec[idx_n]
      sub_dist <- master_dist[seq_len(curr_n), seq_len(curr_n), drop = FALSE]
      dip_mat[i, idx_n] <- compute_dip_statistic(sub_dist)
    }
  }
  
  res_list <- lapply(seq_along(N_vec), function(idx_n) {
    dips <- dip_mat[, idx_n]
    data.frame(
      N = N_vec[idx_n],
      Median = stats::median(dips, na.rm = TRUE),
      Q05 = unname(stats::quantile(dips, 0.05, na.rm = TRUE)),
      Q95 = unname(stats::quantile(dips, 0.95, na.rm = TRUE)),
      Threshold = sorted_scores[N_vec[idx_n]]
    )
  })
  
  return(do.call(rbind, res_list))
}

#' @title Compute Hartigan's Dip Statistic
#' @description Evaluates the pairwise distance distribution of a manifold to test for structural 
#' multimodality (clustering tendency). 
#' @param dist_mat An N x N distance matrix or a native R `dist` object.
#' @return A numeric value representing the dip statistic, or NA_real_ if the sample size is too small.
#' @examples
#' set.seed(42)
#' # Bimodal synthetic clusters
#' pts <- rbind(matrix(rnorm(30, mean = 0), ncol = 2),
#'              matrix(rnorm(30, mean = 5), ncol = 2))
#' d_mat <- stats::dist(pts)
#' compute_dip_statistic(d_mat)
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