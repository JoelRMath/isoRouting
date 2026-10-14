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
        Max_Silhouette = NA_real_, Hartigans_D = NA_real_
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
    
    # --- CALCULATE HARTIGAN'S DIP STATISTIC (D) ---
    d_mat <- tmp_engine@distance_matrix
    
    # Safely handle 'dist' vs 'matrix' format dynamically
    if (inherits(d_mat, "dist")) {
      d_mat_square <- as.matrix(d_mat)
      d_vec <- d_mat_square[upper.tri(d_mat_square)]
    } else {
      d_vec <- d_mat[upper.tri(d_mat)]
    }
    
    if (length(d_vec) >= 4) {
      dip_stat <- diptest::dip(d_vec)
    } else {
      dip_stat <- NA_real_
    }
    # ---------------------------------------------------
    
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
    
    results_list[[as.character(th)]] <- data.frame(
      Threshold = th, N_Anomalies = length(idx), Opt_k = opt_k, 
      Max_Silhouette = max_sil, Hartigans_D = dip_stat
    )
  }
  
  res_df <- dplyr::bind_rows(results_list)
  
  return(res_df)
})