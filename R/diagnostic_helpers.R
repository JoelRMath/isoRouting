#' @title Explore Anomaly Thresholds
#' @description Rapidly scores the feature space and returns a mapping of 
#' anomaly thresholds to the number of extracted events.
#' @export
explore_thresholds <- function(object, thresholds = seq(0.50, 0.80, by = 0.01), ntrees = NULL, sample_size = NULL) {
  if (object@domain_state == "uninitialized") stop("Telemetry must be ingested.")
  
  # Inherit from YAML if not manually provided
  hyps <- get_hyperparameters(object)
  if (is.null(ntrees)) ntrees <- ifelse(is.null(hyps$ntrees), 500, hyps$ntrees)
  if (is.null(sample_size)) sample_size <- ifelse(is.null(hyps$sample_size), 256, hyps$sample_size)
  
  message("Scoring feature space to build threshold curve...")
  set.seed(42)
  actual_size <- min(sample_size, nrow(object@feature_matrix))
  
  iso_model <- suppressWarnings(isotree::isolation.forest(
    object@feature_matrix, ntrees = ntrees, sample_size = actual_size, missing_action = "fail"
  ))
  scores <- stats::predict(iso_model, object@feature_matrix)
  
  counts <- purrr::map_int(thresholds, ~sum(scores > .x))
  return(data.frame(Threshold = thresholds, Event_Count = counts))
}

#' @title Evaluate K-Stability Across Thresholds
#' @description Iteratively computes MDTW and cluster calibration across a sequence of 
#' anomaly thresholds to test the stability of physical fault archetypes.
#' @export
evaluate_k_stability <- function(object, thresholds = seq(0.65, 0.72, by = 0.01), min_k = 2, max_k = 10, max_n = 300) {
  if (object@domain_state == "uninitialized") stop("Telemetry must be ingested.")
  
  message("Establishing baseline scores for stability evaluation...")
  set.seed(42)
  actual_size <- min(256, nrow(object@feature_matrix))
  iso_model <- suppressWarnings(isotree::isolation.forest(
    object@feature_matrix, ntrees = 500, sample_size = actual_size, missing_action = "fail"
  ))
  base_scores <- stats::predict(iso_model, object@feature_matrix)
  
  results <- list()
  
  for (thresh in thresholds) {
    anomalies <- object@raw_telemetry[base_scores > thresh, ]
    n_events <- nrow(anomalies)
    
    if (n_events < min_k || n_events > max_n) {
      results[[as.character(thresh)]] <- data.frame(
        Threshold = thresh, N_Anomalies = n_events, Opt_k_Silhouette = NA, Opt_k_Harmonic = NA
      )
      next
    }
    
    tmp_engine <- object
    tmp_engine@anomalies <- anomalies
    tmp_engine@domain_state <- "anomalies_extracted"
    
    capture.output({ suppressMessages({
      tmp_engine <- compute_distances(tmp_engine)
      tmp_engine <- calibrate(tmp_engine, min_k = min_k, max_k = max_k)
    })})
    
    metrics <- tmp_engine@k_metrics
    results[[as.character(thresh)]] <- data.frame(
      Threshold = thresh, 
      N_Anomalies = n_events, 
      Opt_k_Silhouette = metrics$k[which.max(metrics$Global_Silhouette)], 
      Opt_k_Harmonic = metrics$k[which.max(metrics$Product_Weighted_Harmonic)]
    )
  }
  
  return(dplyr::bind_rows(results))
}

#' @title Extract Medoid Trajectory Windows
#' @description Extracts the multivariate time-series windows surrounding the 
#' geometric medoids for a specific cluster configuration, returning a long-format 
#' dataframe optimized for faceted plotting.
#' @param object A calibrated DiagnosticEngine object.
#' @param k Integer. The number of clusters to extract (must exist in the calibration metrics).
#' @return A long-format tibble containing timestamp, relative time, value, and variable type.
#' @export
extract_medoid_windows <- function(object, k) {
  if (object@domain_state != "calibrated") {
    stop("Engine must be calibrated before extracting medoid windows.")
  }
  
  k_char <- as.character(k)
  if (is.null(object@medoids[[k_char]])) {
    stop(sprintf("Medoids for k=%d not found. Ensure this k was evaluated during calibrate().", k))
  }
  
  medoids <- object@medoids[[k_char]]
  
  # Fetch parameters using getters to respect encapsulation
  hyps <- get_hyperparameters(object)
  window <- hyps$temporal_window_hours
  if (is.null(window)) window <- 2
  
  feats <- get_features(object)
  targets <- feats$target_variables
  contexts <- feats$context_variables
  if (is.null(contexts)) contexts <- character(0)
  
  # Combine and deduplicate in case a variable is used as both context and target
  all_vars <- unique(c(targets, contexts))
  
  plot_df <- data.frame()
  
  for (i in seq_along(medoids)) {
    ts_center <- lubridate::ymd_hms(medoids[i])
    
    window_data <- object@raw_telemetry %>%
      dplyr::filter(
        timestamp >= (ts_center - lubridate::hours(window)) & 
          timestamp <= (ts_center + lubridate::hours(window))
      ) %>%
      dplyr::select(timestamp, dplyr::all_of(all_vars)) %>%
      dplyr::mutate(
        Archetype = paste("Archetype", i),
        Relative_Time = as.numeric(difftime(timestamp, ts_center, units = "hours"))
      )
    
    plot_df <- dplyr::bind_rows(plot_df, window_data)
  }
  
  # Pivot long and assign architectural tags
  plot_df_long <- plot_df %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(all_vars),
      names_to = "Variable",
      values_to = "Value"
    ) %>%
    dplyr::mutate(
      Variable_Type = dplyr::if_else(
        Variable %in% contexts & !Variable %in% targets, 
        "Descriptive Context", 
        "Mathematical Feature"
      )
    )
  
  return(plot_df_long)
}

#' @title Evaluate Isolation Forest Stability
#' @description Evaluates the reproducibility of anomaly scores across varying 
#' hyperparameter configurations by comparing them to a high-density baseline model.
#' @param object A DiagnosticEngine object with ingested telemetry.
#' @param ntrees_seq Numeric vector of tree counts to evaluate.
#' @param sample_size_seq Numeric vector of sample sizes to evaluate.
#' @param baseline_ntrees Integer. Trees for the gold-standard baseline (default 2000).
#' @param baseline_sample Integer. Sample size for the gold-standard baseline (default 1024).
#' @return A data.frame mapping hyperparameter combinations to Spearman rank correlation.
#' @export
evaluate_forest_stability <- function(object, 
                                      ntrees_seq = c(50, 100, 250, 500, 1000), 
                                      sample_size_seq = c(64, 128, 256, 512),
                                      baseline_ntrees = 2000,
                                      baseline_sample = 1024) {
  
  if (object@domain_state == "uninitialized") {
    stop("Telemetry must be ingested before evaluating stability.")
  }
  
  message(sprintf("Building baseline model (%d trees, sample size %d)...", 
                  baseline_ntrees, baseline_sample))
  
  set.seed(42)
  base_model <- suppressWarnings(isotree::isolation.forest(
    object@feature_matrix, 
    ntrees = baseline_ntrees, 
    sample_size = baseline_sample, 
    missing_action = "fail"
  ))
  base_scores <- stats::predict(base_model, object@feature_matrix)
  
  results <- list()
  grid <- expand.grid(ntrees = ntrees_seq, sample_size = sample_size_seq)
  
  message(sprintf("Evaluating %d hyperparameter combinations...", nrow(grid)))
  
  for (i in seq_len(nrow(grid))) {
    nt <- grid$ntrees[i]
    ss <- grid$sample_size[i]
    
    # Iterate the seed slightly to ensure unique random cuts for each test run
    set.seed(42 + i) 
    test_model <- suppressWarnings(isotree::isolation.forest(
      object@feature_matrix, 
      ntrees = nt, 
      sample_size = ss, 
      missing_action = "fail"
    ))
    test_scores <- stats::predict(test_model, object@feature_matrix)
    
    # Evaluate rank preservation
    rank_cor <- stats::cor(base_scores, test_scores, method = "spearman")
    
    results[[i]] <- data.frame(
      ntrees = nt,
      sample_size = ss,
      spearman_rho = rank_cor
    )
  }
  
  return(dplyr::bind_rows(results))
}

#' @title Evaluate Prognostic Drift
#' @description Evaluates out-of-sample predictive stability by training an Isolation Forest 
#' on historical data and scoring future data, comparing results to a full-context baseline.
#' @param object A DiagnosticEngine object with ingested telemetry.
#' @param train_fraction Numeric. Fraction of chronological data to use for training (default 0.6).
#' @param ntrees Integer. Number of trees for the forests (default 500).
#' @param sample_size Integer. Sample size for the forests (default 256).
#' @return A data.frame containing the test timestamps, out-of-sample scores, and baseline scores.
#' @export
evaluate_prognostic_drift <- function(object, 
                                      train_fraction = 0.6, 
                                      ntrees = 500, 
                                      sample_size = 256) {
  
  if (object@domain_state == "uninitialized") {
    stop("Telemetry must be ingested before evaluating prognostic drift.")
  }
  
  fm <- object@feature_matrix
  n_total <- nrow(fm)
  n_train <- floor(n_total * train_fraction)
  
  if (n_train < sample_size) stop("Training fraction yields fewer rows than sample_size.")
  
  # Strict chronological split
  train_mat <- fm[1:n_train, , drop = FALSE]
  test_mat <- fm[(n_train + 1):n_total, , drop = FALSE]
  
  message(sprintf("Building baseline model on full dataset (N=%d)...", n_total))
  set.seed(42)
  base_model <- suppressWarnings(isotree::isolation.forest(
    fm, ntrees = ntrees, sample_size = sample_size, missing_action = "fail"
  ))
  # Score only the test portion using the full-context model
  base_scores <- stats::predict(base_model, test_mat)
  
  message(sprintf("Building prognostic model on training split (N=%d)...", n_train))
  set.seed(42)
  prog_model <- suppressWarnings(isotree::isolation.forest(
    train_mat, ntrees = ntrees, sample_size = sample_size, missing_action = "fail"
  ))
  # Score the test portion using the historically-trained model
  oos_scores <- stats::predict(prog_model, test_mat)
  
  # Extract chronological timestamps for the test set
  test_timestamps <- object@raw_telemetry$timestamp[(n_train + 1):n_total]
  
  results <- data.frame(
    timestamp = test_timestamps,
    baseline_score = base_scores,
    oos_score = oos_scores
  )
  
  # Calculate global fit metric
  r_sq <- stats::cor(base_scores, oos_scores)^2
  message(sprintf("Prognostic Evaluation Complete. Out-of-Sample R^2: %.4f", r_sq))
  
  return(results)
}

#' @title Explain Anomaly Feature Importance
#' @description Conducts Explainable AI (XAI) feature attribution on a specific anomaly,
#' calculating Operational Robust Z-Scores and algorithmic Score Ablation Drops.
#' @param object A DiagnosticEngine object with ingested telemetry.
#' @param iso_model A trained isotree::isolation.forest model.
#' @param target_idx Numeric. The row index of the anomaly to explain.
#' @return A data.frame containing Z-Scores and Ablation Drops for all variables.
#' @export
explain_anomaly <- function(object, iso_model, target_idx) {
  if (object@domain_state == "uninitialized") {
    stop("Telemetry must be ingested to extract features for explanation.")
  }
  
  baseline_matrix <- object@feature_matrix
  
  # 1. Global baseline statistics
  global_medians <- apply(baseline_matrix, 2, median, na.rm = TRUE)
  global_mads <- apply(baseline_matrix, 2, mad, na.rm = TRUE)
  global_mads[global_mads == 0] <- 1e-6 # Prevent zero-division
  
  # 2. Extract target row and base score
  target_row <- baseline_matrix[target_idx, , drop = FALSE]
  base_score <- stats::predict(iso_model, target_row)
  
  # 3. Compute Robust Z-Scores
  z_scores <- abs(target_row[1, ] - global_medians) / global_mads
  
  # 4. Compute Algorithmic Ablation Drops
  ablation_drops <- numeric(ncol(baseline_matrix))
  names(ablation_drops) <- colnames(baseline_matrix)
  
  for (var in names(ablation_drops)) {
    ablated_row <- target_row
    ablated_row[1, var] <- global_medians[var]
    new_score <- stats::predict(iso_model, ablated_row)
    ablation_drops[var] <- base_score - new_score
  }
  
  # 5. Compile comprehensive results
  df_explain <- data.frame(
    Variable = names(ablation_drops),
    Original_Value = as.numeric(target_row[1, ]),
    Global_Median = as.numeric(global_medians),
    Robust_Z_Score = as.numeric(z_scores),
    Ablation_Drop = as.numeric(ablation_drops)
  )
  
  # Sort by Ablation Drop (highest impact first)
  df_explain <- df_explain[order(-df_explain$Ablation_Drop), ]
  rownames(df_explain) <- NULL
  
  return(df_explain)
}