
#' @title Explore Anomaly Thresholds
#' @description Rapidly scores the feature space and returns a mapping of 
#' anomaly thresholds to the number of extracted events.
#' @export
explore_thresholds <- function(object, thresholds = seq(0.50, 0.80, by = 0.01), ntrees = 500, sample_size = 256) {
  if (object@domain_state == "uninitialized") stop("Telemetry must be ingested.")
  
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
  window <- object@feature_schema$temporal_window_hours
  if (is.null(window)) window <- 2
  
  targets <- object@feature_schema$target_variables
  contexts <- object@feature_schema$context_variables
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