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