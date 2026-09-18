#| label: diagnostic-engine-s4-skeleton
#| warning: false
#| message: false

# 1. Define the Class Structure
setClass(
  "DiagnosticEngine",
  slots = list(
    # Configuration & Metadata
    config_path = "character",
    feature_schema = "list",         # Parsed YAML configuration
    domain_state = "character",      # e.g., "initialized", "features_engineered", "calibrated"
    
    # Data & Feature Space
    raw_telemetry = "data.frame",    # The base ingest
    feature_matrix = "data.frame",   # Post-YAML dynamic mutations
    anomalies = "data.frame",        # Filtered extreme events (AUC > threshold)
    
    # Computational Core (The heavy lifting)
    distance_matrix = "matrix",      # The MDTW N x N matrix
    hierarchical_tree = "ANY",       # The DIANA hclust object (ANY handles S3 inheritance)
    
    # Optimization & Validation
    k_metrics = "data.frame",        # Stores k, Global_Silhouette, Edge_Weighted_Harmonic, Min_T_Score
    medoids = "list",                # The extracted physical archetypes for plotting
    
    # Final Operational Outputs
    active_k = "numeric",            # The currently selected or math-recommended k
    routing_queues = "data.frame"    # Final clustered anomalies ready for dispatch
  ),
  
  # 2. Validation Function
  validity = function(object) {
    if (length(object@config_path) == 0) {
      return("Engine requires a valid YAML configuration path upon instantiation.")
    }
    return(TRUE)
  }
)

# 3. Define the Core Generic Methods (The API)
setGeneric("ingest_telemetry", function(object, data) standardGeneric("ingest_telemetry"))
setGeneric("extract_anomalies", function(object, threshold) standardGeneric("extract_anomalies"))
setGeneric("compute_distances", function(object) standardGeneric("compute_distances"))

# The Human-in-the-Loop Optimization
setGeneric("calibrate", function(object, min_k = 2, max_k = 8) standardGeneric("calibrate"))
setGeneric("commit_queues", function(object, k) standardGeneric("commit_queues"))