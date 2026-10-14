test_that("DiagnosticEngine lifecycle and predictive routing completes successfully", {
  
  # 1. Setup minimal mocked environment
  mock_yaml_path <- tempfile(fileext = ".yaml")
  mock_config <- list(
    target_variables = c("temp_error", "energy_deriv"),
    anomaly_threshold = 0.50,
    temporal_window_hours = 1,
    sample_size = 16,  # Scaled down for unit testing
    ntrees = 50        # Scaled down for rapid unit testing
  )
  yaml::write_yaml(mock_config, mock_yaml_path)
  
  # 2. Mock a continuous 40-row time course with extreme outliers at the end
  mock_data <- data.frame(
    timestamp = seq(as.POSIXct("2026-01-01 00:00:00"), by = "15 mins", length.out = 40),
    temp_error = c(rnorm(35, 0, 0.1), 10, 11, 10.5, 12, 11.5), 
    energy_deriv = c(rnorm(35, 0, 1), 50, -50, 45, -45, 55)
  )
  
  # 3. Test Initialization
  engine <- new("DiagnosticEngine", config_path = mock_yaml_path)
  expect_equal(engine@domain_state, "initialized")
  
  # 4. Test Ingestion
  engine <- ingest_time_course(engine, mock_data)
  expect_equal(engine@domain_state, "time_course_ingested")
  expect_equal(ncol(engine@feature_matrix), 2)
  
  # 5. Test Anomaly Extraction
  set.seed(42)
  engine <- extract_anomalies(engine)
  expect_equal(engine@domain_state, "anomalies_extracted")
  expect_true(nrow(engine@anomalies) > 0)
  
  # 6. Test Topological Distance Matrix
  engine <- compute_distances(engine)
  expect_equal(engine@domain_state, "distances_computed")
  expect_true(is.matrix(engine@distance_matrix))
  
  # 7. Test Calibration (Test k=2 to k=3 using Global Silhouette)
  suppressWarnings({
    engine <- calibrate(engine, min_k = 2, max_k = 3)
  })
  expect_equal(engine@domain_state, "calibrated")
  expect_true("global_silhouette" %in% colnames(engine@k_metrics))
  
  # 8. Test Medoid & Baseline Signature Extraction
  engine <- extract_medoids(engine, k = 2)
  expect_true(length(engine@medoids) == 2)
  expect_true(nrow(engine@baseline_signatures) == 2)
  
  # 9. Test Commit Forensic Ledger
  engine <- commit_ledger(engine, k = 2)
  expect_equal(engine@domain_state, "ledger_committed")
  expect_true("anomaly_cluster" %in% colnames(engine@forensic_ledger))
  expect_true("cluster_rank" %in% colnames(engine@forensic_ledger))
  
  # 10. Test DS Accessors
  ledger_df <- get_ledger(engine)
  expect_s3_class(ledger_df, "data.frame")
  medoid_df <- get_medoids(engine)
  expect_s3_class(medoid_df, "data.frame")
  
  # 11. Test Predictive Streaming
  # Feed the last 5 rows back in as "new" streaming data
  stream_data <- mock_data[36:40, ]
  routing_results <- predict_stream(engine, stream_data)
  
  expect_true("predicted_cluster" %in% colnames(routing_results))
  expect_true("distance_to_medoid" %in% colnames(routing_results))
  expect_true(any(!is.na(routing_results$predicted_cluster))) # Should flag the anomalies
  
  # Cleanup
  unlink(mock_yaml_path)
})

test_that("DiagnosticEngine catches missing YAML targets on initialization", {
  
  # Mock a broken config (missing target_variables entirely)
  broken_yaml_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(list(facility_name = "Broken_Config", ntrees = 100), broken_yaml_path)
  
  # The hardened initialize() method should immediately reject this
  expect_error(
    new("DiagnosticEngine", config_path = broken_yaml_path),
    "Fatal Error: YAML configuration must contain at least one variable in 'target_variables'."
  )
  
  unlink(broken_yaml_path)
})

test_that("ingest_time_course mathematically enforces temporal tolerance", {
  
  # 1. Setup minimal engine using a temporary YAML configuration
  yaml_config <- "
target_variables:
  - t_supply
"
  config_file <- tempfile(fileext = ".yml")
  writeLines(yaml_config, config_file)
  
  engine <- new("DiagnosticEngine", config_path = config_file)
  
  # 2. Create mock data with a clear baseline and exactly one 10-hour gap (36,000 seconds)
  mock_data <- data.frame(
    timestamp = as.POSIXct(c(
      "2026-10-14 09:00:00",
      "2026-10-14 10:00:00",   # 1-hour gap
      "2026-10-14 11:00:00",   # 1-hour gap
      "2026-10-14 12:00:00",   # 1-hour gap
      "2026-10-14 22:00:00"    # 10-hour gap
    ), tz = "UTC"),
    t_supply = c(20.0, 20.1, 20.2, 20.3, 20.5)
  )
  
  # 3. Test Success: Engine must accept data if tolerance is larger than the gap (e.g., 12 hours)
  expect_no_error({
    valid_engine <- ingest_time_course(engine, data = mock_data, tolerance = 43200)
  })
  expect_equal(valid_engine@domain_state, "time_course_ingested")
  
  # 4. Test Failure: Engine must throw a fatal error if tolerance is smaller than the gap (e.g., 5 hours)
  expect_error(
    ingest_time_course(engine, data = mock_data, tolerance = 18000),
    regexp = "Fatal Error: Ingested data contains time discontinuities exceeding the 18000-second tolerance."
  )
  
  # Cleanup
  unlink(config_file)
})

test_that("explore_thresholds returns correct structure and catches state errors", {
  yaml_config <- "
target_variables:
  - t_supply
anomaly_threshold: 0.60
ntrees: 50
sample_size: 16
"
  config_file <- tempfile(fileext = ".yml")
  writeLines(yaml_config, config_file)
  engine <- new("DiagnosticEngine", config_path = config_file)
  
  # 1. Test failure on uninitialized state (before ingestion)
  expect_error(
    explore_thresholds(engine, thresholds = c(0.5, 0.6)), 
    "Fatal Error: Time course must be ingested"
  )
  
  # 2. Ingest valid data
  mock_data <- data.frame(
    timestamp = as.POSIXct(c("2026-10-14 09:00:00", "2026-10-14 10:00:00", "2026-10-14 11:00:00"), tz = "UTC"),
    t_supply = c(20.0, 20.1, 20.5)
  )
  engine <- ingest_time_course(engine, data = mock_data)
  
  # 3. Test successful execution and correct output structure
  curve_df <- explore_thresholds(engine, thresholds = c(0.5, 0.6, 0.7))
  
  expect_s3_class(curve_df, "data.frame")
  expect_equal(colnames(curve_df), c("Threshold", "Event_Count"))
  expect_equal(nrow(curve_df), 3)
  expect_type(curve_df$Event_Count, "integer")
  
  unlink(config_file)
})

#' @export
setMethod("get_threshold_at_N", "DiagnosticEngine", function(object, n) {
  if (!object@domain_state %in% c("time_course_ingested", "anomalies_extracted", "distances_computed", "calibrated", "ledger_committed")) {
    stop("Fatal Error: Time course must be ingested before querying thresholds.")
  }
  
  # Use existing scores if extract_anomalies has already run
  if ("anomaly_score" %in% colnames(object@raw_time_course)) {
    scores <- object@raw_time_course$anomaly_score
  } else {
    # Otherwise, train a quick seeded forest using current hyperparameters
    hyps <- object@hyperparameters
    set.seed(42)
    actual_size <- min(hyps$sample_size, nrow(object@feature_matrix))
    iso_model <- suppressWarnings(isotree::isolation.forest(
      object@feature_matrix, ntrees = hyps$ntrees, sample_size = actual_size, missing_action = "fail"
    ))
    scores <- stats::predict(iso_model, object@feature_matrix)
  }
  
  sorted_scores <- sort(scores, decreasing = TRUE)
  actual_n <- min(n, length(sorted_scores))
  
  return(sorted_scores[actual_n])
})