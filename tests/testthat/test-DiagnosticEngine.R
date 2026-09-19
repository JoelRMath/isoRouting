test_that("DiagnosticEngine lifecycle completes successfully", {
  
  # 1. Setup minimal mocked environment
  mock_yaml_path <- tempfile(fileext = ".yaml")
  mock_config <- list(
    facility_name = "Test Facility",
    target_variables = c("temp_error", "energy_deriv"),
    anomaly_threshold = 0.50,
    temporal_window_hours = 1
  )
  yaml::write_yaml(mock_config, mock_yaml_path)
  
  # 2. Mock a continuous 20-row dataframe with extreme outliers at the end
  mock_data <- data.frame(
    timestamp = seq(as.POSIXct("2026-01-01 00:00:00"), by = "15 mins", length.out = 20),
    temp_error = c(rnorm(18, 0, 0.1), 5.5, 6.0), 
    energy_deriv = c(rnorm(18, 0, 1), 50, -50)
  )
  
  # 3. Test Initialization
  engine <- new("DiagnosticEngine", config_path = mock_yaml_path)
  expect_equal(engine@domain_state, "initialized")
  
  # 4. Test Ingestion
  engine <- ingest_telemetry(engine, mock_data)
  expect_equal(engine@domain_state, "telemetry_ingested")
  expect_equal(ncol(engine@feature_matrix), 2)
  
  # 5. Test Anomaly Extraction
  # Set seed for isotree reproducibility
  set.seed(42)
  engine <- extract_anomalies(engine)
  expect_equal(engine@domain_state, "anomalies_extracted")
  expect_true(nrow(engine@anomalies) > 0)
  
  # 6. Test MDTW Distance Matrix
  engine <- compute_distances(engine)
  expect_equal(engine@domain_state, "distances_computed")
  expect_true(is.matrix(engine@distance_matrix))
  
  # 7. Test Calibration (Test k=2 to k=3)
  # Suppress warnings that arise from testing tiny mocked clusters
  suppressWarnings({
    engine <- calibrate(engine, min_k = 2, max_k = 3)
  })
  expect_equal(engine@domain_state, "calibrated")
  expect_true("Product_Weighted_Harmonic" %in% colnames(engine@k_metrics))
  
  # 8. Test Commit Routing
  engine <- commit_queues(engine, k = 2)
  expect_equal(engine@domain_state, "ready_for_dispatch")
  expect_true("operational_queue" %in% colnames(engine@routing_queues))
  
  # Cleanup
  unlink(mock_yaml_path)
})

test_that("DiagnosticEngine catches missing YAML targets", {
  
  # Mock a broken config (missing target_variables)
  broken_yaml_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(list(facility_name = "Broken"), broken_yaml_path)
  
  engine <- new("DiagnosticEngine", config_path = broken_yaml_path)
  
  mock_data <- data.frame(timestamp = Sys.time(), temp = 25)
  
  # Expect the specific error message we programmed in ingest_telemetry
  expect_error(
    ingest_telemetry(engine, mock_data), 
    "YAML schema missing 'target_variables' array."
  )
  
  unlink(broken_yaml_path)
})