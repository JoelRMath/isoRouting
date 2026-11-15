test_that("DiagnosticEngine lifecycle and predictive routing completes successfully", {
  
  # 1. Setup minimal mocked environment
  mock_yaml_path <- tempfile(fileext = ".yaml")
  mock_config <- list(
    target_variables = c("temp_error", "energy_deriv"),
    context_variables = c("outdoor_temp"),
    anomaly_threshold = 0.50,
    temporal_window_hours = 1,
    sample_size = 16,  # Scaled down for unit testing
    ntrees = 50        # Scaled down for rapid unit testing
  )
  yaml::write_yaml(mock_config, mock_yaml_path)
  on.exit(unlink(mock_yaml_path), add = TRUE)
  
  # 2. Mock a continuous 40-row time course with extreme outliers at the end
  set.seed(42)
  mock_data <- data.frame(
    timestamp = seq(as.POSIXct("2026-01-01 00:00:00", tz = "UTC"), by = "15 mins", length.out = 40),
    temp_error = c(rnorm(35, 0, 0.1), 10, 11, 10.5, 12, 11.5), 
    energy_deriv = c(rnorm(35, 0, 1), 50, -50, 45, -45, 55),
    outdoor_temp = rnorm(40, 5, 1)
  )
  
  # 3. Test Initialization & Metadata Accessors
  engine <- new("DiagnosticEngine", config_path = mock_yaml_path)
  expect_equal(engine@domain_state, "initialized")
  
  hyps <- get_hyperparameters(engine)
  expect_equal(hyps$ntrees, 50)
  expect_equal(hyps$sample_size, 16)
  
  schema <- get_features(engine)
  expect_equal(schema$target_variables, c("temp_error", "energy_deriv"))
  expect_equal(schema$context_variables, "outdoor_temp")
  
  # Empty accessors should warn before ingestion/extraction
  expect_warning(get_time_course(engine), "Time course is empty")
  expect_warning(get_scored_time_course(engine), "Scored time course is empty")
  expect_warning(get_medoids(engine), "Medoids list is empty")
  expect_warning(get_ledger(engine), "Forensic ledger is empty")
  
  # 4. Test Ingestion
  engine <- ingest_time_course(engine, mock_data)
  expect_equal(engine@domain_state, "time_course_ingested")
  expect_equal(ncol(engine@feature_matrix), 2)
  expect_equal(nrow(get_time_course(engine)), 40)
  
  # 5. Test Anomaly Extraction
  engine <- extract_anomalies(engine)
  expect_equal(engine@domain_state, "anomalies_extracted")
  expect_true(nrow(engine@anomalies) > 0)
  expect_equal(nrow(get_scored_time_course(engine)), 40)
  
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
  expect_true(!is.na(engine@optimal_k))
  
  # 8. Test Medoid, Baseline Signature & Cluster Radii Extraction
  engine <- extract_medoids(engine, k = 2)
  expect_equal(length(engine@medoids), 2)
  expect_equal(nrow(engine@baseline_signatures), 2)
  expect_equal(length(engine@cluster_radii), 2)
  expect_equal(names(engine@cluster_radii), c("Cluster 1", "Cluster 2"))
  
  # 9. Test Commit Forensic Ledger
  engine <- commit_ledger(engine, k = 2)
  expect_equal(engine@domain_state, "ledger_committed")
  expect_true(all(c("anomaly_cluster", "cluster_rank", "topological_distance") %in% colnames(engine@forensic_ledger)))
  
  # 10. Test DS Accessors
  ledger_df <- get_ledger(engine)
  expect_s3_class(ledger_df, "data.frame")
  medoid_df <- get_medoids(engine)
  expect_s3_class(medoid_df, "data.frame")
  
  # 11. Test Feature Ablation (LOCO)
  ablation_df <- ablate_features(engine)
  expect_s3_class(ablation_df, "data.frame")
  expect_equal(colnames(ablation_df), c("cluster", "variable", "score_drop", "rank"))
  expect_equal(nrow(ablation_df), 4) # 2 clusters * 2 target variables
  
  # 12. Test Full Time Course Routing via Cluster Radii
  routed_tc <- route_time_course(engine)
  expect_s3_class(routed_tc, "data.frame")
  expect_equal(nrow(routed_tc), 40)
  expect_true(all(c("closest_archetype", "distance_to_archetype", "archetype_bucket") %in% colnames(routed_tc)))
  
  # 13. Test Predictive Streaming
  stream_data <- mock_data[36:40, ]
  routing_results <- predict_stream(engine, stream_data)
  
  expect_true(all(c("closest_archetype", "distance_to_archetype", "predicted_state") %in% colnames(routing_results)))
  expect_true(any(routing_results$predicted_state != "Background"))
})

test_that("DiagnosticEngine catches missing YAML targets and missing files on initialization", {
  
  # Non-existent file
  expect_error(
    new("DiagnosticEngine", config_path = "non_existent_config.yaml"),
    "Fatal Error: Configuration file not found"
  )
  
  # Mock a broken config (missing target_variables entirely)
  broken_yaml_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(list(facility_name = "Broken_Config", ntrees = 100), broken_yaml_path)
  on.exit(unlink(broken_yaml_path), add = TRUE)
  
  expect_error(
    new("DiagnosticEngine", config_path = broken_yaml_path),
    "Fatal Error: YAML configuration must contain at least one variable in 'target_variables'."
  )
})

test_that("ingest_time_course enforces temporal tolerance and schema validation", {
  
  yaml_config <- "
target_variables:
  - t_supply
"
  config_file <- tempfile(fileext = ".yml")
  writeLines(yaml_config, config_file)
  on.exit(unlink(config_file), add = TRUE)
  
  engine <- new("DiagnosticEngine", config_path = config_file)
  
  # 1. Rejects < 3 rows
  expect_error(
    ingest_time_course(engine, data = data.frame(timestamp = Sys.time(), t_supply = 20)),
    "at least 3 observations"
  )
  
  # 2. Rejects non-POSIXt timestamp
  expect_error(
    ingest_time_course(engine, data = data.frame(timestamp = c("a", "b", "c"), t_supply = 1:3)),
    "must be a POSIXct/POSIXlt datetime object"
  )
  
  # 3. Rejects missing target columns
  expect_error(
    ingest_time_course(engine, data = data.frame(timestamp = Sys.time() + 1:3, wrong_col = 1:3)),
    "Data missing schema targets: t_supply"
  )
  
  # 4. Temporal continuity tolerance checks
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
  
  expect_no_error({
    valid_engine <- ingest_time_course(engine, data = mock_data, tolerance = 43200)
  })
  expect_equal(valid_engine@domain_state, "time_course_ingested")
  
  expect_error(
    ingest_time_course(engine, data = mock_data, tolerance = 18000),
    regexp = "Fatal Error: Ingested data contains time discontinuities exceeding the 18000-second tolerance."
  )
})

test_that("explore_thresholds and get_threshold_at_N work properly and catch state errors", {
  yaml_config <- "
target_variables:
  - t_supply
anomaly_threshold: 0.60
ntrees: 50
sample_size: 16
"
  config_file <- tempfile(fileext = ".yml")
  writeLines(yaml_config, config_file)
  on.exit(unlink(config_file), add = TRUE)
  
  engine <- new("DiagnosticEngine", config_path = config_file)
  
  # 1. Test failure before ingestion
  expect_error(
    explore_thresholds(engine, thresholds = c(0.5, 0.6)), 
    "Fatal Error: Time course must be ingested"
  )
  expect_error(
    get_threshold_at_N(engine, n = 2),
    "Feature matrix is empty"
  )
  
  # 2. Ingest valid data
  mock_data <- data.frame(
    timestamp = seq(as.POSIXct("2026-10-14 09:00:00", tz = "UTC"), by = "1 hour", length.out = 10),
    t_supply = c(rep(20.0, 8), 35.0, 40.0)
  )
  engine <- ingest_time_course(engine, data = mock_data)
  
  # 3. Test explore_thresholds
  curve_df <- explore_thresholds(engine, thresholds = c(0.5, 0.6, 0.7))
  expect_s3_class(curve_df, "data.frame")
  expect_equal(colnames(curve_df), c("Threshold", "Event_Count"))
  expect_equal(nrow(curve_df), 3)
  expect_type(curve_df$Event_Count, "integer")
  
  # 4. Test get_threshold_at_N (fallback path before extract_anomalies, and warning when n > nrow)
  th_2 <- unname(get_threshold_at_N(engine, n = 2))
  expect_type(th_2, "double")
  expect_warning(
    th_max <- get_threshold_at_N(engine, n = 50),
    "exceeds dataset size"
  )
  
  # 5. Test get_threshold_at_N fast path after extract_anomalies
  engine <- extract_anomalies(engine, threshold = th_2)
  expect_equal(unname(get_threshold_at_N(engine, n = 2)), th_2)
})

test_that("evaluate_k_stability enforces domain state requirements and returns valid metrics", {
  yaml_content <- "
  ntrees: 50
  sample_size: 32
  temporal_window_hours: 1
  anomaly_threshold: 0.5
  target_variables:
    - var1
    - var2
  context_variables: []
  "
  tmp_yml <- tempfile(fileext = ".yml")
  writeLines(yaml_content, tmp_yml)
  on.exit(unlink(tmp_yml), add = TRUE)
  
  engine_empty <- new("DiagnosticEngine", config_path = tmp_yml)
  
  expect_error(
    evaluate_k_stability(engine_empty, max_N = 50, min_N = 15),
    "Fatal Error: Time course must be ingested"
  )
  
  set.seed(42)
  dummy_data <- data.frame(
    timestamp = seq(as.POSIXct("2026-01-01 00:00:00", tz = "UTC"), by = "15 mins", length.out = 120),
    var1 = rnorm(120),
    var2 = rnorm(120)
  )
  
  engine <- ingest_time_course(engine_empty, dummy_data)
  
  res <- evaluate_k_stability(
    engine, 
    max_N = 40, 
    min_N = 15, 
    nb_steps = 4, 
    min_k = 2, 
    max_k = 4
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 4)
  
  expected_cols <- c("Threshold", "N_Anomalies", "Opt_k", "Max_Silhouette", 
                     "Hartigans_D", "Global_LID", "Min_Cluster_LID", "Max_Cluster_LID")
  expect_true(all(expected_cols %in% colnames(res)))
  expect_true(all(diff(res$Threshold) >= 0))
  expect_true(all(diff(res$N_Anomalies) <= 0))
  expect_type(res$Hartigans_D, "double")
  expect_type(res$Global_LID, "double")
  
  # Test warning when min_N <= max_k
  expect_warning(
    res_warn <- evaluate_k_stability(engine, max_N = 30, min_N = 3, nb_steps = 3, max_k = 5),
    "min_N \\(3\\) is too small for max_k \\(5\\)\\. Adjusting min_N to 6"
  )
  expect_s3_class(res_warn, "data.frame")
})

test_that("extract_medoids dynamic slicing (via N or threshold) and isolate_novel_anomalies work end-to-end", {
  yaml_content <- "
  ntrees: 50
  sample_size: 32
  temporal_window_hours: 1
  anomaly_threshold: 0.55
  target_variables:
    - var1
    - var2
    - var3
  context_variables: []
  "
  tmp_yml <- tempfile(fileext = ".yml")
  writeLines(yaml_content, tmp_yml)
  on.exit(unlink(tmp_yml), add = TRUE)
  
  set.seed(123)
  # Baseline with two tight fault modes on var1 (var3 has normal variation but no extreme spikes)
  baseline_df <- data.frame(
    timestamp = seq(as.POSIXct("2026-01-01 00:00:00", tz = "UTC"), by = "15 mins", length.out = 100),
    var1 = c(rnorm(90, 0, 0.5), rep(10, 5), rep(-10, 5)),
    var2 = c(rnorm(90, 0, 0.5), rep(10, 5), rep(-10, 5)),
    var3 = rnorm(100, 0, 1)
  )
  
  engine <- new("DiagnosticEngine", config_path = tmp_yml)
  engine <- ingest_time_course(engine, baseline_df)
  
  # Direct extraction via threshold = 0.55 (without prior extract_anomalies() call)
  engine <- extract_medoids(engine, threshold = 0.55, k = 2)
  engine <- commit_ledger(engine, k = 2)
  expect_equal(engine@domain_state, "ledger_committed")
  
  # Also verify N-based slicing works on a separate copy
  engine_N <- extract_medoids(engine, N = 10, k = 2)
  expect_equal(length(engine_N@medoids), 2)
  
  # Create streaming telemetry with extreme novel anomalies on var3 & var2 (outside baseline radii)
  stream_df <- data.frame(
    timestamp = seq(as.POSIXct("2026-02-01 00:00:00", tz = "UTC"), by = "15 mins", length.out = 30),
    var1 = c(rnorm(22, 0, 0.5), rep(0, 4), rep(0, 4)),
    var2 = c(rnorm(22, 0, 0.5), rep(-25, 4), rep(25, 4)),
    var3 = c(rnorm(22, 0, 1.0), rep(30, 4), rep(-30, 4))
  )
  
  routed_stream <- predict_stream(engine, stream_df)
  novel_count <- sum(routed_stream$predicted_state == "Novel Anomaly")
  expect_true(novel_count >= 2)
  
  # Branch a novel engine onto the isolated novel anomalies
  novel_engine <- isolate_novel_anomalies(engine, routed_stream, min_k = 2, max_k = 3)
  expect_s4_class(novel_engine, "DiagnosticEngine")
  expect_equal(novel_engine@domain_state, "ledger_committed")
  expect_equal(nrow(get_ledger(novel_engine)), novel_count)
  
  # Verify error when fewer than 2 Novel Anomalies exist
  no_novel_stream <- routed_stream
  no_novel_stream$predicted_state <- "Background"
  expect_error(
    isolate_novel_anomalies(engine, no_novel_stream),
    "At least 2 'Novel Anomaly' observations are required"
  )
})

test_that("diagnostic_helpers functions operate accurately", {
  # 1. check_time_continuity
  ts_clean <- seq(as.POSIXct("2026-01-01 00:00:00", tz = "UTC"), by = "15 mins", length.out = 10)
  res_clean <- check_time_continuity(ts_clean, tolerance = 0)
  expect_true(res_clean$is_continuous)
  expect_equal(res_clean$expected_interval_secs, 900)
  expect_length(res_clean$gap_indices, 0)
  
  ts_gapped <- c(ts_clean[1:5], ts_clean[6:10] + 3600)
  res_gapped <- check_time_continuity(ts_gapped, tolerance = 60)
  expect_false(res_gapped$is_continuous)
  expect_equal(res_gapped$gap_indices, 5)
  
  expect_error(check_time_continuity(1:10), "must be a datetime object")
  
  # 2. compute_dip_statistic & compute_lid_profile
  set.seed(42)
  pts <- rbind(matrix(rnorm(20, -5, 0.5), ncol = 2), matrix(rnorm(20, 5, 0.5), ncol = 2))
  d_mat <- as.matrix(dist(pts))
  
  dip_val <- compute_dip_statistic(d_mat)
  expect_type(dip_val, "double")
  expect_false(is.na(dip_val))
  expect_equal(compute_dip_statistic(d_mat[1:2, 1:2]), NA_real_)
  
  lid_res <- compute_lid_profile(d_mat, cluster_labels = rep(1:2, each = 10), k = 4)
  expect_true(all(c("Global_LID", "Min_Cluster_LID", "Max_Cluster_LID") %in% names(lid_res)))
  expect_gte(lid_res$Global_LID, 0)
  
  # 3. simulate_lid_control & simulate_dip_control
  yaml_content <- "
  ntrees: 30
  sample_size: 16
  target_variables: [v1, v2]
  "
  tmp_yml <- tempfile(fileext = ".yml")
  writeLines(yaml_content, tmp_yml)
  on.exit(unlink(tmp_yml), add = TRUE)
  
  df_sim <- data.frame(
    timestamp = seq(as.POSIXct("2026-01-01 00:00:00", tz = "UTC"), by = "15 mins", length.out = 40),
    v1 = rnorm(40),
    v2 = rnorm(40)
  )
  eng_sim <- new("DiagnosticEngine", config_path = tmp_yml)
  eng_sim <- ingest_time_course(eng_sim, df_sim)
  
  lid_ctrl <- simulate_lid_control(eng_sim, N = c(15, 25), iterations = 5, k = 3)
  expect_s3_class(lid_ctrl, "data.frame")
  expect_equal(nrow(lid_ctrl), 2)
  expect_true(all(c("N", "Median", "Q05", "Q95", "Threshold") %in% colnames(lid_ctrl)))
  
  dip_ctrl <- simulate_dip_control(eng_sim, N = c(15, 25), iterations = 5)
  expect_s3_class(dip_ctrl, "data.frame")
  expect_equal(nrow(dip_ctrl), 2)
  expect_true(all(c("N", "Median", "Q05", "Q95", "Threshold") %in% colnames(dip_ctrl)))
  
  expect_error(simulate_lid_control(eng_sim, N = 100, iterations = 2), "cannot exceed total ingested")
  expect_error(simulate_dip_control(eng_sim, N = 100, iterations = 2), "cannot exceed total ingested")
  
  # 4. assess_forest_stability
  stab_df <- assess_forest_stability(
    data = df_sim,
    target_features = c("v1", "v2"),
    tree_counts = c(10, 20),
    sample_sizes = c(16, 32),
    baseline_trees = 50,
    seed = 42
  )
  expect_s3_class(stab_df, "data.frame")
  expect_equal(nrow(stab_df), 4)
  expect_equal(colnames(stab_df), c("ntrees", "sample_size", "spearman_rho"))
  
  expect_error(
    assess_forest_stability(df_sim, target_features = "non_existent"),
    "Missing target features"
  )
})