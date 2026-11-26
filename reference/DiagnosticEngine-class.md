# DiagnosticEngine Class

A stateful S4 class that acts as a deterministic forensic and predictive
engine for multivariate time series. It utilizes Isolation Forests to
extract extreme events, avoiding Euclidean distances by constructing a
proximity matrix based on terminal node signatures (Hamming distance).
It applies divisive hierarchical clustering and global silhouette to
extract mathematically stable anomaly archetypes.

## Usage

``` r
# S4 method for class 'DiagnosticEngine'
initialize(.Object, config_path, ...)
```

## Arguments

- .Object:

  The prototype `DiagnosticEngine` instance being initialized.

- config_path:

  Character. Path to the YAML configuration file.

- ...:

  Additional slot arguments passed to the default S4 initializer.

## Slots

- `config_path`:

  character. Path to the YAML configuration file.

- `feature_schema`:

  list. Parsed rules for dynamic feature engineering.

- `hyperparameters`:

  list. Machine learning tuning parameters (thresholds, ntrees).

- `domain_state`:

  character. Current lifecycle state of the engine.

- `raw_time_course`:

  data.frame. The base ingested dataset.

- `scored_time_course`:

  data.frame. Anomaly scores at each timestamp.

- `feature_matrix`:

  data.frame. Data after applying YAML mutations.

- `anomalies`:

  data.frame. Isolated extreme events triggering the analysis.

- `model`:

  ANY. The trained isotree model object.

- `distance_matrix`:

  matrix. Topological (Hamming) distance matrix of terminal nodes.

- `hierarchical_tree`:

  ANY. The cached DIANA clustering object.

- `k_metrics`:

  data.frame. Tracking of structural integrity across k-values.

- `medoids`:

  list. Extracted physical archetypes representing anomaly signatures.

- `baseline_signatures`:

  matrix. Cached terminal node paths of the medoids for predictive
  routing.

- `active_k`:

  numeric. The mathematically validated number of fault clusters.

- `optimal_k`:

  numeric. The optimal number k of cluster (global silouhette).

- `forensic_ledger`:

  data.frame. Ranked historical ledger mapping anomalies to their
  medoid.

- `cluster_radii`:

  numeric. Named vector of empirical cluster radii (maximum topological
  distance to each medoid).

## Examples

``` r
cfg <- tempfile(fileext = ".yaml")
yaml::write_yaml(list(
  ntrees = 20, sample_size = 32, temporal_window_hours = 2,
  anomaly_threshold = 0.55, context_variables = list(),
  target_variables = c("x1", "x2")
), cfg)
eng <- methods::new("DiagnosticEngine", config_path = cfg)
#> DiagnosticEngine initialized. Schema loaded from: file1a7350e3b621.yaml
unlink(cfg)
```
