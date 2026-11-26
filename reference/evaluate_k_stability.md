# Evaluate Topological Stability of Clusters Across Anomaly Thresholds

Sweeps a range of anomaly thresholds defined by sample size bounds
(`max_N` and `min_N`) to evaluate the structural stability of the
clustering manifold. To optimize performance, a master distance matrix
is computed once at `max_N` and instantaneously subsetted for higher
thresholds.

## Usage

``` r
evaluate_k_stability(
  object,
  max_N,
  min_N,
  nb_steps = 50,
  min_k = 2,
  max_k = 10
)

# S4 method for class 'DiagnosticEngine'
evaluate_k_stability(
  object,
  max_N,
  min_N,
  nb_steps = 50,
  min_k = 2,
  max_k = 10
)
```

## Arguments

- object:

  A `DiagnosticEngine` object.

- max_N:

  Integer. The maximum number of anomalies to evaluate (defines the
  lower threshold bound).

- min_N:

  Integer. The minimum number of anomalies to evaluate (defines the
  upper threshold bound).

- nb_steps:

  Integer. The number of discrete threshold steps to evaluate between
  the bounds. Default is 50.

- min_k:

  Integer. The minimum number of clusters to evaluate during DIANA
  partitioning. Default is 2.

- max_k:

  Integer. The maximum number of clusters to evaluate during DIANA
  partitioning. Default is 10.

## Value

A `data.frame` containing the stability profile with columns:
`Threshold`, `N_Anomalies`, `Opt_k`, `Max_Silhouette`, and
`Hartigans_D`.

## Examples

``` r
set.seed(42)
df <- data.frame(
  timestamp = seq.POSIXt(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
                         by = "hour", length.out = 100),
  x1 = rnorm(100), x2 = rnorm(100)
)
df$x1[85:92] <- df$x1[85:92] + 8
df$x1[93:100] <- df$x1[93:100] - 8
cfg <- tempfile(fileext = ".yaml")
yaml::write_yaml(list(
  ntrees = 20, sample_size = 32, temporal_window_hours = 2,
  anomaly_threshold = 0.55, context_variables = list(),
  target_variables = c("x1", "x2")
), cfg)
eng <- methods::new("DiagnosticEngine", config_path = cfg)
#> DiagnosticEngine initialized. Schema loaded from: file1a731cc4e48d.yaml
eng <- ingest_time_course(eng, df)
#> Time course ingested successfully. Data locked at 100 rows by 3 schema variables.
evaluate_k_stability(eng, max_N = 20, min_N = 10, nb_steps = 3, min_k = 2, max_k = 4)
#> Training static Isolation Forest for stability sweep...
#> Precomputing master topological distance matrix at t_min = 0.5631...
#> Sweeping threshold interval via matrix subsetting...
#>   Threshold N_Anomalies Opt_k Max_Silhouette Hartigans_D Global_LID
#> 1 0.5631221          20     4      0.4281869  0.10789474   3.237505
#> 2 0.5944475          12     2      0.4123939  0.06060606   3.612777
#> 3 0.6257729          10     2      0.4534744  0.06923077   3.171687
#>   Min_Cluster_LID Max_Cluster_LID
#> 1        2.231095        4.813655
#> 2        2.224446        4.306942
#> 3        2.867398        3.881694
unlink(cfg)
```
