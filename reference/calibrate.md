# Calibrate Cluster Integrity

Iterates through k-values and calculates global silhouette to locate the
structural fracture point.

## Usage

``` r
calibrate(object, min_k = 2, max_k = 8)

# S4 method for class 'DiagnosticEngine'
calibrate(object, min_k = 2, max_k = 8)
```

## Arguments

- object:

  A DiagnosticEngine object.

- min_k:

  Integer. Minimum number of clusters to test. Default is 2.

- max_k:

  Integer. Maximum number of clusters to test. Default is 8.

## Value

An updated DiagnosticEngine object with k_metrics populated.

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
#> DiagnosticEngine initialized. Schema loaded from: file1a4f2c65c426.yaml
eng <- ingest_time_course(eng, df)
#> Time course ingested successfully. Data locked at 100 rows by 3 schema variables.
eng <- extract_anomalies(eng, threshold = 0.55)
#> YAML threshold overridden. Using runtime threshold: 0.55
#> Training Isolation Forest (ntrees=20, sample_size=32) and scoring feature space...
#> Anomaly extraction complete. 22 extreme events isolated (Score >= 0.55).
head(get_scored_time_course(eng))
#>             timestamp         x1         x2 anomaly_score
#> 1 2025-01-01 00:00:00  1.3709584  1.2009654     0.4642434
#> 2 2025-01-01 01:00:00 -0.5646982  1.0447511     0.4270152
#> 3 2025-01-01 02:00:00  0.3631284 -1.0032086     0.4365019
#> 4 2025-01-01 03:00:00  0.6328626  1.8484819     0.5580022
#> 5 2025-01-01 04:00:00  0.4042683 -0.6667734     0.3888140
#> 6 2025-01-01 05:00:00 -0.1061245  0.1055138     0.3592879
eng <- compute_distances(eng)
#> Extracting terminal node topology for 22 isolated events...
#> Computing topological Hamming distance proximity matrix...
#> Constructing DIANA hierarchical tree from forest topology...
#> Distance computation complete.
eng <- calibrate(eng, min_k = 2, max_k = 4)
#> Calibrating cluster integrity from k=2 to k=4 using Global Silhouette...
#> Calibration complete. Mathematical recommendation based on maximum Global Silhouette: k=4
unlink(cfg)
```
