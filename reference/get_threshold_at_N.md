# Retrieve Anomaly Score Threshold for a Target Sample Size

Identifies the exact anomaly score threshold required to isolate the top
`n` most anomalous observations. If anomaly scores have not yet been
computed in the `DiagnosticEngine`, a temporary model is trained using
the initialized hyperparameters.

## Usage

``` r
get_threshold_at_N(object, n)

# S4 method for class 'DiagnosticEngine'
get_threshold_at_N(object, n)
```

## Arguments

- object:

  A `DiagnosticEngine` object.

- n:

  Integer. The target number of top anomalies to isolate.

## Value

Numeric. The anomaly score threshold corresponding to the target sample
size.

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
#> DiagnosticEngine initialized. Schema loaded from: file19e94dbef5c4.yaml
eng <- ingest_time_course(eng, df)
#> Time course ingested successfully. Data locked at 100 rows by 3 schema variables.
get_threshold_at_N(eng, n = 12)
#> [1] 0.6089539
unlink(cfg)
```
