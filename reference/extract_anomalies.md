# Extract Anomalies via Isolation Forest

Scores the feature matrix and extracts extreme events exceeding the
specified threshold.

## Usage

``` r
extract_anomalies(object, threshold)

# S4 method for class 'DiagnosticEngine'
extract_anomalies(object, threshold)
```

## Arguments

- object:

  A DiagnosticEngine object.

- threshold:

  Numeric limit for empirical p-values (e.g., 0.60).

## Value

An updated DiagnosticEngine object with the anomalies slot populated.

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
#> DiagnosticEngine initialized. Schema loaded from: file1a4fb4287be.yaml
eng <- ingest_time_course(eng, df)
#> Time course ingested successfully. Data locked at 100 rows by 3 schema variables.
eng <- extract_anomalies(eng, threshold = 0.55)
#> YAML threshold overridden. Using runtime threshold: 0.55
#> Training Isolation Forest (ntrees=20, sample_size=32) and scoring feature space...
#> Anomaly extraction complete. 22 extreme events isolated (Score >= 0.55).
unlink(cfg)
```
