# Explore Anomaly Thresholds

Rapidly scores the feature space and returns a mapping of anomaly
thresholds to the number of extracted events.

## Usage

``` r
explore_thresholds(
  object,
  thresholds = seq(0.5, 0.8, by = 0.01),
  ntrees = NULL,
  sample_size = NULL
)

# S4 method for class 'DiagnosticEngine'
explore_thresholds(
  object,
  thresholds = seq(0.5, 0.8, by = 0.01),
  ntrees = NULL,
  sample_size = NULL
)
```

## Arguments

- object:

  A DiagnosticEngine object.

- thresholds:

  Numeric vector of thresholds to test.

- ntrees:

  Integer. Overrides YAML if provided.

- sample_size:

  Integer. Overrides YAML if provided.

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
#> DiagnosticEngine initialized. Schema loaded from: file19e92c561558.yaml
eng <- ingest_time_course(eng, df)
#> Time course ingested successfully. Data locked at 100 rows by 3 schema variables.
explore_thresholds(eng, thresholds = c(0.50, 0.55, 0.60))
#> Scoring feature space to build threshold curve...
#>   Threshold Event_Count
#> 1      0.50          26
#> 2      0.55          22
#> 3      0.60          12
unlink(cfg)
```
