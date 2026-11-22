# Ingest Time Course Data

Ingests raw multivariate time series and applies dynamic feature
engineering as defined by the engine's YAML schema. Includes optional
temporal continuity enforcement.

## Usage

``` r
ingest_time_course(object, data, tolerance = NULL)

# S4 method for class 'DiagnosticEngine'
ingest_time_course(object, data, tolerance = NULL)
```

## Arguments

- object:

  A DiagnosticEngine object.

- data:

  A data.frame containing the raw time course.

- tolerance:

  Numeric. Allowable variance in seconds between timestamps. If NULL,
  enforcement is skipped.

## Value

An updated DiagnosticEngine object with the feature_matrix populated.

## Examples

``` r
set.seed(42)
df <- data.frame(
  timestamp = seq.POSIXt(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
                         by = "hour", length.out = 100),
  x1 = rnorm(100), x2 = rnorm(100)
)
cfg <- tempfile(fileext = ".yaml")
yaml::write_yaml(list(
  ntrees = 20, sample_size = 32, temporal_window_hours = 2,
  anomaly_threshold = 0.55, context_variables = list(),
  target_variables = c("x1", "x2")
), cfg)
eng <- methods::new("DiagnosticEngine", config_path = cfg)
#> DiagnosticEngine initialized. Schema loaded from: file1a347d1f70b5.yaml
eng <- ingest_time_course(eng, df)
#> Time course ingested successfully. Data locked at 100 rows by 3 schema variables.
unlink(cfg)
```
