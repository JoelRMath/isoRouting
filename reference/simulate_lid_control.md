# Simulate Control Quantiles for Local Intrinsic Dimensionality (LID)

Generates Monte Carlo null distribution quantiles for Local Intrinsic
Dimensionality across sample sizes.

## Usage

``` r
simulate_lid_control(engine, N, iterations = 200, k = 5)
```

## Arguments

- engine:

  A `DiagnosticEngine` object with an ingested time course.

- N:

  Integer vector of sample sizes to evaluate.

- iterations:

  Integer. Number of Monte Carlo iterations (default 200).

- k:

  Integer. Number of nearest neighbors for LID estimation (default 5).

## Value

A `data.frame` containing simulated LID quantiles and score thresholds
across `N`.

## Examples

``` r
set.seed(42)
df <- data.frame(
  timestamp = seq.POSIXt(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
                         by = "hour", length.out = 100),
  x1 = rnorm(100),
  x2 = rnorm(100)
)
cfg <- tempfile(fileext = ".yaml")
yaml::write_yaml(list(
  ntrees = 20,
  sample_size = 32,
  temporal_window_hours = 2,
  anomaly_threshold = 0.60,
  context_variables = list(),
  target_variables = c("x1", "x2")
), cfg)

eng <- methods::new("DiagnosticEngine", config_path = cfg)
#> DiagnosticEngine initialized. Schema loaded from: file1a4f52b21642.yaml
eng <- ingest_time_course(eng, df)
#> Time course ingested successfully. Data locked at 100 rows by 3 schema variables.
lid_ctrl <- simulate_lid_control(eng, N = c(10, 20), iterations = 10, k = 5)
print(lid_ctrl)
#>     N   Median      Q05      Q25      Q50      Q75       Q95 Threshold
#> 99 10 7.171225 5.696036 6.477057 7.171225 9.192844 14.155150 0.5667017
#> 90 20 6.135869 5.165702 5.390238 6.135869 7.079546  8.845529 0.5294994
unlink(cfg)
```
