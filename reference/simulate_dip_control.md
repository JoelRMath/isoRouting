# Simulate Control Quantiles for Hartigan's Dip Statistic

Generates Monte Carlo null distribution quantiles for Hartigan's dip
statistic across sample sizes.

## Usage

``` r
simulate_dip_control(object, N, iterations = 200)
```

## Arguments

- object:

  A `DiagnosticEngine` object with an ingested time course.

- N:

  Integer vector of sample sizes to evaluate.

- iterations:

  Integer. Number of Monte Carlo iterations (default 200).

## Value

A `data.frame` containing simulated Hartigan's dip quantiles and score
thresholds across `N`.

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
#> DiagnosticEngine initialized. Schema loaded from: file1a4f203c346f.yaml
eng <- ingest_time_course(eng, df)
#> Time course ingested successfully. Data locked at 100 rows by 3 schema variables.
dip_ctrl <- simulate_dip_control(eng, N = c(10, 20), iterations = 10)
print(dip_ctrl)
#>     N     Median        Q05        Q95 Threshold
#> 99 10 0.08333333 0.06666667 0.11722222 0.5667017
#> 90 20 0.07894737 0.06552632 0.08565789 0.5294994
unlink(cfg)
```
