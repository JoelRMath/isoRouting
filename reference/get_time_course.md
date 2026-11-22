# Get Ingested Time Course

Returns the schema-validated raw time course data.

## Usage

``` r
get_time_course(object)

# S4 method for class 'DiagnosticEngine'
get_time_course(object)
```

## Arguments

- object:

  A DiagnosticEngine object.

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
#> DiagnosticEngine initialized. Schema loaded from: file1a34682c029a.yaml
eng <- ingest_time_course(eng, df)
#> Time course ingested successfully. Data locked at 100 rows by 3 schema variables.
head(get_time_course(eng))
#>             timestamp         x1         x2
#> 1 2025-01-01 00:00:00  1.3709584  1.2009654
#> 2 2025-01-01 01:00:00 -0.5646982  1.0447511
#> 3 2025-01-01 02:00:00  0.3631284 -1.0032086
#> 4 2025-01-01 03:00:00  0.6328626  1.8484819
#> 5 2025-01-01 04:00:00  0.4042683 -0.6667734
#> 6 2025-01-01 05:00:00 -0.1061245  0.1055138
unlink(cfg)
```
