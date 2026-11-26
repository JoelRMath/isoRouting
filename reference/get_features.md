# Get Feature Schema

Returns the target and context variables defined for the engine.

## Usage

``` r
get_features(object)

# S4 method for class 'DiagnosticEngine'
get_features(object)
```

## Arguments

- object:

  A DiagnosticEngine object.

## Examples

``` r
cfg <- tempfile(fileext = ".yaml")
yaml::write_yaml(list(
  ntrees = 20, sample_size = 32, temporal_window_hours = 2,
  anomaly_threshold = 0.55, context_variables = list(),
  target_variables = c("x1", "x2")
), cfg)
eng <- methods::new("DiagnosticEngine", config_path = cfg)
#> DiagnosticEngine initialized. Schema loaded from: file1a7328978281.yaml
get_features(eng)
#> $target_variables
#> [1] "x1" "x2"
#> 
#> $context_variables
#> character(0)
#> 
unlink(cfg)
```
