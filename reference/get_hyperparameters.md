# Get Engine Hyperparameters

Returns the machine learning parameters parsed from the configuration.

## Usage

``` r
get_hyperparameters(object)

# S4 method for class 'DiagnosticEngine'
get_hyperparameters(object)
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
#> DiagnosticEngine initialized. Schema loaded from: file1a4f1923a15b.yaml
get_hyperparameters(eng)
#> $anomaly_threshold
#> [1] 0.55
#> 
#> $temporal_window_hours
#> [1] 2
#> 
#> $sample_size
#> [1] 32
#> 
#> $ntrees
#> [1] 20
#> 
unlink(cfg)
```
