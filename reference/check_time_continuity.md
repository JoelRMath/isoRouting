# Detect gaps or irregular intervals in a time series

Detect gaps or irregular intervals in a time series

## Usage

``` r
check_time_continuity(time_vector, tolerance = 0)
```

## Arguments

- time_vector:

  A POSIXct vector of timestamps.

- tolerance:

  Numeric. Allowable variance in seconds (default 0).

## Value

A list containing a boolean flag and a summary of intervals.

## Examples

``` r
ts <- seq.POSIXt(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
                 by = "hour", length.out = 10)
check_time_continuity(ts)
#> $is_continuous
#> [1] TRUE
#> 
#> $expected_interval_secs
#> [1] 3600
#> 
#> $gap_indices
#> integer(0)
#> 
#> $summary
#> time_diffs
#> 3600 
#>    9 
#> 

# Introduce a 5-hour gap
ts_gap <- ts[-c(5:8)]
check_time_continuity(ts_gap)
#> $is_continuous
#> [1] FALSE
#> 
#> $expected_interval_secs
#> [1] 3600
#> 
#> $gap_indices
#> [1] 4
#> 
#> $summary
#> time_diffs
#>  3600 18000 
#>     4     1 
#> 
```
