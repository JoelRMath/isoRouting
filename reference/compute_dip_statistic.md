# Compute Hartigan's Dip Statistic

Evaluates the pairwise distance distribution of a manifold to test for
structural multimodality (clustering tendency).

## Usage

``` r
compute_dip_statistic(dist_mat)
```

## Arguments

- dist_mat:

  An N x N distance matrix or a native R `dist` object.

## Value

A numeric value representing the dip statistic, or NA_real\_ if the
sample size is too small.

## Examples

``` r
set.seed(42)
# Bimodal synthetic clusters
pts <- rbind(matrix(rnorm(30, mean = 0), ncol = 2),
             matrix(rnorm(30, mean = 5), ncol = 2))
d_mat <- stats::dist(pts)
compute_dip_statistic(d_mat)
#> [1] 0.03982344
```
