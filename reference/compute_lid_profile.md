# Compute Local Intrinsic Dimensionality (LID) Profile

Compute Local Intrinsic Dimensionality (LID) Profile

## Usage

``` r
compute_lid_profile(dist_mat, cluster_labels, k = 5, eps = 1e-08)
```

## Arguments

- dist_mat:

  An N x N distance matrix (from isotree topology).

- cluster_labels:

  A numeric or character vector of cluster assignments.

- k:

  Integer. The number of nearest neighbors to evaluate (default: 5).

- eps:

  Numeric. Small constant to prevent log(0) for topological duplicates.

## Value

A list containing Global_LID, Min_Cluster_LID, and Max_Cluster_LID

## Examples

``` r
set.seed(42)
pts <- rbind(matrix(rnorm(30, mean = 0), ncol = 2),
             matrix(rnorm(30, mean = 5), ncol = 2))
d_mat <- as.matrix(stats::dist(pts))
labels <- rep(c(1L, 2L), each = 15)
compute_lid_profile(d_mat, cluster_labels = labels, k = 5)
#> $Global_LID
#> [1] 3.37619
#> 
#> $Min_Cluster_LID
#> [1] 3.154589
#> 
#> $Max_Cluster_LID
#> [1] 3.59779
#> 
```
