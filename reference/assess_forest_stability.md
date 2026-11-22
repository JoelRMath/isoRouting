# Assess Isolation Forest Rank Stability

Conducts a grid search across tree counts and sample sizes to evaluate
the Spearman rank correlation of anomaly scores against a massive
baseline model. Useful for pre-flight hyperparameter tuning.

## Usage

``` r
assess_forest_stability(
  data,
  target_features,
  tree_counts = c(10, 50, 100, 200, 300, 400, 500),
  sample_sizes = c(64, 128, 256, 512),
  baseline_trees = 2000,
  seed = 42
)
```

## Arguments

- data:

  A data.frame containing the numeric features to score.

- target_features:

  Character vector of column names to include in the model.

- tree_counts:

  Numeric vector of tree counts to evaluate (e.g., c(10, 50, 100, 500)).

- sample_sizes:

  Numeric vector of sample sizes to evaluate (e.g., c(64, 128, 256)).

- baseline_trees:

  Integer. The number of trees for the ground-truth baseline model
  (default 2000).

- seed:

  Integer for reproducibility.

## Value

A tidy data.frame containing the Spearman correlation for each parameter
combination.

## Examples

``` r
set.seed(42)
df <- data.frame(x1 = rnorm(100), x2 = rnorm(100))
res <- assess_forest_stability(
  data = df,
  target_features = c("x1", "x2"),
  tree_counts = c(10, 25),
  sample_sizes = c(32, 64),
  baseline_trees = 100,
  seed = 42
)
#> Fitting 100-tree baseline model for ground-truth rankings...
#> Testing 1/4: ntrees = 10, sample_size = 32
#> Testing 2/4: ntrees = 25, sample_size = 32
#> Testing 3/4: ntrees = 10, sample_size = 64
#> Testing 4/4: ntrees = 25, sample_size = 64
#> Stability assessment complete.
print(res)
#>   ntrees sample_size spearman_rho
#> 1     10          32    0.8949714
#> 2     25          32    0.9479856
#> 3     10          64    0.8524640
#> 4     25          64    0.9573357
```
