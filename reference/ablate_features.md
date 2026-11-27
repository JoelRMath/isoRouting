# Ablate Features for Medoid Interpretation

Performs Leave-One-Covariate-Out (LOCO) ablation on the extracted
medoids. By sequentially replacing each target variable with its global
median, it measures the corresponding drop in anomaly score to rank
feature importance.

## Usage

``` r
ablate_features(object)

# S4 method for class 'DiagnosticEngine'
ablate_features(object)
```

## Arguments

- object:

  A DiagnosticEngine object (must have extracted medoids).

## Value

A data.frame containing the ranked feature contributions for each
archetype.

## Examples

``` r
set.seed(42)
df <- data.frame(
  timestamp = seq.POSIXt(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
                         by = "hour", length.out = 100),
  x1 = rnorm(100), x2 = rnorm(100)
)
df$x1[85:92] <- df$x1[85:92] + 8
df$x1[93:100] <- df$x1[93:100] - 8
cfg <- tempfile(fileext = ".yaml")
yaml::write_yaml(list(
  ntrees = 20, sample_size = 32, temporal_window_hours = 2,
  anomaly_threshold = 0.55, context_variables = list(),
  target_variables = c("x1", "x2")
), cfg)
eng <- methods::new("DiagnosticEngine", config_path = cfg)
#> DiagnosticEngine initialized. Schema loaded from: file19e974703e33.yaml
eng <- ingest_time_course(eng, df)
#> Time course ingested successfully. Data locked at 100 rows by 3 schema variables.
eng <- extract_medoids(eng, N = 14, k = 2)
#> Mapped N = 14 to anomaly threshold = 0.5694
#> Slicing feature space at threshold = 0.5694...
#> YAML threshold overridden. Using runtime threshold: 0.56942060984756
#> Training Isolation Forest (ntrees=20, sample_size=32) and scoring feature space...
#> Anomaly extraction complete. 14 extreme events isolated (Score >= 0.56942060984756).
#> Extracting terminal node topology for 14 isolated events...
#> Computing topological Hamming distance proximity matrix...
#> Constructing DIANA hierarchical tree from forest topology...
#> Distance computation complete.
#> Calibrating cluster integrity from k=2 to k=8 using Global Silhouette...
#> Calibration complete. Mathematical recommendation based on maximum Global Silhouette: k=2
#> Extracting topological medoids for k = 2...
#> Medoid extraction complete. Archetypes and cluster radii saved.
eng <- commit_ledger(eng, k = 2)
#> Committing ranked forensic ledger for k=2 anomaly archetypes...
#> Ledger successfully committed and ranked. Call `get_ledger(engine)` to export.
#> Empirical event horizons locked for predictive routing.
ablate_features(eng)
#> Computing global medians for LOCO ablation...
#> Scoring baseline medoid peaks...
#> Ablating 2 engineered targets...
#> Ablation complete. Features ranked by anomaly score drop.
#>     cluster variable score_drop rank
#> 1 Cluster 1       x1 0.25093867    1
#> 2 Cluster 1       x2 0.01974564    2
#> 3 Cluster 2       x1 0.22428297    1
#> 4 Cluster 2       x2 0.02684890    2
unlink(cfg)
```
