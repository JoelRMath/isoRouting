# Get Topological Medoids

Extracts the unscaled physical anomaly archetypes as a combined
data.frame.

## Usage

``` r
get_medoids(object)

# S4 method for class 'DiagnosticEngine'
get_medoids(object)
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
df$x1[85:92] <- df$x1[85:92] + 8
df$x1[93:100] <- df$x1[93:100] - 8
cfg <- tempfile(fileext = ".yaml")
yaml::write_yaml(list(
  ntrees = 20, sample_size = 32, temporal_window_hours = 2,
  anomaly_threshold = 0.55, context_variables = list(),
  target_variables = c("x1", "x2")
), cfg)
eng <- methods::new("DiagnosticEngine", config_path = cfg)
#> DiagnosticEngine initialized. Schema loaded from: file19e92f660cb4.yaml
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
head(get_medoids(eng))
#>             timestamp        x1         x2   cluster    medoid_timestamp
#> 1 2025-01-04 22:00:00 -9.110789 -0.9591704 Cluster 1 2025-01-05 00:00:00
#> 2 2025-01-04 23:00:00 -8.860793  1.0857749 Cluster 1 2025-01-05 00:00:00
#> 3 2025-01-05 00:00:00 -9.131739  0.4037749 Cluster 1 2025-01-05 00:00:00
#> 4 2025-01-05 01:00:00 -9.459214  0.5864875 Cluster 1 2025-01-05 00:00:00
#> 5 2025-01-05 02:00:00 -7.920017  1.8152284 Cluster 1 2025-01-05 00:00:00
#> 6 2025-01-04 14:00:00  7.782860  0.7007488 Cluster 2 2025-01-04 16:00:00
unlink(cfg)
```
