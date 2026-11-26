# Get Forensic Ledger

Extracts the ranked anomaly ledger as a flat data.frame.

## Usage

``` r
get_ledger(object)

# S4 method for class 'DiagnosticEngine'
get_ledger(object)
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
#> DiagnosticEngine initialized. Schema loaded from: file1a4f5fbfd6f5.yaml
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
head(get_ledger(eng))
#>    anomaly_cluster cluster_rank topological_distance           timestamp
#> 97       Cluster_1            1                 0.00 2025-01-05 00:00:00
#> 98       Cluster_1            2                 0.10 2025-01-05 01:00:00
#> 96       Cluster_1            3                 0.30 2025-01-04 23:00:00
#> 93       Cluster_1            4                 0.35 2025-01-04 20:00:00
#> 95       Cluster_1            4                 0.35 2025-01-04 22:00:00
#> 99       Cluster_1            6                 0.50 2025-01-05 02:00:00
#>           x1         x2 anomaly_score
#> 97 -9.131739  0.4037749     0.6359882
#> 98 -9.459214  0.5864875     0.6575294
#> 96 -8.860793  1.0857749     0.6288221
#> 93 -7.349651  0.2526117     0.5694206
#> 95 -9.110789 -0.9591704     0.6650226
#> 99 -7.920017  1.8152284     0.6408108
unlink(cfg)
```
