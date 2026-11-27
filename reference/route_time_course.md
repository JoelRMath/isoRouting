# Route Full Ingested Time Course via Cluster Radii

Routes all observations in the ingested time course (including
sub-threshold points) to their closest topological medoid if they fall
within that cluster's empirical radius.

## Usage

``` r
route_time_course(object)

# S4 method for class 'DiagnosticEngine'
route_time_course(object)
```

## Arguments

- object:

  A DiagnosticEngine object with extracted medoids and cluster radii.

## Value

A data.frame containing the scored time course along with
`closest_archetype`, `distance_to_archetype`, and `archetype_bucket`.

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
#> DiagnosticEngine initialized. Schema loaded from: file19e93301989a.yaml
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
head(route_time_course(eng))
#> Extracting terminal nodes and computing topological distances for full time course...
#>             timestamp         x1         x2 anomaly_score closest_archetype
#> 1 2025-01-01 00:00:00  1.3709584  1.2009654     0.4642434         Cluster 1
#> 2 2025-01-01 01:00:00 -0.5646982  1.0447511     0.4270152         Cluster 1
#> 3 2025-01-01 02:00:00  0.3631284 -1.0032086     0.4365019         Cluster 2
#> 4 2025-01-01 03:00:00  0.6328626  1.8484819     0.5580022         Cluster 1
#> 5 2025-01-01 04:00:00  0.4042683 -0.6667734     0.3888140         Cluster 2
#> 6 2025-01-01 05:00:00 -0.1061245  0.1055138     0.3592879         Cluster 1
#>   distance_to_archetype archetype_bucket
#> 1                  0.95        Cluster 1
#> 2                  0.95        Cluster 1
#> 3                  0.90       Background
#> 4                  0.95        Cluster 1
#> 5                  0.85       Background
#> 6                  0.95        Cluster 1
unlink(cfg)
```
