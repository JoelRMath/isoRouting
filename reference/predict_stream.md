# Predict Streaming Data

Routes new multivariate time course data into established historical
medoids or flags them as novel anomalies based on terminal node
proximity.

## Usage

``` r
predict_stream(object, new_data)

# S4 method for class 'DiagnosticEngine'
predict_stream(object, new_data)
```

## Arguments

- object:

  A DiagnosticEngine object (must be fully calibrated with medoids
  extracted).

- new_data:

  A data.frame of new observations.

## Value

A data.frame containing the anomaly scores, assigned clusters, and
proximity metrics.

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
  anomaly_threshold = 0.50, context_variables = list(),
  target_variables = c("x1", "x2")
), cfg)
eng <- methods::new("DiagnosticEngine", config_path = cfg)
#> DiagnosticEngine initialized. Schema loaded from: file1a7334d32537.yaml
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

# New stream with a novel anomaly mode along x2
new_df <- data.frame(
  timestamp = seq.POSIXt(as.POSIXct("2025-01-06 00:00:00", tz = "UTC"),
                         by = "hour", length.out = 30),
  x1 = rnorm(30), x2 = rnorm(30)
)
new_df$x2[20:28] <- new_df$x2[20:28] + 10

routed <- predict_stream(eng, new_df)
#> Ingesting 30 new streaming observations...
#> Extracting terminal nodes and computing topological distances...
#> Routing data using empirical topology radii...
#> Routing complete. Return dataframe contains anomaly scores and predicted physical states.
table(routed$predicted_state)
#> 
#> Background  Cluster 1 
#>         10         20 
unlink(cfg)
```
