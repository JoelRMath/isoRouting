# manuscript/R/04_plot_prognostic_drift.R
library(dynaRchetypes)
library(ggplot2)
library(dplyr)
library(patchwork)
library(isotree)

dir.create("manuscript/figures", recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. DATA PREPARATION & SPLITTING
# -----------------------------------------------------------------------------
message("Initializing Engine and evaluating prognostic drift...")
data("hvac_features")
engine <- new("DiagnosticEngine", config_path = "inst/extdata/turin_hvac_config.yaml")
engine <- ingest_telemetry(engine, hvac_features)

# Define exact temporal split (60% Train / 40% Test)
n_total <- nrow(engine@feature_matrix)
n_train <- floor(n_total * 0.6)

train_idx <- 1:n_train
test_idx <- (n_train + 1):n_total

train_matrix <- engine@feature_matrix[train_idx, , drop = FALSE]
test_matrix <- engine@feature_matrix[test_idx, , drop = FALSE]

# -----------------------------------------------------------------------------
# 2. MODELING (THE NEW BASELINE DEFINITION)
# -----------------------------------------------------------------------------
message("Training Historical Model (Past predicting Future)...")
set.seed(42)
model_past <- suppressWarnings(isolation.forest(
  train_matrix, ntrees = 500, sample_size = 256, missing_action = "fail"
))

message("Training Full-Context Baseline (Future predicting Future)...")
set.seed(42)
model_future <- suppressWarnings(isolation.forest(
  test_matrix, ntrees = 500, sample_size = 256, missing_action = "fail"
))

# Score the EXACT SAME future data using both models
df_drift <- data.frame(
  timestamp = engine@raw_telemetry$timestamp[test_idx],
  baseline_score = stats::predict(model_future, test_matrix),
  oos_score = stats::predict(model_past, test_matrix)
)

# -----------------------------------------------------------------------------
# 3. DATA PROCESSING (Ranks)
# -----------------------------------------------------------------------------
# Convert raw scores to normalized percentiles (ECDF equivalent)
df_drift <- df_drift %>%
  mutate(
    base_percentile = percent_rank(baseline_score),
    oos_percentile = percent_rank(oos_score)
  )

# Calculate fit metrics
r_sq_raw <- cor(df_drift$baseline_score, df_drift$oos_score)^2
rho_rank <- cor(df_drift$base_percentile, df_drift$oos_percentile, method = "spearman")

# -----------------------------------------------------------------------------
# 4. VISUALIZATION
# -----------------------------------------------------------------------------
message("Generating two-panel prognostic validation plot...")

# Panel A: Raw Scores
p1 <- ggplot(df_drift, aes(x = baseline_score, y = oos_score)) +
  geom_point(alpha = 0.1, color = "#0072B2", size = 1.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
  geom_smooth(method = "lm", color = "#D55E00", se = FALSE, linewidth = 1.2) +
  annotate(
    "text", x = min(df_drift$baseline_score) + 0.02, y = max(df_drift$oos_score) - 0.02, 
    label = sprintf("R^2 == %.3f", r_sq_raw), parse = TRUE, size = 5, fontface = "bold", hjust = 0
  ) +
  labs(
    title = "A) Raw Anomaly Scores",
    x = "Future-Trained Baseline Score",
    y = "Historical Model Score (Out-of-Sample)"
  ) +
  theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))

# Panel B: Normalized Ranks (Percentiles)
p2 <- ggplot(df_drift, aes(x = base_percentile, y = oos_percentile)) +
  geom_point(alpha = 0.1, color = "#009E73", size = 1.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
  annotate(
    "text", x = 0.05, y = 0.95, 
    label = sprintf("rho == %.3f", rho_rank), parse = TRUE, size = 5, fontface = "bold", hjust = 0
  ) +
  labs(
    title = "B) Normalized Triage Rank (Percentile)",
    x = "Future-Trained Baseline Percentile",
    y = "Historical Model Percentile"
  ) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))

# -----------------------------------------------------------------------------
# 5. STITCH AND EXPORT
# -----------------------------------------------------------------------------
fig4 <- p1 | p2

output_path <- "manuscript/figures/fig4_prognostic_drift.pdf"
ggsave(filename = output_path, plot = fig4, width = 12, height = 6, device = "pdf")
output_png <- "manuscript/figures/fig4_prognostic_drift.png"
ggsave(filename = output_png, plot = fig4, width = 12, height = 6, device = "png")

message(sprintf("Figure saved to %s", output_path))

# -----------------------------------------------------------------------------
# 6. TAIL INTERSECTION VALIDATION
# -----------------------------------------------------------------------------
# Determine how many anomalies the 0.65 threshold yields in the future-trained model
threshold_val <- 0.65
n_anomalies <- sum(df_drift$baseline_score >= threshold_val)

if (n_anomalies > 0) {
  # Extract the exact timestamps of the top N anomalies for both models
  base_anomalies <- df_drift %>% 
    arrange(desc(baseline_score)) %>% 
    slice(1:n_anomalies) %>% 
    pull(timestamp)
  
  oos_anomalies <- df_drift %>% 
    arrange(desc(oos_score)) %>% 
    slice(1:n_anomalies) %>% 
    pull(timestamp)
  
  # Calculate the exact intersection
  overlap <- length(intersect(base_anomalies, oos_anomalies))
  overlap_pct <- (overlap / n_anomalies) * 100
  
  message(sprintf("Top %d Anomaly Overlap: %.1f%%", n_anomalies, overlap_pct))
} else {
  message("No anomalies found above threshold in the baseline future model.")
}